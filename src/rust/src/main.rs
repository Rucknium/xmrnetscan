
// MIT License
// 
// Copyright (c) 2025 Boog900
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Forked from https://github.com/Boog900/p2p-proxy-checker


//! # Monero network crawler
//!
//! A simple tool to find all the reachable nodes on the Monero P2P network. It works by recursively connecting to
//! every peer we are told about in a peer list message, starting by connecting to the seed nodes.

use dashmap::DashSet;
use futures::{future::BoxFuture, stream, FutureExt};
// use std::net::{Ipv4Addr, SocketAddrV4};
use std::{
    collections::HashSet,
    convert::Infallible,
    fs::OpenOptions,
    io::Write,
    net::SocketAddr, // IpAddr,
    sync::{LazyLock, OnceLock},
    task::Poll,
    time::Duration,
};
use tokio::{
    sync::{mpsc, Semaphore},
    time::timeout, // sleep,
};
use tower::{make::Shared, util::MapErr, Service, ServiceExt};
use tracing::error;
use tracing_subscriber::{filter::LevelFilter, FmtSubscriber};

use cuprate_p2p_core::{
    client::{
        handshaker::builder::{DummyCoreSyncSvc, DummyProtocolRequestHandler},
        ConnectRequest, Connector, HandshakerBuilder, InternalPeerID,
    },
    services::{AddressBookRequest, AddressBookResponse},
    BroadcastMessage, ClearNet, NetZoneAddress, Network, NetworkZone, PeerRequest, PeerResponse,
};
use cuprate_wire::{
    common::PeerSupportFlags, AdminRequestMessage, AdminResponseMessage, BasicNodeData,
};

use clap::Parser;
// use tokio_sqlite::Connection;

use sqlx::{
    Connection,
    SqliteConnection
};

/// A simple tool to find all the reachable nodes on the Monero P2P network. It works by recursively connecting to
/// every peer we are told about in a peer list message, starting by connecting to the seed nodes.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
   /// Collect peer lists provided by other nodes and write to handshake_data.txt
   #[arg(short, long)] // Defines -c/--collect-peer-lists flag
   collect_peer_lists: bool,
}

/// A set of all node's [`SocketAddr`] that we have successfully connected to.
static SCANNED_NODES: LazyLock<DashSet<SocketAddr>> = LazyLock::new(|| DashSet::new());

/// The [`Connector`] service to make outbound connections to nodes.
static CONNECTOR: OnceLock<
    Connector<
        ClearNet,
        AddressBookService,
        DummyCoreSyncSvc,
        MapErr<Shared<DummyProtocolRequestHandler>, fn(Infallible) -> tower::BoxError>,
        fn(InternalPeerID<<ClearNet as NetworkZone>::Addr>) -> stream::Pending<BroadcastMessage>,
    >,
> = OnceLock::new();

/// The channel that is used to communicate a successful connection.
static BAD_PEERS_CHANNEL: OnceLock<mpsc::Sender<(SocketAddr, Vec<u64>, bool)>> = OnceLock::new();

/// A [`Semaphore`] to limit the amount of concurrent connection attempts so we don't overrun ourself.
static CONNECTION_SEMAPHORE: OnceLock<Semaphore> = OnceLock::new();


struct HandshakeAttempt {
    connected_node: String,
}

struct HandshakeData {
    connected_node: String,
    rpc_port: u16,
    pruning_seed: String,
    peer_id: i64,
    support_flags: String,
    core_sync_data: String,
    my_port: u32
}

struct Peerlists {
    connected_node: String,
    peerlist: String,
}


#[tokio::main(flavor = "multi_thread")]
async fn main() {

    let mut conn = SqliteConnection::connect("crawler-netscan.db").await.unwrap();

    // conn.execute(
    //     "PRAGMA busy_timeout=60000",
    //     [],
    // )
    // .await
    // .unwrap();
    // Set DB write timeout to 60 seconds to prevent "database is locked" errors:
    // https://github.com/launchbadge/sqlx/issues/451

    sqlx::query(
        "CREATE TABLE handshake_attempts (
            connected_node  TEXT
        )")
        .execute(&mut conn)
        .await
        .unwrap();

    sqlx::query(
        "CREATE TABLE handshake_data (
            connected_node  TEXT,
            rpc_port        INTEGER,
            pruning_seed    TEXT,
            peer_id         BIGINT,
            support_flags   TEXT,
            core_sync_data  TEXT,
            my_port         INTEGER
        )")
        .execute(&mut conn)
        .await
        .unwrap();

    sqlx::query(
        "CREATE TABLE peerlists (
            connected_node  TEXT,
            peerlist        TEXT
        )")
        .execute(&mut conn)
        .await
        .unwrap();

    // If collecting peer lists, use fewer threads because of DB write locks
    let n_semaphore_permits: usize = match Cli::parse().collect_peer_lists {
        true => 10,
        false => 100
    };
    let _ = CONNECTION_SEMAPHORE.set(Semaphore::new(n_semaphore_permits));
    // Linter says that the result should be captured, but ignored by "_"
    
    FmtSubscriber::builder()
        .with_max_level(LevelFilter::DEBUG)
        .init();

    let handshaker = HandshakerBuilder::<ClearNet>::new(BasicNodeData {
        my_port: 0,
        network_id: Network::Mainnet.network_id(),
        peer_id: rand::random(),
        support_flags: PeerSupportFlags::FLUFFY_BLOCKS,
        rpc_port: 0,
        rpc_credits_per_hash: 0,
    })
    .with_address_book(AddressBookService)
    .build();

    // Create and set the CONNECTOR global.
    let connector = Connector::new(handshaker);

    let _ = CONNECTOR.get_or_init(|| connector.clone());

    // Create and set the BAD_PEERS_CHANNEL global.
    let (bad_peers_tx, mut bad_peers_rx) = mpsc::channel(508);

    BAD_PEERS_CHANNEL.set(bad_peers_tx).unwrap();

    // seed nodes
    [
        "176.9.0.187:18080",
        "88.198.163.90:18080",
        "66.85.74.134:18080",
        "51.79.173.165:18080",
        "192.99.8.110:18080",
        "37.187.74.171:18080",
        "77.172.183.193:18080",
    ]
    .into_iter()
    .for_each(|ip| {
        tokio::spawn(check_node(ip.parse().unwrap()));
    });

    let mut bad_peers = HashSet::new();
    let mut bad_peers_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("bad_peers.txt")
        .unwrap();

    let mut good_peers_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("good_peers.txt")
        .unwrap();

    loop {
        let (peer, peer_ids, peer_bad) = bad_peers_rx.recv().await.unwrap();

        if peer_bad {
            error!("Found bad peer: {peer:?}");
            if !bad_peers.insert(peer) {
                continue;
            }

            bad_peers_file
                .write_fmt(format_args!("peer: {peer:?}, peer_ids: {peer_ids:?}, \n"))
                .unwrap();
        } else {
            good_peers_file
                .write_fmt(format_args!("peer: {peer:?}, peer_ids: {peer_ids:?}, \n"))
                .unwrap();
        }
    }
}

/// Check a node is reachable, sending the address down the [`BAD_PEERS_CHANNEL`] if it is.
async fn check_node(addr: SocketAddr) -> Result<(), tower::BoxError> {
    // Acquire a semaphore permit.
    let _guard = CONNECTION_SEMAPHORE.get().unwrap().acquire().await.unwrap();
    
    if Cli::parse().collect_peer_lists {

        let handshake_attempt = HandshakeAttempt {
            connected_node: addr.to_string()
        };

        let mut conn = SqliteConnection::connect("crawler-netscan.db").await.unwrap();

        sqlx::query(
        "INSERT INTO handshake_attempts (connected_node) VALUES ($1)")
        .bind(handshake_attempt.connected_node)
        .execute(&mut conn)
        .await
        .unwrap();

    }

    // Grab the connector from the `CONNECTOR` global
    let mut connector = CONNECTOR.get().unwrap().clone();

    let mut client = timeout(
        Duration::from_secs(5),
        connector
            .ready()
            .await?
            .call(ConnectRequest { addr, permit: None }),
    )
    .await??;


    if Cli::parse().collect_peer_lists {

        let rpc_port = client.info.basic_node_data.rpc_port;
        let pruning_seed = client.info.pruning_seed;
        let peer_id = client.info.basic_node_data.peer_id;
        let support_flags = client.info.basic_node_data.support_flags;
        let core_sync_data = client.info.core_sync_data.clone();
        let my_port = client.info.basic_node_data.my_port;

        let handshake_data = HandshakeData {
            connected_node: addr.to_string(),
            rpc_port: rpc_port,
            pruning_seed: format_args!("{pruning_seed:?}").to_string(),
            peer_id: peer_id as i64,
            support_flags: format_args!("{support_flags:?}").to_string(),
            core_sync_data: format_args!("{core_sync_data:?}").to_string(),
            my_port: my_port
        };

        let mut conn = SqliteConnection::connect("crawler-netscan.db").await.unwrap();

        sqlx::query(
        "INSERT INTO handshake_data (connected_node, rpc_port, pruning_seed, peer_id, support_flags, core_sync_data, my_port) VALUES ($1, $2, $3, $4, $5, $6, $7)")
        .bind(handshake_data.connected_node)
        .bind(handshake_data.rpc_port)
        .bind(handshake_data.pruning_seed)
        .bind(handshake_data.peer_id)
        .bind(handshake_data.support_flags)
        .bind(handshake_data.core_sync_data)
        .bind(handshake_data.my_port)
        .execute(&mut conn)
        .await
        .unwrap();

    }


    let PeerResponse::Admin(AdminResponseMessage::Ping(ping)) = client
        .ready()
        .await?
        .call(PeerRequest::Admin(AdminRequestMessage::Ping))
        .await?
    else {
        unreachable!();
    };

    let PeerResponse::Admin(AdminResponseMessage::Ping(ping_2)) = client
        .ready()
        .await?
        .call(PeerRequest::Admin(AdminRequestMessage::Ping))
        .await?
    else {
        unreachable!();
    };

    let PeerResponse::Admin(AdminResponseMessage::Ping(ping_3)) = client
        .ready()
        .await?
        .call(PeerRequest::Admin(AdminRequestMessage::Ping))
        .await?
    else {
        unreachable!();
    };

    let peer_ids = vec![
        client.info.basic_node_data.peer_id,
        ping.peer_id,
        ping_2.peer_id,
        ping_3.peer_id,
    ];
    let bad = client.info.basic_node_data.peer_id != ping.peer_id
        || ping.peer_id != ping_2.peer_id
        || ping_2.peer_id != ping_3.peer_id;

    BAD_PEERS_CHANNEL
        .get()
        .unwrap()
        .send((addr, peer_ids, bad))
        .await?;

    Ok(())
}

/// An address book service used in the [`CONNECTOR`] that just calls [`check_node`] on each peer in an
/// incoming peer list and does not actually track peer addresses.
#[derive(Clone)]
pub struct AddressBookService;

impl Service<AddressBookRequest<ClearNet>> for AddressBookService {
    type Error = tower::BoxError;
    type Response = AddressBookResponse<ClearNet>;
    type Future = BoxFuture<'static, Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, _: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: AddressBookRequest<ClearNet>) -> Self::Future {
        async {
            match req {
                AddressBookRequest::IncomingPeerList(internal_peer_id, peers) => {

                    if Cli::parse().collect_peer_lists {

                        let peer_adr_canon: Vec<SocketAddr> = peers.into_iter().map(|mut e| {
                            e.adr.make_canonical();
                            e.adr
                            }).collect();

                        let peerlists = Peerlists {
                            connected_node: format_args!("{internal_peer_id:?}").to_string(),
                            peerlist: format_args!("{peer_adr_canon:?}").to_string()
                        };

                        let mut conn = SqliteConnection::connect("crawler-netscan.db").await.unwrap();

                        sqlx::query(
                            "INSERT INTO peerlists (connected_node, peerlist) VALUES ($1, $2)")
                            .bind(peerlists.connected_node)
                            .bind(peerlists.peerlist)
                            .execute(&mut conn)
                            .await
                            .unwrap();

                        for peer in peer_adr_canon {
                            if SCANNED_NODES.insert(peer) {
                                tokio::spawn(async move {
                                    if check_node(peer).await.is_err() {
                                        SCANNED_NODES.remove(&peer);
                                    }
                                });
                            }
                        }
                    } else {
                    
                      for mut peer in peers {
                          peer.adr.make_canonical();
                          if SCANNED_NODES.insert(peer.adr) {
                              tokio::spawn(async move {
                                  if check_node(peer.adr).await.is_err() {
                                      SCANNED_NODES.remove(&peer.adr);
                                  }
                              });
                          }
                       }
                    }

                    Ok(AddressBookResponse::Ok)
                }
                AddressBookRequest::NewConnection { .. } => Ok(AddressBookResponse::Ok),
                AddressBookRequest::TakeRandomWhitePeer { .. } => Err("no peers".into()),
                AddressBookRequest::TakeRandomGrayPeer { .. } => Err("no peers".into()),
                AddressBookRequest::TakeRandomPeer { .. } => Err("no peers".into()),
                AddressBookRequest::GetWhitePeers(_) => Ok(AddressBookResponse::Peers(vec![])),
                AddressBookRequest::PeerlistSize => Err("no peers".into()),
                AddressBookRequest::ConnectionCount => Err("no peers".into()),
                AddressBookRequest::SetBan(_) => Err("no peers".into()),
                AddressBookRequest::GetBan(_) => Err("no peers".into()),
                AddressBookRequest::GetBans => Err("no peers".into()),
                AddressBookRequest::ConnectionInfo => Err("no peers".into()),
            }
        }
        .boxed()
    }
}
