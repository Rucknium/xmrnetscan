

probe.rpc <- function(rpc.urls) {
  
  handle <- RCurl::getCurlHandle()
  
  json.post.get_info <- RJSONIO::toJSON(
    list(
      jsonrpc = "2.0",
      id = "0",
      method = "get_info",
      params = ""
    )
  )
  
  get_info <- function(rpc.url, timeout = 5) {
    RJSONIO::fromJSON(
      RCurl::postForm(paste0(rpc.url, "/json_rpc"),
        .opts = list(
          userpwd = "",
          postfields = json.post.get_info,
          httpheader = c('Content-Type' = 'application/json', Accept = 'application/json'),
          timeout = timeout # In seconds
        ),
        curl = handle
      ), asText = TRUE
    )
  }
  
  if (.Platform$OS.type == "unix" & .Platform$GUI != "RStudio") {
    # Windows and RStudio do not support multicore
    future::plan(future::multicore, workers = 2 * parallelly::availableCores())
    on.exit(future::plan(future::sequential))
  } else {
    future::plan(future::sequential)
  }
  
  future.apply::future_sapply(rpc.urls, function(x) {
    response <- tryCatch(get_info(x), error = function(e) NULL)
    length(response) > 0
  }, USE.NAMES = FALSE, future.globals = c("get_info", "json.post.get_info", "handle"))
  
}



get.ditatompel.domains <- function() {
  
  ditatompel.nodes <- tryCatch(
    RJSONIO::fromJSON(
      RCurl::getForm("https://xmr.ditatompel.com/api/v1/nodes",
        nettype = "mainnet", limit = "10000"
      ), asText = TRUE
    ),
    error = function(e) NULL)
  
  if (length(ditatompel.nodes) == 0) {
    return(data.table::data.table(connected_node_ip = NA_character_, rpc_domain = NA_character_))
  }
  
  ditatompel.nodes <- data.table::rbindlist(lapply(ditatompel.nodes$data$items, FUN = function(x)  {
    data.table::data.table(connected_node_ip = strsplit(x$ip_addresses, ",")[[1]], rpc_domain = x$hostname) 
  }), fill = TRUE)
  # The [[1]] means that if the string is empty, this will give a NULL. Therefore, the
  # record will be skipped
  
  ditatompel.nodes[ is.na(IP::ip(rpc_domain)), ]
  # If hostname is an IP address, then exclude it
  
}


process.raw.data <- function(scan.dir, data.date, confirm.rpc = TRUE, get.domains = TRUE) {
  
  # blocklist.moneropulse.se
  # dig @1.0.0.1 blocklist.moneropulse.se. TXT
  # https://www.nslookup.io/domains/blocklist.moneropulse.se/dns-records/
  # TODO: Make this a data()
  
  dns.blocklist <- c("23.88.126.42;23.88.34.81;23.88.34.86;23.88.35.29;23.88.36.64;23.88.37.110;23.88.38.196;23.88.39.157;23.88.40.186;23.88.40.60;23.88.41.0;23.88.41.111;23.88.42.188;23.88.43.143;49.12.10.221",
    "49.12.228.133;49.12.228.14;49.12.228.5;49.12.228.6;49.12.228.7;49.12.228.8;49.12.228.9;49.12.239.116;49.12.239.155;49.12.239.156;49.12.239.157;49.12.239.158;49.12.239.159;49.12.239.160;49.12.239.161",
    "159.65.28.9;159.65.82.164;161.35.59.213;161.35.88.140;161.35.90.212;162.218.65.0/24;165.22.10.49;165.22.10.5;165.22.12.133;165.22.15.144;165.22.2.129;165.22.2.201;165.22.2.255;165.22.2.48;165.22.8.167",
    "23.88.124.126;23.88.124.135;23.88.124.161;23.88.124.171;23.88.124.173;23.88.124.246;23.88.124.94;23.88.125.242;23.88.125.254;23.88.125.49;23.88.125.83;23.88.126.104;23.88.126.19;23.88.126.25;23.88.126.26",
    "65.108.49.24;65.108.49.82;65.108.50.7;65.108.51.229;65.108.56.225;65.108.58.135;65.108.60.175;65.108.61.145;65.108.80.101;65.108.80.216;65.108.81.118;65.108.81.124;65.108.81.125;65.108.83.46;65.108.85.204",
    "65.108.90.194;65.108.90.196;65.108.93.105;65.108.93.131;65.108.93.73;65.21.186.59;68.183.111.36;68.183.16.126;68.183.96.163;68.183.96.21;68.183.96.59;78.46.142.161;78.46.147.229;78.46.172.253;78.46.182.24",
    "78.46.186.96;78.46.188.228;78.46.188.82;78.46.189.158;78.46.189.74;78.46.190.238;78.46.191.8;78.46.192.174;78.46.192.204;78.47.43.59;78.47.45.21;91.198.115.0/24;95.216.136.170;95.216.138.15;95.216.139.114",
    "49.12.109.154;49.12.221.137;49.12.221.3;49.12.221.73;49.12.227.222;49.12.227.223;49.12.227.224;49.12.227.225;49.12.227.226;49.12.227.227;49.12.227.228;49.12.227.229;49.12.227.230;49.12.227.231;49.12.228.10",
    "49.12.228.11;49.12.228.12;49.12.228.120;49.12.228.122;49.12.228.123;49.12.228.124;49.12.228.125;49.12.228.126;49.12.228.127;49.12.228.128;49.12.228.129;49.12.228.13;49.12.228.130;49.12.228.131;49.12.228.132",
    "49.12.239.177;49.12.239.178;49.12.239.179;49.12.239.180;49.12.239.181;49.12.239.182;49.12.239.183;49.12.239.184;49.12.239.185;49.12.239.186;64.225.66.253;64.225.70.236;64.227.64.156;64.227.66.185;65.108.48.41",
    "49.12.239.162;49.12.239.163;49.12.239.164;49.12.239.165;49.12.239.166;49.12.239.167;49.12.239.168;49.12.239.169;49.12.239.170;49.12.239.171;49.12.239.172;49.12.239.173;49.12.239.174;49.12.239.175;49.12.239.176",
    "167.172.94.47;167.71.220.59;167.99.33.107;174.138.10.123;174.138.3.164;174.138.45.150;178.128.216.2;178.62.209.127;178.62.25.68;178.62.9.149;188.166.103.192;188.166.112.13;188.166.116.64;188.166.186.6;188.166.240.25",
    "188.166.36.103;199.116.84.0/24;206.189.9.131;209.222.252.0/24;209.97.135.247;209.97.185.45;23.88.113.138;23.88.118.41;23.88.119.226;23.88.120.188;23.88.121.112;23.88.122.101;23.88.122.248;23.88.123.202;23.88.123.242",
    "95.216.139.44;95.216.140.48;95.216.187.176;95.216.189.202;95.216.189.35;95.216.189.86;95.216.189.87;95.216.189.98;95.216.190.212;95.216.199.160;95.216.199.217;95.216.200.97;95.216.201.10;95.216.202.32;95.216.203.255",
    "135.181.86.200;135.181.86.201;135.181.86.204;135.181.86.208;135.181.86.255;135.181.86.63;135.181.86.88;135.181.86.95;135.181.86.98;135.181.87.18;135.181.87.25;138.201.191.218;138.201.191.237;138.201.191.244;138.201.191.51",
    "104.248.206.131;104.248.231.61;116.203.249.163;128.199.45.242;134.122.50.39;134.122.61.72;134.209.16.242;134.209.16.244;134.209.19.30;134.209.24.146;134.209.31.107;134.209.31.128;134.209.31.191;134.209.31.237;134.209.98.94",
    "135.181.86.105;135.181.86.113;135.181.86.114;135.181.86.127;135.181.86.146;135.181.86.148;135.181.86.158;135.181.86.164;135.181.86.167;135.181.86.178;135.181.86.184;135.181.86.187;135.181.86.188;135.181.86.193;135.181.86.198",
    "143.244.143.183;143.244.143.184;143.244.143.185;143.244.143.186;143.244.143.187;143.244.143.189;143.244.163.97;147.182.160.251;157.245.193.97;157.245.51.61;157.245.62.247;157.245.62.99;157.245.63.120;157.245.77.11;159.65.28.120",
    "165.232.177.22;165.232.180.169;165.232.181.14;165.232.181.42;165.232.185.205;165.232.190.122;165.232.190.183;165.232.190.251;167.172.71.178;167.172.72.198;167.172.82.181;167.172.82.200;167.172.82.213;167.172.90.178;167.172.90.70",
    "138.201.244.104;138.201.244.130;138.201.244.138;138.201.244.148;138.201.244.163;138.68.161.191;138.68.168.34;139.59.116.122;139.59.183.149;142.93.48.86;143.110.255.50;143.244.143.172;143.244.143.174;143.244.143.177;143.244.143.182")
  
  dns.blocklist <- unlist(strsplit(dns.blocklist, ";"))
  
  data(ban_list, package = "xmrpeers")
  
  bad_peers <- readLines(paste0(scan.dir, "/bad_peers.txt"))
  
  con <- DBI::dbConnect(RSQLite::SQLite(), paste0(scan.dir, "/crawler-netscan.db"))
  on.exit(DBI::dbDisconnect(con))
  
  handshake_data <- DBI::dbGetQuery(con, paste0(
    "SELECT * FROM handshake_data"
  ))
  
  peerlists <- DBI::dbGetQuery(con, paste0(
    "SELECT * FROM peerlists"
  ))
  
  data.table::setDT(peerlists)
  
  peerlists[, connected_node := gsub("(KnownAddr[(])|([)])", "", connected_node)]
  
  peerlists[, peerlist := gsub("(^[[])|([]]$)", "", peerlist)]
  
  split.peerlists <- strsplit(peerlists$peerlist, ", ")
  split.peerlists <- lapply(split.peerlists, function(x) {
    if (length(x) == 0) { x <- "127.0.0.1" } # Add so that peer list is not empty
    gsub("[:][0-9]+$", "", x) # Remove port
  })
  
  disseminated.peerlists <- setdiff(unique(unlist(split.peerlists)), "127.0.0.1")
  disseminated.peerlists <- disseminated.peerlists[ !is.na(IP::ipv4(disseminated.peerlists))]
  # Keep only IPv4 addresses
  
  mrl.ban.list.peers <- disseminated.peerlists[xmrpeers::in.ip.set(disseminated.peerlists, ban_list)]
  dns.ban.list.peers <- disseminated.peerlists[xmrpeers::in.ip.set(disseminated.peerlists, dns.blocklist)]
  
  banlist.status <- lapply(split.peerlists, function(x) {
    
    n_disseminated_peers_on_mrl_ban_list <- sum(x %in% mrl.ban.list.peers)
    n_disseminated_peers_on_dns_ban_list <- sum(x %in% dns.ban.list.peers)
    
    mrl_ban_list_enabled <- length(x) >= 200 & n_disseminated_peers_on_mrl_ban_list == 0
    dns_ban_list_enabled <- length(x) >= 200 & n_disseminated_peers_on_dns_ban_list == 0
    # Must have disseminated a peer list with 200 or more peers (there are 
    # 250 usually) or else there is greater risk of false positive
    
    data.table(
      n_disseminated_peers_on_mrl_ban_list,
      n_disseminated_peers_on_dns_ban_list,
      mrl_ban_list_enabled,
      dns_ban_list_enabled
    )
  })
  
  banlist.status <- cbind(
    connected_node = peerlists$connected_node,
    data.table::rbindlist(banlist.status)
  )
  
  
  handshake_data <- DBI::dbGetQuery(con, paste0(
    "SELECT * FROM handshake_data"
  ))
  
  data.table::setDT(handshake_data)
  
  connections <- merge(handshake_data, banlist.status, by = "connected_node", all = TRUE)
  
  bad_peers <- stringr::str_extract(bad_peers, "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[:][0-9]{1,5}")
  
  connections[, is_spy_node := connected_node %in% bad_peers]
  
  connections[, connected_node_ip := stringr::str_extract(connected_node, "[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}")]
  
  connections <- connections[ ! is.na(connected_node_ip), ]
  # Removes IPv6 IP addresses
  
  connections[, connected_node_port := stringr::str_extract(connected_node, "[0-9]{1,5}$")]
  
  connections[, rpc_available := rpc_port != 0]
  connections[, is_pruned := pruning_seed != "NotPruned"]
  
  connections[, rpc_confirmed := FALSE]
  
  if (confirm.rpc) {
    rpc.to.confirm <- connections[(rpc_available), paste0("http://", connected_node_ip, ":", rpc_port)]
    message(base::date(), " Probing ", length(rpc.to.confirm), " RPC ports.")
    rpc.confirmation <- probe.rpc(rpc.to.confirm)
    connections[(rpc_available), rpc_confirmed := rpc.confirmation]
  }
  
  
  if (get.domains) {
    domains <- get.ditatompel.domains()
    message(base::date(), " Found ", nrow(domains), " RPC domains.")
    connections <- merge(connections, domains, all.x = TRUE, by = "connected_node_ip")
    connections[ is.na(rpc_domain), rpc_domain := "None"]
  } else {
    connections[, rpc_domain := "None"]
  }
  
  
  connections.by.ip <- connections[, .(
    ports = paste(connected_node_port, collapse = ";"),
    is_spy_node = any(is_spy_node),
    rpc_available = any(rpc_available),
    rpc_confirmed = any(rpc_confirmed),
    rpc_domain = unique(rpc_domain),
    is_pruned = any(is_pruned),
    n_disseminated_peers_on_mrl_ban_list = median(as.numeric(n_disseminated_peers_on_mrl_ban_list)),
    n_disseminated_peers_on_dns_ban_list = median(as.numeric(n_disseminated_peers_on_dns_ban_list)),
    mrl_ban_list_enabled = all(mrl_ban_list_enabled),
    dns_ban_list_enabled = all(dns_ban_list_enabled)
  ), by = "connected_node_ip"]
  
  connections.by.ip[, connected_node_ip_subnet_16 := xmrpeers::as.subnet(connected_node_ip, 16)]
  
  spy.share.subnet <- connections.by.ip[, .(spy_share_subnet_16 = mean(is_spy_node)), by = "connected_node_ip_subnet_16"]
  color.maker <- colorRampPalette(c("blue", "grey", "red"))
  color.gradient <- color.maker(11)
  spy.share.subnet[, spy_color_subnet_16 := color.gradient[round(10 * spy_share_subnet_16) + 1] ]
  
  connections.by.ip <- merge(connections.by.ip, spy.share.subnet, by = "connected_node_ip_subnet_16")
  
  data.table::setcolorder(connections.by.ip, c("connected_node_ip",
    "connected_node_ip_subnet_16", "ports", "is_spy_node", "rpc_available",
    "rpc_confirmed", "rpc_domain", "is_pruned", "n_disseminated_peers_on_mrl_ban_list",
    "n_disseminated_peers_on_dns_ban_list", "mrl_ban_list_enabled", "dns_ban_list_enabled",
    "spy_share_subnet_16", "spy_color_subnet_16"))
  
  connections.by.ip <- cbind(date = data.date, connections.by.ip)
  
  n_unreachable_nodes <- length(setdiff(disseminated.peerlists, connections.by.ip$connected_node_ip))
  
  daily.data <- connections.by.ip[, .(
    n_nodes = .N + n_unreachable_nodes,
    n_spy_nodes = sum(is_spy_node),
    n_honest_nodes = sum(!is_spy_node),
    n_unreachable_nodes = n_unreachable_nodes,
    n_honest_mrl_ban_list_enabled = sum(mrl_ban_list_enabled & !is_spy_node),
    n_honest_dns_ban_list_enabled = sum(dns_ban_list_enabled & !is_spy_node),
    n_honest_is_pruned = sum(is_pruned & !is_spy_node),
    n_honest_rpc_available = sum(rpc_available & !is_spy_node),
    n_honest_rpc_confirmed = sum(rpc_confirmed & !is_spy_node)
  )]
  
  
  daily.data[, percent_spy_nodes := 100 * n_spy_nodes / n_nodes]
  daily.data[, percent_honest_nodes := 100 * n_honest_nodes / n_nodes]
  daily.data[, percent_unreachable_nodes := 100 * n_unreachable_nodes / n_nodes]
  
  daily.data[, percent_honest_is_pruned := 100 * n_honest_is_pruned / n_honest_nodes]
  daily.data[, percent_honest_rpc_available := 100 * n_honest_rpc_available / n_honest_nodes]
  daily.data[, percent_honest_rpc_confirmed := 100 * n_honest_rpc_confirmed / n_honest_nodes]
  daily.data[, percent_honest_mrl_ban_list_enabled := 100 * n_honest_mrl_ban_list_enabled / n_honest_nodes]
  daily.data[, percent_honest_dns_ban_list_enabled := 100 * n_honest_dns_ban_list_enabled / n_honest_nodes]
  
  
  daily.data <- cbind(date = data.date, daily.data)
  
  list(daily.data = daily.data, connections.by.ip = connections.by.ip, connections = connections)
  
}





get.raw.data <- function(skipped.dates, confirm.rpc = TRUE, get.domains = TRUE) {
  
  date.dirs <- list.dirs("raw", full.names = FALSE, recursive = FALSE)
  date.dirs <- date.dirs[date.dirs != ""]
  # rm top level dir
  if (length(date.dirs) == 0) {
    stop("No data in the 'raw' directory.")
  }
  
  date.dirs <- setdiff(date.dirs, skipped.dates)
  
  if (length(date.dirs) == 0) {
    message(base::date(), " No new data")
    return(NULL)
  }
  
  processed.data <- list(daily.data = NULL, connections.by.ip = NULL)
  
  for (scanned.date in date.dirs) {
    process.data.tmp <- process.raw.data(
      scan.dir = paste0("raw/", scanned.date), data.date = scanned.date,
      confirm.rpc = confirm.rpc, get.domains = get.domains)
    processed.data[["daily.data"]][[scanned.date]] <- process.data.tmp[["daily.data"]]
    processed.data[["connections.by.ip"]][[scanned.date]] <- process.data.tmp[["connections.by.ip"]]
    message(base::date(), " Processed raw data for ", scanned.date)
  }
  
  processed.data[["daily.data"]] <- data.table::rbindlist(processed.data[["daily.data"]])
  processed.data[["connections.by.ip"]] <- data.table::rbindlist(processed.data[["connections.by.ip"]])
  
  processed.data
  
}



#' Update netscan database
#'
#' @param db.file Character. File path and name of the SQLite database to update.
#' @param confirm.rpc Logical. Whether to test the RPC ports of the nodes
#' that claim to have them available.
#' @param query.asn Logical. Whether to query Team Cymru for the Autonomous
#' System Number (ASN) of each node.
#' @param get.domains Logical. Whether to query
#' \url{https://xmr.ditatompel.com/remote-nodes} for the domain names
#' associated with the remote nodes.
#'
#' @return NULL (invisible)
#' @export
#'
#' @examples
#' 1
database.update <- function(db.file = "netscan-test.db", confirm.rpc = TRUE,
  query.asn = TRUE, get.domains = TRUE) {
  
  if (! dir.exists("raw")) {
    stop("'raw' directory does not exist in current working directory. Aborting.")
  }
  
  if (! file.exists(db.file)) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), db.file)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
    # Functions can read while another function writes
    # https://stackoverflow.com/questions/15143871/simplest-way-to-retry-sqlite-query-if-db-is-locked
    
    DBI::dbExecute(con,
      "CREATE TABLE daily_data (
date TEXT,
n_nodes INTEGER,
n_spy_nodes INTEGER,
n_honest_nodes INTEGER,
n_unreachable_nodes INTEGER,
n_honest_mrl_ban_list_enabled INTEGER,
n_honest_dns_ban_list_enabled INTEGER,
n_honest_is_pruned INTEGER,
n_honest_rpc_available INTEGER,
n_honest_rpc_confirmed INTEGER,
percent_spy_nodes REAL,
percent_honest_nodes REAL,
percent_unreachable_nodes REAL,
percent_honest_is_pruned REAL,
percent_honest_rpc_available REAL,
percent_honest_rpc_confirmed REAL,
percent_honest_mrl_ban_list_enabled REAL,
percent_honest_dns_ban_list_enabled REAL,
unique(date)
)")
    # unique(date) prevents the same dates being inserted more than once
    
    DBI::dbExecute(con,
      "CREATE TABLE individual_node_data (
date TEXT,
connected_node_ip TEXT,
connected_node_ip_subnet_16 TEXT,
ports TEXT,
is_spy_node INTEGER,
rpc_available INTEGER,
rpc_confirmed INTEGER,
rpc_domain TEXT,
is_pruned INTEGER,
n_disseminated_peers_on_mrl_ban_list REAL,
n_disseminated_peers_on_dns_ban_list REAL,
mrl_ban_list_enabled INTEGER,
dns_ban_list_enabled INTEGER,
spy_share_subnet_16 REAL,
spy_color_subnet_16 TEXT,
asn INTEGER,
as_name TEXT,
unique(date, connected_node_ip)
)")
    # NOTE: unique(date, connected_node_ip) prevents the same
    # date/IP combination being inserted more than once
    
    DBI::dbExecute(con,
      "CREATE TABLE asn (
connected_node_ip TEXT,
asn INTEGER,
bgp_prefix TEXT,
cc TEXT,
registry TEXT,
allocated TEXT,
as_name TEXT,
unique(connected_node_ip)
)")
    
  } else {
    con <- DBI::dbConnect(RSQLite::SQLite(), db.file)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
  }
  
  date.have.data <- DBI::dbGetQuery(con, "SELECT date FROM daily_data")
  
  processed.data <- get.raw.data(skipped.dates = date.have.data$date,
    confirm.rpc = confirm.rpc, get.domains = get.domains)
  
  if (length(processed.data) == 0) { return(invisible(NULL)) }
  
  if (query.asn) {
    
    already.queried.ASN <- DBI::dbGetQuery(con, paste0(
      "SELECT connected_node_ip FROM asn"
    ))
    
    ASN.to.query <- setdiff(unique(processed.data$connections.by.ip$connected_node_ip),
      already.queried.ASN$connected_node_ip)
    
    if (length(ASN.to.query) > 0) {
      
      message(base::date(), " Querying Team Cymru for the ASN of ", length(ASN.to.query), " IP addresses.")
      
      ASN.response <- cymruservices::bulk_origin(ASN.to.query)
      
      data.table::setDT(ASN.response)
      data.table::setnames(ASN.response, c("as", "ip"), c("asn", "connected_node_ip"))
      data.table::setcolorder(ASN.response, "connected_node_ip", before = 1)
      
      asn.statement <- DBI::dbSendQuery(con,
        "INSERT OR IGNORE INTO asn VALUES (:connected_node_ip,:asn,:bgp_prefix,:cc,:registry,:allocated,:as_name)")
      # "IGNORE" prevents the same data from being inserted more than once
      DBI::dbBind(asn.statement, params = ASN.response)
      DBI::dbClearResult(asn.statement)
      
    }
    
  }
  
  asn <- DBI::dbGetQuery(con, paste0(
    "SELECT connected_node_ip, asn, as_name FROM asn"
  ))
  
  data.table::setDT(asn)
  
  processed.data[["connections.by.ip"]] <- merge(
    processed.data[["connections.by.ip"]], asn[, .(connected_node_ip, asn, as_name)], all.x = TRUE)
  
  processed.data[["connections.by.ip"]][is.na(asn), asn := 0]
  processed.data[["connections.by.ip"]][is.na(as_name), as_name := ""]
  
  print(str(processed.data[["daily.data"]]))
  
  daily_data.statement <- DBI::dbSendQuery(con,
    "INSERT OR IGNORE INTO daily_data VALUES (:date,:n_nodes,:n_spy_nodes,:n_honest_nodes,:n_unreachable_nodes,:n_honest_mrl_ban_list_enabled,:n_honest_dns_ban_list_enabled,:n_honest_is_pruned,:n_honest_rpc_available,:n_honest_rpc_confirmed,:percent_spy_nodes,:percent_honest_nodes,:percent_unreachable_nodes,:percent_honest_is_pruned,:percent_honest_rpc_available,:percent_honest_rpc_confirmed,:percent_honest_mrl_ban_list_enabled,:percent_honest_dns_ban_list_enabled)")
  # "IGNORE" prevents the same data from being inserted more than once
  DBI::dbBind(daily_data.statement, params = processed.data[["daily.data"]])
  DBI::dbClearResult(daily_data.statement)
  
  print(str(processed.data[["connections.by.ip"]]))
  
  individual_node_data.statement <- DBI::dbSendQuery(con,
    "INSERT OR IGNORE INTO individual_node_data VALUES (:date,:connected_node_ip,:connected_node_ip_subnet_16,:ports,:is_spy_node,:rpc_available,:rpc_confirmed,:rpc_domain,:is_pruned,:n_disseminated_peers_on_mrl_ban_list, :n_disseminated_peers_on_dns_ban_list,:mrl_ban_list_enabled,:dns_ban_list_enabled,:spy_share_subnet_16,:spy_color_subnet_16,:asn,:as_name)")
  # "IGNORE" prevents the same data from being inserted more than once
  DBI::dbBind(individual_node_data.statement, params = processed.data[["connections.by.ip"]])
  DBI::dbClearResult(individual_node_data.statement)
  
  invisible(NULL)
  
}

