#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      # golem::golem_welcome_page() # Remove this line to start building your UI
      titlePanel("Monero Network Scan"),
      shiny::h4(shiny::HTML('Looking for remote nodes to sync your wallet and send transactions? Check out <a href="https://github.com/feather-wallet/feather-nodes/blob/master/nodes.yaml">Feather Wallet\'s list of remote nodes</a>,  <a href="https://xmr.ditatompel.com">ditatompel\'s list</a>, or <a href="https://monero.fail/">monero.fail</a>.')),
      shiny::h4(shiny::HTML('The open source code for this web app is available <a href="https://github.com/Rucknium/xmrnetscan">here</a>.')),
      shiny::br(),
      plotly::plotlyOutput("line_chart1", height = "500px") |>
        shinycssloaders::withSpinner(size = 3) |>
        shinyhelper::helper(colour = "red", type = "inline",
          content = 'Reachable nodes are classified as suspected spy nodes if they are marked as spies by the <a href="https://github.com/Boog900/p2p-proxy-checker">p2p-proxy-checker</a>. All other reachable nodes (i.e. nodes that accept inbound connections) are classified as honest reachable nodes. The number of unreachable nodes is estimated by counting the unique IP addresses in all of the peerlists shared by reachable nodes during the p2p handshake.'),
      shiny::br(),
      plotly::plotlyOutput("line_chart2", height = "500px") |>
        shinyhelper::helper(colour = "red", type = "inline",
          content = 'A node is assumed to be using an IP ban list if none of its 250 peer IP addresses shared during a p2p handshake are on the ban list. Some false positives are possible. The DNS ban list is enabled by using the "--enable-dns-blocklist" node startup flag. The Monero Research Lab ban list is <a href="https://github.com/Boog900/monero-ban-list">here</a>.'), 
      shiny::br(),
      shiny::fluidRow(
        shiny::column(6, plotly::plotlyOutput("line_chart3", height = "500px")),
        shiny::column(6, plotly::plotlyOutput("line_chart4", height = "500px"))
      ),
      shiny::br(),
      shiny::h4("Node IP address interactive treemap, grouped by /16 subnet"),
      shiny::h5("Click on the boxes to get more information about each node"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("subnet_treemap", height = "800px"), size = 3, caption = "Subnet treemap"),
      shiny::h4("Node IP address interactive treemap, grouped by Autonomous System"),
      shiny::h5("Click on the boxes to get more information about each node"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("asn_treemap", height = "800px"), size = 3, caption = "Autonomous System (AS) treemap"),
      shinycssloaders::withSpinner(
        DT::dataTableOutput("individual_node_table"), size = 3, caption = "Node data table"),
      shiny::hr(),
      shiny::h4(shiny::HTML('Created by <a href="https://github.com/Rucknium">Rucknium</a> at the <a href="https://github.com/monero-project/research-lab">Monero Research Lab</a>')),
      shiny::h5(shiny::HTML('Autonomous System (AS) data from <a href="https://www.team-cymru.com/ip-asn-mapping">Team Cymru</a>.')),
      shiny::h5(shiny::HTML('Domain-IP address association data from <a href="https://xmr.ditatompel.com/remote-nodes">ditatompel</a>.'))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "xmrnetscan"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
