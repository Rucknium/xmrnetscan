




#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  shinyOptions(cache = cachem::cache_disk("./cache_dir"))
  
  shinyhelper::observe_helpers()
  
  db.file <- "data-raw/net-scans/netscan-test.db"
  con <- DBI::dbConnect(RSQLite::SQLite(), db.file)
  shiny::onSessionEnded(function() { DBI::dbDisconnect(con) })
  # DBI::dbExecute(con, "PRAGMA journal_mode=WAL;")
  # Don't use journal mode=WAL because different Linux users will
  # need to write the "extra" DB files in the same directory, but they are
  # not the same owner.
  daily.data <- DBI::dbGetQuery(con, "SELECT * FROM daily_data")
  
  most.recent.date <- sort(daily.data$date, decreasing = TRUE)[1]
  
  individual.node.data <- DBI::dbGetQuery(con,
    paste0("SELECT * FROM individual_node_data WHERE date == '", most.recent.date, "'"))
  
  xmr.orange <- "#FF6600FF"
  xmr.grey <- "#4C4C4CFF"
  # From https://raw.githubusercontent.com/fluffypony/monero-logo-artefacts/master/Logo%20Subsequent%20Tweaks/monero%20file.svg
  mrl.blue <- "#00aebf"
  # Color from https://www.getmonero.org/press-kit/logos/mrl-logo.svg
  
  tickformatstops.arg <- list(
    list(dtickrange=list(NULL, 604800000), value="%e. %b d"),
    list(dtickrange=list(604800000, "M1"), value="%e. %b w"),
    list(dtickrange=list("M1", "M12"), value="%b '%y M"),
    list(dtickrange=list("M12", NULL), value="%Y Y")
  )
  
  # shiny::observe({
    
    output$line_chart1 <- plotly::renderPlotly({
      
      data <- daily.data
      
      data_list <- split(data, seq_len(nrow(data)))
      
      fig <- plotly::plot_ly(name = "Unreachable nodes", data = data,
        x = ~date, y = ~n_unreachable_nodes, type = 'scatter',
        mode = 'lines', stackgroup = 'one', line = list(color = 'transparent', fillcolor = xmr.grey),
        customdata = data_list,
        hovertemplate = "Nodes: %{y} (%{customdata.percent_unreachable_nodes:.0f}% of total)") |>
        # https://plotly.com/r/hover-text-and-formatting/
        # https://stackoverflow.com/questions/69278251/plotly-including-additional-data-in-hovertemplate
        # https://github.com/plotly/plotly.R/issues/1548
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE),
          tickformatstops = tickformatstops.arg)) |>
        plotly::add_trace(y = ~n_honest_nodes, name = "Honest reachable nodes", fillcolor = xmr.orange,
          hovertemplate = "Nodes: %{y} (%{customdata.percent_honest_nodes:.0f}% of total)") |>
        plotly::add_trace(y = ~n_spy_nodes, name = "Suspected spy nodes", fillcolor = "red",
          hovertemplate = "Nodes: %{y} (%{customdata.percent_spy_nodes:.0f}% of total)")
      fig <- fig |>
        plotly::layout(
          title = list(text = "Estimated number of nodes"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE) 
      fig
      
    }) |>
      shiny::bindCache(most.recent.date)
  # }, domain = NULL)
  
  # shiny::observe({
    
    output$line_chart2 <- plotly::renderPlotly({
      
      data <- daily.data
      
      data_list <- split(data, seq_len(nrow(data)))
      
      fig <- plotly::plot_ly(name = "DNS ban list enabled", data = data,
        x = ~date, y = ~n_honest_dns_ban_list_enabled, type = 'scatter',
        mode = 'lines', line = list(color = xmr.orange),
        customdata = data_list,
        hovertemplate = "Nodes: %{y} (%{customdata.percent_honest_dns_ban_list_enabled:.0f}% of total)") |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE),
          tickformatstops = tickformatstops.arg)) |>
        plotly::add_trace(y = ~n_honest_mrl_ban_list_enabled, name = "MRL ban list enabled",
          line = list(color = mrl.blue),
          hovertemplate = "Nodes: %{y} (%{customdata.percent_honest_mrl_ban_list_enabled:.0f}% of total)")
      fig <- fig |>
        plotly::layout(
          title = list(text = "Estimated number of honest nodes with ban lists enabled"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff',
            rangemode = "tozero"),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE) 
      fig
      
    }) |>
      shiny::bindCache(most.recent.date)
  # }, domain = NULL)
    
    
    # shiny::observe({
    
    output$line_chart3 <- plotly::renderPlotly({
      
      data <- daily.data
      
      data_list <- split(data, seq_len(nrow(data)))

      fig <- plotly::plot_ly(name = "Pruning enabled", data = data, color = xmr.orange,
        x = ~date, y = ~n_honest_is_pruned, type = 'scatter', fill = 'tozeroy',
        mode = 'lines', line = list(color = 'black'),
        customdata = data_list,
        hovertemplate = "Nodes: %{y} (%{customdata.percent_honest_is_pruned:.0f}% of total)") |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE),
          tickformatstops = tickformatstops.arg))
        fig <- fig |>
        plotly::layout(
          title = list(text = "Estimated number of honest nodes\nwith blockchain pruning enabled"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff',
            rangemode = "tozero"),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE) 
      fig
      
    }) |>
      shiny::bindCache(most.recent.date)
    # }, domain = NULL)
    
  
    
    # shiny::observe({
    
    output$line_chart4 <- plotly::renderPlotly({
      
      data <- daily.data
      
      data_list <- split(data, seq_len(nrow(data)))

      fig <- plotly::plot_ly(name = "RPC enabled", data = data, color = xmr.orange,
        x = ~date, y = ~n_honest_rpc_confirmed, type = 'scatter', fill = 'tozeroy',
        mode = 'lines', line = list(color = 'black'),
        customdata = data_list,
        hovertemplate = "Nodes: %{y} (%{customdata.percent_honest_rpc_confirmed:.0f}% of total)") |>
        plotly::layout(xaxis = list(rangeslider = list(visible = TRUE),
          tickformatstops = tickformatstops.arg))
      fig <- fig |>
        plotly::layout(
          title = list(text = "Estimated number of honest nodes\nwith RPC enabled"),
          margin = list(t = 100, l = 0, r = 0),
          xaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
          yaxis = list(title = '',
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff',
            rangemode = "tozero"),
          plot_bgcolor= '#f2f8ee', hovermode = 'x', paper_bgcolor = '#f2f8ee',
          legend = list(orientation = "h", xanchor = "center", x = 0.5, yanchor = "top", y = 1.1)) |>
        plotly::config(displayModeBar = FALSE) 
      fig
      
    }) |>
      shiny::bindCache(most.recent.date)
    # }, domain = NULL)
    
    
    
  
  # shiny::observe({
    
    load_plot_later <- shiny::reactiveVal(1)
    # https://stackoverflow.com/questions/65839072/forcing-render-order-in-r-shiny
    observe({
    if (shiny::isolate(load_plot_later() == 1)) {
      # skip first reactive sequence
      load_plot_later(0)
      # launch next reactive sequence
      shiny::invalidateLater(1, session)
    } else {
      
    
    output$subnet_treemap <- plotly::renderPlotly({
      
      message("plotting subnet treemap")
      
      individual.node.data <- individual.node.data[order(individual.node.data$connected_node_ip), ]
      # Necessary to line up the data in the treemap display
      # At the time of writing, the database gives individual.node.data in
      # the "correct" order, but this may not always be the case
      
      subnet_16 <- unique(individual.node.data[, c(
        "connected_node_ip_subnet_16", "spy_color_subnet_16")])
      
      labels = c(subnet_16$connected_node_ip_subnet_16, individual.node.data$connected_node_ip)
      parents = c(rep("", nrow(subnet_16)), individual.node.data$connected_node_ip_subnet_16)
      colors = c(subnet_16$spy_color_subnet_16,
        ifelse(individual.node.data$is_spy_node == 1, "red", "blue"))
      
      display.data <- data.table::as.data.table(individual.node.data)
      
      display.data <- rbind(data.table(connected_node_ip = 
          rep("", nrow(subnet_16))), display.data, fill = TRUE)
      
      data.table::setorder(display.data, connected_node_ip)
      # necessary to line up the data in the treemap display
      
      display.data[, rpc_confirmed := ifelse(rpc_confirmed == 1, "Yes", "No")]
      display.data[, is_pruned := ifelse(is_pruned == 1, "Yes", "No")]
      display.data[, mrl_ban_list_enabled := ifelse(mrl_ban_list_enabled == 1, "Yes", "No")]
      display.data[, dns_ban_list_enabled := ifelse(dns_ban_list_enabled == 1, "Yes", "No")]
      
      
      data_list <- split(display.data, seq_len(nrow(display.data)))
      
      fig <- plotly::plot_ly(
        type = "treemap",
        labels = labels,
        parents = parents,
        marker = list(colors = colors),
        tiling = list(pad = 0),
        customdata = data_list,
        texttemplate = "IP: %{customdata.connected_node_ip}<br>AS: %{customdata.as_name} (%{customdata.asn})<br>Domain: %{customdata.rpc_domain}<br>Port(s): %{customdata.ports}<br>RPC enabled: %{customdata.rpc_confirmed}<br>Pruned: %{customdata.is_pruned}<br>MRL ban list: %{customdata.mrl_ban_list_enabled}<br>DNS ban list: %{customdata.dns_ban_list_enabled}")
      
      shapes = list(
        list(x0 = 0, y0 = 1, x1 = 0.01, y1 = 0.99, fillcolor = "red", line = list(width = 1)),
        list(x0 = 0 + 0.2, y0 = 1, x1 = 0.01 + 0.2, y1 = 0.99, fillcolor = "blue", line = list(width = 1))
      )
      
      annotations <- list(
        list(text = "Suspected spy node", x = 0.015, y = 1.007, showarrow = FALSE, font = list(size = 15, color = "black")),
        list(text = "Honest reachable node", x = 0.015 + 0.2, y = 1.007, showarrow = FALSE, font = list(size = 15, color = "black"))
      )
      

      fig <- fig |> plotly::layout(
        uniformtext = list(minsize = 10, mode = "hide"),
        shapes = shapes,
        annotations = annotations
        ) |>
        plotly::config(displayModeBar = FALSE)
      
      fig

      
    })  |>
      shiny::bindCache(most.recent.date)
    
    
    output$asn_treemap <- plotly::renderPlotly({
      
      message("plotting asn treemap")
      
      display.data <- data.table::copy(individual.node.data)
      data.table::setDT(display.data)
      
      spy.share.asn <- display.data[, .(spy_share_asn = mean(is_spy_node, na.rm = TRUE)), by = "as_name"]
      color.maker <- colorRampPalette(c("blue", "grey", "red"))
      color.gradient <- color.maker(11)
      spy.share.asn[, spy_color_asn := color.gradient[round(10 * spy_share_asn) + 1] ]
      
      display.data <- merge(display.data, spy.share.asn, by = "as_name")
      
      data.table::setorder(display.data, "connected_node_ip")
      # Necessary to line up the data in the treemap display
      
      ASN <- unique(display.data[, c(
        "as_name", "spy_color_asn")])
      
      labels = c(ASN$as_name, display.data$connected_node_ip)
      parents = c(rep("", nrow(ASN)), display.data$as_name)
      colors = c(ASN$spy_color_asn,
        ifelse(display.data$is_spy_node == 1, "red", "blue"))
      
      display.data <- rbind(data.table(connected_node_ip = 
          rep("", nrow(ASN))), display.data, fill = TRUE)
      
      data.table::setorder(display.data, connected_node_ip)
      # Necessary to line up the data in the treemap display
      
      display.data[, rpc_confirmed := ifelse(rpc_confirmed == 1, "Yes", "No")]
      display.data[, is_pruned := ifelse(is_pruned == 1, "Yes", "No")]
      display.data[, mrl_ban_list_enabled := ifelse(mrl_ban_list_enabled == 1, "Yes", "No")]
      display.data[, dns_ban_list_enabled := ifelse(dns_ban_list_enabled == 1, "Yes", "No")]
      
      
      data_list <- split(display.data, seq_len(nrow(display.data)))
      
      fig <- plotly::plot_ly(
        type = "treemap",
        labels = labels,
        parents = parents,
        marker = list(colors = colors),
        tiling = list(pad = 0),
        customdata = data_list,
        texttemplate = "IP: %{customdata.connected_node_ip}<br>AS: %{customdata.as_name} (%{customdata.asn})<br>Domain: %{customdata.rpc_domain}<br>Port(s): %{customdata.ports}<br>RPC enabled: %{customdata.rpc_confirmed}<br>Pruned: %{customdata.is_pruned}<br>MRL ban list: %{customdata.mrl_ban_list_enabled}<br>DNS ban list: %{customdata.dns_ban_list_enabled}")
      
      shapes = list(
        list(x0 = 0, y0 = 1, x1 = 0.01, y1 = 0.99, fillcolor = "red", line = list(width = 1)),
        list(x0 = 0 + 0.2, y0 = 1, x1 = 0.01 + 0.2, y1 = 0.99, fillcolor = "blue", line = list(width = 1))
      )
      
      annotations <- list(
        list(text = "Suspected spy node", x = 0.015, y = 1.007, showarrow = FALSE, font = list(size = 15, color = "black")),
        list(text = "Honest reachable node", x = 0.015 + 0.2, y = 1.007, showarrow = FALSE, font = list(size = 15, color = "black"))
      )
      
      
      fig <- fig |> plotly::layout(
        uniformtext = list(minsize = 10, mode = "hide"),
        shapes = shapes,
        annotations = annotations
      ) |>
        plotly::config(displayModeBar = FALSE)
      
      fig
      
      
    })  |>
      shiny::bindCache(most.recent.date)
    
    
    
    
    output$individual_node_table <- DT::renderDataTable({
      
      message("creating node data table")
      
      display.data <- data.table::copy(individual.node.data)
      data.table::setDT(display.data)
      
      display.data[, IP_address := connected_node_ip]
      display.data[, Subnet := factor(gsub("[.]0[.]0$", "", connected_node_ip_subnet_16))]
      display.data[, Spy_node := factor(ifelse(is_spy_node == 1, "Yes", "No"))]
      display.data[, RPC_domain := factor(rpc_domain)]
      display.data[, RPC := factor(ifelse(rpc_confirmed == 1, "Yes", "No"))]
      display.data[, Pruned := factor(ifelse(is_pruned == 1, "Yes", "No"))]
      display.data[, MRL_banlist := factor(ifelse(mrl_ban_list_enabled == 1, "Yes", "No"))]
      display.data[, DNS_banlist := factor(ifelse(dns_ban_list_enabled == 1, "Yes", "No"))]
      display.data[, Autonomous_System := factor(as_name)]
      
      display.data <- display.data[, .(IP_address, Subnet, Spy_node, RPC_domain, RPC,
        Pruned, MRL_banlist, DNS_banlist, Autonomous_System)]
      
      DT::datatable(display.data,
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons", "ColReorder"),
        escape = FALSE,
        style = "default",
        options = list(dom = "Blfrtip", buttons = I("colvis"),
          colReorder = list(realtime = FALSE))
      )
      
      # https://rstudio.github.io/DT/extensions.html
      # https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
      # https://github.com/rstudio/DT/issues/153
      # The dom argument: https://datatables.net/reference/option/dom#Options
      # and https://stackoverflow.com/questions/51730816/remove-showing-1-to-n-of-n-entries-shiny-dt
      # and https://rstudio.github.io/DT/options.html
      
    }, server = FALSE) |>
      shiny::bindCache(most.recent.date) # , domain = NULL)
    # Must have server = FALSE to make it cachable. See help
    # file for DT::renderDataTable()
    
    
    }
    
    })
    
  # })
  
}
