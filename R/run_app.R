#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  
  active_connections_table <- netstat::parse_netstat(netstat::netstat())
  # netstat::ports_in_use() doesn't work.
  
  port.already.bound <- with(active_connections_table, any(grepl(":8000$", V4) & V6 == "LISTEN"))
  
  if (! port.already.bound) {
    # Check if there is already a process listening on port 8000
    
    rx <- callr::r_bg( function() { 
      pr <- plumber::plumb("R/plumber.R")
      pr$run(port = 8000) 
    }, cmdargs = c("--no-save", "--no-restore"), cleanup = FALSE ) 
    # cleanup = FALSE means that the process continues running even
    # after the parent R process is terminated.
    # This process will die if there is already a plumber running because
    # the port will fail to bind
    
    message("\nplumber API process ID: ", rx$get_pid())
    cat(rx$get_pid(), "\n", file = "plumber.pid", sep = "")
    
  } else {
    plumber.pid <- tryCatch(readLines("plumber.pid"), error = function(e) "<no plumber.pid file found>" )
    message("\nThere is already a program listening on port 8000, so a new plumber API process ",
      "has not been started. The program already listening on port 8000 is probably an existing ",
      "plumber API process with process ID ", plumber.pid)
  }
  
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
