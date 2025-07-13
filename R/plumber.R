# plumber.R

#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}



select.date.plumber <- function(desired.table, date, con) {
  
  daily.data <- DBI::dbGetQuery(con, "SELECT date FROM daily_data")
  possible.dates <- unique(daily.data$date)
  
  if ( ! date %in% c("latest", "all", possible.dates)) {
    stop("Date not available.")
  }
  
  if (date == "latest") {
    most.recent.date <- sort(possible.dates, decreasing = TRUE)[1]
    return.data <- DBI::dbGetQuery(con,
      paste0("SELECT * FROM ", desired.table, " WHERE date == '", most.recent.date, "'"))
  }
  
  if (date == "all") {
    return.data <- DBI::dbGetQuery(con,
      paste0("SELECT * FROM ", desired.table))
  }
  
  if (date %in% possible.dates) {
    return.data <- DBI::dbGetQuery(con,
      paste0("SELECT * FROM ", desired.table, " WHERE date == '", date, "'"))
  }
  
  return.data
  
}




#* Get daily_data table
#* @param date If provided, get data for a specific date. Valid values include
#* "latest" (the default), "all" for all dates, and dates formatted as 
#* yyyy-mm-dd, e.g. 2025-07-13.
#* @get /daily_data
function(date = "latest"){

  db.file <- "../data-raw/net-scans/netscan-test.db"
  # The working directory is `/R` because this plumber.R file
  # is in the `/R` directory. Must go upward one directory.
  con <- DBI::dbConnect(RSQLite::SQLite(), db.file)
  on.exit(function() { DBI::dbDisconnect(con) })
  
  daily.data <- select.date.plumber(desired.table = "daily_data", date = date, con = con)
  
  daily.data
}


#* Get individual_node_data table
#* @param date If provided, get data for a specific date. Valid values include
#* "latest" (the default), "all" for all dates, and dates formatted as 
#* yyyy-mm-dd, e.g. 2025-07-13.
#* @get /individual_node_data
function(date = "latest"){
  
  db.file <- "../data-raw/net-scans/netscan-test.db"
  # The working directory is `/R` because this plumber.R file
  # is in the `/R` directory. Must go upward one directory.
  con <- DBI::dbConnect(RSQLite::SQLite(), db.file)
  on.exit(function() { DBI::dbDisconnect(con) })
  
  individual.node.data <- select.date.plumber(desired.table = "daily_data", date = date, con = con)
  
  individual.node.data
  
}

#* Get asn (Autonomous System Number) table. Data was gathered from Team Cymru.
#* https://www.team-cymru.com/ip-asn-mapping
#* @get /asn
function(){
  
  db.file <- "../data-raw/net-scans/netscan-test.db"
  # The working directory is `/R` because this plumber.R file
  # is in the `/R` directory. Must go upward one directory.
  con <- DBI::dbConnect(RSQLite::SQLite(), db.file)
  on.exit(function() { DBI::dbDisconnect(con) })
  
  asn <- DBI::dbGetQuery(con, "SELECT * FROM asn")
  
  asn
}

