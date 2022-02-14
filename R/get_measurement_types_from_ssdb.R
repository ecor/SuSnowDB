NULL
#' Get variables ("measurement_types" table) from a SumavaDB-like spatiotemporal database  
#'
#' @param conn database (preferably PostgresSQL) connection. See output of \code{\link{dbConnect}}.
#' @param ... further arguments for \code{\link{dbReadTable}}
#' 
#' @importFrom dplyr tbl arrange
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' library(RPostgreSQL) 
#' dbname <- "hydroclimatedb_testing00"
#' val <- sumava_snow_dataset()
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' add_measurement_types_into_ssdb(conn,val$measurement_types,new=TRUE)
#' ### Add weather station from MeteoTrentino network, Trentino, Italy 
#' val_smet <- meteotrentino_smet_dataset(smet_files=c("T0175","T0179"))
#' add_measurement_types_into_ssdb(conn,val_smet$measurement_types,append=TRUE)
#' 
#' dbDisconnect(conn)
#' 
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' out <- get_measurement_types_from_ssdb(conn)
#' dbDisconnect(conn)
#' 
#' }
get_measurement_types_from_ssdb <- function (conn,...) {

 
##  out <- dbReadTable(conn,"measurement_types",...)
  out <- tbl(conn,"measurement_types",...)
  return(out)
  
  
}