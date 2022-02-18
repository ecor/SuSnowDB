NULL
#' Get locations ("locations" table) from a SumavaDB-like spatiotemporal database  
#'
#' @param conn database (preferrably PodtgresSQL) connection. See ouput of \code{\link{dbConnect}}.
#' @param ... further arguments for \code{\link{st_read}}
#' 
#'
#' @importFrom  sf st_read st_crs st_transform
#' 
#' 
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' library(RPostgreSQL) 
#' dbname <- "hydroclimatedb_testing00"
#' val <- sumava_snow_dataset()
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' add_locations_into_ssdb(conn,val$locations,new=TRUE)
#' ### Add weather station from MeteoTrentino network, Trentino, Italy 
#' val_smet <- meteotrentino_smet_dataset(smet_files=c("T0175","T0179"))
#' add_locations_into_ssdb(conn,val_smet$locations,append=TRUE)
#' 
#' dbDisconnect(conn)
#' 
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' out <- get_locations_from_ssdb(conn)
#' dbDisconnect(conn)
#' 
#' }
get_locations_from_ssdb <- function (conn,...) {

  cc <- class(conn)
  out <- NULL
  if (class(conn)=="PostgreSQLConnection") {
    out <- st_read(conn,"locations",...)
  } else if (cc=="list") {
    
    out <- conn$locations
  } else {
    
    msg <- sprintf("Object type %s not supported to extract locations!",cc)
    stop(msg)
    
    
  }
  return(out)
  
  
}