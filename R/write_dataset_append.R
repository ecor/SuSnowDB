NULL
#' Append "dataset" to a SmavaDB-like spatiotemporal database  
#'
#' @param conn database (preferrably PodtgresSQL) connection. See ouput of \code{\link{dbConnect}}.
#' @param dataset dataset . See output of \code{\link{write_dataset_append}}
#' @param append,... further arguments. Default is \code{append=TRUE}.
#'
#' @importFrom sf st_write
#' @importFrom DBI dbWriteTable
#' @export
#' @examples
#' \dontrun{
#' library(RPostgreSQL) 
#' 
#' val <- sumava_snow_dataset()
#' dbname = "hydroclimatedb_ver03"
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' write_dataset_append(conn,dataset)
#' dbDisconnect(conn) 
#' }
##https://www.r-bloggers.com/2016/02/using-postgresql-in-r-a-quick-how-to/
###https://github.com/tidyverse/dplyr/issues/3026

###conn = dbConnect(PostgreSQL(), dbname = dbname) ##, user = dbname) 
####help("dbSendQuery")
###meuse = st_read(conn, "meuse")
##meuse_1_3 = st_read(conn, query = "select * from meuse limit 3;")

write_dataset_append <- function(conn,dataset,append=TRUE,...) {
  
  o_locations <- st_write(dataset$locations,conn,"locations",append=append)
  o_measurement_types <- dbWriteTable(conn=conn,name="measurement_types",value=dataset$measurement_types,append=append)
  o_measurements <- dbWriteTable(conn=conn,name="measurements",value=dataset$measurements,append=append)
  
  return(list(locations=o_locations,measurement_types=o_measurement_types,measurements=o_measurements))
  
  

}
