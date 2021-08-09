NULL
#' Append "dataset" to a SumavaDB-like spatiotemporal database  
#'
#' @param conn database (preferrably PodtgresSQL) connection. See ouput of \code{\link{dbConnect}}.
#' @param dataset dataset . See output of \code{\link{sumava_snow_dataset}}
#' @param append logical. If \code{append==FALSE} database tables are created or replaced. If \code{append==FALSE} data and metedata are appended as new rows of the database tables. Default is \code{append=TRUE}.
#' @param new logical. If \code{new==TRUE} database tables are created or replaced. If \code{new==FALSE} tables are modidied according to \code{append}. Default is \code{FALSE}.  
#' @param sql_files file names containing database initialization SQL queries. They are used in case \code{append==FALSE} or \code{new=TRUE}.
#' @param ... further arguments.
#' 
#' 
#' @importFrom sf st_write
#' @importFrom DBI dbWriteTable dbSendQuery dbReadTable
#' @export
#' @examples
#' \dontrun{
#' library(RPostgreSQL) 
#' 
#' val <- sumava_snow_dataset()
#' dbname = "hydroclimatedb_ver03"
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' 
#' ## Create a new DB layout
#' write_dataset_into_ssdb(conn,dataset=val,new=TRUE)
#' dbDisconnect(conn) 
#' }
##https://www.r-bloggers.com/2016/02/using-postgresql-in-r-a-quick-how-to/
###https://github.com/tidyverse/dplyr/issues/3026


write_dataset_into_ssdb <- function(conn,dataset,append=TRUE,new=FALSE,sql_files=system.file("sql/create_or_replace_functions.sql",package="SuSnowDB"),...) {
  
  if (new==TRUE) append <- FALSE;
  
  if (append==FALSE)  {
    sql_commands <- sql_files %>% map(readLines) %>% unlist() %>% paste(collapse="\n")
    dbSendQuery(conn,statement=sql_commands)
    query_create_tables <-"SELECT create_or_replace_all_measurement_location_tables();"
    dbSendQuery(conn,statement=query_create_tables)
  }
  
  o_locations <- st_write(dataset$locations,conn,"locations",append=TRUE)
  o_measurement_types <- dbWriteTable(conn=conn,name="measurement_types",value=dataset$measurement_types,append=TRUE,row.names=FALSE)
  o_measurements <- dbWriteTable(conn=conn,name="measurements",value=dataset$measurements,append=TRUE,row.names=FALSE)
  
  return(list(locations=o_locations,measurement_types=o_measurement_types,measurements=o_measurements))
  
  

}
