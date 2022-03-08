NULL
#' Get measurements ("measurements" table) from a SumavaDB-like spatiotemporal database  
#'
#' @param conn database (preferably PostgresSQL) connection. See output of \code{\link{dbConnect}}.
#' @param locations_code0 code of the locations (\code{location_code0}) where to extract the measurements
#' @param variables_code0 code of the types (\code{variable_code0}) of the measurements that are requested to be extracted.
#' @param start_time,end_time start and end time instants of the time interval in  which measurements are to be extrated.
#' @param all logical. if it is \code{TRUE} all measurements (types) are extracted. Default is \code{FALSE} if something else is specified. 
#' @param returns.data.frame logical. if it is \code{TRUE} function output is cated as a data frame. Default is \code{FALSE}. 
#' @param table_name name of the table . Default is \code{"measurements"}
#' @param ... further arguments for \code{\link{tbl}}
#' 
#'
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' library(RPostgreSQL) 
#' dbname <- "hydroclimatedb_testing00"
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' 
#' 
#' ### Add weather data from stations from MeteoTrentino network, Trentino, Italy 
#' val_smet <- meteotrentino_smet_dataset(smet_files=c("T0175","T0179"))
#' add_locations_into_ssdb(conn,val_smet$locations,new=TRUE)
#' add_measurement_types_into_ssdb(conn,val_smet$measurement_type)
#' add_measurements_into_ssdb(conn,val_smet$measurements)
#' 
#' dbDisconnect(conn)
#' 
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' out <- get_measurements_from_ssdb(conn)
#' out <- get_measurements_from_ssdb(conn,locations_code0="IT-TN_T0179")
#' out <- get_measurements_from_ssdb(conn,locations_code0="IT-TN_T0179",variables_code0=c("SMET_TA","SMET_PINT"))
#' out <- get_measurements_from_ssdb(conn,variables_code0=c("SMET_TA","SMET_PINT"))
#' 
#' time_interval <- as.Date(c("2021-03-01","2021-03-31"))
#' out <- get_measurements_from_ssdb(conn,variables_code0=c("SMET_TA","SMET_PINT"),start_time=time_interval[1],end_time=time_interval[2])
#' dbDisconnect(conn)
#' 
#' }

get_measurements_from_ssdb <- function (conn,table_name="measurements",all=FALSE,variables_code0=NULL,locations_code0=NULL,returns.data.frame=FALSE,start_time=NA,end_time=NA,...) {

  
  if (length(table_name)>1) {
    
    msg <- sprintf("Table names more than one: %s (Please use get_multitable_measurements_table() function instead!)",paste(table_name,collapse=" "))
    stop(msg)
    
  }
  cval <- length(variables_code0)>0
  cloc <- length(locations_code0)>0
  cboth <- cval & cloc
  cany <-  cval | cloc
  all <- (all==FALSE) & (cany==FALSE)
  if (all) {
    out <- tbl(conn,table_name,...)
  } else if (cboth==TRUE) {
  
    out <- tbl(conn,table_name) %>% filter(.data$variable_code0 %in% variables_code0,.data$location_code0 %in% locations_code0)
  }else if (cloc==TRUE) {
    
    out <- tbl(conn,table_name) %>% filter(.data$location_code0 %in% locations_code0)
  }  else if (cval==TRUE) {
    
    out <- tbl(conn,table_name) %>% filter(.data$variable_code0 %in% variables_code0) 
  }
  if (length(start_time)<1) start_time <- NA
  if (length(end_time)<1) end_time <- NA
  
  if (!is.na(start_time)) out <- out %>% filter(.data$time>=start_time) 
  if (!is.na(end_time)) out <- out %>% filter(.data$time<=end_time) 
  out <- out %>% arrange(.data$time)
  
  if (!is.logical(returns.data.frame)) returns.data.frame <- FALSE
  if (returns.data.frame) out <- as.data.frame(out)
  ###
  
  
  return(out)
  
  
}