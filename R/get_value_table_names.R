NULL
#' Extract table names containing values
#'
#' 
#' @param ssdb spatio-temporal dataset or a connection (TO BE IMPLEMENTED)
#' @param meta_table_mames names of the meta tables. Default is \code{c("locations","measurement_types")}.
#' @param value_table_names names of the meta table. Default is \code{NULL}. Ignored if it is \code{NULL}.
#'   
#' @export
#' 
#' @examples 
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
#' 
#' tnames <- get_value_table_names(conn)
#' dbDisconnect(conn)
#' 
#' 
#' sumava <- sumava_snow_dataset()
#' tnames_sumava <- get_value_table_names_from_ssdb(sumava)
#' }


get_value_table_names_from_ssdb <- function (ssdb,meta_table_names=c("locations","measurement_types"),value_table_names=NULL)  {
  
  if (length(value_table_names)==0) value_table_names <- NULL
  if (is.null(value_table_names)) value_table_names <- NA
  
  tnn <- tablenames(ssdb)
  if (all(is.na(value_table_names)))  value_table_names <- tnn[!(tnn %in% meta_table_names)]
 
  out <- value_table_names
  
  
  return(out)
  
}