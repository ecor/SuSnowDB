NULL
#'
#'

# > dbListTables(conn)
# [1] "measurement_types"                                "locations"                                        "measurements"                                    
# [4] "_hyper_29_34918_chunk"                            "hypertable"                                       "hypertable_data_node"                            
# [7] "tablespace"                                       "dimension"                                        "dimension_slice"                                 
# [10] "chunk"                                            "chunk_constraint"                                 "chunk_index"                                     
# [13] "chunk_data_node"                                  "metadata"                                         "continuous_agg"                                  
# [16] "continuous_aggs_invalidation_threshold"           "continuous_aggs_hypertable_invalidation_log"      "continuous_aggs_materialization_invalidation_log"
# [19] "hypertable_compression"                           "compression_algorithm"                            "compression_chunk_size"                          
# [22] "remote_txn"                                       "bgw_job"                                          "bgw_job_stat"                                    
# [25] "bgw_policy_chunk_stats"                           "cache_inval_hypertable"                           "cache_inval_bgw_job"                             
# [28] "cache_inval_extension"                            "spatial_ref_sys"                                 
# > names()
# Error in names() : 0 arguments passed to 'names' which requires 1
# > names


NULL
# TODO: Add comment
# 
# Author: cordaem
###############################################################################


NULL


#' @title Lorem ipsum
#' 
#' @description Lorem ipsum
#'
#' 
#' 
#' @param ssdb object 
#' @param ignored ingored table names that are ignored in case \code{all==FALSE}.
#' @param all logical. If it is \code{TRUE} no names are ignored. Default is \code{FALSE}. 
#' @param ... further arguments 
#' 
#' @details 
#' 
#' Lorem ipsum
#' 
#'
#' @importFrom DBI dbListTables
#' @importFrom stringr str_detect
#' 
#' @rdname tablenames
#' @export
#' 
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
#' out <- get_measurements_from_ssdb(conn,
#' locations_code0="IT-TN_T0179",variables_code0=c("SMET_TA","SMET_PINT"))
#' out <- get_measurements_from_ssdb(conn,
#' variables_code0=c("SMET_TA","SMET_PINT"))
#' 
#' time_interval <- as.Date(c("2021-03-01","2021-03-31"))
#' out <- get_measurements_from_ssdb(conn,
#' variables_code0=c("SMET_TA","SMET_PINT"),
#' start_time=time_interval[1],end_time=time_interval[2])
#' 
#' tnames <- tablenames(conn)
#' dbDisconnect(conn)
#' 
#' 
#' sumava <- sumava_snow_dataset()
#' tnames_sumava <- tablenames(sumava)
#' }

#' 

tablenames <- function (object=NULL,...)  {
  
  
  return(standardGeneric("tablenames"))
  
}

NULL
#' @title tablenames
#' @rdname tablenames
#' @aliases tablenames
#' @export
#' 


setGeneric("tablenames",function (ssdb=NULL,...)  {
  
  out <- names(ssdb)
  return(out)
  
  
  
})



NULL
#' @title tablenames
#' @rdname tablenames
#' @method tablenames list
#' @aliases tsblenames 
#' @export


setMethod("tablenames","list",function (ssdb=NULL,...)  {
  
  out <- names(ssdb)
  return(out)
  
  
})


NULL
#' @title tablenames
#' @rdname tablenames
#' @method tablenames PostgreSQLConnection
#' @aliases tsblenames 
#' @export

setMethod("tablenames","PostgreSQLConnection",function (ssdb=NULL,ignored=c(
"_hyper_29_34918_chunk","hypertable" ,"hypertable_data_node","tablespace","dimension",
"dimension_slice","chunk","chunk_constraint","chunk_index","chunk_data_node",
"metadata","continuous_agg","continuous_aggs_invalidation_threshold","continuous_aggs_hypertable_invalidation_log",
"continuous_aggs_materialization_invalidation_log","hypertable_compression","compression_algorithm","compression_chunk_size","remote_txn",
"bgw_job","bgw_job_stat","bgw_policy_chunk_stats","cache_inval_hypertable","cache_inval_bgw_job",                             
"cache_inval_extension","spatial_ref_sys","hyper","chunk"),
all=FALSE,...)  {
  
  out <- dbListTables(ssdb)
  if (all==FALSE) {
    out <- out[!(out %in% ignored)]
    for (ign in ignored) out <- out[!str_detect(out,ign)]
  }
  return(out)
  
  
})






