NULL
#' 
#' Crop or get a dataset from ssdb
#'
#' @param ssdb database (preferably PostgresSQL) connection (see output of \code{\link{dbConnect}}) or on  a generic object or a dataset, e.g. an object returned by \code{\link{gsodr_dataset}} OR a list of these objects.
#' @param crop argument \code{"y"} of \code{\link{st_crop}}
#' @param table_name vector with names of the tables . Default is \code{get_value_table_names_from_ssdb(conn)}. See also @seealso \code{\link{get_value_table_names_from_ssdb}}.
#' @param variables_code0 code of the types (\code{variable_code0}) of the measurements that are requested to be extracted.
#' @param ... further arguments for  \code{\link{get_measurements_from_ssdb}}
#' @export
#' 
#' 
#' @importFrom sf st_crop
#' @examples 
#' 
#' library(magrittr)
#' library(raster)
#' 
#' ssdb <- system.file('sumava_extdata/sumava.rds',package="SuSnowDB") %>% readRDS()
#' 
#' crop_ext <- system.file('sumava_extdata/sumava_v2_extent.tif',package="SuSnowDB")  %>% raster() 
#' 
#' ssdb_cropped <- crop_dataset_from_ssdb(ssdb,crop=crop_ext)
#' 
#'

crop_dataset_from_ssdb <- function (ssdb,crop=NULL,table_name=get_value_table_names_from_ssdb(ssdb),variables_code0=NULL,...){
  
  out <- list()
  locs <- get_locations_from_ssdb(ssdb)
  out$locations <- st_crop(x=locs,y=crop,...)
  locations_code0 <- out$locations$location_code0
  vars <- get_measurement_types_from_ssdb(ssdb)
  if (!is.null(variables_code0)) {
    
   ## variables_code0 <- variables_code0[variables_code0 %in% vars$variable_code0]
    vars <- vars[which(vars$variable_code0 %in% variables_code0),]
    variables_code0 <- vars$variable_code0
    
    
    
  }
  out$measurement_types <- vars
##  vt_names <- get_value_table_names_from_ssdb(ssdb) 
  for (nn in table_name) {
    
    out[[nn]] <- get_measurements_from_ssdb(ssdb,table_name=nn,all=FALSE,variables_code0=variables_code0,locations_code0=locations_code0,...)
    
  }
  
  
  
   
 ###get_measurements_from_ssdb <- function (conn,table_name="measurements",all=FALSE,variables_code0=NULL,locations_code0=NULL,returns.data.frame=FALSE,start_time=NA,end_time=NA,...) {
  
  return(out)
  
}



