NULL
#' Column names of the SuSnowDB Tables
#'
#'See Vignette
#'
#' @param nn names of the columns in the "locations" table. See the vignette.
#'
#' @export
#' @examples 
#' 
#' default_nn_measurement_types()
#' default_nn_measurements()
#' default_nn_locations()
#' 
#' 
default_nn_measurement_types <- function(nn=c("variable_code0","variable","unit","description","measurement_time_interval")){return(nn)}
NULL
#'
#' @rdname default_nn_measurement_types
#' @export

default_nn_locations <- function(nn=c("location_code","location_code0","location_name","altitude" ,"city_name",             
               "country_code_iso_3166_1","country_code_iso_3166_2","country_name" ,"description","geometry",
               "location_source" ,"location_url" ,"use_limitations")){return(nn)}
NULL
#'
#' @rdname default_nn_measurement_types
#' @export


default_nn_measurements <- function(nn=c("time","value","flag","location_code0","variable_code0","description")){return(nn)}