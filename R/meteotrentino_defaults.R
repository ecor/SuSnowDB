NULL
#' Meteotrentino defaults
#
#' @param nn argument
#'
#' @export
#'
#' @examples
#' 
#' 
#' defaults <- meteotrentino_defaults()
#' 

meteotrentino_defaults <- function(nn=NULL) {
  # country_name="Italy",
  # country_code_iso_3166_1="IT",
  # country_code_iso_3166_2="IT-TN",
  # location_source="MeteoTrentino",
  # location_url="http://storico.meteotrentino.it/web.htm?ppbm=%s&rs&1&df",
  # measurement_types=read.table(system.file("smet_extdata/smet_variables.csv",
  #                                          package="SuSnowDB"),header=TRUE,sep=",",stringsAsFactors = FALSE),
  # use_limitations="https://dati.trentino.it/dataset/dati-recenti-delle-stazioni-meteo",
  # location_description="https://dati.trentino.it/dataset/dati-recenti-delle-stazioni-meteo",
  
  out <- list()
  out$location_url    <- "http://storico.meteotrentino.it/web.htm?ppbm=%s&rs&1&df"
  out$use_limitations <- 'https://dati.trentino.it/dataset/dati-recenti-delle-stazioni-meteo'
  out$location_source <- "MeteoTrentino"
  out$location_description = "https://dati.trentino.it/dataset/dati-recenti-delle-stazioni-meteo"
  out$measurement_types <- read.table(system.file("smet_extdata/smet_variables.csv",package="SuSnowDB"),header=TRUE,sep=",",stringsAsFactors = FALSE)
  if (!is.null(nn)) out <- out[[nn]]
  return(out)
}