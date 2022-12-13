NULL
#' GSOD defaults
#
#' @param nn argument
#'
#' @export
#'
#' @examples
#' 
#' 
#' defaults <- gsod_defaults()
#' 

gsod_defaults <- function(nn=NULL) {
  
  out <- list()
  out$location_url    <- "https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc%3AC00516/html#"
  out$use_limitations <- 'The following data and products may have conditions placed on their international commercial use. They can be used within the U.S. or for non-commercial international activities without restriction. The non-U.S. data cannot be redistributed for commercial purposes. Re-distribution of these data by others must provide this same notification." WMO Resolution 40. NOAA Policy'
  out$location_source <- "GSOD"
  out$measurement_types <- read.table(system.file("west_africa_gsod_extdata/gsod_variable_measurement_types_v1.csv",package="SuSnowDB"),header=TRUE,sep=",",stringsAsFactors = FALSE)
  if (!is.null(nn)) out <- out[[nn]]
  return(out)
}