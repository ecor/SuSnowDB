NULL
#' Is it a dataset formatted for a SuSnowDB ? Are they all datasets formatted for a SuSnowDB ?
#'
#' 
#' @param x a generic object or a dataset, e.g. an object returned by \code{\link{gsodr_dataset}} OR a list of these objects.
#' @param  nn_measurement_types see \code{\link{add_measurement_types_into_ssdb}}.
#' @param nn_locations see \code{\link{add_locations_into_ssdb}}.
#' @param nn_measurements see see \code{\link{add_measurements_into_ssdb}}.
#' @param ... further arguments, other object like \code{x} 
#' @export
#' @examples
#'  library(readr)
#'  x <- system.file("west_africa_gsod_extdata/gsod_bf.csv",package="SuSnowDB") %>% read_csv()
#'  val <- gsodr_dataset(x=x)
#'  out <- is_dataset(val)
#'  
#'  val2 <- data.frame(a=3,b=1:10)
#'  out2 <- is_dataset(val2)
#'
#'  are_all_datasets(val,val2)
#'  are_all_datasets(list(val,val2))
#'  
#'  

## MAKE A METHOD is_ssbd ?? 


is_dataset <- function (x,
                        nn_measurement_types=default_nn_measurement_types(),
                        nn_locations=default_nn_locations(),
                        nn_measurements=default_nn_measurements()) {
 ### c("time","value","flag","location_code0","variable_code0","description")
  out <- FALSE
  if (all(c("locations","measurement_types","measurements") %in% names(x))) {

    out <- TRUE
    nvars <- nrow(x$measurement_types)
    nlocs <- nrow(x$locations)
    nvals <- nrow(x$measurements)
    ####
   
    if (nvars>0) out <- out & all(nn_measurement_types %in% names(x$measurement_types))
    if (nlocs>0) out <- out & all(nn_locations %in% names(x$locations))
    if (nvals>0) out <- out & all(nn_measurements %in% names(x$measurements))
 
    if (out==TRUE) {
      
      out <- out & !any(duplicated(x$locations$location_code0)) & !any(duplicated(x$measurement_types$variable_code0))
      
      out <- out & all(x$measurements$location_code0 %in% x$locations$location_code0)
      out <- out & all(x$measurements$variable_code0 %in% x$measurement_types$variable_code0)
      
      
      
      
    }
    
  } else {
    
    out <- FALSE
  }

  return(out)  
}      

NULL
#'
#' @rdname is_dataset
#' 
#' @export
#' 
are_all_datasets <- function (x, ...,
                        nn_measurement_types=default_nn_measurement_types(),
                        nn_locations=default_nn_locations(),
                        nn_measurements=default_nn_measurements())

{
  out <- FALSE
  ll <- list(...)
  if (length(ll)>0) {
    nll <- length(ll)
    ll[[nll+1]] <- x
    x <- ll[c(nll+1,1:nll)]
    
  } else if (is_dataset(x,nn_measurement_types=nn_measurement_types,nn_locations=nn_locations,nn_measurements=nn_measurements)) {
    
    
    x <- list(x)
    
  }
  
  out <- sapply(X=x,FUN=is_dataset,nn_measurement_types=nn_measurement_types,nn_locations=nn_locations,nn_measurements=nn_measurements)
  out <- all(out)
  


  return(out)

}


