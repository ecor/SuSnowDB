NULL
#' Union of one or more Weather Datasets from (UDER DEVELOPMENT)
#'
#' 
#' @param x dataset, e.g. an object returned by \code{\link{gsodr_dataset}}
#' @param ... further arguments, e.g other object like \code{x}.
#' @param nn_measurement_types see \code{\link{add_measurement_types_into_ssdb}}.
#' @param nn_locations see \code{\link{add_locations_into_ssdb}}.
#' @param nn_measurements see \code{\link{add_measurements_into_ssdb}}.    
#' @param warn logical argument if any elements of \code{x,...} are not dataset-like objects, function displays a warning message. Default is \code{TRUE}.
#' @param err  logical argument if any elements of \code{x,...} are not dataset-like objects, function returns an error. Default is \code{!warn}.
#'
#' 
#'

#' @export
#' @examples
#'  library(readr)
#'  x <- system.file("west_africa_gsod_extdata/gsod_bf_v2.csv",package="SuSnowDB") %>% read_csv() %>% gsodr_dataset()
#'  y <- system.file("west_africa_gsod_extdata/gsod_ml.csv",package="SuSnowDB") %>% read_csv() %>% gsodr_dataset()
#'  z <- system.file("west_africa_gsod_extdata/gsod_sn.csv",package="SuSnowDB") %>% read_csv() %>% gsodr_dataset()
#' 
#'  out <- datasets_union(x,y,z)
#'  
#'  outl <- datasets_union(list(x,y,z))
#'  
#'  is_dataset(out)
#'  is_dataset(outl)
#'  
datasets_union <- function (x,...,  warn=FALSE,err=!warn,nn_measurement_types=default_nn_measurement_types(),
                            nn_locations=default_nn_locations(),
                            nn_measurements=default_nn_measurements()) {
  
  
  ll <- list(...)
  if (length(ll)>0) {
    nll <- length(ll)
    ll[[nll+1]] <- x
    x <- ll[c(nll+1,1:nll)]
    
  } else if (is_dataset(x,nn_measurement_types=nn_measurement_types,nn_locations=nn_locations,nn_measurements=nn_measurements)) {
    
    
    x <- list(x)
    
  }
  
  if (!are_all_datasets(x)) {
    
    ii <- which(!sapply(X=x,FUN=is_dataset,nn_measurement_types=nn_measurement_types,nn_locations=nn_locations,nn_measurements=nn_measurements))
    msg <- sprintf("Object %s are not datasets!",as.character(paste(ii,collapse=",")))
    if (err==TRUE) {
      
      stop(msg)
      
    } else if (warn==TRUE) {
      
      warning(msg)
    }  
    
    
    
  }
  
  out <- list()
  
  out$locations <- lapply(X=x,FUN=function(y,nn_locations=nn_locations){y$locations[,nn_locations]}) %>% do.call(what="rbind")
  out$measurement_types <- lapply(X=x,FUN=function(y,nn_measurement_types=nn_measurement_types){y$measurement_types[,nn_measurement_types]}) %>% do.call(what="rbind")
  out$measurements <- lapply(X=x,FUN=function(y,nn_measurements=nn_measurements){y$measurements[,nn_measurements]}) %>% do.call(what="rbind")
  
  ### CEHECK LOCATIONS 
  
  ilocs <- which(!duplicated(out$locations$location_code0))
  out$locations <- out$locations[ilocs,]
    ##
    ##
    ##
  ivars <- which(!duplicated(out$measurement_types$variable_code0))
  out$measurement_types <- out$measurement_types[ivars,]
  return(out)
  
  }
  
  
  
  
 
