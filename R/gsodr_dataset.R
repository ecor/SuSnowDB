NULL
#' Create GSOD Weather Dataset from (UDER DEVELOPMENT)
#'
#' @param ... arguments for \code{\link{get_GSOD}}
#' @param x data frame returned from a call of \code{\link{get_GSOD}}. Default is \code{NULL}.
#' @param measurement_types data frame from a CSV file containing table with measuremet type attributes . It must contain an additional column \code{has_flag} .See \code{measurement_types} table and default. 
#' @param crs Coordinate refence system. Default is 4326 (epsg). 
#' @param description_url,location_url,location_source,use_limitations attributes for \code{locations} table. See \code{locations} table and default. 
#' @param location_code0,variable_code0 attributes for \code{locations} and \code{measurement_types} tables, as above. If it is one-element,it may contain a sub-string "\code{\%s}" that will be replaced with the GSOD station identification code (STNID).
#'   
#'        
#'
#' 
#' 
#' @importFrom dplyr full_join
#' @importFrom GSODR get_GSOD
#' @export
#' @examples
#'  library(readr)
#'  x <- system.file("west_africa_gsod_extdata/gsod_bf.csv",package="SuSnowDB") %>% read_csv()
#'  out <- gsodr_dataset(x=x)
#'  
#'  \dontrun{
#'  t <- system.time(out <- gsodr_dataset(years=2020,country="IT"))
#'  }

gsodr_dataset <- function(x=NULL,...,description_url="https://www7.ncdc.noaa.gov/CDO/GSOD_DESC.txt",location_code0="GSOD_%s",variable_code0="GSODR_%s",location_url="https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc%3AC00516/html#",use_limitations='The following data and products may have conditions placed on their international commercial use. They can be used within the U.S. or for non-commercial international activities without restriction. The non-U.S. data cannot be redistributed for commercial purposes. Re-distribution of these data by others must provide this same notification." WMO Resolution 40. NOAA Policy',location_source="GSOD",crs=4326,measurement_types=read.table(system.file("west_africa_gsod_extdata/gsod_variable_measurement_types_v1.csv",package="SuSnowDB"),header=TRUE,sep=",",stringsAsFactors = FALSE)) {
  
   if (is.null(x)) x <- get_GSOD(...)
   if (nrow(x)==0) x <- NULL
   if (is.null(x)) return(x)
   
   
   ilocations <- which(!duplicated(x$STNID))
   ###
   x$location_code0 <- location_code0 %>% sprintf(x$STNID)
   x$description <- description_url
   
   ###
   
   ###xout <<- x
   nlocs <-c("location_code0","STNID","NAME","ISO2C","COUNTRY_NAME","LATITUDE","LONGITUDE","ELEVATION","description")
   locations <- as.data.frame(x[ilocations,])[,nlocs]
   locations <- locations %>% st_as_sf(coords=c("LONGITUDE","LATITUDE"),crs=crs)
   names(locations)[names(locations)=="STNID"] <- "location_code"
   names(locations)[names(locations)=="ISO2C"] <- "country_code_iso_3166_1"
   names(locations)[names(locations)=="NAME"] <-   "location_name"
   names(locations)[names(locations)=="COUNTRY_NAME"] <-   "country_name"
   names(locations)[names(locations)=="ELEVATION"] <-   "altitude"
   locations$location_url <- location_url
   locations$location_source <- location_source
   locations$use_limitations <- use_limitations
   
   locations$city_name <- as.character(NA)              
   locations$country_code_iso_3166_2 <- as.character(NA)
   
   
   
   ###return(locations)
   
   ##locations <- NULL
    # * __location_code__:location code ,alphanumeric;
    # 
    # * __location_code0__:location code (PRIMARY KEY),alphanumeric,unique;
    # * __location_name__	:location human readable (geographic) name
    # * __altitude__:	altitude in meters above sea level; 
    # * __city_name__:	name of the city related to the location (if exsists);
    # * __country_code_iso_3166_1__:	ISO 3166-1 administrative area code (e.g  country code)(see https://en.wikipedia.org/wiki/ISO_3166-1);
    # * __country_code_iso_3166_2__:	ISO 3166-2 administrative area code (e.g. region/province/state code) (see https://en.wikipedia.org/wiki/ISO_3166-2);
    # * __country_name__	Country Human Readable Name;
    # * __description__	Description of the location/siter;
    # * __geometry__	Geospatial coordinates (in case of point), managed by geospatial ttols (e.g. sf or postGIS);
    # * __location_source__	Source of the data for the location;
    # * __location_url__	Reference URL for the location;
    # * __use_limitations__	Terms of Use for location data and metadata.
    
    
    
    ####
    nmeas <-  c("location_code0","YEARMODA", "TEMP","TEMP_ATTRIBUTES","DEWP","DEWP_ATTRIBUTES", "SLP","SLP_ATTRIBUTES","STP",
    "STP_ATTRIBUTES","VISIB","VISIB_ATTRIBUTES" ,"WDSP","WDSP_ATTRIBUTES","MXSPD","GUST", 
    "MAX","MAX_ATTRIBUTES","MIN","MIN_ATTRIBUTES","PRCP","PRCP_ATTRIBUTES","SNDP",
    "I_FOG","I_RAIN_DRIZZLE","I_SNOW_ICE","I_HAIL","I_THUNDER","I_TORNADO_FUNNEL","EA",
    "ES","RH")
   ### measurements <- x[,nmeas]
    
    measurement_types <- measurement_types
    measurement_types$variable_code0 <- measurement_types$variable %>% sprintf(fmt=variable_code0)
    gsodr_ref_url <- "https://joss.theoj.org/papers/10.21105/joss.00177"
    measurement_types$description <- "%s  Data downloaded though GSDOR R package (<%s>)" %>% sprintf(measurement_types$description,gsodr_ref_url)
    # * __variable_code0__: variable code (PRIMARY KEY),alphanumeric,unique;
    # * __variable__:    human readable name of the variable;
    # * __unit__: measurement unit;
    # * __description__: textual description of the measurement type;
    # * __measurement_time_interval__: time interval at which variable has been measured and/or reported, e.g. hourly, daily, monthly, etc.. .
    
   # * __time__:timestamp-TZ instant   ;
    #* __value__: numeric value of the measurement (expressed in unit of _variable_code0_)    ;
    #* __flag__:  flag related to the measurent (see _description_);
    #* __location_code0__:  location code (FOREIGN KEY from table _locations_) of the location of the measurement;
    #* __variable_code0__:  variable code (FOREIGN KEY from thable _measurement_types) of the measured variable;
    #* __description__:  textual description and notes of the measurement.
    ##measurements <- NULL
    
  measurements <- as.data.frame(x)[,c(c("location_code0","YEARMODA"),measurement_types$variable)]
  
  names(measurements) <- c("location_code0","time",(measurement_types$variable %>% sprintf(fmt=variable_code0)))
  
  ###
  flags <- as.data.frame(x)[,c(c("location_code0","YEARMODA"),(measurement_types$variable[measurement_types$has_flag] %>% sprintf(fmt="%s_ATTRIBUTES"))) ]
  names(flags) <- c("location_code0","time",(measurement_types$variable[measurement_types$has_flag] %>% sprintf(fmt=variable_code0)))
  
  flags <- melt(flags,id=c("location_code0","time"))
  names(flags)[names(flags)=="value"] <- "flag"
  
  measurements <- melt(measurements,id=c("location_code0","time")) 
 ### xmeasurements <<- measurements
 ####  xflags <<- flags
  
  measurements <- measurements %>% full_join(flags) %>% as_tibble()
  names(measurements)[names(measurements)=="variable"] <- "variable_code0"
  measurements$description <- as.character(NA)
  ###measurements$description <- sprintf("NCEI URL: description_url: %s
  
  return(list(locations=locations,measurement_types=measurement_types,measurements=measurements))
 ## return(list(locations=locations,measurement_types=measurement_types,measurements=measurements))
}
