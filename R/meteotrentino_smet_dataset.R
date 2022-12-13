NULL
#' Create MeteoTrentino Weather Dataset from a directory of SMET files downloaded by the Trentino Open Data Portal
#'
#' @param data_dir directory containing original CHMI and DWD snow data
#' 
#' @param smet_files SMET files used contained in the \code{data_dir} directory. Full path to SMET files if \code{data_dir} is \code{NULL}.
#' @param crs Coordinate refence system. Default is 4326 (epsg). 
#' @param country_name,country_code_iso_3166_1,country_code_iso_3166_2,location_source,use_limitations,location_description attributes for \code{locations} table. See \code{locations} table and default. 
#' @param location_url attribute for \code{locations} table, as above. If it is one-element,it may contain a sub-string "\code{\%s}" that will be replaced with the SMET station identification code.
#' @param measurement_types CSV file containing table with measuremet type attributes . See \code{measurement_types} table and default. 
#' @param ... further arguments
#' 
#' 
#' @importFrom RSMET as.smet header_attr
#' @importFrom stringr str_detect
#' @importFrom utils read.table
#' @importFrom sf st_as_sf
#' @importFrom dplyr left_join
#' 
#' 
#' @export
#' @examples
#'  data_dir <- system.file("snow_extdata",package="SuSnowDB")
#'  \dontrun{
#'  out <- meteotrentino_smet_dataset(smet_files=c("T0175","T0179"))
#'  }

meteotrentino_smet_dataset <- function(data_dir=system.file("smet_extdata/meteotrentino",package="SuSnowDB"),smet_files=NULL,crs=4326,
                                       country_name="Italy",
                                       country_code_iso_3166_1="IT",
                                       country_code_iso_3166_2="IT-TN",
                                       location_source=meteotrentino_defaults("location_source"), ##"MeteoTrentino",
                                       location_url=meteotrentino_defaults("location_url"),##"http://storico.meteotrentino.it/web.htm?ppbm=%s&rs&1&df",
                                       measurement_types=meteotrentino_defaults("location_url"), ##read.table(system.file("smet_extdata/smet_variables.csv",
                                ##package="SuSnowDB"),header=TRUE,sep=",",stringsAsFactors = FALSE),
                                use_limitations=meteotrentino_defaults("use_limitations"), ##"https://dati.trentino.it/dataset/dati-recenti-delle-stazioni-meteo",
                                location_description=meteotrentino_defaults("location_description"), ####"https://dati.trentino.it/dataset/dati-recenti-delle-stazioni-meteo",
                                       ...){ 
  
  files <- list.files(data_dir,pattern=".smet",full.names = TRUE)
  names(files) <- list.files(data_dir,pattern=".smet",full.names = FALSE)
 
  if (!is.null(smet_files)) {
    
    iext <- which(str_sub(smet_files,-5,-1)!=".smet")
    smet_files[iext] <- paste0(smet_files[iext],".smet")
    
    ismet <- which(names(files) %in% smet_files)
    if (length(ismet)>0) {
      files <- files[ismet]
    } else {
      
      files <- smet_files
    }
    ####
    
    ####
    smets <- lapply(files,FUN=as.smet) 
    
    ##
    ## c("location_code","location_code0","location_name","altitude" ,"city_name",             
    ## "country_code_iso_3166_1","country_code_iso_3166_2","country_name" ,"description","geometry",
    ## "location_source" ,"location_url" ,"use_limitations")
    ##
    
    
    
    locations <- list()
    locations$location_code <- header_attr(smets,attr="station_id")
    locations$latitude <- header_attr(smets,attr="latitude")
    locations$longitude <- header_attr(smets,attr="longitude")
    locations$altitude <- header_attr(smets,attr="altitude")
    locations$location_name <- header_attr(smets,attr="station_name")
    locations$country_name <- country_name
    locations$country_code_iso_3166_1=country_code_iso_3166_1
    locations$country_code_iso_3166_2=country_code_iso_3166_2
    locations$location_source=location_source
    locations$use_limitations <- use_limitations
    locations$descriptions <- location_description
    if ((length(location_url)==1) & str_detect(location_url[1],"%s")) {
      
      locations$location_url <- location_url %>% sprintf(locations$location_code)
      
    } else {
      
      locations$location_url <- location_url
    }
    
    locations$location_code0 <- paste(locations$country_code_iso_3166_2, locations$location_code,sep="_")
   
    ###
   ## locations$geometry <- data.frame(locations$latitude,locations$longitude) %>% st_
    ###
    locations <- as.data.frame(locations) %>% st_as_sf(coords=c("longitude","latitude"),crs=crs)
    
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
    measurement_types0 <- header_attr(smets,attr="fields")
    measurement_types0 <- unique(as.vector(unlist(measurement_types0)))
    measurement_types <- measurement_types[measurement_types$variable_smet_code %in% measurement_types0,]
    measurement_types$variable_code0 <- sprintf("SMET_%s",measurement_types$variable_smet_code)
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
    measurements <- map(smets,as.data.frame,use.melt=TRUE,add.header=TRUE) %>% do.call(what="rbind")
    measurements$flag <- as.character(NA)
    measurements$description <- as.character(NA)
    nn_mmeasurements01 <- c("timestamp","value","flag","station_id","variable","description")
    nn_mmeasurements02 <- c("time","value","flag","location_code0","variable_code0","description")
    measurements <- measurements[,nn_mmeasurements01]
    names(measurements)[names(measurements)=="station_id"] <- "location_code"
    names(measurements)[names(measurements)=="variable"] <- "variable_smet_code"
    names(measurements)[names(measurements)=="timestamp"] <- "time"
    measurements <- measurements %>% left_join(as.data.frame(locations)[,c("location_code","location_code0")]) %>% left_join(measurement_types[,c("variable_code0","variable_smet_code")]) 
  
    ####
    itemp <- which(measurements$variable_smet_code %in% c("TA","TSS","TSG"))
    irh <- which(measurements$variable_smet_code=="RH")
    ihs <- which(measurements$variable_smet_code=="HS")
    ihn <- which(measurements$variable_smet_code=="HN")
    ####
    measurements$value[itemp] <- measurements$value[itemp]-273.15 ## From Kelvin to Celsius Degree
    measurements$value[irh] <- measurements$value[irh]*100  ## From [dimensionless 0-1] to [%]
    measurements$value[ihs] <- measurements$value[ihs]*100  ## From meter [m] to centimeter [cm]
    measurements$value[ihn] <- measurements$value[ihn]*100  ## From meter [m] to centimeter [cm]
    ####
    measurements <- measurements[,nn_mmeasurements02]
     ## str(measurements)
    
  ###  measurements <- measurements
  }
  return(list(locations=locations,measurement_types=measurement_types,measurements=measurements))
 ## return(list(locations=locations,measurement_types=measurement_types,measurements=measurements))
}
