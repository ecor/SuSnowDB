## 
##  Reading the zip files of the Czech Meteorological (CHMI) Service / Snow depth observation
## URL: https://www.chmi.cz/historicka-data/pocasi/denni-data/Denni-data-dle-z.-123-1998-Sb#
##
## Author : Emanuele Cordano
## email  : emanuele.cordano@rendena100.eu
## Date   : 2021-04-20  
##
# rm(list=ls())
# library(magrittr)
# library(dplyr)
# library(purrr)
# library(stringr)
# library(sf)
# library(reshape2)
##

NULL
#' Create Sumava Snow Dataset from DWD and CHMI sources
#'
#' @param data_dir directory containing original CHMI and DWD snow data
#' @param remove_multipoints logical. It splits station and related time series if a station has resulted in different (even if near) point sites in different time periods (e.g. some CHMI stations). DEfault is \code{TURE}.
#' @param snap_distance maximum distance  expressed  in meters between points for snapping. Default is 5.
#' 
#'     
#' @importFrom stringr str_sub str_split str_replace str_trim 
#' @importFrom utils unzip
#' @importFrom purrr map
#' @importFrom sf st_sf st_sfc  st_point st_distance
#' @importFrom magrittr %>% extract2
#' @importFrom dplyr group_by as_tibble select filter summarize ungroup
#' @importFrom rdwd dataDWD selectDWD
#' @importFrom rlang .data
#' @importFrom reshape2 melt
#'
#'@export
#'@examples
#'  data_dir <- system.file("snow_extdata",package="SuSnowDB")
#'  
#'  out <- sumava_snow_dataset()
#'  is_dataset(out)
#'  

sumava_snow_dataset <- function(data_dir=system.file("snow",package="SuSnowDB"),remove_multipoints=TRUE,snap_distance=5){ ##} ,dwd_download_snow_path=tempdir()){ ##'/home/ecor/activity/2021/local/SuSnowDB/inst/snow_extdata') { 
  
  ## DA COMPLETARE ... 
  if ((data_dir %>% str_sub(-1,-1))=="/") data_dir <- data_dir %>% str_sub(1,-2)
  
  
  chmi_snow_path <- data_dir %>% paste0('/chmi/')
  dwd_snow_path <-  data_dir %>% paste0('/dwd/')
  ##dwd_download_snow_path <- tempdir()
  
  ## CHMI DATA
  regions <- c("chmi_pilsen","chmi_southern_bohemia") 
  regions_iso_3166_2 <- c("CZ-PL","CZ-JC") ###"CZ-PL or CZ-CZ-JC" <- c("chmi_pilsen","chmi_southern_bohemia") 
  names(regions) <- regions
  names(regions_iso_3166_2) <- regions
  ## zip filess were already downloaded
  zips <- regions %>% sprintf(fmt=paste(chmi_snow_path,"%s",sep="/"))  %>% map(list.files,full.name=TRUE,pattern="_SCE_N.csv.zip")
  
  names(zips) <- regions
  
  ####
  
  uu <- list()
  exdir <- tempdir() 
  
  ####
  data <- NULL ##list()
  metadata <- NULL
  metadata_l <- list()
  
  for (region in regions) {
    ##print(region)
    for (it in zips[[region]]) {
      
      out <- it %>% unzip(exdir=exdir) %>% readLines(encoding = "Latin 3 (ISO-8859-3)",warn=FALSE) ##readLines(encoding = "Latin 3 (ISO-8859-3)")
      Encoding(out) <- "latin1"
      hds <- c("PRVEK","DATA","METADATA","PØÍSTROJE")
      
      aout <- which(out %in% hds)
      bout <- c((aout-1)[-1],length(out))
      
      uu[[it]] <- list()
      
      for (i in 1:length(aout)) {
        
        vtmp <- out[aout[i]:bout[i]]  %>% str_trim()
        vtmp <- vtmp[vtmp!=""]
        uu[[it]][[i]] <- vtmp  
        
      }
      names(uu[[it]]) <- out[aout]
      
      uu[[it]] <- uu[[it]][hds]
      
      
      
    }
    
    writeLines(uu[[1]]$PØÍSTROJE)
    writeLines(uu[[1]]$PRVEK)
    writeLines(uu[[1]]$METADATA)
    
    # writeLines(uu[[1]]$PØÍSTROJE)
    # PØÍSTROJE
    # Pøístroj;Zaèátek mìøení;Konec mìøení;Výška pøístroje
    # Pravítko;01.04.2005;31.12.2019;0
    #
    
    ## ENGLISH
    # INSTRUMENTS
    # Instrument; Start of measurement; End of measurement; Height of the instrument
    # Ruler; 01.04.2005; 31.12.2019; 0
    
    
    # >writeLines(uu[[1]]$METADATA)
    # METADATA
    # Stanice ID;Jméno stanice;Zaèátek mìøení;Konec mìøení;Zemìpisná délka;Zemìpisná šíøka;Nadmoøská výška
    # C1BRZKTT;Bøezník, Hranièní sla<9d>;04.06.1991;31.12.2019;13,4892;48,9547;1154
    
    ## ENGLISH
    # METADATA
    # Station ID; Station name; Start of measurement; End of measurement; Longitude; Latitude; Altitude
    # C1BRZKTT; Bøezník, Hranièní sla <9d>; 04.06.1991; 31.12.2019; 13,4892; 48,9547; 1154
    
    
    
    # > writeLines(uu[[1]]$PRVEK)
    # PRVEK
    # Celková výška snìhové pokrývky [SCE.07:00, cm]
    # 
    # Popis:
    #   Hodnota - hodnota prvku
    # Pøíznak - rozšiøující informace o hodnotì
    # 
    # Pøíznak;Popis
    # A;Ovlivnìno umìlým snìžením
    # N;Nesouvislá snìhová pokrývka
    # P;Poprašek
    # R;Padal a roztál
    # U;Mìøení není možné
    #
    
    ## ENGLISH
    # ELEMENT
    # Total snow cover height [SCE.07: 00, cm]
    #
    # Description:
    #Value - the value of the element
    # Symptom - extended value information
    #
    # Symptom; Description
    # A; Affected by artificial reduction
    # N; Discontinuous snow cover
    # P; Powder
    #R; He fell and melted
    # U; Measurement is not possible
    
    
    
    
    
    # > writeLines(head(uu[[1]]$DATA))
    # DATA
    # Rok;Mìsíc;Den;Hodnota;Pøíznak
    # 2005;04;1;122;
    # 2005;04;2;;
    # 2005;04;3;;
    # 2005;04;4;;
    
    ## ENGLISH
    # DATA
    # Year; Month; Day; Value; flag
    # 2005; 04; 1; 122;
    # 2005; 04; 2 ;;
    # 2005; 04; 3 ;;
    # 2005; 04; 4 ;;
    
    #######
    #######
    
    
    
    
    
    for (i in 1:length(uu)) {
      
      out <- uu[[i]]$DATA
      out <- out[-c(1,2)]
      nn <- c("Year","Month","Day","value","flag")
      out <- out %>% str_split(";",simplify=TRUE) %>% as.data.frame()
      names(out) <- nn
      ## 
      
      out$time <- paste(out$Year,out$Month,out$Day,sep="-") %>% as.Date()
      out$value <- as.numeric(out$value)
      out <- out[,c("time","value","flag")]
      
      ###
      
      
      
      nn_metadata <- str_split("Station_ID; Station_name; Start_of_measurement; End_of_measurement; Longitude; Latitude; Altitude",";",simplify=TRUE) %>% str_trim()
      metaout <- uu[[i]]$METADATA[-c(1,2)] %>% str_split(";",simplify=TRUE) %>% as.data.frame()
      names(metaout) <- nn_metadata
      ##
      metaout$Start_of_measurement <- as.Date(metaout$Start_of_measurement,format="%d.%m.%Y")
      metaout$End_of_measurement <- as.Date(metaout$End_of_measurement,format="%d.%m.%Y")
      metaout$Latitude <-  metaout$Latitude %>% str_replace(",",".") %>% as.numeric()
      metaout$Longitude <-  metaout$Longitude %>% str_replace(",",".") %>% as.numeric()
      metaout$Altitude <- metaout$Altitude %>% as.numeric() %>% suppressWarnings()
      crs <- 4326 ## epsg 4326 crs applied
      geometry <- metaout[,c("Longitude","Latitude")] %>% t() %>% as.data.frame() %>% as.list() %>% lapply(st_point) %>% st_sfc()
      metaout <- st_sf(metaout,geometry=geometry,crs=crs) ## epsg 4326 crs applied
      out$Station_ID <- metaout$Station_ID[1]
      ###attr(out,"metadata")  <- metaout
      metaout$region_iso_3166_2 <- regions_iso_3166_2[region] 
      metadata_l[[i]] <- metaout 
      data <- rbind(data,out)
      metadata <- rbind(metadata,metaout)
      
      
      
    }
  }
  
  NoteText <- "Measurement on Snow Height  (SCE) Unit: cm Start_of_Measurement:%s End_of_Measurement:%s Longitude:%s Latitude:%s Altitude:%s" ## EC 20220127
  metadata1 <- metadata %>% group_by(.data[["Station_ID"]]) %>% summarize(Station_name=rev(.data[["Station_name"]])[1],Altitude_L=rev(.data[["Altitude"]])[1],
                                                                          Note=sprintf(NoteText,paste(.data[["Start_of_measurement"]],collapse=";"),
                                                                                       paste(.data[["End_of_measurement"]],collapse=";"),paste(.data[["Longitude"]],collapse=";"),paste(.data[["Latitude"]],collapse=";"),paste(.data[["Altitude"]],collapse=";")),region_iso_3166_2=.data[["region_iso_3166_2"]][1]) %>% ungroup()### DA FINIRE
  ## EC 20220127
  metadata1$URL <- "https://www.chmi.cz/historicka-data/pocasi/denni-data/Denni-data-dle-z.-123-1998-Sb"
  metadata1$source <- "CHMI" 
  names(metadata1)[names(metadata1)=="Altitude_L"] <- "Altitude" 
  
  
  
  
  ## HOW TO CLEAN 
  ## HOW TO 
  
  #########################################
  #########################################
  #########################################
  # CREATE TABLE locations (
  #   location_id SERIAL PRIMARY KEY,
  #   location_code0 TEXT NOT NULL,
  #   location_code  TEXT NOT NULL,
  #   location_name TEXT,
  #   city_name TEXT,
  #   country_name TEXT,
  #   country_code_iso_3166 TEXT NOT NULL,
  #   description TEXT,
  #   geometry geometry,
  #   UNIQUE(location_code0)
  #
  ########################################
  ## LOCATIONS 
  locations <- metadata1 
  names(locations)[names(locations)=="Station_ID"] <- "location_code"  
  names(locations)[names(locations)=="Station_name"] <- "location_name"  
  names(locations)[names(locations)=="source"] <- "location_source"  
  names(locations)[names(locations)=="Note"] <- "description"
  names(locations)[names(locations)=="URL"] <- "location_url"
  names(locations)[names(locations)=="region_iso_3166_2"] <- "country_code_iso_3166_2"
  names(locations)[names(locations)=="Altitude"] <- "altitude"
  location_code0_prefix <- "CZ_CHMI_SNOW_%s"
  locations$location_code0 <- locations$location_code %>% sprintf(fmt=location_code0_prefix)
  locations$city_name <- as.character(NA)
  locations$country_name <- "Czech Republic"
  locations$country_code_iso_3166_1 <- "CZ"
  locations$use_limitations <- "https://www.chmi.cz/files/portal/docs/meteo/ok/open_data/Podminky_uziti_udaju.pdf"
  ###locations$country_code_iso_3166_2 <- metadata1$region_iso_3166_2
  ####
  ###
  nn1 <- c("location_code","location_code0","location_name")
  nn2 <- sort(names(locations))
  nn2 <- c(nn1,nn2[!(nn2 %in% nn1)])
  
  ####
  locations <- locations[,nn2]
  ####
  ####
  ####
  ####
  #locations$location_description <- metadata1$Note
  ##locations$location_source <- metadata1$source
  
  ###  MEASURENT_TYPE
  # CREATE TABLE IF NOT EXISTS measurement_types (
  #   variable_code0 TEXT PRIMARY KEY,
  #   variable TEXT NOT NULL,
  #   unit TEXT NOT NULL,
  #   description TEXT,
  #   UNIQUE(variable_code0)
  # );
  ##
  measurement_types <- data.frame(variable_code0="snow_depth_cm",variable="snow depth",unit="cm",
                                  description="snow depth")
  ### MEASUREMENTS 
  # 
  # CREATE TABLE IF NOT EXISTS measurements (
  #   time time NOT NULL,
  #   variable_code0 TEXT REFERENCES measurement_types(variable_code0),
  #   location_code0 TEXT REFERENCES locations(location_code0),
  #   value FLOAT,
  #   flag TEXT,
  #   description TEXT
  # );
  ####
  
  measurements <- data
  
  ###
  names(measurements)[names(measurements)=="time"] <- "time"
  names(measurements)[names(measurements)=="Station_ID"] <- "location_code0"
  measurements$variable_code0 <- measurement_types$variable_code0[1]
  measurements$location_code0 <- data$Station_ID %>% sprintf(fmt=location_code0_prefix)
  measurements$description <- as.character(NA)
  #### ADD DATA DWD 
  ##library(rdwd)  
  #data(rdwd:::geoIndex)  
  #geoIndex <- system.file("data/geoIndex.rda",package="rdwd") %>% load() ##%>% get(envir=globalenv())
  #system.file("data/geoIndex.rda",package="rdwd") %>% load()
  data(geoIndex,package="rdwd")
  
  dir <- dwd_snow_path
  ###dir_gadm <- '/home/ecor/activity/2021/sumava/sumava_validation/data/gadm'
  ####elevation  <- get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath) 
  ###
  ###
  ####
  
  
  
  crs <- 4326
  geoIndex <- geoIndex  %>% filter(.data[["state"]]=="Bayern") %>% filter(!duplicated(.data[["id"]])) ## remova possible duplicated ids (errors)
  geometry  <- geoIndex  %>% dplyr::select(.data[["lon"]],.data[["lat"]]) %>% t() %>% as.data.frame() %>% as.list() %>% lapply(st_point) %>% st_sfc()
  locations_bavaria <- st_sf(geoIndex,geometry=geometry,crs=crs)## %>% st_transform(crs=st_crs(elevation))
  
  
  
  locations_bavaria$zipfile <- selectDWD(locations_bavaria$name,res="daily",var="water_equiv",per="historical") %>% unlist()
  no_snow_text <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate"
  locations_bavaria_snow <- locations_bavaria %>% filter(.data[["zipfile"]]!=no_snow_text)
  
  names(locations_bavaria_snow)[names(locations_bavaria_snow)=="name"] <- "location_name"
  names(locations_bavaria_snow)[names(locations_bavaria_snow)=="id"] <- "location_code"
  names(locations_bavaria_snow)[names(locations_bavaria_snow)=="display"] <- "description"
  names(locations_bavaria_snow)[names(locations_bavaria_snow)=="zipfile"] <- "location_url"
  names(locations_bavaria_snow)[names(locations_bavaria_snow)=="ele"] <- "altitude"
  location_bavaria_code0_prefix <- "DE_DWD_SNOW_%05d"
  locations_bavaria_snow$location_code0 <- locations_bavaria_snow$location_code %>% sprintf(fmt=location_bavaria_code0_prefix)
  locations_bavaria_snow$city_name <- as.character(NA)
  locations_bavaria_snow$location_source <- "DWD"
  locations_bavaria_snow$country_name <- "Germany"
  locations_bavaria_snow$country_code_iso_3166_1 <- "DE"
  locations_bavaria_snow$country_code_iso_3166_2 <- "DE-BY"
  locations_bavaria_snow$use_limitations <- "https://opendata.dwd.de/climate_environment/CDC/Terms_of_use.pdf"
  locations <- rbind(locations,locations_bavaria_snow[,names(locations)])
 
  
  
  layer <- "country_code_iso_3166_2"
  ## mapview::mapview(locations, zcol = layer, col.regions=rainbow(3))
  
  ## Getting DWD data values 
  ##
  ## if errors: type > rdwd::updateRdwd()
  dwd_mes0 <- locations %>% filter(.data[["country_code_iso_3166_2"]]=="DE-BY") %>% dplyr::select(.data[["location_url"]]) %>% extract2(1) 
  dwd_mes1 <- dwd_mes0 %>% dataDWD(dir=dir,read=TRUE) ###
  dwd_mes2 <- dwd_mes1 %>% do.call(what="rbind")
  names(dwd_mes2)[names(dwd_mes2)=="STATIONS_ID"] <- "location_code0"
  names(dwd_mes2)[names(dwd_mes2)=="MESS_DATUM"] <- "time"
  names(dwd_mes2)[names(dwd_mes2)=="QN_6"] <- "flag"
  dwd_mes2$flag <-  paste0("QN_6=",dwd_mes2$flag)
  dwd_mes2$location_code0 <- dwd_mes2$location_code0 %>% sprintf(fmt=location_bavaria_code0_prefix) 
  
  ids <- c("location_code0","time","flag")
  vars <- names(dwd_mes2)[!(names(dwd_mes2) %in% ids)]
  ##>>>"ASH_6"  "SH_TAG" "WASH_6" "WAAS_6" "eor"  
  measurement_types$description[measurement_types$variable_code0=="snow_depth_cm"] <- measurement_types$description[measurement_types$variable_code0=="snow_depth_cm"] %>% sprintf(fmt="%s code_DWD=%s","SH_TAG")
  names(dwd_mes2)[names(dwd_mes2)=="SH_TAG"] <- "snow_depth_cm"
  ##"ASH_6"  "WASH_6" "WAAS_6" 
  measurement_types[nrow(measurement_types)+1,] <- c(variable_code0="swe_mm",variable="snow water equivalent",unit="mm",description="snow water equivalent DWD=WASH_6")[names(measurement_types)]
  names(dwd_mes2)[names(dwd_mes2)=="WASH_6"] <- "swe_mm"
  ##"ASH_6"  "WAAS_6" 
  measurement_types[nrow(measurement_types)+1,] <- c(variable_code0="snow_depth_sample_cm",variable="height of snow pack sample",unit="cm",description="height of snow pack sample DWD=ASH_6")[names(measurement_types)]
  names(dwd_mes2)[names(dwd_mes2)=="ASH_6"] <- "snow_depth_sample_cm"
  
  measurement_types[nrow(measurement_types)+1,] <- c(variable_code0="sampled_swe_mm",variable="sampled snow pack water equivalent",unit="mm",description="sampled snow pack water equivalent DWD=WAAS_6")[names(measurement_types)]
  names(dwd_mes2)[names(dwd_mes2)=="WAAS_6"] <- "sampled_swe_mm"
  
  
  
  ###locations_bavaria_snow$location_code0
  
  # The file produkt*.txt comprises following parameters:
  #  STATIONS_ID station id
  # MESS_DATUM date yyyymmdd
  # QN_6 quality level of next columns coding see paragraph "Quality
  # information"
  # ASH_6 height of snow pack sample cm
  # SH_TAG total snow depth cm
  # WASH_6 total snow water equivalent mm
  # WAAS_6 sampled snow pack water eqivalent mm
  
  # QN_6 Qualitätsniveau der nachfolgenden
  # Spalten code siehe Absatz "Qualitätsinformation"
  # ASH_6 Höhe ausgestochener Schnee cm
  # SH_TAG Schneehöhe cm
  # WASH_6 Wasseräquivalent der Gesamtschneehöhe mm
  # WAAS_6 Wasseräquivalent ausgestochene Schneehöhe mm
  # eor Ende data record
  # Fehlwerte sind mit -999 gekennzeichnet.
  # 
  # "1- only formal control during decoding and import
  # 2- controlled with individually defined criteria
  # 3- ROUTINE control with QUALIMET and QCSY
  # 5- historic, subjective procedures
  # 7- ROUTINE control, not yet corrected
  # 8- quality control outside ROUTINE
  # 9- ROUTINE control, not all parameters corrected
  # 10- ROUTINE control finished, respective corrections
  # finished"
  
  ### ADD MEASURWNT TYPES
  measurement_types$measurement_time_interval <- "daily"
  ## c("sub-daily","hourly","continuous")
  measurements_bavaria <- dwd_mes2 %>% dplyr::select(-.data[["eor"]]) %>% melt(id=c("location_code0","time","flag"),na.rm=FALSE)
  measurements_bavaria$description <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/water_equiv/historical/BESCHREIBUNG_obsgermany_climate_daily_water_equiv_historical_de.pdf"
  names(measurements_bavaria)[names(measurements_bavaria)=="variable"] <- "variable_code0"
  measurements_bavaria <- measurements_bavaria[,names(measurements)]
  ##
  ##
  measurements <- rbind(measurements,measurements_bavaria) %>% as_tibble()
  ###
  
  ####
  sumava <- list(locations=locations,measurement_types=measurement_types,measurements=measurements)
  
  
  if (remove_multipoints==TRUE) { ## addecd EC20220128
    
    is_multipoint <- sapply(sumava$locations$geometry,function(x){return((class(x)=="MULTIPOINT")[2])})
    ##is_multipoint <- (sumava$locations$location_code0=="CZ_CHMI_SNOW_C1FILH01")
    
    for (im in which(is_multipoint)) {
      
      temp0  <- sumava$locations[im,]
      location_code0_temp <- temp0$location_code0
      temp1 <- temp0$description %>% str_split(" ") %>% unlist()
      pattern <- c("Start_of_Measurement","End_of_Measurement","Longitude","Latitude","Altitude")
      itemp1 <- temp1 %>% str_detect(pattern=pattern[1])
      for (it in pattern[-1]) {
        itemp1 <- (temp1 %>% str_detect(pattern=it)) | itemp1
      }  
      temp1 <- temp1[itemp1]
      # temp1 <- temp1 %>% sapply(function(x,pattern,...) {x[any(str_detect(x,pattern=pattern,...))]},pattern=c("Start_of_Measurement","End_of_Measurement","Longitude","Latitude","Altitude"))
      temp1 <- temp1 %>% str_split(":")
      ndesc_temp <-  temp1 %>% sapply(function(x){x[1]})
      temp1 <- temp1 %>% sapply(function(x){x[2]}) %>% str_split(";") 
      temp1 <- temp1 %>% as.data.frame()
      names(temp1) <-  ndesc_temp
      
      
      
      #####
      
      temp1$Start_of_Measurement <- as.Date(temp1$Start_of_Measurement)
      temp1$End_of_Measurement   <- as.Date(temp1$End_of_Measurement)
      temp1$Longitude <- as.numeric(temp1$Longitude)
      temp1$Latitude <- as.numeric(temp1$Latitude)
      temp1$Altitude <- as.numeric(temp1$Altitude)
      #####
      
      
      geoms <- temp1[,c("Longitude","Latitude")] %>% t() 
      geoms <- geoms %>% as.data.frame() %>% as.list() 
      geoms <- geoms %>% lapply(st_point) %>% st_sfc(crs=4326) %>% st_transform(crs=st_crs(temp0))
      ### 
      temp1$geometry <- geoms
      temp1 <- temp1 %>% st_sf()
      temp0 <- temp0[rep(1,nrow(temp1)),]
      
      ## ()
      ##  temp1 <- 
      
      #####
      
      formatd <-"%Y%m%d"
      temp0$geometry <- temp1$geometry
      temp0$altitude <- temp1$Altitude
      temp0$location_code0 <- paste(location_code0_temp,as.character(temp1$Start_of_Measurement,format=formatd),as.character(temp1$End_of_Measurement,format=formatd),sep="_")
      temp0$location_code1 <-  temp0$location_code0
      #### POINTS CANNOT BE DUPLICATED
      temp0$line <- 1:nrow(temp0)
      mm <- st_distance(temp0)
      
      
      if (nrow(temp0)>1) for (ii in 1:nrow(temp0)) {
        if (temp0$line[ii]>=ii) {
          
          jj <- which(as.vector(mm[ii,])<=snap_distance)
          dd <- temp0$line[ii]
          temp0$line[jj] <- dd 
          
          
        }
        
        
        
      }
      #### END POINTS CANNOT BE DUPLICATED # 20220221
   
     
      temp0$location_code0 <- temp0$location_code0[temp0$line]
    
      
      for (i in 1:nrow(temp0)) {
        
        start_date <- temp1$Start_of_Measurement[i]
        end_date   <- temp1$End_of_Measurement[i]
        cond <- which((as.Date(sumava$measurements$time) >= start_date) & (as.Date(sumava$measurements$time) <= end_date) & (sumava$measurements$location_code0==location_code0_temp))
        sumava$measurements$location_code0[cond] <- temp0$location_code0[i]
      }
   
      uulines <- unique(temp0$line)
      temp0 <- temp0[unique(temp0$line),names(sumava$locations)]
      
      #####str(temp0)
      ####
      sumava$locations <- rbind(sumava$locations,temp0)
      
      
    }
    
    #is_multipoint2 <- (sapply(sumava$locations$geometry,function(x){return((class(x)=="MULTIPOINT")[2])}))
    ##is_multipoint2 <- (sumava$locations$location_code0=="CZ_CHMI_SNOW_C1FILH01")
    ##is_multipoint2 <- is_multipoint
    #sumava$location$location_code0[is_multipoint2]
    # 
    ##cond2 <- which(!(sumava$location$location_code0[is_multipoint2] %in% unique(sumava$measurements$location_code0)))
    ####
    noNAcond <- which(!is.na(sumava$measurements$value))
    sumava$measurements <- sumava$measurements[noNAcond,]
    cond2 <- which(sumava$locations$location_code0 %in% unique(sumava$measurements$location_code0))
    
    if (length(cond2)>0) {
      
      sumava$locations <- sumava$locations[cond2,]
      
    }
    
    
    ####
    
    
    
    
  }
  
  return(sumava)
}
