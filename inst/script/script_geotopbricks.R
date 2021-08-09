rm(list=ls())
library(geotopbricks)
library(terra)
library(RPostgreSQL) 
library(sf)
library(dplyr)
library(zoo)
library(dygraphs)
library(lubridate)
library(reshape2)

#####
###wpath <- '/stablo/local/simulations/sumava_test009b_distr_temp_v2_1d'
wpath <- '/stablo/local/simulations/sumava_test009b_distr_temp_v2_1d_COPY_20210607/'
wpath <- '/stablo/local/simulations/sumava_test009b_distr_temp_v2_1d_COPY_20210702_WANI_SCF_1_0/'
##wpath <- '/stablo/local/simulations/sumava_test009b_distr_temp_v2_1d_COPY_20210614/'
elevation  <- get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath) 
land  <- get.geotop.inpts.keyword.value("LandCoverMapFile",raster=TRUE,wpath=wpath) 
#####
tz <- "GMT"

start <- as.Date("1998-08-01")
end <- as.Date("2020-08-01")-months(1)
when <- seq(from=start,to=end,by=1)


##stop("HERE")
###val <- sumava_snow_dataset()
dbname = "hydroclimatedb_ver03"
conn = dbConnect(PostgreSQL(), dbname = dbname)
locations <- st_read(conn, "locations") %>% st_transform(crs=st_crs(land)) %>% st_crop(land) ###
locations$icell <- locations %>% st_cast("POINT") %>% as_Spatial() %>% cellFromXY(object=land)
locations$elevation <- elevation[locations$icell]

###### EXTRACT SNOW VALUES FROM GEOTOP SNOW MAPS

# Not Run: uncomment the following line
### 
nx <- ncol(land)
neigh <- c(0,1,nx,-1,-nx)
names(neigh) <- sprintf("neigh_%d",neigh)
###
sim <- list()
simmap <- list()
vars <- c("SnowDepthMapFile","SWEMapFile")

## SNOW DEPTH
for (var in vars) {
  
  simmap[[var]] <- rasterFromOutput2DMap(var,when=as.POSIXlt(when),wpath=wpath,timestep="OutputSnowMaps",tz=tz) %>% stack() 
  nn <- names(simmap[[var]])
  simmap[[var]] <- simmap[[var]] %>% rast()
  nn -> names(simmap[[var]])
}  
##x_e <- "SnowDepthMapFile"
##m <- rasterFromOutput2DMap(x_e,when=as.POSIXlt(when),wpath=wpath,timestep="OutputSnowMaps",tz=tz) %>% stack() %>% rast()
## SNOW WATER EQUIVALENT
###x_e <- "SWEMapFile"
##me <- rasterFromOutput2DMap(x_e,when=as.POSIXlt(when),wpath=wpath,timestep="OutputSnowMaps",tz=tz) %>% stack() %>% rast()


###
sim <- NULL ## data.frame(var=vars[1])
for (var in vars) {
  ###  sim[[var]] <- list()
  for (it in names(neigh)) {
    
    print(it)
    sim_temp <- simmap[[var]][locations$icell+neigh[it]] %>% as.data.frame()
    sim_temp$icell <- locations$icell
    sim_temp <- sim_temp %>% melt(id="icell")
    names(sim_temp)[names(sim_temp)=="variable"] <- "time"
    sim_temp$time <- as.character(sim_temp$time) %>% as.POSIXct(format="DATE.TIME.%Y.%m.%d.%H.%M",tz=tz)
    ###names(sim_temp)[names(sim_temp)=="value"] <- paste("value",neigh[it],sep="_")
    sim_temp$variable <- var
    sim_temp$neigh <- neigh[it]
    if (is.null(sim)) {
      sim <- sim_temp
    } else {
      sim <- rbind(sim,sim_temp)  
    }  
  }
}
###
sim1 <- sim
ic1 <- which(sim1$variable=="SnowDepthMapFile") 
sim1$variable[ic1] <- "snow_depth_cm"
sim1$value[ic1] <- sim1$value[ic1]/10
##

ic2 <- which(sim1$variable=="SWEMapFile") 
sim1$variable[ic2] <- "swe_mm"

##


sim2 <- locations %>% as_tibble()  %>% dplyr::select(icell,location_code0) %>% full_join(sim1) %>% mutate(variable_code0=variable,neigh=as.character(neigh)) %>% dplyr::select(-variable)


measurement_types <- tbl(conn, "measurement_types") ##%>% as.data.frame()
##measurements <- tbl(conn, "measurements") %>% dplyr::select(location_code0,variable_code0,time,value,flag,description) %>% dplyr::filter(location_code0 %in% unique(sim2$location_code0))  ####%>% as.data.frame()
measurements <- dbReadTable(conn, "measurements")  %>% dplyr::filter(location_code0 %in% unique(sim2$location_code0))  %>% mutate(neigh="observation") ####%>% as.data.frame()
####
##out <- right_join(measurements,sim2)
nn <- intersect(names(sim2),names(measurements))
out2 <- sim2[,nn] %>% rbind(measurements[,nn]) 
out2$time <- as.Date(out2$time)

###out2 <- out2 %>% dplyr::filter(!is.na(out2$icell)) 

istations_ids <- which(locations$location_code0 %in% unique(out2$location_code0))
stations_ids <- locations$location_code0[istations_ids]
elevation_stations_ids <- locations$elevation[istations_ids]
names(elevation_stations_ids) <- stations_ids


vars <- unique(out2$variable_code0)
elevation <- 
dd <- list()

vars2 <- c("snow_depth_cm","swe_mm")

for (var in vars2) {
  dd[[var]] <- list() 
  
  for (it in stations_ids) {
    
    main <- "%s  %s (%d m )" %>%  sprintf(var,it,as.integer(elevation_stations_ids[it]))
    temp <- out2 %>% dplyr::filter(location_code0==it,variable_code0==var) %>% dplyr::select(time,neigh,value) %>% dcast(time ~ neigh) %>% arrange(time)
    time <- temp$time
    temp <- as.zoo(temp)
    index(temp) <- as.Date(time) ## time has a daily frequency!!!
    dd[[var]][[it]] <- dygraph(temp,main=main) %>% dyRangeSelector() 
  }  
    
}
##out3 <- measurements %>% full_join(sim2) 
######

### https://github.com/r-spatial/sf/issues/114
# st_un_multipoint = function(x) {
#   g = st_geometry(x)
#   i = rep(seq_len(nrow(x)), sapply(g, nrow))
#   x = x[i,]
#   st_geometry(x) = st_sfc(do.call(c,
#                                   lapply(g, function(geom) lapply(1:nrow(geom), function(i) st_point(geom[i,])))))
#   x$original_geom_id = i
#   x
# }


##dd$snow_depth_cm[[10]]
##dd$snow_depth_cm[[11]]
##dd$snow_depth_cm[[13]]
##> 

## Create a new DB layout
###write_dataset_into_ssdb(conn,val,new=TRUE)
dbDisconnect(conn) 




## CONTROLLARE  dd$snow_depth_cm$DE_DWD_SNOW_05801








## CREARE UNA SIMULAZIONE CON SOLI SPOT NEI PUNTI DI MISURA!!! EE

nn2 <- out2 %>% filter(variable_code0=="swe_mm") %>% filter(neigh=="observation" & !is.na(value))   %>% select(location_code0) %>% magrittr::extract2(1) %>% unique()

fout3 <- function(x,...){x %>% select(time,value,neigh) %>% dcast(time ~ neigh) %>% return()}
####out3 <- out2 %>% dplyr::select(location_code0,variable_code0,time,neigh,value) %>% dplyr::filter(variable_code0==vars[1]) %>% dplyr::group_by(location_code0,variable_code0) %>% group_modify(fout3) %>% ungroup()
out3 <- out2 %>% dplyr::select(location_code0,variable_code0,time,neigh,value) %>% dplyr::group_by(location_code0,variable_code0) %>% group_modify(fout3) %>% ungroup()

gg <- ggplot(out3 %>% filter(variable_code0==vars[1]) %>% mutate(month=factor(month(time))))+geom_point(mapping=aes(x=observation,y=`0`,group=month))+ facet_grid(. ~ month) ## facet_grid(rows= . ~ variable_code0)
