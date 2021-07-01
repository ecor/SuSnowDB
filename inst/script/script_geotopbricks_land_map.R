rm(list=ls())
library(geotopbricks)
library(terra)
library(RPostgreSQL) 
library(sf)
library(dplyr)
library(zoo)
library(dygraphs)
#####
wpath <- '/stablo/local/simulations/sumava_test009b_distr_temp_v2_1d_COPY_20210607/'
##wpath1 <- '/stablo/local/simulations/sumava_test009b_distr_temp_v4_1d/'
elevation  <- get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath) 
land  <- get.geotop.inpts.keyword.value("FormerLandCoverMapFile",raster=TRUE,wpath=wpath) 
##land1  <- get.geotop.inpts.keyword.value("LandCoverMapFile",raster=TRUE,wpath=wpath1) 
landfile <- get.geotop.inpts.keyword.value("LandCoverMapFile",add_wpath=TRUE,wpath=wpath) 
extension(landfile) <- ".asc"
#####
points <- get.geotop.points(prefix="CoordinatePoint",suffix=c("Code","Source"),wpath=wpath)
points$location_code0 <- "DE_%s_%s" %>% sprintf(points$CoordinatePointSource,points$CoordinatePointCode)
points <- points %>% dplyr::select(location_code0) 
####
dbname = "hydroclimatedb_ver03"
conn = dbConnect(PostgreSQL(), dbname = dbname)
locations <- st_read(conn, "locations") %>% st_transform(crs=st_crs(land)) %>% st_crop(land) %>% dplyr::select(location_code0) ###
points <- rbind(points,locations)

###locations$icell <- locations %>% st_cast("POINT") %>% as_Spatial() %>% cellFromXY(object=land)

dbDisconnect(conn) 

new_land <- (st_buffer(points,dist=500) %>% as_Spatial() %>% rasterize(land))*0+land
writeRasterxGEOtop(new_land,filename=landfile,overwrite = TRUE)