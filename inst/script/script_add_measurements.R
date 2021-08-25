## Not run: 
##

library(SuSnowDB)
library(RPostgreSQL) 

##source('~/activity/2021/local/SuSnowDB/R/add_locations_into_ssdb.R')
##source('~/activity/2021/local/SuSnowDB/R/add_measurement_types_into_ssdb.R')
##source('~/activity/2021/local/SuSnowDB/R/add_measurements_into_ssdb.R')


dbname <- "hydroclimatedb_testing00"
val <- sumava_snow_dataset()
conn = dbConnect(PostgreSQL(), dbname = dbname)

add_measurement_types_into_ssdb(conn,val$measurement_types,new=TRUE)
add_locations_into_ssdb(conn,val$locations)
add_measurements_into_ssdb(conn,val$measurements)


### Add weather station from MeteoTrentino network, Trentino, Italy 
val_smet <- meteotrentino_smet_dataset(smet_files=c("T0175","T0179"))
##val_smet <- meteotrentino_smet_dataset()
add_locations_into_ssdb(conn,val_smet$locations,append=TRUE)
add_measurement_types_into_ssdb(conn,val_smet$measurement_type,append=TRUE)
add_measurements_into_ssdb(conn,val_smet$measurements,append=TRUE)


### Add weather station from GSOD (https://catalog.data.gov/dataset/global-surface-summary-of-the-day-gsod#sec-dates) though GSODR package for the following contry: Senegal
val_gsod <- gsodr_dataset(years=2010:2020,country="SN")
##val_gsod <- meteotrentino_smet_dataset()
add_locations_into_ssdb(conn,val_gsod$locations,append=TRUE)
add_measurement_types_into_ssdb(conn,val_gsod$measurement_type,append=TRUE)
add_measurements_into_ssdb(conn,val_gsod$measurements,append=TRUE)






dbDisconnect(conn)

conn = dbConnect(PostgreSQL(), dbname = dbname)
out <- list()
out$locations <- st_read(conn,"locations") ###%>% filter(location_code0!="empty_code0")
out$measurement_types <- dbReadTable(conn,"measurement_types")
out$measurements <- dbReadTable(conn,"measurements")
dbDisconnect(conn)

