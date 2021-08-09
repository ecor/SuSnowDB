rm(list=ls())
library(SuSnowDB)
library(RPostgreSQL) 

val <- sumava_snow_dataset()
dbname = "hydroclimatedb_ver03"
conn = dbConnect(PostgreSQL(), dbname = dbname)

## Create a new DB layout
write_dataset_into_ssdb(conn,val,new=TRUE) ##,sql_files='/home/ecor/activity/2021/local/SuSnowDB/inst/sql/create_or_replace_functions.sql')
dbDisconnect(conn) 

## VERIFICARE SRID: 
## https://postgis.net/docs/Find_SRID.html
## https://postgis.net/docs/postgis_usage.html#spatial_ref_sys
#dsn <- "/home/ecor/activity/2021/local/SuSnowDB/inst/shp/locations.shp"
#st_write(val$locations %>% filter(st_geometry_type(val$locations)=="POINT"),dsn=dsn)
