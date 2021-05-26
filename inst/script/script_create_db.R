rm(list=ls())
library(SuSnowDB)
library(RPostgreSQL) 

val <- sumava_snow_dataset()
dbname = "hydroclimatedb_ver03"
conn = dbConnect(PostgreSQL(), dbname = dbname)

## Create a new DB layout
write_dataset_into_ssdb(conn,val,new=TRUE)
dbDisconnect(conn) 

