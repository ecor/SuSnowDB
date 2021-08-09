## Not run: 
library(RPostgreSQL) 

source('~/activity/2021/local/SuSnowDB/R/add_locations_into_ssdb.R')


out <- sumava_snow_dataset()
dbname <- "hydroclimatedb_testing00"
val <- sumava_snow_dataset()
conn = dbConnect(PostgreSQL(), dbname = dbname)
add_locations_into_ssb(conn,val$locations,new=TRUE,sql_files='/home/ecor/activity/2021/local/SuSnowDB/inst/sql/create_or_replace_functions.sql')
dbDisconnect(conn)

