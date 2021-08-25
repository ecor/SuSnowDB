###
### https://en.wikipedia.org/wiki/West_Africa
###
## 
library(SuSnowDB)
library(readr)
library(GSODR)
library(magrittr)


west_africa <- system.file("west_africa_gsod_extdata/west_africa.csv",package="SuSnowDB") %>% read_csv()

year=2015:2017 ###1950:2020

cc <- system.time(out <- get_GSOD(year=year,country=west_africa$ISO_3166_1[-1][1]))

file <- "/home/ecor/activity/2021/local/SuSnowDB/inst/west_africa_gsod_extdata/gsod_bf_v2.csv"
write_csv(out,file=file)


###west_africa <- system.file("west_africa_gsod_extdata/west_africa.csv",package="SuSnowDB") %>% read_csv()

cc <- system.time(out <- get_GSOD(year=year,country=west_africa$ISO_3166_1[10]))

file <- "/home/ecor/activity/2021/local/SuSnowDB/inst/west_africa_gsod_extdata/gsod_ml.csv"
write_csv(out,file=file)

cc <- system.time(out <- get_GSOD(year=year,country=west_africa$ISO_3166_1[14]))

file <- "/home/ecor/activity/2021/local/SuSnowDB/inst/west_africa_gsod_extdata/gsod_sn.csv"
write_csv(out,file=file)







###west_africa <- read_csv("inst/west_africa_gsod_extdata/west_africa.csv")
##View(west_africa)
### AMMA : http://bd.amma-catch.org/main.jsf