###
### https://en.wikipedia.org/wiki/West_Africa
###
## 
library(SuSnowDB)
library(readr)
library(GSODR)
library(magrittr)


west_africa <- system.file("west_africa_gsod_extdata/west_africa.csv",package="SuSnowDB") %>% read_csv()

cc <- system.time(out <- get_GSOD(year=1950:2020,country=west_africa$ISO_3166_1[-1][1]))

file <- "/home/ecor/local/rpackages/SuSnowDB/inst/west_africa_gsod_extdata/gsdod_bf.csv"
write_csv(out,file=file)
###west_africa <- read_csv("inst/west_africa_gsod_extdata/west_africa.csv")
##View(west_africa)
### AMMA : http://bd.amma-catch.org/main.jsf