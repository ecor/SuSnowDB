###
### https://en.wikipedia.org/wiki/West_Africa
###
## 
library(SuSnowDB)
library(readr)
library(GSODR)
library(magrittr)


west_africa <- system.file("west_africa_gsod_extdata/west_africa.csv",package="SuSnowDB") %>% read_csv()

system.time(out <- get_GSOD(year=1980:1982,country=west_africa$ISO_3166_1[-1][1]))
###west_africa <- read_csv("inst/west_africa_gsod_extdata/west_africa.csv")
##View(west_africa)