###
### https://en.wikipedia.org/wiki/West_Africa
###
## 
library(SuSnowDB)
library(readr)
library(GSODR)
library(magrittr)


west_africa <- system.file("west_africa_gsod_extdata/west_africa.csv",package="SuSnowDB") %>% read_csv()

out <- get_GSOD(year=1980:2019,country=west_africa$ISO_3166_1[-1])
###west_africa <- read_csv("inst/west_africa_gsod_extdata/west_africa.csv")
##View(west_africa)