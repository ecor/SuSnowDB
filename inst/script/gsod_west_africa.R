
rm(list=ls())
library(SuSnowDB)
library(readr)
library(magrittr)

source('~/activity/2021/local/SuSnowDB/R/gsodr_dataset.R')

countries <- system.file("west_africa_gsod_extdata/west_africa.csv",package="SuSnowDB") %>% read_csv()
icountries <- countries$ISO_3166_1

years <- (1991:2020)[10]
out1 <- list()
cc <- list()

for (icountry in icountries) {
    print(icountry)
    cc[[icountry]] <- system.time( 
      out1[[icountry]] <-   gsodr_dataset(years=years,country=icountry)
    )
    
}

locl <- lapply(X=out1,FUN=function(x){x$locations}) %>% do.call(what="rbind")