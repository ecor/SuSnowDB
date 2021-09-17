
rm(list=ls())
library(SuSnowDB)
library(readr)
library(magrittr)
library(GSODR)


countries <- system.file("west_africa_gsod_extdata/west_africa.csv",package="SuSnowDB") %>% read_csv()
outfile <- '/home/ecor/activity/2021/local/data/gsod/%s.rds'
icountries <- countries$ISO_3166_1
icountry <- "SN"
years <- 1940:2020
for (icountry in icountries) {
  
  cc1 <- system.time(out1 <- get_GSOD(years=years,country=icountry) %>% gsodr_dataset())
  ##cc2 <- system.time(out2 <- gsodr_dataset(years=years,country=icountry))
  outfile1 <- outfile %>% sprintf(icountry)
  saveRDS(out1,file=outfile1)

}
# stop("HERE")
# 
# 
# 
# years <- (1991:2020)[10]
# out1 <- list()
# cc <- list()
# 
# for (icountry in icountries) {
#     print(icountry)
#     cc[[icountry]] <- system.time( 
#       out1[[icountry]] <-   gsodr_dataset(years=years,country=icountry)
#     )
#     
# }
# 
# locl <- lapply(X=out1,FUN=function(x){x$locations}) %>% do.call(what="rbind")