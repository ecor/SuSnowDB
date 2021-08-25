###
### https://en.wikipedia.org/wiki/West_Africa
###
## 
library(SuSnowDB)
library(readr)
library(GSODR)
library(magrittr)


source('~/activity/2021/local/SuSnowDB/R/gsodr_dataset.R')

x <- system.file("west_africa_gsod_extdata/gsod_bf.csv",package="SuSnowDB") %>% read_csv()

out <- gsodr_dataset(x=x)
#'  
