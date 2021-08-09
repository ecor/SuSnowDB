library(readr)
library(magrittr)
library(stringr)
library(purrr)
smet_variables <- system.file("smet_extdata/smet_variables.csv",package="SuSnowDB") %>% read_csv()
smet_variables_old <- "/home/ecor/activity/2021/local/SuSnowDB/inst/smet_extdata/smet_variables_old.csv" %>% read_csv()
smet_variables$variable_description <- smet_variables$description
tt <- smet_variables$description %>% str_split(pattern="[(]") %>% map(function(x){x[[1]]}) %>% str_trim() %>% unlist()
names(smet_variables)[names(smet_variables)=="description"] <- "variable"
names(smet_variables)[names(smet_variables)=="variable_description"] <- "description"
smet_variables$variable <- tt

smet_variables_file <- "/home/ecor/activity/2021/local/SuSnowDB/inst/smet_extdata/smet_variables.csv"
write_csv(smet_variables,smet_variables_file)
##View(smet_names)