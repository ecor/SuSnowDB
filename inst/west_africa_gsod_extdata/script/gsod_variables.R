#  
# R script to create a table with GSOD variable information (measurent types) 
# Author: Emanuele Cordno
#
#
rm(list=ls())
library(stringr)
library(magrittr)
library(dplyr)
library(readr)
uu <- '/home/ecor/activity/2021/local/SuSnowDB/inst/west_africa_gsod_extdata/gsod_variables_table.txt' 
uu <- uu %>% readLines()

uu <- uu[uu!=""] %>% str_split(" - ")
nvar <- sapply(uu,function(x){x[[1]]})

fist_car <- "- **"
i1c <- which(str_sub(nvar,1,4)==fist_car)
uu[i1c] <- lapply(uu[i1c],function(x){x[-1]})
ivar <- 1:length(uu)
ivar2 <- sapply(ivar, function(x,i1c){max(i1c[i1c<=x])},i1c=i1c)
nvar2 <- nvar[ivar2] %>% str_replace_all(fixed("\\"),"") %>% str_replace_all(fixed("-"),"") %>% str_replace_all(fixed("*"),"") %>% str_trim()

#uu <- uu %>% str_split(" - ")
vv <- split(uu,nvar2) %>% lapply(unlist) %>% lapply(function(x){x[length(x)>0]})  %>% lapply(paste,collapse=" ") %>% lapply(str_trim)
#nvar <- sapply(uu,function(x){x[[1]]})




#################
# * __variable_code0__: variable code (PRIMARY KEY),alphanumeric,unique;
# * __variable__:    human readable name of the variable;
# * __unit__: measurement unit;
# * __description__: textual description of the measurement type;
# * __measurement_time_interval__: time interval at which variable has been measured and/or reported, e.g. hourly, daily, monthly, etc.. .
nmeas <-  c("TEMP","TEMP_ATTRIBUTES","DEWP","DEWP_ATTRIBUTES", "SLP","SLP_ATTRIBUTES","STP",
            "STP_ATTRIBUTES","VISIB","VISIB_ATTRIBUTES" ,"WDSP","WDSP_ATTRIBUTES","MXSPD","GUST", 
            "MAX","MAX_ATTRIBUTES","MIN","MIN_ATTRIBUTES","PRCP","PRCP_ATTRIBUTES","SNDP",
            "I_FOG","I_RAIN_DRIZZLE","I_SNOW_ICE","I_HAIL","I_THUNDER","I_TORNADO_FUNNEL","EA",
            "ES","RH")

vv <- vv[nmeas]

##iflag <- which(str_detect(nmeas,"ATTRIBUTES"))

##nflag <- nmeas[iflag]
##nmeas <- nmeas[-iflag]

#################

indflag <- "_ATTRIBUTES"

measurement_types <- data.frame(variable=names(vv),description=unlist(vv))

gsodr_ref_url <- "https://joss.theoj.org/papers/10.21105/joss.00177"

measurement_types$measurement_time_interval <- "daily"
measurement_types$unit <- "See Description"
measurement_types$variable_code0 <- ""
measurement_types$is_flag <- str_detect(nmeas,indflag)
##measurement_types$has_flag <- FALSE


measurement_flags <- measurement_types %>% filter(is_flag) %>% transmute(variable=str_replace_all(variable,indflag,""),description_flag=description)
measurement_types <- measurement_types %>% filter(!is_flag) %>% full_join(measurement_flags) %>% mutate(description=if_else(is.na(description_flag),description,paste0(description," FLAGS_",variable,": ",description_flag)))

measurement_types <- measurement_types %>% mutate(has_flag=if_else(is.na(description_flag),FALSE,TRUE)) %>%  select(-is_flag,-description_flag)



###
####





##################

out_file <- '/home/ecor/activity/2021/local/SuSnowDB/inst/west_africa_gsod_extdata/gsod_variable_measurement_types_v1.csv' 
write_csv(measurement_types,out_file)



