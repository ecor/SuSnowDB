NULL
#' Get measurements ("measurements" or other "measurements"-like tables) from several tables of a SumavaDB-like spatio-temporal database  
#'
#' @param conn database (preferably PostgresSQL) connection. See output of \code{\link{dbConnect}}.
#' @param table_name vector with names of the tables . Default is \code{get_value_table_names_from_ssdb(conn)}. See also @seealso \code{\link{get_value_table_names_from_ssdb}}.
#' @param return.zoo logical. If \code{TRUE}, function returns a time series (\code{\link{zoo}} object) for any time series of each variable (\code{"measurement_type"} at each site (\code{"location"}). Default is \code{FALSE}. 
#' @param ... further arguments for \code{\link{get_measurements_from_ssdb}}
#'
#' @importFrom  zoo as.zoo index<-
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' library(RPostgreSQL) 
#' dbname <- "hydroclimatedb_testing00"
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' 
#' 
#' ### Add weather data from stations from MeteoTrentino network, Trentino, Italy 
#' val_smet <- meteotrentino_smet_dataset(smet_files=c("T0175","T0179"))
#' add_locations_into_ssdb(conn,val_smet$locations,new=TRUE)
#' add_measurement_types_into_ssdb(conn,val_smet$measurement_type)
#' add_measurements_into_ssdb(conn,val_smet$measurements)
#' 
#' dbDisconnect(conn)
#' 
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' out <- get_measurements_from_ssdb(conn)
#' out <- get_measurements_from_ssdb(conn,locations_code0="IT-TN_T0179")
#' out <- get_measurements_from_ssdb(conn,locations_code0="IT-TN_T0179",
#' variables_code0=c("SMET_TA","SMET_PINT"))
#' out <- get_measurements_from_ssdb(conn,
#' variables_code0=c("SMET_TA","SMET_PINT"))
#' 
#' time_interval <- as.Date(c("2021-03-01","2021-03-31"))
#' out <- get_multitable_measurements_from_ssdb(conn,
#' variables_code0=c("SMET_TA","SMET_PINT"),
#' start_time=time_interval[1],end_time=time_interval[2])
#' dbDisconnect(conn)
#' 
#' 
#' 
#' }
#' 
#' 
#' library(dygraphs)
#' 
#' ssdb <- system.file('sumava_extdata/sumava.rds',package="SuSnowDB") %>% readRDS()
#' ####ssdb <- '/home/ecor/activity/2022/local/shiny/SuSnowApp/ext-data/sumava.rds' %>% readRDS()
#' table_name <- get_value_table_names_from_ssdb(ssdb)
#' out2 <- get_multitable_measurements_from_ssdb(ssdb)
#' ## FIND A LOCATION
#' locs <- get_locations_from_ssdb(ssdb)
#' 
#' ilocations_filipova <- which(str_detect(locs$location_name,"Filipova"))
#' locs[ilocations_filipova,]
#' out3 <- get_multitable_measurements_from_ssdb(ssdb,
#' locations_code0=locs$location_code0[ilocations_filipova][2])
#' out4 <- get_multitable_measurements_from_ssdb(ssdb,
#' locations_code0=locs$location_code0[ilocations_filipova][2],return.zoo=TRUE)
#' dygraph(out4[[1]][[1]]) %>% dyRangeSelector()
#' 
#' out4_all <- get_multitable_measurements_from_ssdb(ssdb,
#' locations_code0=locs$location_code0[ilocations_filipova],return.zoo=TRUE)
#' str(out4_all$snow_depth_cm$CZ_CHMI_SNOW_C1FILH01_20040401_20070912)
#' dygraph(out4_all$snow_depth_cm$CZ_CHMI_SNOW_C1FILH01_20070913_20191231) %>% dyRangeSelector()
#' 
#' 
#' 
#' 

get_multitable_measurements_from_ssdb <- function (conn,table_name=get_value_table_names_from_ssdb(conn),...,return.zoo=FALSE) {

  out <- list()
  for (it in table_name) {
    print(it)
    out[[it]] <- get_measurements_from_ssdb(conn,table_name=it,...) %>% as_tibble()
    out[[it]]$table <- it
    
    if (return.zoo) {
      out[[it]] <- out[[it]][,c("time","value","variable_code0","location_code0")]
      names(out[[it]])[names(out[[it]])=="value"]=paste0(it,"_value")
      
    } else {
      
      out[[it]]$table <- it
    }
    
    
  }
  
  
  
  if (return.zoo) {
    
   #   out <- lapply(out,function(x) {x[,c("time","value","variable_code0","location_code0","table")]})
      ###out <- out[,c("time","value","variable_code0","location_code0","table")]
    #  out <- lapply(out, function(x) {names(x)[names(x)=="value"]=paste0(x$table[1],"_value");return(x)})
      ##out <- lapply(out, function(x) {split(x,x$variable_code0)}) 
      out2 <- out[[1]]
      if (length(out)>1) for (i in 2:length(out)) {
        
        out2 <- full_join(out2,out[[i]])
        
      } 
      out <- out2
      out2 <- NULL
      out <- lapply(split(out,out$variable_code0), function(x) {split(x,x$location_code0)}) 
      for (nmvar in names(out)) for (nmloc in names(out[[nmvar]])) {
        
        ctormv <- which(names(out[[nmvar]][[nmloc]]) %in% c("variable_code0","location_code0"))
        out[[nmvar]][[nmloc]] <- out[[nmvar]][[nmloc]][,-ctormv]
        
        vvtime <- out[[nmvar]][[nmloc]]$time
        ivvtime <- order(vvtime)
        
        ctime <- which(names(out[[nmvar]][[nmloc]]) %in% c("time"))
        out[[nmvar]][[nmloc]] <- as.zoo(out[[nmvar]][[nmloc]][ivvtime,-ctime])
        index(out[[nmvar]][[nmloc]]) <- vvtime[ivvtime]
        
      }
  } else {
    
    out <- do.call(out,what="rbind")
    
  }
  
  
  
  ##
    
  #out <- NULL
  # if (length(table_name)>1) {
  #   
  #   msg <- sprintf("Table names more than one: %s (Plese use get_multitable_measurements_table() function",paste(table_name,collapse=" "))
  #   stop(msg)
  #   
  # }
  # cval <- length(variables_code0)>0
  # cloc <- length(locations_code0)>0
  # cboth <- cval & cloc
  # cany <-  cval | cloc
  # all <- (all==FALSE) & (cany==FALSE)
  # if (all) {
  #   out <- tbl(conn,table_name,...)
  # } else if (cboth==TRUE) {
  # 
  #   out <- tbl(conn,table_name) %>% filter(.data$variable_code0 %in% variables_code0,.data$location_code0 %in% locations_code0)
  # }else if (cloc==TRUE) {
  #   
  #   out <- tbl(conn,table_name) %>% filter(.data$location_code0 %in% locations_code0)
  # }  else if (cval==TRUE) {
  #   
  #   out <- tbl(conn,table_name) %>% filter(.data$variable_code0 %in% variables_code0) 
  # }
  # if (length(start_time)<1) start_time <- NA
  # if (length(end_time)<1) end_time <- NA
  # 
  # if (!is.na(start_time)) out <- out %>% filter(.data$time>=start_time) 
  # if (!is.na(end_time)) out <- out %>% filter(.data$time<=end_time) 
  # out <- out %>% arrange(.data$time)
  # 
  # if (!is.logical(returns.data.frame)) returns.data.frame <- FALSE
  # if (returns.data.frame) out <- as.data.frame(out)
  # ###
  # 
  

  
  return(out)
  
  
}