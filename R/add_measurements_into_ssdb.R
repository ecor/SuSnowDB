NULL
#' Add measurements ("measurements" table) to a SumavaDB-like spatiotemporal database  
#'
#' @param conn database (preferrably PodtgresSQL) connection. See ouput of \code{\link{dbConnect}}.
#' @param measurements measurements table, data frame or an object that can be coerced to a data frame. See output of \code{\link{sumava_snow_dataset}}
#' @param append logical. If \code{append==FALSE} database tables are created or replaced. If \code{append==FALSE} data and metedata are appended as new rows of the database tables. Default is \code{append=TRUE}.
#' @param new logical. If \code{new==TRUE} database tables are created or replaced. If \code{new==FALSE} tables are modidied according to \code{append}. Default is \code{FALSE}.  
#' @param sql_files file names containing database initialization SQL queries. They are used in case \code{append==FALSE} or \code{new=TRUE}.
#' @param nn_measurements names of the columns in the "measurements" table. See the vignette.
#' @param warn logical argument if the column names of \code{measurements} are different from \code{nn_measurements} functions displays a warning message. Default is \code{TRUE}.
#' @param err  logical argument if the column names of \code{measurements} are different from \code{nn_measurements} functions returns an error. Default is \code{!warn}.
#' @param ... further arguments (currently not used)
#' 
#'
#' @importFrom  sf st_read st_crs st_transform
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' library(RPostgreSQL) 
#' dbname <- "hydroclimatedb_testing00"
#' val <- sumava_snow_dataset()
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' 
#' add_locations_into_ssdb(conn,val$locations,new=TRUE)
#' add_measurement_types_into_ssdb(conn,val$measurement_types)
#' add_measurements_into_ssdb(conn,val$measurements)
#' 
#' 
#' ### Add weather station from MeteoTrentino network, Trentino, Italy 
#' val_smet <- meteotrentino_smet_dataset(smet_files=c("T0175","T0179"))
#' add_locations_into_ssdb(conn,val_smet$locations,append=TRUE)
#' add_measurement_type_into_ssdb(conn,val_smet$measurement_type,append=TRUE)
#' add_measurements_into_ssdb(conn,val_smet$measurements,append=TRUE)
#' 
#' dbDisconnect(conn)
#' 
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' out <- dbReadTable(conn,"measurements")
#' dbDisconnect(conn)
#' 
#' }
add_measurements_into_ssdb <- function (conn,measurements,new=FALSE,append=!new,sql_files=system.file("sql/create_or_replace_functions.sql",package="SuSnowDB"),
                                    warn=TRUE,err=!warn,
                                    nn_measurements=c("time","value","flag","location_code0","variable_code0","description"),...) {
  
  

  
  
  
  if (new==TRUE) append <- FALSE
  if (new==FALSE) append <- TRUE
  if (append==FALSE)  {
    # sql_commands <- sql_files %>% map(readLines) %>% unlist() %>% paste(collapse="\n")
    # dbSendQuery(conn,statement=sql_commands)
    # query_create_tables <-"SELECT create_or_replace_all_measurement_location_tables();"
    # dbSendQuery(conn,statement=query_create_tables)
    msg <- "A new schema cannot be created, please create 'locations' and 'measurement_types' previously and then re-run with new=FALSE or append=TRUE"
    if (warn==TRUE) {
      warning(msg)
    } else {
      stop(msg)
    }
    
  } 
  measurements_table <- dbReadTable(conn,"measurements")
  nn_measurements <- measurements_table %>% names()
  # i_already_existing_codes <- which(measurements$variable_code0 %in%  measurements_table$variable_code0)
  # if (length(i_already_existing_codes)>0) {
  #   msq <- sprintf("The variable_code0  %s are already present in the database and will not be added!",paste(measurements$location_code0[i_already_existing_codes],collapse=" "))
  #   warning(msg)
  #   measurements <- measurements[-i_already_existing_codes,]
  #   }
  #}  
  ##
  
  inn <- which(!(names(measurements) %in% nn_measurements))
  if (length(inn)>0) {
    
    msg <- sprintf("Columns named as %s in measurements variable cannot be imported to the DB.",paste(names(measurements)[inn],collapse=" "))
    measurements <- measurements[,-inn]
    if (warn==TRUE) {
      warning(msg)
    } else {
      stop(msg)
    }
  }
  inl <- which(!(nn_measurements %in% names(measurements)))
  if (length(inl)>0) {
    
    msg <- sprintf("Columns named as %s in measurements variable are missing and may be filled as NA in the DB.",paste(nn_measurements[inn],collapse=" "))
    measurements[,nn_measurements[inl]] <- NA
    if (warn==TRUE) {
      warning(msg)
    } else {
      stop(msg)
    }
  }
  measurements <- measurements[,nn_measurements]
  ###
  locations <- st_read(conn,"locations")
  i_un_loc <- which(!(measurements$location_code0 %in% as.vector(locations$location_code0)))
  if (length(i_un_loc)>0) {
    un_loc <- unique(measurements$location_code0[i_un_loc])
    
    msg <- sprintf("Location _code0(s)  %s are missing in the 'locations' table' and must be insterted. Values not inserted!",paste(un_loc,collapse=" "))
    measurements[,nn_measurements[inl]] <- NA
    if (warn==TRUE) {
      warning(msg)
    } else {
      stop(msg)
    }
    measurements <- measurements[-i_un_loc,]
  }
  
  measurement_types <- dbReadTable(conn,"measurement_types")
  i_un_var <- which(!(measurements$variable_code0 %in% as.vector(measurement_types$variable_code0)))
  if (length(i_un_var)>0) {
    un_loc <- unique(measurements$variable_code0[i_un_var])
    
    msg <- sprintf("Variable_code0(s)  %s are missing in the 'measurement_types' table' and must be insterted.Values not inserted!",paste(un_loc,collapse=" "))
    measurements[,nn_measurements[inl]] <- NA
    if (warn==TRUE) {
      warning(msg)
    } else {
      stop(msg)
    }
    measurements <- measurements[-i_un_var,]
    
  }
 
  
  o_measurements <- dbWriteTable(conn=conn,name="measurements",value=measurements,append=TRUE,row.names=FALSE)
  remove_placeholder <- dbSendQuery(conn,statement="SELECT remove_placeholder();")
 ## o_measurements <- dbWriteTable(conn=conn,name="measurements",value=dataset$measurements,append=TRUE,row.names=FALSE)
 ##  o_measurements <- dbWriteTable(conn=conn,name="measurements",value=dataset$measurements,append=TRUE,row.names=FALSE)
  
  ##return(list(measurements=o_measurements,measurements=o_measurements,measurements=o_measurements))
  
  
  
  
  out <- measurements
  attr(out,"remove_placeholder") <- remove_placeholder
  
  
  return(out)
  
  
}