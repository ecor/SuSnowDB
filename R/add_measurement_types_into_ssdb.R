NULL
#' Add measurement_types ("measurement_types" table) to a SumavaDB-like spatiotemporal database  
#'
#' @param conn database (preferably PostgresSQL) connection. See output of \code{\link{dbConnect}}.
#' @param measurement_types measurement types table, data frame or an object that can be coerced to a data frame.. See output of \code{\link{sumava_snow_dataset}}
#' @param append logical. If \code{append==FALSE} database tables are created or replaced. If \code{append==FALSE} data and metedata are appended as new rows of the database tables. Default is \code{append=TRUE}.
#' @param new logical. If \code{new==TRUE} database tables are created or replaced. If \code{new==FALSE} tables are modidied according to \code{append}. Default is \code{FALSE}.  
#' @param sql_files file names containing database initialization SQL queries. They are used in case \code{append==FALSE} or \code{new=TRUE}.
#' @param nn_measurement_types names of the columns in the "measurement_types" table. See the vignette.
#' @param warn logical argument if the column names of \code{measurement_types} are different from \code{nn_measurement_types} functions displays a warning message. Default is \code{TRUE}.
#' @param err  logical argument if the column names of \code{measurement_types} are different from \code{nn_measurement_types} functions returns an error. Default is \code{!warn}.
#' @param ... further arguments (currently not used)
#' 
#'
#' @importFrom  sf st_read st_crs st_transform
#' @importFrom DBI dbWriteTable dbSendQuery dbReadTable dbGetQuery
#' @export
#' 
#' @examples
#' \dontrun{
#' library(RPostgreSQL) 
#' dbname <- "hydroclimatedb_testing00"
#' val <- sumava_snow_dataset()
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' add_measurement_types_into_ssdb(conn,val$measurement_types,new=TRUE)
#' ### Add weather station from MeteoTrentino network, Trentino, Italy 
#' val_smet <- meteotrentino_smet_dataset(smet_files=c("T0175","T0179"))
#' add_measurement_types_into_ssdb(conn,val_smet$measurement_types,append=TRUE)
#' 
#' dbDisconnect(conn)
#' 
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' out <- dbReadTable(conn,"measurement_types")
#' dbDisconnect(conn)
#' 
#' }
add_measurement_types_into_ssdb <- function (conn,measurement_types,new=FALSE,append=!new,sql_files=system.file("sql/create_or_replace_functions.sql",package="SuSnowDB"),
                                    warn=TRUE,err=!warn,
                                    nn_measurement_types=default_nn_measurement_types(),...) {
  
  
  if (new==TRUE) append <- FALSE;
  
  if (append==FALSE)  {
    sql_commands <- sql_files %>% map(readLines) %>% unlist() %>% paste(collapse="\n")
    dbSendQuery(conn,statement=sql_commands)
    query_create_tables <-"SELECT create_or_replace_all_measurement_location_tables();"
    #query_create_tables <-"SELECT create_or_replace_measurement_types();"
    dbSendQuery(conn,statement=query_create_tables)
  } else {
 
    measurement_types_table <- dbReadTable(conn,"measurement_types")

    nn_measurement_types <- measurement_types_table %>% names()
    i_already_existing_codes <- which(measurement_types$variable_code0 %in%  measurement_types_table$variable_code0)
    if (length(i_already_existing_codes)>0) {
      msg <- sprintf("The variable_code0  %s are already present in the database and will not be added!",paste(measurement_types$location_code0[i_already_existing_codes],collapse=" "))
      warning(msg)
      measurement_types <- measurement_types[-i_already_existing_codes,]
      
    }
  }  
  ##
  
  inn <- which(!(names(measurement_types) %in% nn_measurement_types))
  if (length(inn)>0) {
    
    msg <- sprintf("Columns named as %s in measurement_types variable cannot be imported to the DB.",paste(names(measurement_types)[inn],collapse=" "))
    measurement_types <- measurement_types[,-inn]
    if (warn==TRUE) {
      warning(msg)
    } else {
      stop(msg)
    }
  }
  inl <- which(!(nn_measurement_types %in% names(measurement_types)))
  if (length(inl)>0) {
    
    msg <- sprintf("Columns named as %s in measurement_types variable are missing and may be filled as NA in the DB.",paste(nn_measurement_types[inn],collapse=" "))
    measurement_types[,nn_measurement_types[inl]] <- NA
    if (warn==TRUE) {
      warning(msg)
    } else {
      stop(msg)
    }
  }
  measurement_types <- measurement_types[,nn_measurement_types]
  
  
  o_measurement_types <- dbWriteTable(conn=conn,name="measurement_types",value=measurement_types,append=TRUE,row.names=FALSE)
 ## o_measurement_types <- dbWriteTable(conn=conn,name="measurement_types",value=dataset$measurement_types,append=TRUE,row.names=FALSE)
 ##  o_measurements <- dbWriteTable(conn=conn,name="measurements",value=dataset$measurements,append=TRUE,row.names=FALSE)
  
  ##return(list(measurement_types=o_measurement_types,measurement_types=o_measurement_types,measurements=o_measurements))
  out <- measurement_types
  
  return(out)
  
  
}