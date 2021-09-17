NULL
#' Add locations ("locations" table) to a SumavaDB-like spatiotemporal database  
#'
#' @param conn database (preferably PostgreSQL) connection. See output of \code{\link{dbConnect}}.
#' @param locations locations, spatial object (or \code{\link{sf}}) . See output of \code{\link{sumava_snow_dataset}}
#' @param append logical. If \code{append==FALSE} database tables are created or replaced. If \code{append==FALSE} data and metedata are appended as new rows of the database tables. Default is \code{append=TRUE}.
#' @param new logical. If \code{new==TRUE} database tables are created or replaced. If \code{new==FALSE} tables are modidied according to \code{append}. Default is \code{FALSE}.  
#' @param sql_files file names containing database initialization SQL queries. They are used in case \code{append==FALSE} or \code{new=TRUE}.
#' @param nn_locations names of the columns in the "locations" table. See the vignette.
#' @param warn logical argument if the column names of \code{locations} are different from \code{nn_locations} functions displays a warning message. Default is \code{TRUE}.
#' @param err  logical argument if the column names of \code{locations} are different from \code{nn_locations} functions returns an error. Default is \code{!warn}.
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
#' add_locations_into_ssdb(conn,val$locations,new=TRUE)
#' ### Add weather station from MeteoTrentino network, Trentino, Italy 
#' val_smet <- meteotrentino_smet_dataset(smet_files=c("T0175","T0179"))
#' add_locations_into_ssdb(conn,val_smet$locations,append=TRUE)
#' 
#' dbDisconnect(conn)
#' 
#' conn = dbConnect(PostgreSQL(), dbname = dbname)
#' out <- st_read(conn,"locations")
#' dbDisconnect(conn)
#' 
#' }
add_locations_into_ssdb <- function (conn,locations,new=FALSE,append=!new,sql_files=system.file("sql/create_or_replace_functions.sql",package="SuSnowDB"),
                                    warn=TRUE,err=!warn,
                                    nn_locations=default_nn_locations(),...) {
  ##### c("location_code","location_code0","location_name","altitude" ,"city_name",             
  ##"country_code_iso_3166_1","country_code_iso_3166_2","country_name" ,"description","geometry",
  ##"location_source" ,"location_url" ,"use_limitations")
  
  if (new==TRUE) append <- FALSE;
  
  if (append==FALSE)  {
    sql_commands <- sql_files %>% map(readLines) %>% unlist() %>% paste(collapse="\n")
    dbSendQuery(conn,statement=sql_commands)
   query_create_tables <-"SELECT create_or_replace_all_measurement_location_tables();"
   # query_create_tables <-"SELECT create_or_replace_locations();"
  #  "SELECT create_or_replace_measurements();"
    dbSendQuery(conn,statement=query_create_tables)
  } else {
    ## GEOGRAPHIC CONTROL 
    locations_table <- st_read(conn,"locations")
    crs <- (locations_table %>% st_crs())$epsg ## epsg 4326 crs applied
    nn_locations <- st_read(conn,"locations") %>% names()
    i_already_existing_codes <- which(locations$location_code0 %in%  locations_table$location_code0)
    if (length(i_already_existing_codes)>0) {
      msg <- sprintf("The location code0  %s are already present in the database and will not be added!",paste(locations$location_code0[i_already_existing_codes],collapse=" "))
      warning(msg)
      locations <- locations[-i_already_existing_codes,]
      
    }
    if (!is.na(crs)) {
      if (st_crs(locations)$epsg!=crs) {
       msg <- sprintf("Transforming from CRS epsg %s to CRS epsg %s",as.character(st_crs(locations)$epsg,as.character(crs)))
       message(msg)
       locations <- st_transform(locations,crs=crs)
      
      
     }
    }
  }  
  ##
  
  inn <- which(!(names(locations) %in% nn_locations))
  if (length(inn)>0) {
    
    msg <- sprintf("Columns named as %s in locations variable cannot be imported to the DB.",paste(names(locations)[inn],collapse=" "))
    locations <- locations[,-inn]
    if (warn==TRUE) {
      warning(msg)
    } else {
      stop(msg)
    }
  }
  inl <- which(!(nn_locations %in% names(locations)))
  if (length(inl)>0) {
    
    msg <- sprintf("Columns named as %s in locations variable are mising and may be filled as NA in the DB.",paste(nn_locations[inn],collapse=" "))
    locations[,nn_locations[inl]] <- NA
    if (warn==TRUE) {
      warning(msg)
    } else {
      stop(msg)
    }
  }
  locations <- locations[,nn_locations]
  
  
  o_locations <- st_write(locations,conn,"locations",append=TRUE)
 ## o_measurement_types <- dbWriteTable(conn=conn,name="measurement_types",value=dataset$measurement_types,append=TRUE,row.names=FALSE)
 ##  o_measurements <- dbWriteTable(conn=conn,name="measurements",value=dataset$measurements,append=TRUE,row.names=FALSE)
  
  ##return(list(locations=o_locations,measurement_types=o_measurement_types,measurements=o_measurements))
 
  
  out <- locations
  
  return(out)
  
  
}