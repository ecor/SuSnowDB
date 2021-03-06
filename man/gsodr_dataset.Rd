% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsodr_dataset.R
\name{gsodr_dataset}
\alias{gsodr_dataset}
\title{Create GSOD Weather Dataset from (UDER DEVELOPMENT)}
\usage{
gsodr_dataset(
  x = NULL,
  ...,
  description_url = "https://www7.ncdc.noaa.gov/CDO/GSOD_DESC.txt",
  location_code0 = "GSOD_\%s",
  variable_code0 = "GSODR_\%s",
 
    location_url = "https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc\%3AC00516/html#",
 
    use_limitations = "The following data and products may have conditions placed on their international commercial use. They can be used within the U.S. or for non-commercial international activities without restriction. The non-U.S. data cannot be redistributed for commercial purposes. Re-distribution of these data by others must provide this same notification.\\" WMO Resolution 40. NOAA Policy",
  location_source = "GSOD",
  crs = 4326,
 
    measurement_types = read.table(system.file("west_africa_gsod_extdata/gsod_variable_measurement_types_v1.csv",
    package = "SuSnowDB"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
)
}
\arguments{
\item{x}{data frame returned from a call of \code{\link{get_GSOD}}. Default is \code{NULL}.}

\item{...}{arguments for \code{\link{get_GSOD}}}

\item{description_url, location_url, location_source, use_limitations}{attributes for \code{locations} table. See \code{locations} table and default.}

\item{location_code0, variable_code0}{attributes for \code{locations} and \code{measurement_types} tables, as above. If it is one-element,it may contain a sub-string "\code{\%s}" that will be replaced with the GSOD station identification code (STNID).}

\item{crs}{Coordinate refence system. Default is 4326 (epsg).}

\item{measurement_types}{data frame from a CSV file containing table with measuremet type attributes . It must contain an additional column \code{has_flag} .See \code{measurement_types} table and default.}
}
\description{
Create GSOD Weather Dataset from (UDER DEVELOPMENT)
}
\examples{
 library(readr)
 x <- system.file("west_africa_gsod_extdata/gsod_bf.csv",package="SuSnowDB") \%>\% read_csv()
 out <- gsodr_dataset(x=x)
 
 \dontrun{
 t <- system.time(out <- gsodr_dataset(years=2020,country="IT"))
 }
}
