# SuSnowDB : Bavarian Forest / Sumava area Snow Observations Database source code (R Packege / PostgreSQL Code)

This  database contains a schema with 3 tables (see below). 

## Tables 

### Locations


* __location_code__:location code ,alphanumeric;

* __location_code0__:location code (PRIMARY KEY),alphanumeric,unique;
* __location_name__	:location human readable (geographic) name
* __altitude__:	altitude in meters above sea level; 
* __city_name__:	name of the city related to the location (if exsists);
* __country_code_iso_3166_1__:	ISO 3166-1 administrative area code (e.g  country code)(see https://en.wikipedia.org/wiki/ISO_3166-1);
* __country_code_iso_3166_2__:	ISO 3166-2 administrative area code (e.g. region/province/state code) (see https://en.wikipedia.org/wiki/ISO_3166-2);
* __country_name__	Country Human Readable Name;
* __description__	Description of the location/siter;
* __geometry__	Geospatial coordinates (in case of point), managed by geospatial ttols (e.g. sf or postGIS);
* __location_source__	Source of the data for the location;
* __location_url__	Reference URL for the location;
* __use_limitations__	Terms of Use for location data and metadata.

### Measurement Types

* __variable_code0__: variable code (PRIMARY KEY),alphanumeric,unique;
* __variable__:    human readable name of the variable;
* __unit__: measurement unit;
* __description__: textual description of the measurement type;
* __measurement_time_interval__: time interval at which variable has been measured and/or reported, e.g. hourly, daily, monthly, etc.. .

### Measurements

* __time__:timestamp-TZ instant   ;
* __value__: numeric value of the measurement (expressed in unit of _variable_code0_)    ;
* __flag__:  flag related to the measurent (see _description_);
* __location_code0__:  location code (FOREIGN KEY from table _locations_) of the location of the measurement;
* __variable_code0__:  variable code (FOREIGN KEY from thable _measurement_types) of the measured variable;
* __description__:  textual description and notes of the measurement. 


## Examples

An example contains snow depth observations pulic access from : 

* DWD : https://cdc.dwd.de/portal/ Terms of use: https://opendata.dwd.de/climate_environment/CDC/Terms_of_use.pdf
* CHMI : https://www.chmi.cz/?l=en ( https://www.chmi.cz/historicka-data/pocasi/denni-data/Denni-data-dle-z.-123-1998-Sb# )





## Installation 


Packege `SuSnowDB` can be installed through R console (currently) private repository):


```
remotes::install_github("ecor/SuSnowDB")
```


Date: 2021-10-20


