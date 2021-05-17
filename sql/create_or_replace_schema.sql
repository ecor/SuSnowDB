-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

--! timescalebd extension (https://www.timescale.com/)
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;
--! postgis extension
CREATE EXTENSION IF NOT EXISTS postgis CASCADE;

--! create measurements_types table
CREATE OR REPLACE FUNCTION create_or_replace_measurements_types() RETURNS integer
AS $$ 
BEGIN
  DROP TABLE IF EXISTS measurement_types();
  CREATE TABLE IF NOT EXISTS measurement_types (
    variable_code0 TEXT PRIMARY KEY,
    variable TEXT NOT NULL,
    unit TEXT NOT NULL,
    description TEXT,
    measurement_time_interval TEXT,
    UNIQUE(variable_code0)
  );
  RETURNS 0;
END;
$$ LANGUAGE plpgsql;

--! create measurements_types locations
CREATE OR REPLACE FUNCTION create_or_replace_locations() RETURNS integer
AS $$ 
BEGIN
  DROP TABLE IF EXISTS locations;
  CREATE TABLE IF NOT EXISTS locations (
    location_code0 TEXT PRIMARY KEY,
    location_code  TEXT NOT NULL,
    location_name TEXT,
    altitude FLOAT,
    city_name TEXT,
    country_code_ISO_3166_1 TEXT,
    country_code_ISO_3166_2 TEXT,
    country_name TEXT,
    description TEXT,
    geometry geometry,
    location_source TEXT,       
    location_URL TEST           
    use_limitations"  TEXT,      
    UNIQUE(location_code0)
  );
  RETURNS 0;
END;
$$ LANGUAGE plpgsql;


--! create measurements_types table
CREATE OR REPLACE FUNCTION create_or_replace_measurements() RETURNS integer
AS $$ 
BEGIN
  DROP TABLE IF EXISTS measurements();
  CREATE TABLE IF NOT EXISTS measurements (
    time TIMESTAMPTZ NOT NULL,
    variable_code0 TEXT REFERENCES measurement_types(variable_code0),
    location_code0 TEXT REFERENCES locations(location_code0),
    value FLOAT,
    flag TEXT,
    description TEXT
  );
  SELECT create_hypertable('measurements', 'time');
  RETURNS 0;
END;
$$ LANGUAGE plpgsql;


--! create measurements_types/mesurements/locations tables
CREATE OR REPLACE FUNCTION create_or_replace_all_measurement_location_tables() RETURNS integer
AS $$ 
BEGIN
 SELECT create_or_replace_measurement_types();
 SELECT create_or_replace_locations();
 SELECT create_or_replace_measurements();
 RETURNS 0;
END;
$$ LANGUAGE plpgsql;


