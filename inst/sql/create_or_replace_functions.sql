
-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

--! timescalebd extension (https://www.timescale.com/)
CREATE EXTENSION IF NOT EXISTS timescaledb CASCADE;
--! postgis extension
CREATE EXTENSION IF NOT EXISTS postgis CASCADE;

--! create measurements_types table
CREATE OR REPLACE FUNCTION create_or_replace_measurement_types() RETURNS integer
AS $$ 
BEGIN
  DROP TABLE IF EXISTS measurement_types CASCADE;
  CREATE TABLE IF NOT EXISTS measurement_types (
    variable_code0 TEXT PRIMARY KEY,
    variable TEXT NOT NULL,
    unit TEXT NOT NULL,
    description TEXT,
    measurement_time_interval TEXT,
    UNIQUE(variable_code0)
  );
  INSERT INTO measurement_types  (variable_code0,variable,unit) VALUES ('placeholder', 'placeholder', 'placeholder');
  RETURN 0;
END;
$$ LANGUAGE plpgsql;

--! create measurements_types locations
CREATE OR REPLACE FUNCTION create_or_replace_locations() RETURNS integer
AS $$ 
BEGIN
  DROP TABLE IF EXISTS locations CASCADE;
  CREATE TABLE IF NOT EXISTS locations (
    location_code0 TEXT PRIMARY KEY,
    location_code  TEXT NOT NULL,
    location_name TEXT NOT NULL,
    altitude FLOAT,
    city_name TEXT,
    country_code_iso_3166_1 TEXT,
    country_code_iso_3166_2 TEXT,
    country_name TEXT,
    description TEXT,
    geometry geometry,
    location_source TEXT,       
    location_url TEXT,           
    use_limitations TEXT,      
    UNIQUE(location_code0)
  );
  INSERT INTO locations  (location_code0, location_code, location_name) VALUES ('placeholder', 'placeholder', 'placeholder');
 
--!  PERFORM ST_SetSRID(locationsgeometry,4326);
  RETURN 0;
END;
$$ LANGUAGE plpgsql;
--! create measurements_types table
CREATE OR REPLACE FUNCTION create_or_replace_measurements() RETURNS integer
AS $$ 
BEGIN
  DROP TABLE IF EXISTS measurements CASCADE;
  CREATE TABLE IF NOT EXISTS measurements (
    time TIMESTAMPTZ NOT NULL,
    variable_code0 TEXT REFERENCES measurement_types(variable_code0),
    location_code0 TEXT REFERENCES locations(location_code0),
    value FLOAT,
    flag TEXT,
    description TEXT
  );
 --!! PERFORM create_distributed_hypertable('measurements', 'time','location_code0');
  --!!PERFORM create_hypertable('measurements', 'time');
  PERFORM create_hypertable('measurements', 'time', 'location_code0', 20); 
  --!, partitioning_func => 'location_hash');
--!! https://docs.timescale.com/api/latest/hypertable/create_hypertable/#create-hypertable
  INSERT INTO measurements (time,variable_code0, location_code0) VALUES ('2021-01-01 00:00:00+01','placeholder','placeholder');
 --!!!  https://docs.timescale.com/api/latest/hypertable/create_index/#create-index-transaction-per-chunk 
 --!!! CREATE INDEX ON measurements(time, location_code0) WITH (timescaledb.transaction_per_chunk);
 --!!! SELECT  add_reorder_policy('measurements','location_code0');

  
  RETURN  0;
END;
$$ LANGUAGE plpgsql;


--! create measurements_types/mesurements/locations tables

CREATE OR REPLACE FUNCTION create_or_replace_all_measurement_location_tables() RETURNS integer
AS $$ 
BEGIN
 PERFORM create_or_replace_measurement_types();
 PERFORM create_or_replace_locations();
 PERFORM create_or_replace_measurements();
 RETURN 0;
END;
$$ LANGUAGE plpgsql;


--! remova empty placeholder on lmeasurements_types/mesurements/locations tables
CREATE OR REPLACE FUNCTION remove_placeholder() RETURNS integer
AS $$ 
BEGIN
 DELETE FROM measurements WHERE location_code0='placeholder';
 DELETE FROM locations WHERE location_code0='placeholder';
 DELETE FROM measurement_types WHERE variable_code0='placeholder';
 RETURN 0;
END;
$$ LANGUAGE plpgsql;


-- ! Add measurement
CREATE OR REPLACE FUNCTION add_measurement (t TIMESTAMPTZ,variable_code0 TEXT ,location_code0 TEXT ,value FLOAT,flag TEXT,description TEXT) RETURNS integer
AS $$ 
BEGIN
 INSERT INTO measurements(time,variable_code0, location_code0,value,flag,description) VALUES (t,variable_code0,location_code0,value,flag,description);
 RETURN 0;
END;
$$ LANGUAGE plpgsql;


