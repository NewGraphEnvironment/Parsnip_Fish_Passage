
library(RPostgreSQL)
library(tidyverse)
# library(dbplyr)


#Enter the values for you database connection and connect
{dsn_database = "postgis"            
  dsn_hostname = "localhost"
  dsn_port = "5432"               
  dsn_uid = "postgres"        
  dsn_pwd = "postgres"
  
  
  #connect and see if the connection to the database is working
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    print("Connecting to database")
    conn <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                      password = dsn_pwd)
    print("Connected!")
  },
  error=function(cond) {
    print("Unable to connect to database.")
  })
}

rds_output <- dbGetQuery(conn,
                         "with dra as
(SELECT
  pt.stream_crossing_id,
  nn.*
    FROM whse_fish.pscis_assessment_svw as pt
  CROSS JOIN LATERAL
  (SELECT
    digital_road_atlas_line_id,
    road_name_full,
    road_surface,
    road_class,
    highway_route_number,
    ST_Distance(rd.geom, pt.geom) as distance_to_road
    FROM whse_basemapping.dra_dgtl_road_atlas_mpar_sp AS rd
    ORDER BY rd.geom <-> pt.geom
    LIMIT 1) as nn
  INNER JOIN whse_basemapping.fwa_watershed_groups_poly wsg
  ON st_intersects(pt.geom, wsg.geom)
  AND nn.distance_to_road < 30),

ften as (
  SELECT
  pt.stream_crossing_id,
  nn.*
    FROM whse_fish.pscis_assessment_svw as pt
  CROSS JOIN LATERAL
  (SELECT
    forest_file_id,
    road_section_id,
    road_responsibility_type_code,
    retirement_date,
    file_status_code,
    file_type_code,
    file_type_description,
    client_number,
    client_name,
    life_cycle_status_code,
    ST_Distance(rd.geom, pt.geom) as distance_to_road
    FROM whse_forest_tenure.ften_road_segment_lines_svw AS rd
    WHERE life_cycle_status_code not in ('RETIRED', 'PENDING')
    ORDER BY rd.geom <-> pt.geom
    LIMIT 1) as nn
  INNER JOIN whse_basemapping.fwa_watershed_groups_poly wsg
  ON st_intersects(pt.geom, wsg.geom)
  AND nn.distance_to_road < 30),

mot as (
  SELECT
  pt.stream_crossing_id,
  nn.*
    FROM whse_fish.pscis_assessment_svw as pt
  CROSS JOIN LATERAL
  (SELECT
    road_feature_invntry_id,
    chris_rfi_highway_id   ,
    rfi_highway_name       ,
    rfi_highway_description,
    rfi_highway_direction  ,
    rfi_highway_length     ,
    service_area           ,
    area_manager_area      ,
    sub_area               ,
    rfi_highway_type       ,
    rfi_highway_number     ,
    rfi_highway_alpha      ,
    highway_number         ,
    ST_Distance(rd.geom, pt.geom) as distance_to_road
    FROM whse_imagery_and_base_maps.mot_road_features_invntry_sp AS rd
    ORDER BY rd.geom <-> pt.geom
    LIMIT 1) as nn
  INNER JOIN whse_basemapping.fwa_watershed_groups_poly wsg
  ON st_intersects(pt.geom, wsg.geom)
  WHERE wsg.watershed_group_code = 'PARS'
  AND nn.distance_to_road < 30)

SELECT
pt.stream_crossing_id,
dra.*,
ften.*,
mot.*
  FROM
whse_fish.pscis_assessment_svw pt
LEFT OUTER JOIN dra ON pt.stream_crossing_id = dra.stream_crossing_id
LEFT OUTER JOIN ften ON pt.stream_crossing_id = ften.stream_crossing_id
LEFT OUTER JOIN mot ON pt.stream_crossing_id = mot.stream_crossing_id;")
