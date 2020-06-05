library("plotKML")
library(sp)
library(RPostgreSQL)
library(tidyverse)
library(sf)
library(data.table)


##https://www.researchgate.net/figure/Example-of-a-multi-layer-KML-file-produced-using-plotKML-Topographic-Wetness-Index_fig4_279322295

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


##listthe schemas in the database
dbGetQuery(conn,
           "SELECT schema_name
           FROM information_schema.schemata")

##list tables in a schema  
dbGetQuery(conn,
           "SELECT table_name 
           FROM information_schema.tables 
           WHERE table_schema='whse_legal_admin_boundaries'")


##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type 
           FROM information_schema.columns 
           WHERE table_name='modelled_habitat_potential'")

##list the unique values in the column of a table
dbGetQuery(conn,"SELECT DISTINCT(watershed_group_name)
           FROM whse_basemapping.fwa_watershed_groups_poly")


##here is the info on the fish habitat etc
query <- "SELECT fh.*, ST_X (ST_Transform (fh.geom, 4326)) AS long, ST_Y (ST_Transform (fh.geom, 4326)) AS lat,
                                  ST_Transform(fh.geom, 4326) as geom
                                  FROM fish_passage.pscis_model_combined fh
                                  WHERE watershed_group_code = 'PARS'"


fish_habitat_info <- st_read(conn, query = query) %>% 
  select(-id)

##lets save it as a csv to keep it simple
write.csv(fish_habitat_info,'data/fish_habitat_info.csv')




query <- "Select dra.digital_road_atlas_line_id, dra.road_name_full, dra.road_surface, dra.road_class, ST_Transform(dra.geom, 4326) as geom
FROM whse_basemapping.dra_dgtl_road_atlas_mpar_sp dra 
INNER JOIN whse_basemapping.fwa_watershed_groups_subdivided wsg
ON ST_Intersects(dra.geom, wsg.geom) 
WHERE wsg.watershed_group_code = 'PARS'"

rds_dra <- st_read(conn, query = query)


# query <- "Select x.*,  ST_Transform(x.geom, 4326) as geom FROM whse_legal_admin_boundaries.abms_municipalities_sp x"
# towns <- st_read(conn, query = query)



####added ST_Transform(geom, 4326) as geom, could not get a result after adding
query <- "
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
  WHERE wsg.watershed_group_code = 'PARS'
  AND nn.distance_to_road < 30"

rds_ften <- st_read(conn, query = query)


##get a list of the assessed sites to filter the road info
priorities <- drake::readd(priorities) %>% 
  tidyr::separate(site_id, into = c('site', 'location'), remove = F) %>% 
  # filter(priority %ilike% 'high') %>% 
  distinct(site, .keep_all = T) %>% 
  pull(site)

##filter the roads to only give me the road sections with my crossings
rds_ften_priority <- rds_ften %>% 
  filter(stream_crossing_id %in% priorities)

rds_ften_priority_wsg <- st_transform(rds_ften_priority, crs = 4326)


##write the roads to a kml ---- NOTE that the crossings are written with the report.rmd script to link with overview table of report
##need to turn into a spatial line data frame
rds_ften_priority_kml <- as(rds_ften_priority, 'Spatial')

kml_open("data/priorities_rds.kml")
kml_layer(rds_ften_priority_kml, colour = '#ff7f00', labels = forest_file_id)  ##I don't see the label
kml_close("data/priorities_rds.kml")


##lets get the railways in the PARS
query <- "SELECT x.*, ST_Transform(x.geom, 4326) as geom
                                  FROM whse_basemapping.gba_railway_tracks_sp x
                                  INNER JOIN
                                  whse_basemapping.fwa_watershed_groups_poly wg
                                  ON ST_Intersects(x.geom,wg.geom)
                                  WHERE wg.watershed_group_id IN
                                  ('166')"



railway <- st_read(conn, query = query)

##make kml for rail
railway_kml <- as(railway, 'Spatial')

kml_open("data/priorities_railway.kml")
kml_layer(railway_kml, colour = '#000000', labels = subdivision1_name)  ##I don't see the label
kml_close("data/priorities_railway.kml")

##now we will zip up the kml files in the data folder and rename with kmz
files_to_zip <- paste0("data/", list.files(path = "data/", pattern = "\\.kml$"))  ##this used to includes the planning file which we don't want to do so watch out
zip::zipr("data/parsnip_priorities.zip", files = files_to_zip)  ##it does not work to zip to kmz!!

##this is how I zipped the planning file
# zip::zipr("data/planning_high_mod_culverts.zip", files = 'data/planning_high_mod_culverts.kml') 

##make the geopackage
st_write(rds_dra,     "./data/parsnip.gpkg", "rds_dra", update = TRUE)
st_write(towns,     "./data/parsnip.gpkg", "towns", update = TRUE)
st_write(rds_ften,     "./data/parsnip.gpkg", "rds_ften", update = TRUE)
st_write(rds_ften_priority_wsg,     "./data/parsnip.gpkg", "rds_ften_priority", update = TRUE)
st_write(fish_habitat_info,     "./data/parsnip.gpkg", "pscis_model_combined", update = TRUE)
st_write(railway,     "./data/parsnip.gpkg", "railway", update = TRUE)

st_layers("./data/parsnip.gpkg")  ##note that the priorities are also in there built from the overview table output

##here is the fish sampling and features for Simon
table_habitat_features <- drake::readd(table_habitat_raw) %>% 
  dplyr::distinct(utm_easting, utm_northing, feature_type, .keep_all = T) %>% 
  filter(!is.na(feature_type)) %>% 
  sf::st_as_sf(coords = c("utm_easting", "utm_northing"), crs = 26910, remove = F) %>%
  st_transform(crs = 4326)

fish_sampling_data_sf <- drake::readd(fish_sampling_data)  %>% 
  dplyr::distinct(utm_easting, utm_northing, species_code, .keep_all = T) %>% 
  filter(!is.na(species_code)) %>% 
  sf::st_as_sf(coords = c("utm_easting", "utm_northing"), crs = 26910, remove = F) %>%
  st_transform(crs = 4326)

##write to the geopackage
sf::st_write(table_habitat_features, "./data/parsnip.gpkg", "habitat_features", update = TRUE)
sf::st_write(fish_sampling_data_sf, "./data/parsnip.gpkg", "fish_sampling_data", update = TRUE)



# write_csv(table_habitat_raw, 'data/table_habitat_raw.csv', na = "NA") ##this was how I did it before with raw inputs
# write_csv(fish_sampling_data, 'data/fish_sampling_data.csv', na = "NA") ##this was how I did it before with raw inputs




