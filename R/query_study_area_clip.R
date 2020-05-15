# rm(list = ls())  


library(RPostgreSQL)
library(tidyverse)
library(sf)
library(data.table)

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

dbGetQuery(conn,
           "SELECT table_name 
           FROM information_schema.tables 
           WHERE table_schema='whse_basemapping'")

##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type 
           FROM information_schema.columns 
           WHERE table_name='modelled_habitat_potential'")

dbGetQuery(conn,
           "SELECT column_name,data_type 
           FROM information_schema.columns 
           WHERE table_name='fwa_watershed_groups_poly'")

##list the unique values in the column of a table
dbGetQuery(conn,"SELECT DISTINCT(watershed_group_name)
           FROM whse_basemapping.fwa_watershed_groups_poly")

##this is using dbplyr
test <- tbl(conn, in_schema("whse_basemapping", "fwa_watershed_groups_poly")) 

##here is the info on the fish habitat etc
fish_habitat_info <- dbGetQuery(conn,
                 "
                                  SELECT fh.*, ST_X (ST_Transform (fh.geom, 4326)) AS long, ST_Y (ST_Transform (fh.geom, 4326)) AS lat
                                  FROM fish_passage.pscis_model_combined fh
                                  INNER JOIN
                                  whse_basemapping.fwa_watershed_groups_poly wg
                                  ON ST_Intersects(fh.geom,wg.geom)
                                  WHERE wg.watershed_group_id IN
                                  ('166')")

# fish_habitat_info2 <- dbGetQuery(conn,
#                                 "
#                                   SELECT fh.*
#                                   FROM fish_passage.pscis_model_combined fh
#                                   INNER JOIN
#                                   whse_basemapping.fwa_watershed_groups_poly wg
#                                   ON ST_Intersects(fh.geom,wg.geom)
#                                   WHERE wg.watershed_group_id IN
#                                   ('166')")



##lets save it as a csv to keep it simple
write.csv(fish_habitat_info,'data/fish_habitat_info.csv')



query <- "Select dra.digital_road_atlas_line_id, dra.road_name_full, dra.road_surface, dra.road_class, ST_Transform(dra.geom, 4326) as geom
FROM whse_basemapping.dra_dgtl_road_atlas_mpar_sp dra 
INNER JOIN whse_basemapping.fwa_watershed_groups_subdivided wsg
ON ST_Intersects(dra.geom, wsg.geom) 
WHERE wsg.watershed_group_code = 'PARS'"

rds_dra <- st_read(conn, query = query)


query <- "Select x.*,  ST_Transform(x.geom, 4326) as geom FROM whse_legal_admin_boundaries.abms_municipalities_sp x"
towns <- st_read(conn, query = query)


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
    geom,
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


##get a list of the assessed sites
priorities <- drake::readd(priorities) %>% 
  tidyr::separate(site_id, into = c('site', 'location'), remove = F) %>% 
  # filter(priority %ilike% 'high') %>% 
  distinct(site, .keep_all = T) %>% 
  pull(site)
  

##filter the roads to only give me the road sections with my crossings
rds_ften_priority <- rds_ften %>% 
  filter(stream_crossing_id %in% priorities)
  
  
##write the crossings to a kml 

##need to turn into a spatial line data frame
rds_ften_priority_kml <- as(rds_ften_priority, 'Spatial')


kml_open("data/priorities_rds.kml")
kml_layer(rds_ften_priority_kml, colour = '#ff7f00', labels = forest_file_id)
kml_close("data/priorities_rds.kml")




st_write(rds_dra,     "./data/parsnip.gpkg", "rds_dra", update = TRUE)
st_write(towns,     "./data/parsnip.gpkg", "towns", update = TRUE)
st_write(rds_ften,     "./data/parsnip.gpkg", "rds_ften", update = TRUE)

st_layers("./data/parsnip.gpkg")



# #this gives us the fish habitat for the Parsnip zone - parsnip (166), parsnip arm (164), carp(22) and nation (152) watersheds...
dl <- dbGetQuery(conn,
                 "
                                  SELECT fh.*
                                  FROM fish_passage.modelled_habitat_potential fh
                                  INNER JOIN
                                  whse_basemapping.fwa_watershed_groups_poly wg
                                  ON ST_Intersects(fh.geom,wg.geom)
                                  WHERE wg.watershed_group_id IN
                                  ('166',
                                  '22',
                                  '164',
                                  '152')")

#this gives us the dra_dpr_rd_layer for the Parsnip zone - parsnip (166), parsnip arm (164), carp(22) and nation (152) watersheds...
dl <- dbGetQuery(conn,
                            "
                                  SELECT dra.*,wg.watershed_group_id, wg.watershed_group_code as watershed_group_code_wg, wg.watershed_group_name
                                  FROM whse_basemapping.dra_dgtl_road_atlas_dpar_sp dra
                                  LEFT JOIN 
                                  whse_basemapping.fwa_watershed_groups_poly wg
                                  ON ST_Intersects(dra.geom,wg.geom)
                                  WHERE wg.watershed_group_id IN
                                  ('166',
                                  '22',
                                  '164',
                                  '152')")
#dra_dgtl_road_atlas_dpar_sp
dl <- dbGetQuery (conn,
                           "WITH wsg AS
                           (SELECT * FROM whse_basemapping.fwa_watershed_groups_poly WHERE watershed_group_id IN
                           ('166',
                           '22',
                           '164',
                           '152'))
                           
                           Select dra.* 
                           FROM whse_basemapping.dra_dgtl_road_atlas_dpar_sp dra, wsg
                           where ST_Intersects(dra.geom,wsg.geom)")
#dra_dgtl_road_atlas_mpar_sp
dl <- dbGetQuery (conn,
                  "WITH wsg AS
                  (SELECT * FROM whse_basemapping.fwa_watershed_groups_poly WHERE watershed_group_id IN
                  ('166',
                  '22',
                  '164',
                  '152'))
                  
                  Select dra.* 
                  FROM whse_basemapping.dra_dgtl_road_atlas_mpar_sp dra, wsg
                  where ST_Intersects(dra.geom,wsg.geom)")


# peace_dram <- dbGetQuery(conn,
#                          "
#                                   SELECT dra.*,wg.watershed_group_id, wg.watershed_group_code as watershed_group_code_wg, wg.watershed_group_name
#                                   FROM whse_basemapping.dra_dgtl_road_atlas_mpar_sp dra
#                                   INNER JOIN 
#                                   whse_basemapping.fwa_watershed_groups_poly wg
#                                   ON ST_Intersects(dra.geom,wg.geom)
#                                   WHERE wg.watershed_group_id IN
#                                   ('166',
#                                   '22',
#                                   '164',
#                                   '152')")

# peace_tr <- dbGetQuery(conn,
#                          "
#                          SELECT tr.*
#                          FROM whse_basemapping.transport_line tr
#                          INNER JOIN 
#                          whse_basemapping.fwa_watershed_groups_poly wg
#                          ON ST_Intersects(tr.geom,wg.geom)
#                          WHERE wg.watershed_group_id IN
#                          ('166',
#                          '22',
#                          '164',
#                          '152')")

##lets just call it dl to make it simple


##check to see that you have a unique identifier
names(dl)
n_distinct(dl$fid) == nrow(dl)
n_distinct(dl$objectid)
nrow(dl)

##view duplicated rows
# duplicates <- dl %>%
#   group_by(objectid) %>%
#   filter(n() > 1)

duplicates <- dl %>%
  group_by(fid) %>%
  filter(n() > 1)

##remove duplicated rows
dl <- dl %>% 
  distinct()
n_distinct(dl$objectid) == nrow(dl)


##add a column for the FID to use as the primary key if you don't have a unique ID (i.e. objectid)
# dl <- mutate(dl, fid = as.integer(rownames(dl))) %>%
#   select(fid, everything())
# names(dl)


##don't forget to change these
names(dl)
schema_name = "peace"
table_name = "modelled_habitat_potential"

dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", schema_name,";"))

##write the table to the database ##should row.names = TRUE then make that the primary key?
dbWriteTable(conn, c(schema_name, table_name), value = dl, overwrite = TRUE)

##use postGIStools to assign geometry type and primary key column
dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom TYPE geometry;"))

##ALTER TABLE tableName ADD PRIMARY KEY (id)
dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ADD PRIMARY KEY (",'"objectid"',");"))





#write  table to postgres -
dbSendQuery(conn, "CREATE SCHEMA IF NOT EXISTS peace;")
dbWriteTable(conn, c(schema = "peace", table = "modelled_habitat_potential"), value = peace_streams, overwrite = TRUE, row.names(TRUE))

##use postGIStools to assign geometry type and primary key column
dbSendQuery(conn, "ALTER TABLE fwcp_coast.fish_habitat_035 ALTER COLUMN geom TYPE geometry;")

##ALTER TABLE tableName ADD PRIMARY KEY (id)
dbSendQuery(conn, 'ALTER TABLE fwcp_coast.fish_habitat_035 ADD PRIMARY KEY ("row.names");')