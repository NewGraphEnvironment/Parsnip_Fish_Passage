# rm(list = ls())  


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


##list tables in a schema  
dbGetQuery(conn,
           "SELECT table_name 
           FROM information_schema.tables 
           WHERE table_schema='peace'")

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