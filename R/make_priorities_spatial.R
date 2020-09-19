##this is a new hack file to make a final priorities file for Simon to place on the maps

source('R/packages.R') 
source('R/Functions.R')


priorities_spreadsheet <- readxl::read_excel(path = "./data/priorities.xlsx") %>% 
  tidyr::separate(site_id, into = c('site', 'location'), remove = F)

PSCIS_submission <-  import_pscis()

table_overview_raw = make_table_overview(
  priorities_spreadsheet = priorities_spreadsheet,
  PSCIS_submission = PSCIS_submission)

table_overview <- drake::readd(table_overview_raw) %>% ##drake::readd(table_overview)
  mutate_at(vars(easting:northing), as.numeric) %>% 
  filter(location %ilike% 'upstream') %>% 
  select(-site_int, -location) %>% 
  mutate(label = paste0(priority, ' priority', '-', site)) %>% 
  sf::st_as_sf(coords = c("easting", "northing"), crs = 26910) %>% 
  mutate(easting = st_coordinates(.)[,1],
         northing = st_coordinates(.)[,2]) %>% 
  st_transform(crs = 4326)
  

#write to the geopackage
sf::st_write(table_overview, "data/parsnip.gpkg", "priorities", delete_layer = T)

##burn to a geojson for Simon
sf::st_write(table_overview, "data/parsnip_priorities.geojson")
