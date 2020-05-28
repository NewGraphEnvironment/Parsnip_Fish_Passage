##filter the points
make_tracks_of_points <- function(track_points, idx){
  track_points %>% 
    slice(min(idx):max(idx))
}

##turn points into a line for GPS tracks
##from https://geocompr.github.io/geocompkg/articles/gps-tracks.html
points2line_trajectory = function(p) {
  c = st_coordinates(p)
  i = seq(nrow(p) - 2)
  l = purrr::map(i, ~ sf::st_linestring(c[.x:(.x + 1), ]))
  s = purrr::map_dbl(i, function(x) {
    geosphere::distHaversine(c[x, ], c[(x + 1), ]) /
      as.numeric(p$time[x + 1] - p$time[x])
  }
  )
  lfc = sf::st_sfc(l)
  a = seq(length(lfc)) + 1 # sequence to subset
  p_data = cbind(sf::st_set_geometry(p[a, ], NULL), s)
  sf::st_sf(p_data, geometry = lfc)
}

##pull something from our data
pull_data <- function(sheet, site, column = 'gazetted_name', direction = 'us'){
  sheet %>% 
    dplyr::filter(alias_local_name == paste0(site, '_', direction)) %>% 
    dplyr::pull(column)
}

##pull UTM 
pull_utm <- function(sheet, site){
paste0(pull_data(sheet, site, column = 'utm_zone'), 
       'U ',
      pull_data(sheet, site, column = 'utm_easting'),
      ' ',
      pull_data(sheet, site, column = 'utm_northing'))
}

##make table from hab assessment data
make_table <- function(loc_dat, site_dat){
  left_join(
    loc_dat %>%
    select(alias_local_name, 'zone' = utm_zone,
           'easting' = utm_easting, 'northing' = utm_northing),
    site_dat %>%
    select(local_name, gazetted_names,avg_channel_width_m,
           avg_wetted_width_m, average_residual_pool_depth_m,
           average_bankfull_depth_m,average_gradient_percent,
           bed_material_dominant, bed_material_subdominant, feature_type,
           utm_zone, utm_easting, utm_northing) %>%
    mutate_at(vars(avg_channel_width_m:average_gradient_percent), as.numeric) %>%
    mutate_at(vars(avg_channel_width_m:average_gradient_percent), round, 2) %>%
    mutate(average_gradient_percent = average_gradient_percent * 100) ,
  by = c('alias_local_name' = 'local_name')) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location'), remove = F) %>% 
    mutate(gazetted_names = stringr::str_replace_all(gazetted_names, 'Unnamed tributary', 'Trib')) 
}

##import pscis info
import_pscis <- function(workbook_name = 'pscis_phase2.xls'){
  read_excel(path = paste0("./data/", workbook_name), 
                    sheet = 'PSCIS Assessment Worksheet') %>% 
  # purrr::set_names(janitor::make_clean_names(names(.))) %>% 
  altools::at_trim_xlsheet2() %>% 
  rename(date = date_of_assessment_yyyy_mm_dd) %>% 
  mutate(date = excel_numeric_to_date(as.numeric(date))) %>% 
  filter(!is.na(date))
}


##----------------------------make a planning table-------------------------------
make_table_planning <- function(){
#establish connection with database 
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, 
                  dbname = 'postgis',
                  host = 'localhost', 
                  port = '5432',
                  user = 'postgres', 
                  password = 'postgres')
query <- "SELECT mw.*, p.stream_crossing_id as psc_stream_crossing_id, p.utm_zone, p.utm_easting, p.utm_northing FROM working.my_pscis_20190709 mw LEFT OUTER JOIN whse_fish.pscis_assessment_svw p on p.stream_crossing_id = mw.stream_crossing_id;"
my_planning_data <- sf::st_read(conn, query = query) %>% 
  filter(watershed_group_code == 'PARS',
         !is.na(my_priority))
dbDisconnect(conn = conn)
table_planning  <- my_planning_data %>% 
  sf::st_set_geometry(NULL) %>% 
  # filter(my_priority == "high" & 
  #        watershed_group_code %like% 'PARS') %>% 
  arrange(stream_crossing_id) %>% 
  mutate(my_stream_name = case_when(is.na(my_stream_name) ~ stream_name,
                                    TRUE ~ my_stream_name),
         my_priority = stringr::str_to_title(my_priority),
         road_name = case_when(is.na(road_name) ~ my_road_name,
                               TRUE ~ road_name)) %>% 
  transmute(Site = stream_crossing_id, 
            stream_word = my_stream_name,
            Stream = paste0("[", my_stream_name, "](", image_view_url, ")"),
            
            map_linked = paste0("[", dbm_mof_50k_grid_map_tile, "](", paste0('https://hillcrestgeo.ca/outgoing/forNewGraph/pars_maps/PARS_CRKD_CARP_', sub("(.{4})(.*)", "\\1.\\2", dbm_mof_50k_grid_map_tile), '.pdf'), ")"), 
            `Map 50k` = dbm_mof_50k_grid_map_tile, 
            Road = road_name,
            `UTM (10N)` = paste0(round(utm_easting,0), " ", round(utm_northing,0)),
            `Habitat Gain (km)` = round(uphab_gross_sub22/1000,1),
            `Lake / Wetland (ha)` = round((upstr_alake_gross_obs + upstr_alake_gross_inf + upstr_awet_gross_all),1),
            `Stream Width (m)` = round(downstream_channel_width,1), 
            `Fish Upstream`= case_when(!is.na(upstr_species) ~ 'Yes', TRUE ~ 'No'), 
            `Habitat Value` = paste0(substr(habitat_value_code, 1, 1), tolower(substr(habitat_value_code, 2, nchar(habitat_value_code)))), 
            `Rank` = my_priority,
            Comments = my_text) %>% 
  mutate_all(~replace_na(.,"-")) %>% 
  mutate(Comments = stringr::str_replace_all(Comments, 'Marlim 2013', 'Gollner et al. (2013)'),
         `Habitat Value` = case_when(`Habitat Value` == 'NANA' ~ '-',
                                     TRUE ~ `Habitat Value`))

table_planning
}

##https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar
fit_to_page <- function(ft, pgwidth = 9.44){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}