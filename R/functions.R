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
make_table_habitat <- function(loc_dat, site_dat){
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

##function to trim up sheet and get names (was previously source from altools package)
at_trim_xlsheet2 <- function(df, column_last = ncol(df)) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}


##import pscis info
import_pscis <- function(workbook_name = 'pscis_phase2.xlsm'){
  read_excel(path = paste0("./data/", workbook_name), 
                    sheet = 'PSCIS Assessment Worksheet') %>% 
  # purrr::set_names(janitor::make_clean_names(names(.))) %>% 
  at_trim_xlsheet2() %>% ##recently added function above and pulled the altools package as it was a week link
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

##--------------------------make the overview table-------------------------
make_table_overview <- function(priorities_spreadsheet, PSCIS_submission){
  #establish connection with database
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv,
                    dbname = 'postgis',
                    host = 'localhost',
                    port = '5432',
                    user = 'postgres',
                    password = 'postgres')
  ##make the priorities a list
  priorities_pscis <- priorities_spreadsheet %>% 
    dplyr::mutate(site_int = as.integer(site)) %>% 
    dplyr::distinct(site_int) %>% 
    dplyr::filter(!is.na(site_int)) %>% 
    dplyr::pull(site_int) 
  
  priorities_modelled <- priorities_spreadsheet %>% 
    dplyr::distinct(model_crossing_id) %>% 
    dplyr::filter(!is.na(model_crossing_id)) %>% ##need CV1 modelled crossing name
    dplyr::pull(model_crossing_id) 
  
  sql <- glue::glue_sql(
    "
                                Select fh.pscis_stream_crossing_id, fh.model_crossing_id, fh.blue_line_key, 
                                fh.downstream_route_measure, fh.wscode, fh.localcode,
                                fh.uphab_gross_sub22, fh.upstr_species,  fh.dbm_mof_50k_grid_map_tile, 
                                fh.upstr_alake_gross_obs, fh.upstr_alake_gross_inf, fh.upstr_awet_gross_all 
                                FROM fish_passage.pscis_model_combined fh
                                WHERE fh.pscis_stream_crossing_id IN
                                ({priorities_pscis*}) OR
                                fh.model_crossing_id IN
                                ({priorities_modelled*})
                                ",
    .con = conn
  )
  query <- DBI::dbSendQuery(conn, sql)
  fish_habitat_info <- DBI::dbFetch(query)
  
  table_overview <- left_join(
    select(priorities_spreadsheet,
           site, model_crossing_id, location, length_surveyed = length, priority, comments), ##just moved the hab_value from here to pscis
    select(PSCIS_submission,
           pscis_crossing_id, utm_zone, easting, northing, stream_name, road_name, road_tenure, hab_value = habitat_value),
    by = c('site' = 'pscis_crossing_id')) %>% 
    mutate(site_int = as.integer(site))
  
  ##add the habitat info for the pscis crossings
  table_overview2 <- left_join(
    table_overview,
    select(fish_habitat_info, -model_crossing_id),
    by = c('site_int' = 'pscis_stream_crossing_id'),
    na_matches = "never"
  ) %>% 
    filter(!is.na(site_int))
  
  ##get the habitat info for the modelled crossings that don't have a pscis id
  table_modelled <- left_join(
    filter(table_overview,
           is.na(site_int)),
    fish_habitat_info,
    by = c('model_crossing_id')
  ) %>% 
    select(-pscis_stream_crossing_id)
  
  ##join the hab data to the modelled crossing
  table_overview <- bind_rows(table_overview2,
                              table_modelled) %>% 
    mutate(uphab_gross_sub22 = round(uphab_gross_sub22/1000, 1),
           priority = fct_relevel(priority, levels = 'High', 'Moderate', 'Low'), 
           location = case_when(location == 'ds' ~ 'Downstream',
                                TRUE ~ 'Upstream'),
           stream_name = stringr::str_replace_all(stream_name, 'Unnamed tributary', 'Trib'),
           stream_name = stringr::str_replace_all(stream_name, 'tributary', 'Trib')) %>% 
    arrange((site_int), desc(location))
  
  dbDisconnect(conn = conn)
  return(table_overview)
}


##https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar
fit_to_page <- function(ft, pgwidth = 9.44){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}