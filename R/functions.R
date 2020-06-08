##things to do
##deal with variable types and rounding issues on inport of spreadsheets
##build reporting tables with shiny to allow user to choose parameters?
##look at need for my_tracks.no longer linked to photo georeferenceing or use datatable for issue which triggered its pull
##get method to transpose habitat_data so we can pull max, min, range etc values.and pull cover info by dominance category

# ##filter the points
# make_tracks_of_points <- function(track_points, idx){
#   track_points %>% 
#     slice(min(idx):max(idx))
# }
# 
# ##turn points into a line for GPS tracks
# ##from https://geocompr.github.io/geocompkg/articles/gps-tracks.html
# points2line_trajectory = function(p) {
#   c = st_coordinates(p)
#   i = seq(nrow(p) - 2)
#   l = purrr::map(i, ~ sf::st_linestring(c[.x:(.x + 1), ]))
#   s = purrr::map_dbl(i, function(x) {
#     geosphere::distHaversine(c[x, ], c[(x + 1), ]) /
#       as.numeric(p$time[x + 1] - p$time[x])
#   }
#   )
#   lfc = sf::st_sfc(l)
#   a = seq(length(lfc)) + 1 # sequence to subset
#   p_data = cbind(sf::st_set_geometry(p[a, ], NULL), s)
#   sf::st_sf(p_data, geometry = lfc)
# }
# 
##pull something from our data
pull_data <- function(sheet, site, column = 'gazetted_name', direction = 'us'){
  sheet %>%
    dplyr::filter(alias_local_name == paste0(site, '_', direction)) %>%
    dplyr::pull(column)
}
# 
# ##pull UTM 
# pull_utm <- function(sheet, site){
# paste0(pull_data(sheet, site, column = 'utm_zone'), 
#        'U ',
#       pull_data(sheet, site, column = 'utm_easting'),
#       ' ',
#       pull_data(sheet, site, column = 'utm_northing'))
# }

##set the default page size for flextable
##https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar
##landscape width is 9.44 - portrait old is 6.49
fit_to_page <- function(ft, pgwidth = 6.75){
  
  ft_out <- ft %>% autofit()
  
  ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
  return(ft_out)
}


my_flextable <- function(df, left_just_col = 2, ...){
  flextable::autofit(flextable::flextable(
    df,
    defaults = list(fontname = 'tahoma'))) %>% 
    flextable::my_theme_booktabs(fontsize = 9) %>% 
    fit_to_page()
}


##-----------make table from hab assessment data
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
           utm_zone, utm_easting, utm_northing),
    # mutate_at(vars(avg_channel_width_m:average_gradient_percent), as.numeric) %>%
    # mutate_at(vars(avg_channel_width_m:average_bankfull_depth_m), round, 1) %>%
    #   mutate(average_gradient_percent = round(average_gradient_percent * 100, 1)),##changed
    # mutate(average_gradient_percent = average_gradient_percent * 100) ,
  by = c('alias_local_name' = 'local_name')) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location'), remove = F) %>% 
    mutate(gazetted_names = stringr::str_replace_all(gazetted_names, 'Unnamed tributary', 'Trib')) 
}

## -------make table habitat_report

make_table_habitat_report <- function(table_habitat_raw, PSCIS_submission, priorities_spreadsheet){
  
  names_hab_table <- c('alias_local_name', 'site', 'location', 'gazetted_names','avg_channel_width_m',
                       'avg_wetted_width_m', 'average_residual_pool_depth_m',
                       'average_gradient_percent')
  
  table_habitat_report <- table_habitat_raw %>%
    dplyr::select(all_of(names_hab_table)) %>% 
    mutate_at(vars(avg_channel_width_m:average_residual_pool_depth_m), round, 1)
  
  table_habitat_report <- left_join(
    table_habitat_report,
    select(PSCIS_submission,
           pscis_crossing_id, habitat_value),
    by = c('site' = 'pscis_crossing_id'))
  
  table_habitat_report <- left_join(
    table_habitat_report,
    select(priorities_spreadsheet,
           site, location, length, comments),
    by = c('site','location')
  )
  
  new_names <-  c('alias_local_name', 'Site', 'Location',
                  'Stream', 'Channel Width (m)', 'Wetted Width (m)',
                  'Pool depth (m)', 'Gradient (%)', 'Habitat Value', 
                  'Length Surveyed (m)','Comments')
  
  table_habitat_report <- table_habitat_report %>%
    purrr::set_names(., nm = new_names) %>% 
    mutate(Location = case_when(Location == 'ds' ~ 'Downstream',
                                TRUE ~ 'Upstream'),
           site_sort = as.numeric(Site)) %>%
    arrange(site_sort) %>% 
    mutate_all(~replace_na(.,"-")) %>% 
    select(Site, Location, `Length Surveyed (m)`, everything(), -alias_local_name, -Stream, -site_sort)
  return(table_habitat_report)
}

##this is just an object.
tablehabvalue <- tibble::tibble(`Habitat Value` = c('High', 'Medium', 'Low'),
                                `Fish Habitat Criteria` = c(
                                  'The presence of high value spawning or rearing habitat (e.g., locations with abundance of suitably sized gravels, deep pools, undercut banks, or stable debris) which are critical to the fish population.', 
                                  'Important migration corridor. Presence of suitable spawning habitat. Habitat with moderate rearing potential for the fish species present.', 'No suitable spawning habitat, and habitat with low rearing potential (e.g., locations without deep pools, undercut banks, or stable debris, and with little or no suitably sized spawning gravels for the fish species present).'
                                )
)

table_habitat_value_html <- function(...){
  knitr::kable(tablehabvalue,
               caption = 'Habitat value criteria (Fish Passage Technical Working Group, 2011).') %>% 
    kableExtra::column_spec(column = 1, width_min = '1.5in') %>% 
    kableExtra::kable_styling(c("condensed"), full_width = T) 
}


table_habitat_value_flextable <- function(...){
  my_flextable(tablehabvalue, ...) %>%
    flextable::width(j = 1, width = 1) %>%
    # align(j = 2, align = 'left', part = "all") %>%
    flextable::set_caption('Habitat value criteria (Fish Passage Technical Working Group, 2011).')
}


table_habitat_flextable <- function(df = table_habitat_report, site = my_site, ...) {
  df %>% 
    filter(Site == site) %>%
    select(-Comments) %>% 
    my_flextable(fontsize = 8, ...) %>%
    flextable::width(j = c(1,3:5), width = 0.8) %>%
    flextable::width(j = 2, width = 0.8) %>%
    # flextable::width(., j = 9, width = 2.2) %>%
    flextable::set_caption('Summary of habitat details')
}

table_habitat_html <- function(df = table_habitat_report, site = my_site){
  df %>% 
    filter(Site == site) %>% 
    select(-Comments) %>% 
    knitr::kable(caption = 'Summary of habitat details') %>% 
    kableExtra::kable_styling(c("condensed"), full_width = T) %>% 
    kableExtra::row_spec(0 ,  bold = F, extra_css = 'vertical-align: middle !important;')
}

table_culvert_flextable <- function(df = table_culvert, site = my_site){
  df %>% 
    filter(Site == site) %>%
    select(-Score) %>% 
    my_flextable(fontsize = 8) %>%
    flextable::width( j = c(1,8,9), width = 0.658) %>%
    flextable::width( j = c(4), width = 0.75) %>%
    flextable::width( j = c(5), width = 0.88) %>%
    # flextable::width(., j = 9, width = 2.2) %>%
    flextable::set_caption('Summary of culvert fish passage assessment.')
}

table_culvert_html <- function(df = table_culvert, site = my_site){
  df %>% 
    filter(Site == site) %>% 
    select(-Score) %>% 
    knitr::kable(caption = 'Summary of culvert fish passage assessment.') %>% 
    kableExtra::kable_styling(c("condensed"), full_width = T) %>% 
    kableExtra::row_spec(0 ,  bold = F, extra_css = 'vertical-align: middle !important;')
}

table_overview_flextable <- function(df = table_overview_report, site = my_site){
  df %>% 
    select(-`Habitat Gain (km)`, -`Habitat Value`, -Comments) %>%
    filter(Site == site) %>%
    my_flextable(fontsize = 8) %>% 
    flextable::width(j = c(1,7), width = 0.658) %>%
    flextable::set_caption('Overview of stream crossing.')
}

table_overview_html <- function(df = table_overview_report, site = my_site){
  df %>% ##changed this
    select(-`Habitat Gain (km)`, -`Habitat Value`, -Comments) %>% 
    filter(Site == site) %>% 
    knitr::kable(caption = 'Overview of stream crossing.') %>% 
    kableExtra::kable_styling(c("condensed"), full_width = T) %>% 
    kableExtra::row_spec(0 ,  bold = F, extra_css = 'vertical-align: middle !important;')
  # kableExtra::scroll_box(width = "100%", height = "500px")
}

table_overview_html_all <- function(df){
  reports_complete = c('125000', '125179', '125180', '125186', '125231') #this needs to be abstract
  reports_complete_withzeros = c('57681')
  df %>% 
    mutate(Site = case_when(Site %in% reports_complete ~ paste0('[', Site, '](03_Parsnip_report_', Site, '.html)'),
                                       TRUE ~ Site),
           Site = case_when(Site %in% reports_complete_withzeros ~ paste0('[', Site, '](03_Parsnip_report_0', Site, '.html)'),
                            TRUE ~ Site)) %>% 
    # select(-`Habitat Gain (km)`, -`Habitat Value`, -Comments) %>% 
    # filter(Site == my_site) %>% 
    knitr::kable() %>%
    kableExtra::column_spec(column = 10, width_min = '2in') %>%
    kableExtra::kable_styling(c("condensed"), full_width = T) %>% 
    kableExtra::row_spec(0 ,  bold = F, extra_css = 'vertical-align: middle !important;')
  # kableExtra::scroll_box(width = "100%", height = "500px")
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

make_table_planning <- function(planning_data){
  table_planning  <- planning_data %>% 
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
              
              map_linked = paste0("[", dbm_mof_50k_grid_map_tile, "](", paste0('https://hillcrestgeo.ca/outgoing/forNewGraph/pars_maps/Parsnip_', sub("(.{4})(.*)", "\\1.\\2", dbm_mof_50k_grid_map_tile), '.pdf'), ")"), 
              `Map 50k` = dbm_mof_50k_grid_map_tile, 
              Road = road_name,
              `UTM (10N)` = paste0(round(utm_easting,0), " ", round(utm_northing,0)),
              `Instream (km)` = round(uphab_gross_sub22/1000,1),
              `Lake (ha)` = round(upstr_alake_gross_obs + upstr_alake_gross_inf + upstr_awet_gross_all,1),
              `Wetland (ha)` = round(upstr_awet_gross_all,1),
              `Channel Width (m)` = round(downstream_channel_width,1), 
              `Fish Upstream`= case_when(!is.na(upstr_species) ~ 'Yes', TRUE ~ 'No'), 
              `Habitat Value` = paste0(substr(habitat_value_code, 1, 1), tolower(substr(habitat_value_code, 2, nchar(habitat_value_code)))), 
              `Rank` = my_priority,
              Comments = my_text) %>% 
    mutate_all(~replace_na(.,"-")) %>% 
    mutate(Comments = stringr::str_replace_all(Comments, 'Marlim 2013', 'Gollner et al. (2013)'),
           `Habitat Value` = case_when(`Habitat Value` == 'NANA' ~ '-',
                                       TRUE ~ `Habitat Value`))
  return(table_planning)
}

table_planning_html <- function(df = table_planning){
  df %>% 
    filter(Site == my_site) %>% 
    select(-stream_word, -`Map 50k`, -Site, -Stream, -Road, -`UTM (10N)`) %>% 
    rename(`Map 50k` = map_linked) %>% 
    knitr::kable(caption = 'Planning map, PSCIS details, Fish Habitat Model outputs and prioritization rank/comments for crossings ranked for follow up with habitat confirmation assessments.') %>% 
    kableExtra::column_spec(column = 9, width_min = '2in') %>% 
    kableExtra::kable_styling(c("condensed"), full_width = T) %>% 
    kableExtra::row_spec(0 ,  bold = F, extra_css = 'vertical-align: middle !important;')
}

table_planning_flextable <- function(df = table_planning, site = my_site){
  df %>% 
    filter(Site == my_site) %>% 
    select(-Stream, -stream_word, -map_linked,  -Road) %>% 
    my_flextable(fontsize = 8) %>% 
    flextable::width(j = c(1,2,10), width = 0.55) %>%
    flextable::width(j = c(6,8), width = 0.69) %>% 
    flextable::set_caption('Historic PSCIS details, Fish Habitat Model outputs and prioritization rank/comments related to crossings ranked for follow up with habitat confirmation assessments.')
}



##--------------------------make the overview table-------------------------
##should break the get_fish_habitat-info out of this function as it is used elsewhere.
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


##get the planning data
get_planning_data <- function(){
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv,
                    dbname = 'postgis',
                    host = 'localhost',
                    port = '5432',
                    user = 'postgres',
                    password = 'postgres')
query <- "SELECT mw.*, p.stream_crossing_id as psc_stream_crossing_id, p.utm_zone, p.utm_easting, p.utm_northing FROM working.my_pscis_20190709 mw LEFT OUTER JOIN whse_fish.pscis_assessment_svw p on p.stream_crossing_id = mw.stream_crossing_id;"
planning_data <- sf::st_read(conn, query = query) %>% 
  filter(watershed_group_code == 'PARS',
         !is.na(my_priority))
dbDisconnect(conn = conn)
return(planning_data)
}

##--------------------------------table overview
make_table_overview_report <- function(table_overview_raw){
  table_overview_raw %>% 
  filter(location == 'Upstream' ) %>% 
  mutate(upstr_species = as.character(upstr_species),
         upstr_species = case_when(site %like% '125000' ~ 'RB, CC', ##add species - could be scripted I guess
                                   site %like% '57690' ~ 'RB',
                                   site %like% 'CV1' ~ 'RB, (BT)',
                                   site %like% '125345' ~ '(RB), CC',
                                   TRUE ~ upstr_species),
         `UTM (10N)` = paste0(easting, " ", northing),
         # road_tenure = str_to_title(road_tenure),
         road_tenure = str_replace_all(road_tenure, 'DMPG', 'FLNRORD'),
         upstr_species = str_replace_all(upstr_species, ',', ', ')) %>% 
  arrange(site_int) %>% 
  mutate_all(~replace_na(.,"-")) %>% 
  select(Site = site, Stream = stream_name, `Road` = road_name, Tenure = road_tenure, `UTM (10N)`, 
         `Fish Species` = upstr_species, `Habitat Gain (km)` = uphab_gross_sub22, `Habitat Value` = hab_value, 
         Priority = priority, Comments = comments, -site_int, -location, -'easting', -'northing')
}


#----------------------table culverts
make_table_culvert <- function(PSCIS_submission){
  PSCIS_submission %>%
  select(Site = pscis_crossing_id, 'Diameter (m)' = diameter_or_span_meters, 'Length (m)' = length_or_width_meters,
         Embedded = average_depth_embededdment_meters, Backwatered = percentage_backwatered,
         'Fill Depth (m)' = fill_depth_meters, 'Outlet Drop (m)' = outlet_drop_meters,
         'Outlet Pool Depth (m)' = outlet_pool_depth_0_01m, 'Stream Width Ratio' = stream_width_ratio,
         'Score' = final_score, 'Barrier Result' = barrier_result) %>%
  mutate(site_sort = as.numeric(Site)) %>%
  mutate_at(vars(`Diameter (m)`:`Stream Width Ratio`), as.numeric) %>%
  mutate_at(vars(`Diameter (m)`:`Stream Width Ratio`), round, 1) %>%
  mutate_at(vars(`Diameter (m)`:`Stream Width Ratio`), as.character) %>%
  dplyr::arrange(site_sort) %>%  
  mutate_all(~replace_na(.,"no")) %>% 
  select(-site_sort)
}

make_fish_sampling_data <- function(fish_data_submission, site_location_data){
  a <- dplyr::left_join(fish_data_submission %>% 
                          purrr::pluck("step_2_fish_coll_data"),
                        dplyr::select(site_location_data,
                                      alias_local_name, utm_zone, utm_easting, utm_northing),
                        by = c('local_name' = 'alias_local_name')
  )
  b <- left_join(a,
                 fish_data_submission %>% 
                   purrr::pluck('species_by_group') %>% 
                   select(common_name, species_code),
                 by = c('species' = 'common_name'))
  return(b)
}



##--------------------------get fish hab info------------------------
##might be safer to just grab entire watershed, save as csv then go from there
get_fish_habitat_info <- function(priorities_spreadsheet){
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
                                Select fh.pscis_model_combined_id, fh.pscis_stream_crossing_id, fh.model_crossing_id, fh.blue_line_key, 
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
  fish_habitat_info <- DBI::dbFetch(query) %>% 
    mutate(stream_crossing_id_either = case_when(!is.na(pscis_stream_crossing_id) ~ pscis_stream_crossing_id, 
                            TRUE ~ model_crossing_id),
           downstream_route_measure_int = as.integer(downstream_route_measure))
  dbDisconnect(conn = conn)
  return(fish_habitat_info)
}

##this was removed from the plan as it was a bottleneck
get_watershed <- function(fish_habitat_info){
  mapply(fwapgr::fwa_watershed, blue_line_key = fish_habitat_info$blue_line_key,
         downstream_route_measure = fish_habitat_info$downstream_route_measure_int) %>%
    purrr::set_names(nm = fish_habitat_info$stream_crossing_id_either) 
    # dplyr::bind_rows(.id = 'crossing_id')

}

####--------------------------for appendix memos
get_photo_utm <- function(site = my_site, photo = my_photo){
  photo_metadata %>% 
    filter(crossing_id == site & filename == photo) %>% 
    mutate(utm = paste0('UTM: 10N ', round(easting,0), " ", round(northing,0))) %>% 
    pull(utm)
}

get_img <- function(site = my_site, photo = my_photo){
  jpeg::readJPEG(paste0('data/photos/', site, '/', photo))
}

at_na_remove <- function(x) x[!is.na(x)]

render_appendices <- function(my_site){
  rmarkdown::render(input = paste0('Parsnip_report_', my_site, '.Rmd'),
                    output_file = paste0('docs/03_Parsnip_report_', my_site, '.html'))
}


