# The workflow plan data frame outlines what you are going to do.
plan1 <- drake_plan(
  fish_data_submission = drake::file_in("./data/parsnip_habitat_assessments.xls") %>% 
    readxl::excel_sheets() %>% 
    purrr::set_names() %>% 
    purrr::map(read_excel, 
        path = "./data/parsnip_habitat_assessments.xls", 
        .name_repair = janitor::make_clean_names) %>% 
    purrr::set_names(janitor::make_clean_names(names(.))) %>% 
    purrr::map(at_trim_xlsheet2) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
    purrr::map(plyr::colwise(type.convert)),  #change 1
  habitat_data = fish_data_submission %>% 
    purrr::pluck("step_4_stream_site_data") %>% 
    tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>% 
    mutate(average_gradient_percent = round(average_gradient_percent * 100, 1)) %>% 
    mutate_if(is.numeric, round, 1), ##change 2
  site_location_data = fish_data_submission %>% 
    purrr::pluck("step_1_ref_and_loc_info") %>% 
    dplyr::filter(!is.na(site_number))%>% 
    mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date))), 
  fish_sampling_data = make_fish_sampling_data(fish_data_submission = fish_data_submission, site_location_data = site_location_data),
  table_habitat_raw = make_table_habitat(loc_dat = site_location_data, site_dat = habitat_data),
  table_habitat_report = make_table_habitat_report(table_habitat_raw = table_habitat_raw, 
                                                   PSCIS_submission = PSCIS_submission, 
                                                   priorities_spreadsheet = priorities_spreadsheet),
  table_overview_raw = make_table_overview(
    priorities_spreadsheet = priorities_spreadsheet,
    PSCIS_submission = PSCIS_submission),
  table_overview_report = make_table_overview_report(table_overview_raw = table_overview_raw),
  table_culvert = make_table_culvert(PSCIS_submission = PSCIS_submission),
  priorities_spreadsheet = readxl::read_excel(path = "./data/priorities.xlsx") %>% 
    tidyr::separate(site_id, into = c('site', 'location'), remove = F),
  PSCIS_submission = import_pscis(),
  # fish_habitat_model_outputs = read.csv('./data/fish_habitat_info.csv'),
  #this section for locational info
  planning_data = get_planning_data(),
  fish_habitat_model_outputs = get_fish_habitat_info(priorities_spreadsheet = priorities_spreadsheet),
  tracks = read_sf(file_in("./data/field_cleaned.gpx"), layer = "tracks") %>% ##we need this to make track of points
    separate(name, into = c('site', 'direction', 'track_num'), remove = F),
  track_points = read_sf(file_in("./data/field_cleaned.gpx"), layer = "track_points") %>% 
    separate(name, into = c('site', 'direction', 'track_num'), remove = F),
  # track_point_idx_list = sf::st_intersects(tracks, track_points) %>%
  #   purrr::set_names(., nm = pull(tracks, name)),
  # tracks_of_points = lapply(track_point_idx_list,
  #                           make_tracks_of_points, track_points = track_points),
  # my_tracks = tracks_of_points %>%
  #   map(points2line_trajectory), ##convert our points to lines,
    # data.table::rbindlist(idcol="site"),  
  photo_metadata = readr::read_csv(file = 'data/photo_metadata.csv'),
  forest_tenure_road_lines = st_read('data/parsnip.gpkg', layer = 'rds_ften_priority'),
  fish_habitat_model_lines = sf::st_read('data/fish_habitat.geojson', layer = 'fish_habitat'),
  table_planning = make_table_planning(planning_data = planning_data),
  crossing_watersheds = sf::st_read("data/parsnip.gpkg", layer = "watersheds"),
  report_appendices_rmd_files = list.files(pattern = "[.]Rmd$") %>% ##automated grab of the appendices
    str_replace_all('Parsnip_report_','') %>% 
    str_replace_all('.Rmd','') %>%
    data.frame(files_df = .) %>% 
    filter(files_df != 'Parsnip_report' &  files_df != 'planning_summary' & files_df != 'test') %>% 
    pull(files_df),
  # report_appendices_rmd_files = c(
  #   '057681',
  #   '057690',
  #   '125000', 
  #   '125179', 
  #   '125180', 
  #   '125186', 
  #   '125231', 
  #   '125247',
  #   '125253',
  #   '125345',
  #   'CV1',
  #   '057695',
  #   '057696',
  #   '125098'),
  ##REMOVED THE HYDROGRAPHs from the plan and intro_methods file to allow report production using png outputs only
  ##now make the report
    report_main = rmarkdown::render(
    knitr_in("Parsnip_report.Rmd"), ##JUST CHANGED FOR A SEC
    output_file = file_out("./docs/index.html"),
    quiet = TRUE
  ),
  # report_appendices = report_appendices_rmd_files %>% map(render_separately_all), ##turned this off for speed
  planning_summary_table = rmarkdown::render(
    knitr_in("Parsnip_report_planning_summary.Rmd"),
    output_file = file_out("./docs/Parsnip_report_planning_summary.html"),
    quiet = TRUE
    )
)

##old news
#   report2 = rmarkdown::render(
#     knitr_in("Parsnip_report_intro_methods.Rmd"),
#     output_file = file_out("./docs/Parsnip_report_intro_methods.html"),
#     quiet = TRUE
#   ),