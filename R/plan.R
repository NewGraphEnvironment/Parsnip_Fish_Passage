# The workflow plan data frame outlines what you are going to do.
plan1 <- drake_plan(
  fish_data_submission = drake::file_in("./data/parsnip_habitat_assessments.xls") %>% 
    readxl::excel_sheets() %>% 
    purrr::set_names() %>% 
    purrr::map(read_excel, 
        path = "./data/parsnip_habitat_assessments.xls", 
        .name_repair = janitor::make_clean_names) %>% 
    purrr::set_names(janitor::make_clean_names(names(.))) %>% 
    purrr::map(at_trim_xlsheet2),  #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
  habitat_data = fish_data_submission %>% 
    purrr::pluck("step_4_stream_site_data") %>% 
    tidyr::separate(local_name, into = c('site', 'location'), remove = F), 
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
  tracks = st_read(file_in("./data/field_cleaned.gpx"), layer = "tracks") %>% ##we need this to make track of points
    separate(name, into = c('site', 'direction', 'track_num'), remove = F),
  track_points = read_sf(file_in("./data/field_cleaned.gpx"), layer = "track_points") %>% 
    separate(name, into = c('site', 'direction', 'track_num'), remove = F),
  track_point_idx_list = sf::st_intersects(tracks, track_points) %>% 
    purrr::set_names(., nm = pull(tracks, name)),
  tracks_of_points = lapply(track_point_idx_list, 
                            make_tracks_of_points, track_points = track_points),
  my_tracks = tracks_of_points %>% 
    map(points2line_trajectory),  ##convert our points to lines
  photo_metadata = readr::read_csv(file = 'data/photo_metadata.csv'),
  forest_tenure_road_lines = st_read('data/parsnip.gpkg', layer = 'rds_ften_priority'),
  fish_habitat_model_lines = sf::st_read('data/fish_habitat.geojson', layer = 'fish_habitat'),
  table_planning = make_table_planning(),
  watersheds = get_watershed(priorities_spreadsheet = priorities_spreadsheet, PSCIS_submission = PSCIS_submission),
  hydrograph = plot_daily_stats(station_number = "07EE007",
                                 start_year = 0,
                                 end_year = 9999,
                                 log_discharge = TRUE,
                                 ignore_missing = TRUE),
  ##now make the report
    report = rmarkdown::render(
    knitr_in("Parsnip_report.Rmd"), ##JUST CHANGED FOR A SEC
    output_file = file_out("./docs/index.html"),
    quiet = TRUE
  ),
  report2 = rmarkdown::render(
    knitr_in("Parsnip_report_intro_methods.Rmd"),
    output_file = file_out("./docs/Parsnip_report_intro_methods.html"),
    quiet = TRUE
  ),
  report3 = rmarkdown::render(
    knitr_in("Parsnip_report_planning_summary.Rmd"),
    output_file = file_out("./docs/Parsnip_report_planning_summary.html"),
    quiet = TRUE
  )
)


