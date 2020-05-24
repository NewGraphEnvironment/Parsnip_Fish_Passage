# The workflow plan data frame outlines what you are going to do.
plan1 <- drake_plan(
  fish_data_submission =file_in("./data/parsnip_habitat_assessments.xls") %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map(read_excel, 
        path = "./data/parsnip_habitat_assessments.xls", 
        .name_repair = janitor::make_clean_names) %>% 
    purrr::set_names(janitor::make_clean_names(names(.))) %>% 
    map(altools::at_trim_xlsheet2),  #https://github.com/NewGraphEnvironment/altools
  habitat_data = fish_data_submission %>% 
    purrr::pluck("step_4_stream_site_data"), 
  site_location_data = fish_data_submission %>% 
    purrr::pluck("step_1_ref_and_loc_info") %>% 
    dplyr::filter(!is.na(site_number))%>% 
    mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date))), 
  fish_sampling_data = fish_data_submission %>% 
    purrr::pluck("step_2_fish_coll_data"),
  table = make_table(loc_dat = site_location_data, site_dat = habitat_data),
  priorities_spreadsheet = read_excel(path = "./data/priorities.xlsx"),
  PSCIS_submission = import_pscis(),
  fish_habitat_model_outputs = read.csv('./data/fish_habitat_info.csv'),
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
  ##now make the report
    report = rmarkdown::render(
    knitr_in("Parsnip_report.Rmd"),
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


