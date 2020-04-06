# The workflow plan data frame outlines what you are going to do.
plan <- drake_plan(
  data_raw = paste0(getwd(),  "./data/parsnip_habitat_assessments.xls") %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map(read_excel, 
        path = paste0(getwd(),  "./data/parsnip_habitat_assessments.xls"), 
        .name_repair = janitor::make_clean_names) %>% 
    purrr::set_names(janitor::make_clean_names(names(.))) %>% 
    map(at_trim_xlsheet2),
  site_data = data_raw %>% 
    purrr::pluck("step_4_stream_site_data"), 
  loc_data = data_raw %>% 
    purrr::pluck("step_1_ref_and_loc_info"), 
  fish_data = data_raw %>% 
    purrr::pluck("step_2_fish_coll_data"),
  tracks = st_read("./data/field_cleaned.gpx", layer = "tracks") %>% 
    separate(name, into = c('site', 'direction', 'track_num'), remove = F),
  track_points = read_sf("./data/field_cleaned.gpx", layer = "track_points") %>% 
    separate(name, into = c('site', 'direction', 'track_num'), remove = F),
  report = callr::r(
    function(...) rmarkdown::render(...),
    args = list(
      input = knitr_in("docs/pars_125000.Rmd"),
      output_file = "pars_125000.html"
    )
  )
)
  
