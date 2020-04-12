# The workflow plan data frame outlines what you are going to do.
plan1 <- drake_plan(
  data_raw =file_in("./data/parsnip_habitat_assessments.xls") %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map(read_excel, 
        path = "./data/parsnip_habitat_assessments.xls", 
        .name_repair = janitor::make_clean_names) %>% 
    purrr::set_names(janitor::make_clean_names(names(.))) %>% #https://github.com/NewGraphEnvironment/altools
    map(altools::at_trim_xlsheet2),
  site_data = data_raw %>% 
    purrr::pluck("step_4_stream_site_data"), 
  loc_data = data_raw %>% 
    purrr::pluck("step_1_ref_and_loc_info") %>% 
    dplyr::filter(!is.na(site_number)), 
  fish_data = data_raw %>% 
    purrr::pluck("step_2_fish_coll_data"),
  tracks = st_read(file_in("./data/field_cleaned.gpx"), layer = "tracks") %>% 
    separate(name, into = c('site', 'direction', 'track_num'), remove = F),
  track_points = read_sf(file_in("./data/field_cleaned.gpx"), layer = "track_points") %>% 
    separate(name, into = c('site', 'direction', 'track_num'), remove = F),
  table = make_table(loc_dat = loc_data, site_dat = site_data),
  report = rmarkdown::render(
    knitr_in("pars_125000.Rmd"),
    output_file = file_out("./docs/pars_125000.html"),
    quiet = TRUE
  )
)

plan2 <- drake_plan(
  track_points2 = read_sf(file_in("./data/field_cleaned.gpx"), layer = "track_points") %>% ##this is just a test
  separate(name, into = c('site', 'direction', 'track_num'), remove = F)
)


