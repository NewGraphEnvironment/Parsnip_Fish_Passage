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
    select(alias_local_name, 'Zone' = utm_zone,
           'Easting' = utm_easting, 'Northing' = utm_northing),
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
  tidyr::separate(alias_local_name, into = c('Site', 'Location'), remove = F)
}


