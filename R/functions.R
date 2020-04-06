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