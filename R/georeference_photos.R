##lets bring our photos and get filename on git as well as lat, longs for the ones that have geo info

##need to add the gpslat long columns if they do not exist - hence the tibble::add_column
# https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
{
make_photo_metadata_list <- function(input_file){
  cols <- c(gpslatitude = NA_real_, gpslongitude = NA_real_, imagedescription = NA)
  exifr::read_exif(input_file,recursive=T) %>% 
    mutate(CreateDate = lubridate::as_datetime(CreateDate, tz="America/Vancouver")) %>% ##put them in the same tz
    purrr::set_names(., nm = tolower(names(.))) %>% ##column names lowercase
    tibble::add_column(!!!cols[setdiff(names(cols), names(.))]) %>% 
    mutate(createdate = case_when(directory =='data/photos/57696' ~ createdate + 64*60, ##we hadn't yet set the time right on our camera!!
           TRUE ~ createdate))
}

files <- paste0('data/photos/',list.files('data/photos'))

photo_metadata_list <- files %>% 
  map(make_photo_metadata_list)


#GPSLatitude and GPSLongitude CreateDate

##add lon, lat to our track points
track_points2 <- dplyr::bind_cols(
  drake::readd(track_points),
  drake::readd(track_points) %>%
    st_coordinates() %>%
    as_tibble() %>%
    setNames(c("lon_gps_linked","lat_gps_linked"))
) %>%
  tibble::rownames_to_column() %>% 
  mutate(time = lubridate::as_datetime(time, tz="America/Vancouver")) ##put them in the same tz


##-just pulled this out b/c broke wiht dplyr update
# ##i want an id of the track with the points
# track_id <- drake::readd(tracks_of_points) %>% 
#   bind_rows(.id = 'track_name') %>% 
#   as_tibble() %>% ##cannot do distinct when sf object
#   distinct(track_fid, track_seg_id, track_seg_point_id, .keep_all = T)
# 
# ##add the name of the track
# track_points2 <- left_join(track_points2,
#                            select(track_id,
#                                   track_name, track_fid, track_seg_id, track_seg_point_id),
#                            by = c('track_fid', 'track_seg_id', 'track_seg_point_id'))



#find the track point that is closest in time to the CreateDate stamp of the photo..
#https://stackoverflow.com/questions/21607247/searching-for-nearest-date-in-data-frame
get_closest_line_in_history <- function(x, history){
  time_diffs <- difftime(x, history)
  time_diffs[time_diffs<0] <- NA
  
  res <- which.min(time_diffs)
  if (length(res) != 1){
    return(NA)  ##return a NA as in the post
  }else{
    return(res)
  }
}


###########try as a function, bind and add back
make_photo_metadata <- function(meta){
  indx_closest_point <- sapply(meta$createdate, 
                               get_closest_line_in_history, 
                               track_points2$time) %>% 
    as.character() %>% 
    as_tibble_col(column_name = "gps_idx")
  meta_w_index <- bind_cols(meta,
                            indx_closest_point)
  meta_joined_to_tracks <- left_join(meta_w_index,
            select(track_points2, rowname, time, ele, lon_gps_linked, lat_gps_linked), ##pulled out track name
            by = c("gps_idx" = "rowname")) %>% 
    mutate(url  = paste0('https://raw.githubusercontent.com/NewGraphEnvironment/Parsnip_Fish_Passage/master/', 
                         sourcefile)) %>% 
    select(crossing_id = directory, filename, createdate, gps_extracted_time = time, ele,  ##pulled out track name
           gpslatitude, gpslongitude, lon_gps_linked, lat_gps_linked, imagedescription, model, url) %>% 
    mutate(lat_map = case_when(!is.na(gpslatitude) ~ gpslatitude,  ##use the gps from the camera when possible
                               TRUE ~ lat_gps_linked),
           lon_map = case_when(!is.na(gpslongitude) ~ gpslongitude,
                               TRUE ~ lon_gps_linked),
           imagedescription = as.character(imagedescription)) %>% ##added this since it broke with dplyr update to 2.0
    mutate(crossing_id = basename(crossing_id))
  ##probably makes sense to just give the photos the crossing lat, lons when the photo is named upstream, downstream, inlet, outlet, barrel, aerial
  ##get lat lons for pscis
  pscis2 <- drake::readd(PSCIS_submission) %>% 
    filter(!is.na(easting)) %>% ##remove the ones with no info yet
    sf::st_as_sf(coords = c("easting", "northing"), crs = 26910) %>% 
    st_transform(crs = 4326) %>% 
    mutate(lon_pscis = st_coordinates(.)[,1],
           lat_pscis = st_coordinates(.)[,2]) %>% 
    select(-geometry) 
  
  crossing_photos <- c('downstream', 'upstream', 'inlet', 'outlet', 'outlet2', 
                       'outlet3', 'barrel', 'barrel2', 'road', 'barrel_2', 'outlet_2')
  
  meta_final_list <- left_join(
    meta_joined_to_tracks,
    select(pscis2, 
           crossing_id = pscis_crossing_id, lon_pscis, lat_pscis),
    by = 'crossing_id'
  )  %>% 
    mutate(
      lat_map = case_when((tools::file_path_sans_ext(filename) %in% 
                            crossing_photos & 
                            is.na(gpslatitude)) ~ 
                            lat_pscis,  #gps info trumps all
                          TRUE ~ lat_map),
      lon_map = case_when((tools::file_path_sans_ext(filename) %in% 
                            crossing_photos & 
                             is.na(gpslatitude)) ~ 
                            lon_pscis,
                          TRUE ~ lon_map)) %>% 
    mutate(time_diff = difftime(createdate, gps_extracted_time)) %>% 
    select(crossing_id, filename, time_diff, ##pulled out track_name
           gpslatitude, lat_map, lat_gps_linked, lat_pscis,  gpslongitude, lon_map,
           lon_gps_linked,lon_pscis, everything(), 
           -geometry) %>% ##not sure this is necessary . read_csv doesn't like our file . 
    ######################  SPECIFIC CASES  ############################
    mutate(lat_map = case_when(crossing_id == '125231' & 
                                 filename == 'downstream2.JPG' ~
                                 lat_pscis,
                               TRUE ~ lat_map),
           lon_map = case_when(crossing_id == '125231' & 
                                 filename == 'downstream2.JPG' ~
                                 lon_pscis,
                               TRUE ~ lon_map),
           lat_map = case_when(crossing_id == '125000' & ##not sure why the timestamp is wrong on this one.....
                                 filename == 'us6.JPG' ~
                                 lat_gps_linked,
                               TRUE ~ lat_map),
           lon_map = case_when(crossing_id == '125000' & 
                                 filename == 'us6.JPG' ~
                                 lon_gps_linked,
                               TRUE ~ lon_map),
           lat_map = case_when(crossing_id == '125000' & ##not sure why the timestamp is wrong on this one.....
                                 tools::file_path_sans_ext(filename) == 'aerial' ~
                                 lat_pscis,
                               TRUE ~ lat_map),
           lon_map = case_when(crossing_id == '125000' & 
                                 tools::file_path_sans_ext(filename) == 'aerial' ~
                                 lon_pscis,
                               TRUE ~ lon_map)
           ) %>% 
    sf::st_as_sf(coords = c("lon_map", "lat_map"), crs = 4326, remove = F) %>%
    st_transform(crs = 26910) %>% 
    mutate(easting = st_coordinates(.)[,1],
           northing = st_coordinates(.)[,2]) %>% ##this gives us coordinates for the report captions
    sf::st_set_geometry(NULL)
    }

photo_metadata_processed <- photo_metadata_list %>% 
  map(make_photo_metadata) %>% 
  bind_rows()
}
##write to a csv
write.csv(photo_metadata_processed, file = 'data/photo_metadata.csv', row.names = F) ##moved this up a level





###---------------------------------------this is before we moved it to a function--------------##############
# cols <- c(gpslatitude = NA_real_, gpslongitude = NA_real_, imagedescription = NA)
# 
photo_metadata <- exifr::read_exif("data/photos/57696",recursive=T) %>%
  mutate(CreateDate = lubridate::as_datetime(CreateDate, tz="America/Vancouver")) %>% ##put them in the same tz
  purrr::set_names(., nm = tolower(names(.))) %>%  ##column names lowercase
  tibble::add_column(!!!cols[!names(cols) %in% names(.)])
# 
# 
# # ##so this give us the index from the gps points
# indx_closest_point <- sapply(photo_metadata$createdate,
#                              get_closest_line_in_history,
#                              track_points2$time) %>%
#   as.character() %>%
#   as_tibble_col(column_name = "gps_idx")
# 
# ##so here we join it to our metadata
# photo_metadata <- bind_cols(photo_metadata,
#                             indx_closest_point)
# 
# ##then we join it to the lat long info we need
# photo_metadata2 <- left_join(photo_metadata,
#                              select(track_points2,  rowname, time, ele, lon_gps_linked, lat_gps_linked), #track_name,
#                              by = c("gps_idx" = "rowname")) %>%
#   mutate(url  = paste0('https://raw.githubusercontent.com/NewGraphEnvironment/Parsnip_Fish_Passage/master/',
#                        sourcefile)) %>%
#   select(crossing_id = directory, filename, createdate, gps_extracted_time = time,  ele,
#          gpslatitude, gpslongitude, lon_gps_linked, lat_gps_linked, imagedescription, model, url)  %>% #track_name
#   mutate(lat_map = case_when(!is.na(gpslatitude) ~ gpslatitude,  ##use the gps from the camera when possible
#                              TRUE ~ lat_gps_linked),
#          lon_map = case_when(!is.na(gpslongitude) ~ gpslongitude,
#                              TRUE ~ lon_gps_linked)) %>%
#   mutate(crossing_id = basename(crossing_id))
# 
# # ##probably makes sense to just give the photos the crossing lat, lons when the photo is named upstream, downstream, inlet, outlet, barrel, aerial
# ##get lat lons for pscis
# pscis2 <- drake::readd(PSCIS_submission) %>%
#   filter(!is.na(easting)) %>% ##remove the ones with no info yet
#   sf::st_as_sf(coords = c("easting", "northing"), crs = 26910) %>%
#   st_transform(crs = 4326) %>%
#   mutate(lon_pscis = st_coordinates(.)[,1],
#          lat_pscis = st_coordinates(.)[,2]) %>%
#   select(-geometry)
# 
# 
# # ##add the pscis coordinates and point to when the shoe fits
# #
# crossing_photos <- c('downstream', 'upstream', 'inlet', 'outlet', 'barrel', 'road')
# 
# photo_metadata3 <- left_join(
#   photo_metadata2,
#   select(pscis2,
#          crossing_id = pscis_crossing_id, lon_pscis, lat_pscis),
#   by = 'crossing_id'
# )  %>%
#   mutate(
#     lat_map = case_when(tools::file_path_sans_ext(filename) %in%
#                           crossing_photos ~ lat_pscis,
#                         TRUE ~ lat_map),
#     lon_map = case_when(tools::file_path_sans_ext(filename) %in%
#                           crossing_photos ~ lon_pscis,
#                         TRUE ~ lon_map)) %>%
#   tibble::rownames_to_column() %>%
#   select(-geometry) ##not sure this is necessary . read_csv doesn't like our file .
