##lets bring our photos and get filename on git as well as lat, longs for the ones that have geo info





photo_metadata <- exifr::read_exif("data/photos/125000",recursive=T) %>% 
  mutate(CreateDate = lubridate::as_datetime(CreateDate, tz="America/Vancouver")) %>% ##put them in the same tz
  purrr::set_names(., nm = tolower(names(.))) ##column names lowercase

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



##i want an id of the track with the points
track_id <- drake::readd(tracks_of_points) %>% 
  bind_rows(.id = 'track_name') %>% 
  as_tibble() %>% ##cannot do distinct when sf object
  distinct(track_fid, track_seg_id, track_seg_point_id, .keep_all = T)

##add the name of the track
track_points2 <- left_join(track_points2,
                           select(track_id,
                                  track_name, track_fid, track_seg_id, track_seg_point_id),
                           by = c('track_fid', 'track_seg_id', 'track_seg_point_id'))



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
##something is happening with UTC converson here


##so this give us the index from the gps points
indx_closest_point <- sapply(photo_metadata$createdate, 
                             get_closest_line_in_history, 
                             track_points2$time) %>% 
  as.character() %>% 
  as_tibble_col(column_name = "gps_idx")

##so here we join it to our metadata
photo_metadata <- bind_cols(photo_metadata,
                             indx_closest_point)

##then we join it to the lat long info we need
photo_metadata2 <- left_join(photo_metadata,
                             select(track_points2, track_name, rowname, time, ele, lon_gps_linked, lat_gps_linked),
                             by = c("gps_idx" = "rowname")) %>% 
  mutate(url  = paste0('https://github.com/NewGraphEnvironment/Parsnip_Fish_Passage/tree/master/', 
                       sourcefile)) %>% 
  select(crossing_id = directory, filename, createdate, gps_extracted_time = time, track_name, ele,
         gpslatitude, gpslongitude, lon_gps_linked, lat_gps_linked, imagedescription, model, url) %>% 
  mutate(lat_map = case_when(!is.na(gpslatitude) ~ gpslatitude,  ##use the gps from the camera when possible
                                 TRUE ~ lat_gps_linked),
         lon_map = case_when(!is.na(gpslongitude) ~ gpslongitude,
                                  TRUE ~ lon_gps_linked)) %>% 
  mutate(crossing_id = basename(crossing_id))

##write to a csv
write.csv(photo_metadata2, file = 'data/photos/photo_metadata.csv')

