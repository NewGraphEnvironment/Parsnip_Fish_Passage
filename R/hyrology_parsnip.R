
library(tidyhydat)
library(fasstr)
library(hydatr)
##download_hydat()
hy_dir() #this is where it put the database


# setwd("C://Users//allan//AppData//Local//tidyhydat//tidyhydat")
#hydat_download()
#hydat_extract() # extracts the downloaded database into readable form
hydatr::hydat_load(source = "C://Users//allan//AppData//Local//tidyhydat//tidyhydat") # loads the database (you'll need to call this one each time you load the package)
# ls("package:hydatr")
# ls("package:tidyhydat")


######had issues downloading hydat file with tidyhydat so used hydatr then
##needed to move the file then rename it to Hydat.sqlite3
## identify the folders
#current.folder <- "C:/hydat"
#new.folder <- "C://Users//Al//AppData//Local//tidyhydat//tidyhydat"


##hmm. cant figure this out...
#new.folder <- "C:/hydat"
#current.folder <- "C://Users//Al//AppData//Local//tidyhydat//tidyhydat"

## find the files that you want
list.of.files <- list.files(current.folder)

# #copy the files to the new folder- had to manually change name of file to .sqlite3 
##should automate
file.copy(list.of.files, new.folder)

##find the source code for a function
# getAnywhere("is_hydat")

#use hydatr to find stations with recent data using place name and google search
# hydat_find_stations("Maple Ridge, bc", year = 1999:2014)

##use hydatr to find stations using lat/long ****added this lately
# hydat_find_stations(loc, year = 1999:2015)
# loc <- matrix(c(-127,54.8), ncol = 2)
# colnames(loc) <- c("lat", "lon")
# loc <- as.data.frame(loc)
# loc <- as.vector(loc[1,1:2])
# loc<- c(-95.3632715, 29.7632836)

##convert lat long 4326 to wkid 3857 ****added this lately
# library(sp)
# library(rgdal)
# coordinates(loc)=~lon+lat
#proj4string(loc)=CRS("+init=epsg:4326")
#spTransform(loc,CRS("+init=epsg:3857")) #don't need this
#loc_df<- as.data.frame(loc)



#Get detailed information about one hydro site with hydatr:
(hydat_station_info("07EE007"))


# #get daily flows with tidyhydat
# daily_flows <- tidyhydat::hy_daily_flows(station_number = "07EE007")


##get info using station name with tidyhydat
##lacks the watershed size and dates info you get from Hydatr so pass it to Hydatr!!
tidyhat_info <- search_stn_name("Parsnip") 
hydatr_info <- as.data.frame(hydat_station_info(tidyhat_info$STATION_NUMBER))



#bulkley river at smithers 08EE005# compared to station at quick (08EE004) and is very similiar
#compared to station near Houston (just upstream of Morice 08EE003) and is much less water
hydrograph <- plot_daily_stats(station_number = "07EE007",
                               start_year = 0,
                               end_year = 9999,
                               log_discharge = TRUE,
                               ignore_missing = TRUE)
hydrograph



##get the figure caption ready for the plot in the report

tidyhat_info <- search_stn_number("07EE007")
hydatr_info <- as.data.frame(hydat_station_info(tidyhat_info$STATION_NUMBER))
hydatr_info <- mutate(hydatr_info, title = paste0(tolower(STATION_NAME),
                                                  " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6)," Lon ",round(LONGITUDE,6), "). Available daily discharge data from ", 
                                                  FIRST_YEAR, " to ",LAST_YEAR, " plotted in R with fasstr (Goetz and Schwarz 2020)."))
hydatr_info$title

citation("tidyhydat")
citation("fasstr")
