##this is one of the files that is not run from scratch because it requires the hydat database to be downloaded to your computer.  Moved here from the rmd file


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
hydrograph <- fasstr::plot_daily_stats(station_number = "07EE007",
                               start_year = 0,
                               end_year = 9999,
                               log_discharge = TRUE,
                               ignore_missing = TRUE)
hydrograph

hydrograph_print <- hydrograph[["Daily_Statistics"]]

test

##For some reason we need to plot the $daily_statistics column for this to work
ggsave(plot = hydrograph_print, file="./fig/hydrology1.png",
       h=3.4, w=5.11, units="in", dpi=300)


# fig/hydrology1-1.png

##get the figure caption ready for the plot in the report

tidyhat_info <- search_stn_number("07EE007")
hydatr_info <- as.data.frame(hydat_station_info(tidyhat_info$STATION_NUMBER))
hydatr_info <- mutate(hydatr_info, title = paste0(stringr::str_to_title(STATION_NAME),
                                                  " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6)," Lon ",round(LONGITUDE,6), "). Available daily discharge data from ", 
                                                  FIRST_YEAR, " to ",LAST_YEAR, "."))
hydatr_info$title

##fasstr::plot_data_screening2 is a custom version of plot_data_screening - modified a fork of fasstr to produce mean lines so specific to this version of the repository.
summary_plot <- fasstr::plot_data_screening3(station_number = "07EE007")[["Data_Screening"]]
summary_plot

ggsave(plot = summary_plot, file="./fig/hydrology2.png",
       h=3.4, w=5.11, units="in", dpi=300)


