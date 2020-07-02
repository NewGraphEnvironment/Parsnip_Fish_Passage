##we run this everytime we add a crossing appendices

##1 add the photo and run eoreference_photos.R
##2 commit photos to the repo
##then run the commands below and see if the photos are in the right spots on the map in the doc/index file


##add the number of the crossing to he make_overview_table_html function in the R/functions.R file then: (this should be automated)

clean(c('table_overview_raw','photo_metadata', 'table_overview_report', 'report_main')) ##not sure why drake does not know that report main needs a rerun...
source('make.R')      


## also run the render manually from the rmd after loading all chunks above.  
## this saves running a full plan with all the appendices getting made which takes a while
##when we want to render all the appendices we need to make all our targets first and then add the appendices to the plan
