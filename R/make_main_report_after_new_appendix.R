##we run this everytime we add a crossing appendices
##add the number of the crossing to the plan and the make_overview_table_html function then:

clean(c('table_overview_raw','photo_metadata', 'table_overview_report', 'report_main')) ##not sure why drake does not know that report main needs a rerun...
source('make.R')      


## also run the render manually from the rmd after loading all chunks above.  
## this saves running a full plan with all the appendices getting made which takes a while