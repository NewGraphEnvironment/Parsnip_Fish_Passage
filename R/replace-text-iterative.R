library(xfun)
library(tidyverse)


##we know this works
# xfun::gsub_file("temp/Parsnip_report_057690.Rmd", 'setup', paste0('setup', '-', '057690'))


##so we build a function
replace_iterative <- function(file, file_id, replace_this){
  xfun::gsub_file(file, replace_this, paste0(replace_this, '-', file_id))
 }


##list of chunk/references to be replaced
replace_this_list <- c('setup', 'load-data', 'tableoverview', 'table-planning',
                       'tableculvert', 'tablehabitat', 'view-photo-names', 'photo-culvert',
                       'photo-extra1', 'photo-downstream1', 'photo-downstream2', 'photo-downstream3', 'photo-upstream1',
                       'photo-upstream2', 'photo-upstream3')

##get your file names
files <- list.files('temp/', full.names = T)

##get your file id's
file_ids = list.files(path = 'temp/', pattern = "[.]Rmd$") %>% ##automated grab of the appendices
  str_replace_all('Parsnip_report_','') %>% 
  str_replace_all('.Rmd','') %>%
  data.frame(files_df = .) %>% 
  filter(files_df != 'Parsnip_report' &  files_df != 'planning_summary' & files_df != 'test') %>% 
  pull(files_df)


##this works for 1 word
# mapply(replace_iterative, file = files, file_id = file_ids, replace_this = 'load-data')

##so build a function to do all the words with an lapply
replace_iterative_multi <- function(words_to_replace){
  mapply(replace_iterative, file = files, file_id = file_ids, replace_this = words_to_replace)  
}


##now loop through each of the words for each of the file/file_id combos
lapply(replace_this_list, replace_iterative_multi)


