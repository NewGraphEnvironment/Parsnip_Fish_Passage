##we use this file to build appendices - I thin

##we will need a seperate list and function for those appendices that need a 0 before them

##there are issues with rmarkdown::render and default environments so to run all the reports we need this weird system
##https://stackoverflow.com/questions/32257970/knitr-inherits-variables-from-a-users-environment-even-with-envir-new-env
render_separately <- function(...) callr::r(
  function(...) rmarkdown::render(..., envir = globalenv()), args = list(...), show = TRUE)

# render_separately(input = paste0('Parsnip_report_', my_site, '.Rmd'), 
#                   output_file = paste0('docs/03_Parsnip_report_', my_site, '.html'), quiet = TRUE)

render_separately_all <- function(site){
  render_separately(input = paste0('Parsnip_report_', site, '.Rmd'), 
                    output_file = paste0('docs/03_Parsnip_report_', site, '.html'), quiet = TRUE, clean = T)
}


##we could build a list of sites from the names of the files I guess if we wanted to automate.  prob should
reports_complete = c('057681', 
                     '125000', 
                     '125179', 
                     '125180', 
                     '125186', 
                     '125231', 
                     '125247',
                     '125253',
                     '125345',
                     'CV1')


##knit the appendices
reports_complete %>% map(render_separately_all)











##https://stackoverflow.com/questions/32257970/knitr-inherits-variables-from-a-users-environment-even-with-envir-new-env
##https://github.com/rstudio/rmarkdown/issues/1204
# callr::r(function() rmarkdown::render("Parsnip_report_125186.Rmd"))



