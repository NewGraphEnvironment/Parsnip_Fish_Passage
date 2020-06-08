reports_complete = c('125000', '125179', '125180', '125186', '125231')


##build a function that runs all the completed reports
##this seems to work sometimes and not others - can't figure out why
render_appendices <- function(my_site){
  rmarkdown::render(input = paste0('Parsnip_report_', my_site, '.Rmd'),
                    output_file = paste0('docs/03_Parsnip_report_', my_site, '.html'))
}

reports_complete %>% map(render_appendices)


# 
# rmarkdown::render(input = paste0('Parsnip_report.Rmd'),
#                   output_file = paste0('docs/test.html'))
