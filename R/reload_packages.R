##this is how we will rebuild our packages looking for the bug that makes us keep resetting Rstudio so that drake will make our project
library(tidyverse)


update.packages(checkBuilt=TRUE)

packages_to_reinstall <- as.data.frame(installed.packages(lib.loc = "C:/Users/allan/OneDrive/Documents/R/win-library/3.6")) %>% 
  filter(Built == '4.0.0') %>% 
  pull(Package) %>% 
  as.vector()
