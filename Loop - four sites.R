install.packages("dataRetrieval")

library(ggplot2)
library(tidyverse)
library(dataRetrieval)
library(lubridate)


1011000
1013500
1017000
1019000

sites_data_list <- list() #create an empty list to add the site data into
site_nums <- list("1011000", "1013500", "1017000", "1019000") # list of site numbers

for (i in 1:length(site_nums)){
  parameterCd <- "00060" # dicharge parameter
  startDate <- "1970-10-01"  
  endDate <- "2020-09-30"
  site_name <- site_nums[[i]] # get site i and get the site data
  site_data <- readNWISdv(site_name, parameterCd,startDate, endDate)# not working
  sites_data_list <- append(sites_data_list, site_data) # append the site data to the site data list
}



