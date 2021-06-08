install.packages("dataRetrieval")

library(ggplot2)
library(tidyverse)
library(dataRetrieval)
library(lubridate)


01011000
01013500
01017000
01019000


sites_data_list <- list() #create an empty list to add the site data into
site_nums <- c("01011000", "01013500", "01017000", "01019000") # list of site numbers

for (i in 1:length(site_nums)){
  startDate <- "1970-10-01"  
  endDate <- "2020-09-30"
  parameterCd <- "00060" # dicharge parameter
  site_name <- site_nums[i] # get site i and get the site data
  site_data <- readNWISdv(site_name, parameterCd,startDate, endDate) # get the site data
  sites_data_list[[i]] <- site_data # append the site data to the site data list
}



