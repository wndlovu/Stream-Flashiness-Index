install.packages("dataRetrieval")

library(ggplot2)
library(tidyverse)
library(dataRetrieval)
library(lubridate)
library (plyr)
library(purrr)

site_nums <- read_csv("nesites.csv") %>% # read sites list into R
  mutate(STAID = as.character(paste0("0", STAID))) # add a 0 to start of all sites

site_nums <- as.character(site_nums$STAID) # re-save data as array


# create a dataframe with the area for each site
drainageArea_df <- data.frame(readNWISsite(site_nums)) %>% 
  select(site_no, drain_area_va) # select the site_no and drain area


sites_data_list <- list() #create an empty list to add the site data into
for (i in 1:length(site_nums)){
  startDate <- "1970-10-01"  
  endDate <- "2020-09-30"
  parameterCd <- "00060" # dicharge parameter
  site_name <- site_nums[i] # get site i and get the site data
  site_data <- readNWISdv(site_name, parameterCd,startDate, endDate) %>% # get the site data
    left_join(drainageArea_df) %>% # add the drainage area variable
    mutate(miles3_s = X_00060_00003/147197940448.88, # cfs to mi3/s
           miles3_day = miles3_s*86400,              # mi3/s to mi3/day
           miles_day = miles3_day/drain_area_va, #divide by area to get mi/day
           mm_day = miles_day*(1609.344*1000))
  sites_data_list[[i]] <- site_data # append the site data to the site data list
}


# add the waterYear variable
for (site in 1:length(sites_data_list)){ # for every site in the site list
  wydates = sites_data_list[[site]]$Date # create a new variable called wydates
  # Shift dates forward 92 days, such that 10-01 becomes 01-01 (this gives you the correct WY)
  wydates = wydates + 92
  # Extract WY for each date
  sites_data_list[[site]]$waterYear = as.numeric(format(wydates, "%Y")) # 
  sites_data_list[[site]] <- sites_data_list[[site]]# append to the site list
}



qdiff <- 0    # initialise discharge difference between 2 days
year_num <- 0 # initialise year counter
rbi_values <- 0 # unitialise calculated rbi


# Create RBI function
Q1 = drainageArea_df$mm_day
rbi <- function(Q){
  qdiff <- 0 # initialise discharge difference between 2 days
  for (i in 2:length(Q)){ # offset discharge values by 1
    qdiff[i] <- Q[i] - Q[i-1] # take the difference between qi and qi-1
  }
  rbi_values <- (sum(abs(qdiff))/sum(Q)) # and calculate rbi for each year number  
  return(rbi_values)
}
s <- rbi(Q=Q1)


rbi_wy_list = list() # create empty list for rbi and wy values
for (t in 1:length(sites_data_list)){ # for every site in the sites data list
  rbi_wy <- data.frame(matrix(ncol = 2, nrow = length(unique(sites_data_list[[t]]$waterYear)))) # create data frame with 2 columns
  rbi_wy$X1 =  unique(sites_data_list[[t]]$waterYear)# make unique waterYear the first variable
  colnames(rbi_wy)[1] <- "waterYear" # rename 1st variable to waterYear
  #colnames(rbi_wy)[2] <-  as.character(paste0("rbiValue_", sites_data_list[[t]]$site_no[1])) # rename 2nd variable to rbiValue and add site_no
  colnames(rbi_wy)[2] <-  as.character(paste0(sites_data_list[[t]]$site_no[1])) # rename 2nd variable to rbiValue and add site_no
  rbi_wy_list[[t]] <- rbi_wy # add the rbi_wy dataframes for each year to the rbi_wy_list 
  
  rbi_values_df <- data.frame() # create an empty df for rbi values
  # loop that calculates rbi for all the waterYears
  for (s in unique(sites_data_list[[t]]$waterYear)){ # for every site get all the waterYears 
    year2 <- filter(sites_data_list[[t]], waterYear == s)
    rbi_val <- rbi(Q = year2$mm_day) # call the function and calculate rbi
    calc_rbi <- data.frame(rbi_val) # add each calculated rbi to calc_rbi dataframe
    rbi_values_df <- rbind(rbi_values_df,calc_rbi) # append the calc_rbi to rbi_values df
    #rbi_wy_list[[t]] <- rbi_values_df
  }
  rbi_wy[,2] <- rbi_values_df$rbi_val # add rbi values to the rbiValue in rbi_wy 
  rbi_wy_list[[t]] <- rbi_wy # add the new rbi_wy dataframes with rbi and WY to the rbi_wy_list 
}


rbiWy_dfAll <- do.call("cbind", rbi_wy_list) # create one dataframe from the rbi_wy_list 
rbiWy_dfAll <- rbiWy_dfAll[!duplicated(as.list(rbiWy_dfAll))] # remove the duplicated waterYears


# rbiWy_dfAll file is downloaded and saved as a .csv file
