
install.packages("dataRetrieval")

library(ggplot2)
library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(data.table)

# download discharge data for site 1 and 2 USGS Gage #
siteNumber1 <- "01011000" # Pemigewasset R - Hubbard Brook
siteNumber2 <- "01076500" # Pemigewasset R - Hubbard Brook

parameterCd <- "00060"  # Discharge in cfs
startDate <- "1970-10-01"  
endDate <- "2020-09-30"
#endDate <- ymd(startDate) + years(50) # download data for 50 more yrs


# read discharge data and save
q2010_s1 <- readNWISdv(siteNumber1,parameterCd,startDate, endDate)
q2010_s2 <- readNWISdv(siteNumber2,parameterCd,startDate, endDate)

# create a list of the discharge data
list_data = list(q2010_s1,q2010_s2)
site <- list_data[[1]]

# read site decription and save (will use to get the discharge area)
drainageArea1 <- readNWISsite(siteNumber1)
drainageArea2 <- readNWISsite(siteNumber2) 

# create list of the site description
list_drainage = list(drainageArea1, drainageArea2) 

# get the drainage area for site 1 in the list
drainageArea <- list_drainage[[1]] %>% 
  filter(site_no %in% site$site_no) %>%  
  select(site_no, drain_area_va) 


# To do this, you're going to need to use a function to get the drainage area of this site
# https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
# Find a function that will extract the drainage area for this site (mi2)


# Extract the drainage area
# Convert from cfs to mm/day


# supposed to calculate discharge using the area for each site, so join the dataframe with drainageArea
q2010_2 <- site %>% 
  left_join(drainageArea) %>% 
  mutate(miles3_s = X_00060_00003/147197940448.88, # cfs to mi3/s
         miles3_day = miles3_s*86400,              # mi3/s to mi3/day
         miles_day = miles3_day/drain_area_va, #divide by area to get mi/day
         mm_day = miles_day*(1609.344*1000))    


# # Add the water year variable to q2010_2 - method 1 (cut-off dates from Oct 1 for water year). 
# method 1 is hard-coded, so prefer method2
#breaks <- seq(as.Date("2009-10-01"), length=3, by="year")
#q2010_2$hydroYear <- cut(q2010_2$Date, breaks, labels=2010:2011)


# Add the water year variable to q2010_2 - method 2 
breaks <- seq(as.Date(q2010_2$Date[1]), length=length(q2010_2$Date), by="year")  # breaks starting from first date 
years_breaks = as.numeric(format(breaks,"%Y")) # extract year from Date
labels_water_year = years_breaks[2:length(breaks)]   
q2010_2$waterYear <- cut(q2010_2$Date, breaks,labels=labels_water_year)




qdiff <- 0    # initialise discharge difference between 2 days
year_num <- 0 # initialise year counter
rbi_values <- 0 # unitialise calculated rbi


# Create RBI function
Q1 = q2010_2$mm_day
rbi <- function(Q){
  qdiff <- 0 # initialise discharge difference between 2 days
  for (i in 2:length(Q)){ # offset discharge values by 1
    qdiff[i] <- Q[i] - Q[i-1] # take the difference between qi and qi-1
  }
  rbi_values <- (sum(abs(qdiff))/sum(Q)) # and calculate rbi for each year number  
  return(rbi_values)
}
s <- rbi(Q=Q1)

rbi_values_df <- data.frame() # create df to save calculate rbi values
rbi_wy = data.frame(water_yr = unique(q2010_2$waterYear), rbiValue = " ")# create df and save the waterYear
colnames(rbi_wy)[1] <- "waterYear" # rename waterYear column

# loop that calculates rbi for all the waterYears
for (t in unique(q2010_2$waterYear)){ # get all the waterYears 
  year2 <- filter(q2010_2, waterYear == t)
  rbi_val <- rbi(Q = year2$mm_day) # call the function and calculate rbi
  calc_rbi <- data.frame(rbi_val) # add each calculated rbi to calc_rbi dataframe
  rbi_values_df <- rbind(rbi_values_df,calc_rbi) # append the calc_rbi to rbi_values df
}
rbi_wy$rbiValue <- rbi_values_df$rbi_val # add the rbi values to the rbi_wy



# References
# https://rpubs.com/tbiggs/GEOG576_Exercise_4v2
# https://stackoverflow.com/questions/3312964/how-to-subtract-years
