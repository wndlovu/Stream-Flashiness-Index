
install.packages("dataRetrieval")

library(ggplot2)
library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(data.table)

# USGS Gage # site in Maine
siteNumber <- "01011000" # Pemigewasset R - Hubbard Brook

parameterCd <- "00060"  # Discharge
startDate <- "1970-10-01"   
#endDate <- "2010-09-30"
endDate <- ymd(startDate) + years(50) # download data for 50 more yrs

# Q is in CFS
q2010 <- readNWISdv(siteNumber,parameterCd,startDate, endDate)

# Convert to mm/day

# To do this, you're going to need to use a function to get the drainage area of this site
# https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
# Find a function that will extract the drainage area for this site (mi2)


# extract drain area from site data
drainageArea <- readNWISsite(siteNumber) %>% 
  select(drain_area_va) 


# Extract the drainage area
# Convert from cfs to mm/day

q2010_2 <- q2010 %>% 
  mutate(miles3_s = X_00060_00003/147197940448.88, # cfs to mi3/s
         miles3_day = miles3_s*86400,              # mi3/s to mi3/day
         miles_day = miles3_day/drainageArea$drain_area_va, #divide by area to get mi/day
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

site_num <- 0



for (t in unique(q2010_2$waterYear)){ # loop through the waterYear and select all unique values
  year_num <- year_num + 1 # first unique waterYear is counted as year 1
  year2 <- filter(q2010_2, waterYear == t) # filter for all unique waterYears
  for (s in unique(year2$site_no)){ # trying to loop over each site 
    site_num <- site_num + 1
    sites <- filter(year2, site_no == s)
    for (i in 2:length(year2$X_00060_00003)){ # offset discharge values by 1
      qdiff[i] <- year2$mm_day[i] - year2$mm_day[i-1] # take the difference between qi and qi-1
    }
  }
  rbi_values[year_num] <- (sum(abs(qdiff))/sum(year2$mm_day)) # and calculate rbi for each year number
}


# create data frame with rbi values and rename the variable to rbi
rbi_values_df <- as.data.frame(rbi_values)
colnames(rbi_values_df) <- "rbi"
#add new variable that counts the rows
rbi_values_df <- rbi_values_df %>% 
  mutate(observation = 1:n())

# create data frame with the water years and rename variable to water year
years_all_df <- as.data.frame(unique(q2010_2$waterYear))
colnames(years_all_df) <- "water_Year"
# add new variable that counts the rows
years_all_df <- years_all_df %>% 
  mutate(observation = 1:n())

#create water year and rbi dataframe by joining the dataframes
wy_rbi <- years_all_df %>% 
  left_join(rbi_values_df) %>% 
  select(observation, water_Year, rbi)





# References
# https://rpubs.com/tbiggs/GEOG576_Exercise_4v2
# https://stackoverflow.com/questions/3312964/how-to-subtract-years
