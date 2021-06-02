
install.packages("dataRetrieval")

library(ggplot2)
library(tidyverse)
library(dataRetrieval)

# USGS Gage #
siteNumber <- "01076500" # Pemigewasset R - Hubbard Brook

parameterCd <- "00060"  # Discharge
startDate <- "2009-10-01"  
endDate <- "2010-09-30"


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
breaks <- seq(as.Date("2009-10-01"), length=3, by="year")  # breaks starting Oct 1, 2009
years_breaks = as.numeric(format(breaks,"%Y")) # extract year from Date
labels_water_year = years_breaks[2:length(breaks)]   
q2010_2$wateryear <- cut(q2010_2$Date, breaks,labels=labels_water_year)


qdiff <- 0
for (i in 2:length(q2010$X_00060_00003)){
  qdiff[i] <- q2010$X_00060_00003[i] - q2010$X_00060_00003[i-1]
  # take the difference between qi and qi-1
  # take the absolute value
}

rbi <- sum(abs(qdiff))/sum(q2010$X_00060_00003)


# References
# https://rpubs.com/tbiggs/GEOG576_Exercise_4v2
