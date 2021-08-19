library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(ggx)
options(scipen = 999)



sites <- read_csv("rbi_Values_Code/nesites.csv")

basinID <- read.table("U_S_GeologicalS/Dataset1_BasinID/BasinID.txt",sep=",",header=T)


# DAM REMOVAL ANALYSIS

# extract dam removal data
damRemoval <- read.table("U_S_GeologicalS/Dataset3_DamRemovals/DamRemovals.txt",sep=",",header=T)%>%
  filter(STAID %in% sites$STAID) %>% #filter for study sites 
  mutate(STAID = as.character(paste0("0", STAID)))


# What are the number of removed dams in each river basin per year
damRemoval_analysisBasin <- damRemoval %>% 
  group_by(River_Basin, YearDamRemoved) %>% 
  count() %>% # count number of dams removed per river_basin per given year (given by n)
  dplyr::rename(num_dams = n) %>% # rename n
  arrange(desc(num_dams)) # arrange in descending order


# What are the number of removed dams each year
damRemoval_analysisYear <- damRemoval %>% 
  group_by(YearDamRemoved) %>% 
  count() %>% #count number of dams removed each year
  dplyr::rename(num_dams = n) %>% 
  arrange(desc(num_dams))


# What are the number dams from each STAID per year  
damRemoval_analysisSTAID <- damRemoval %>% 
  group_by(STAID, YearDamRemoved) %>% 
  count() %>% # count number of dams removed from each STAID per year
  dplyr::rename(num_dams = n) %>% 
  arrange(desc(num_dams)) %>% 
  left_join(basinID) %>% # joining df with the basinID data
  select(1:4, 9, 7:8, 5) %>% 
  mutate(LAT_GAGE = as.numeric(LAT_GAGE), # save lat and lng as numeric variables
         LNG_GAGE = as.numeric(LNG_GAGE))


# What are the trends at sites where dams were removed?/ is there a pattern between trends and when dams are removed?
# dam removal trends (siginificant p value)
damRemoval_trends <- damRemoval %>% 
  left_join(read_csv("results/trend_analysis_df.csv"), by = c("STAID" = "site_no")) %>% # join with trend analysis df
  filter(p.value < 0.05) %>% # filter for sites with significant p values
  mutate(highlight_flag = ifelse(estimates > 0, "upward", "downward")) # add variable to classify trends 

  

# change variable orientation (create dataframe with WaterYear, STAID and rbi_val)
rbiWy_dfAll_pivotLong <- read_csv("Trend_Analysis_Code/rbiWy_dfAll.csv") %>% 
  pivot_longer(cols = starts_with("0"),
               names_to = "STAID",
               values_to = "rbi_val") 

# Flashiness values (for each year) versus time (for sites with dam removals)
damRemoval_rbi <- rbiWy_dfAll_pivotLong %>% 
  inner_join(damRemoval, by = c("STAID" = "STAID")) # join wit damRemoval df to include all sites with 


# Compare trends at sites where dams were removed and sites where there is no dam removal
damsRemovedAtSites <- read_csv("site_nums.csv") %>% 
  left_join(damRemoval, by = c("STAID" = "STAID")) %>% # create df with all study sites (with and without dam removals)
  mutate(YearDamRemoved = replace(YearDamRemoved,is.na(YearDamRemoved),0), # sites where no dams were removed have NAs, so replace NAs in YearDamRemoved variable with 0 
         damRemoved = ifelse(YearDamRemoved == 0, "No Dam Removed", "Dam Removed")) %>% # create new variable to indicate whether a dam was removed
  full_join(read_csv("results/trend_analysis_df.csv"), by = c("STAID" = "site_no")) %>% # join with trend analysis df
  filter(p.value < 0.05) # filter for sites with significant p value







# IMPERVIOUSNESS ANALYSIS

#Imperv-Canopy: Three tables with percent imperviousness from the National 
#Land Cover Dataset (NLCD) (every five years, 2001-2011) and from the NAWQA Wall-to-wall
#Anthropogenic Landuse Trends (NWALT) product (every 10 years, 1974-2012) and percent forestcanopy from NLCD 2011, including riparian zone canopy. 


# Do changes in imperviousness (changes between 1974 and 2012) vary with drainage size? 
imperv <- read.table("U_S_GeologicalS/Dataset4_Imperviousness-Canopy/imperv_NLCD_2001-2011.txt",sep=",",header=T) %>% 
  left_join(read.table("U_S_GeologicalS/Dataset4_Imperviousness-Canopy/imperv_NWALT_1974-2012.txt",sep=",",header=T)) %>% 
  filter(STAID %in% sites$STAID) %>% #filter for study sites 
  select(STAID, imperv1974est, imperv1982est, # organise the variables 
         imperv1992est, imperv2001, imperv2002est,
         imperv2006, imperv2011, imperv2012est) %>% 
  mutate(imperv_percent_diff = imperv2012est - imperv1974est, # calculate percent change in imperviousness form 1974 to 2012
         STAID = as.character(paste0("0", STAID))) %>% 
  left_join(basinID) %>% # join basinID df
  mutate(DRAIN_SQKM = as.numeric(DRAIN_SQKM)) %>% # save drainage area as a numeric
  select(1, 11, 16, 12, 14:15, 2:10) %>% # organise variables
  arrange(desc(imperv_percent_diff))

# rename the imperv variables
colnames(imperv)[7] <- "1974"
colnames(imperv)[8] <- "1982"
colnames(imperv)[9] <- "1992"
colnames(imperv)[10] <- "2001"
colnames(imperv)[11] <- "2002"
colnames(imperv)[12] <- "2006"
colnames(imperv)[13] <- "2011"
colnames(imperv)[14] <- "2012"


# Is there a change in number of urban and rural areas between 1974 - 2012?
imperv_pivoted <- imperv %>% 
  pivot_longer(cols = '1974':'2012', names_to = "year", values_to = "imperv_value") %>% # pivot longer from 1974 to 2012 and have all imperv percents in one column
  mutate(highlight_flag = ifelse(imperv_value > 20, "urban", "rural")) # sites with imper_value greater than 20% are urban



# What is the relationship between trend and drainage area for reference sites? Assume any trends shown in reference sites is due to climate change (sites with significant p values)
reference_sites <- imperv_pivoted %>% 
  distinct(STAID, .keep_all = TRUE) %>% # remove duplicated sites
  full_join(read_csv("results/trend_analysis_df.csv"), by = c("STAID" = "site_no")) %>% 
  filter(imperv_value < 20,
         p.value < .05) %>% 
  mutate(lat = as.numeric(LAT_GAGE),
         long = as.numeric(LNG_GAGE))


# What is the spatial distribution of reference sites?
reference_sitesMap <- imperv_pivoted %>% 
  distinct(STAID, .keep_all = TRUE) %>% # remove duplicated sites
  full_join(read_csv("results/trend_analysis_df.csv"), by = c("STAID" = "site_no")) %>% 
  filter(imperv_value < 20) %>% # assume reference sites have < 20% imperv values 
  mutate(lat = as.numeric(LAT_GAGE),
         long = as.numeric(LNG_GAGE))



# DEVELOPED LAND ANALYSIS

# Is there a change in number of urban and rural areas between 2001 - 2011? (using the LandUse data)
developedLandUse_NLCD <- read.table("U_S_GeologicalS/Dataset5_LandUse/LandUse_NLCD_2001.txt",sep=",",header=T) %>% 
  left_join(read.table("U_S_GeologicalS/Dataset5_LandUse/LandUse_NLCD_2006.txt",sep=",",header=T)) %>% 
  left_join(read.table("U_S_GeologicalS/Dataset5_LandUse/LandUse_NLCD_2011.txt",sep=",",header=T)) %>% 
  filter(STAID %in% sites$STAID) %>% #filter for study sites 
  select(STAID, contains(c("21", "22", "23", "24"))) %>% # select only the developed land variables
  mutate(STAID = as.character(paste0("0", STAID))) %>% 
  left_join(basinID) %>% 
  mutate('2001' = rowSums(across(starts_with("NLCD01")), na.rm = T), # total developed land in 2001
         '2006' = rowSums(across(starts_with("NLCD06")), na.rm = T), # total developed land in 2006
         '2011' = rowSums(across(starts_with("NLCD11")), na.rm = T), # total developed land in 2011
         developed_percent_diff = `2011` - `2001`) %>% 
  select(1, 14, 19, 15, 17:18, 2:13, 23:26) # organise variables


developedLandUse_NLCDpivoted <- developedLandUse_NLCD %>% 
  pivot_longer(cols = '2001': '2011', names_to = "year", values_to = "imperv_value") %>% # pivot longer and have all imperv values in one column
  mutate(type = ifelse(imperv_value > 20, "urban", "rural"), # sites with greater than 20% are urban
         lat = as.numeric(LAT_GAGE), # resave lat and long as numeric
         long = as.numeric(LNG_GAGE)) 


# used to show spatial distribution of urban and rural areas from 2001 - 2011
developedLandUse_NLCD_types <- developedLandUse_NLCD %>% 
  mutate(type2001 = ifelse(`2001` > 20, "urban", "rural"),
         type2006 = ifelse(`2006` > 20, "urban", "rural"),
         type2011 = ifelse(`2011` > 20, "urban", "rural"),
         lat = as.numeric(LAT_GAGE),
         long = as.numeric(LNG_GAGE)) %>% 
  full_join(read_csv("results/trend_analysis_df.csv"), by = c("STAID" = "site_no")) %>% 
  filter(p.value < .05)



# Are there similar trends between changes in development and imperviousness values? (comparison of the NLCD and Imperviousness datasets)
imperv_developed <- developedLandUse_NLCD %>% 
  left_join(imperv, by = c("STAID" = "STAID")) %>% 
  left_join(read_csv("results/trend_analysis_df.csv"), by = c("STAID" = "site_no")) %>%
  select(1:6, 22, 36:41) %>% 
  filter(p.value < 0.05) %>% 
  pivot_longer(cols = developed_percent_diff:imperv_percent_diff, names_to = "type", values_to = "percent_diff")


# What is the spatial distribution of high and low trends for sites with significant p values?
trends_rbi <- sites %>% 
  mutate(STAID = as.character(paste0("0", STAID))) %>% 
  left_join(basinID) %>% # join basinID data to add basinID info
  left_join(read_csv("results/trend_analysis_df.csv"), by = c("STAID" = "site_no")) %>% # join trend analysis results
  mutate(trend_type = ifelse(estimates > 0, "high", "low"), # split trends into high(>0) and low(<0)
         lat = as.numeric(LAT_GAGE),
         long = as.numeric(LNG_GAGE)) %>% 
  filter(p.value < 0.05) %>% # filtr for sites with significant p values
  select(1,2,7,3,17,18, 11:16)


# MAP VISUALS

# code for adding state abbreviations to maps
centroids <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y) # create df with state name
centroids$abb<-state.abb[match(centroids$region,tolower(state.name))] # create abbr variable

# filter for states that are in reference sites. Same states are also in trends_rbi 
states <- reference_sites %>% 
  select(STATE) %>% 
  distinct(STATE) %>% 
  left_join(centroids, by = c("STATE" = "abb"))

