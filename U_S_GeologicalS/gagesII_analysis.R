library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(ggx)

sites <- read_csv("rbi_Values_Code/nesites.csv")

basinID <- read.table("U_S_GeologicalS/Dataset1_BasinID/BasinID.txt",sep=",",header=T)


#Dam removals
damRemoval <- read.table("U_S_GeologicalS/Dataset3_DamRemovals/DamRemovals.txt",sep=",",header=T)%>%
  filter(STAID %in% sites$STAID) %>% 
  mutate(STAID = as.character(paste0("0", STAID)))


# dam removal analysis 
# best to use river basin because of sites sharing the same STAID
damRemoval_analysisBasin <- damRemoval %>% 
  group_by(River_Basin, YearDamRemoved) %>% 
  count() %>% 
  dplyr::rename(num_dams = n) %>% 
  arrange(desc(num_dams))
damRemoval_analysisBasin

damRemoval_analysisYear <- damRemoval %>% 
  group_by(YearDamRemoved) %>% 
  count() %>% 
  dplyr::rename(num_dams = n) %>% 
  arrange(desc(num_dams))
  

damRemoval_analysisSTAID <- damRemoval %>% 
  group_by(STAID, YearDamRemoved) %>% 
  count() %>% 
  dplyr::rename(num_dams = n) %>% 
  arrange(desc(num_dams)) %>% 
  left_join(basinID)
damRemoval_analysisSTAID


# dam removal trends
damRemoval_trends <- damRemoval %>% 
  left_join(read_csv("results/trend_analysis_df.csv"), by = c("STAID" = "site_no")) %>% 
  filter(p.value <= 0.05) %>% 
  mutate(highlight_flag = ifelse(estimates > 0, "upward", "downward"))
  

# check trends
damRemoval_trends2 <- damRemoval %>% 
  full_join(read_csv("site_nums.csv"), by = c("STAID" = "STAID")) %>% 
  mutate(YearDamRemoved = replace(YearDamRemoved,is.na(YearDamRemoved),0),
         damRemoved = ifelse(YearDamRemoved == 0, "No Dam Removed", "Dam Removed")) %>% # have 0 as a place holder for years when there are no dam removals
  full_join(read_csv("results/trend_analysis_df.csv"), by = c("STAID" = "site_no")) %>% 
  filter(p.value <= 0.05)






# imperviousness analysis

#Imperv-Canopy: Three tables with percent imperviousness from the National 
#Land Cover Dataset (NLCD) (every five years, 2001-2011) and from the NAWQA Wall-to-wall
#Anthropogenic Landuse Trends (NWALT) product (every 10 years, 1974-2012) and percent forest
#canopy from NLCD 2011, including riparian zone canopy.  
imperv <- read.table("U_S_GeologicalS/Dataset4_Imperviousness-Canopy/imperv_NLCD_2001-2011.txt",sep=",",header=T) %>% 
  left_join(read.table("U_S_GeologicalS/Dataset4_Imperviousness-Canopy/imperv_NWALT_1974-2012.txt",sep=",",header=T)) %>% 
  filter(STAID %in% sites$STAID) %>% 
  select(STAID, imperv1974est, imperv1982est, 
         imperv1992est, imperv2001, imperv2002est,
         imperv2006, imperv2011, imperv2012est) %>% 
  mutate(percent_diff = imperv2011 - imperv2001) %>% 
  mutate(STAID = as.character(paste0("0", STAID))) %>% 
  left_join(basinID) %>% 
  mutate(DRAIN_SQKM = as.numeric(DRAIN_SQKM)) %>% 
  select(1, 11, 16, 12, 14:15, 2:10) %>% 
  arrange(desc(percent_diff))


colnames(imperv)[7] <- "1974"
colnames(imperv)[8] <- "1982"
colnames(imperv)[9] <- "1992"
colnames(imperv)[10] <- "2001"
colnames(imperv)[11] <- "2002"
colnames(imperv)[12] <- "2006"
colnames(imperv)[13] <- "2011"
colnames(imperv)[14] <- "2012"


imperv_pivoted <- imperv %>% 
  pivot_longer(cols = '1974':'2012', names_to = "year", values_to = "imperv_value") %>% 
  mutate(highlight_flag = ifelse(imperv_value > 20, "urban", "rural")) # sites with imper_value greater than 20% are urban


# developed land analysis
developedLandUse_NLCD <- read.table("U_S_GeologicalS/Dataset5_LandUse/LandUse_NLCD_2001.txt",sep=",",header=T) %>% 
  left_join(read.table("U_S_GeologicalS/Dataset5_LandUse/LandUse_NLCD_2006.txt",sep=",",header=T)) %>% 
  left_join(read.table("U_S_GeologicalS/Dataset5_LandUse/LandUse_NLCD_2011.txt",sep=",",header=T)) %>% 
  filter(STAID %in% sites$STAID) %>% 
  select(STAID, contains(c("21", "22", "23", "24"))) %>% # select only the developed land variables
  mutate(STAID = as.character(paste0("0", STAID))) %>% 
  left_join(basinID) %>% 
  mutate('2001' = rowSums(across(starts_with("NLCD01")), na.rm = T), # total developed land in 2001
         '2006' = rowSums(across(starts_with("NLCD06")), na.rm = T), # total developed land in 2006
         '2011' = rowSums(across(starts_with("NLCD11")), na.rm = T)) %>% # total developed land in 2011
  select(1, 14, 19, 15, 17:18, 2:13, 23:25)


developedLandUse_NLCDpivoted <- developedLandUse_NLCD %>% 
  pivot_longer(cols = '2001': '2011', names_to = "year", values_to = "imperv_value") %>% 
  mutate(type = ifelse(imperv_value > 20, "urban", "rural")) 


# reference sites
reference_sites <- imperv_pivoted %>% 
  filter(imperv_value < 5) %>% 
  full_join(read_csv("results/trend_analysis_df.csv"), by = c("STAID" = "site_no")) %>% 
  mutate(lat = LAT_GAGE,
         long = LNG_GAGE)
 
