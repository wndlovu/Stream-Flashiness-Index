library(tidyverse)
library(dataRetrieval)
library(lubridate)

sites <- read_csv("rbi_Values_Code/nesites.csv")

basinID <- read.table("U_S_GeologicalS/Dataset1_BasinID/BasinID.txt",sep=",",header=T)

# we want to focus on the following: 
#-Dam removals
#-Imperviousness
#-Agricultural change (maybe combine?) + timber
#-Landuse
#-Wateruse

#-Dam removals
damRemoval <- read.table("U_S_GeologicalS/Dataset3_DamRemovals/DamRemovals.txt",sep=",",header=T)%>%
  filter(STAID %in% sites$STAID) 


# dam removal analysis 
# best to use river basin because of sites sharing the same STAID
damRemoval_analysisBasin <- damRemoval %>% 
  group_by(River_Basin, YearDamRemoved) %>% 
  count() %>% 
  dplyr::rename(num_dams = n) %>% 
  arrange(desc(num_dams))
damRemoval_analysisBasin

ggplot()

# most dams (8) removed in 2013 at 01463500 (DELAWARE RIVER AT TRENTON NJ)
# dams ermoved due to floding
damRemoval_analysisSTAID <- damRemoval %>% 
  group_by(STAID, YearDamRemoved) %>% 
  count() %>% 
  dplyr::rename(num_dams = n) %>% 
  arrange(desc(num_dams)) %>% 
  mutate(STAID = as.character(paste0("0", STAID))) %>% 
  left_join(basinID)
damRemoval_analysisSTAID


# no trend
ggplot(y, aes(y = n, x = DRAIN_SQKM))+
  geom_point()


#STAID shows site number but focus on the lat and long
site 01453000

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
  mutate(percent_diff = imperv2011 - imperv2001,
        percent_diff_est = imperv2012est - imperv1974est) %>% 
  mutate(STAID = as.character(paste0("0", STAID))) %>% 
  left_join(basinID) %>% 
  arrange(desc(percent_diff_est))

# no trend
ggplot(imperv, aes(x = DRAIN_SQKM, y = percent_diff_est))+
  geom_point()



# landuse
developedLandUse_NLCD <- read.table("U_S_GeologicalS/Dataset5_LandUse/LandUse_NLCD_2001.txt",sep=",",header=T) %>% 
  left_join(read.table("U_S_GeologicalS/Dataset5_LandUse/LandUse_NLCD_2006.txt",sep=",",header=T)) %>% 
  left_join(read.table("U_S_GeologicalS/Dataset5_LandUse/LandUse_NLCD_2011.txt",sep=",",header=T)) %>% 
  filter(STAID %in% sites$STAID) %>% 
  select(STAID, contains(c("21", "22", "23", "24"))) %>% 
  mutate(STAID = as.character(paste0("0", STAID))) %>% 
  left_join(basinID) %>% 
  mutate(diff21 = NLCD11_21 - NLCD01_21,
         diff22 = NLCD11_22 - NLCD01_22,
         diff23 = NLCD11_23 - NLCD11_23,
         diff24 = NLCD11_24 - NLCD01_24)






# Timber: One table with percent of watershed affected 
# by timber or forest cutting, annually from 1999-2012.
timber <- read.table("Timber_1999-2012.txt",sep=",",header=T) %>% 
  filter(STAID %in% sites_trendAnalysis$STAID)

# forest canopy
forestCanopy <- read.table("forest_canopy_watershed_and_mainstem_riparian_2011.txt",
                           sep=",",header=T) %>% 
  filter(STAID %in% sites_trendAnalysis$STAID)





landUse_NWALT_2002 <- read.table("LandUse_NWALT_2002.txt",sep=",",header=T)






#basinID <- read.table("BasinID.txt",sep=",",header=T)
#timber <- read.table("Timber_1999-2012.txt",sep=",",header=T)
#waterUse <- read.table("WaterUse_1985-2010.txt",sep=",",header=T)
#peakFlow <- read.table("PeakFlow_Codes-AnthroInfl.txt",sep=",",header=T)
#ppltnHousing <- read.table("PopulationHousing.txt",sep=",",header=T)
#agric_census <- read.table("CensusOfAgriculture_1950-2012.txt",sep=",",header=T) 
