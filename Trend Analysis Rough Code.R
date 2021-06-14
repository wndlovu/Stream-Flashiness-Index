install.packages("dataRetrieval")

library(ggplot2)
library(tidyverse)
library(dataRetrieval)
library(lubridate)

sites_trendAnalysis <- read_csv("nesites.csv") 

# we want to focus on the following: 
#-Dam removals
#-Imperviousness
#-Agricultural change (maybe combine?) + timber
#-Landuse
#-Wateruse

#-Dam removals
damRemoval <- read.table("DamRemovals.txt",sep=",",header=T) %>%
  filter(STAID %in% sites_trendAnalysis$STAID)

#Imperv-Canopy: Three tables with percent imperviousness from the National 
#Land Cover Dataset (NLCD) (every five years, 2001-2011) and from the NAWQA Wall-to-wall
#Anthropogenic Landuse Trends (NWALT) product (every 10 years, 1974-2012) and percent forest
#canopy from NLCD 2011, including riparian zone canopy.  
imperv_NCLD_2001_11 <- read.table("imperv_NLCD_2001-2011.txt",sep=",",header=T)
imperv_NWALT_1974_2012 <- read.table("imperv_NWALT_1974-2012.txt",sep=",",header=T)

imperv_all <- imperv_NWALT_1974_2012 %>% 
  left_join(imperv_NCLD_2001_11) %>% 
  filter(STAID %in% sites_trendAnalysis$STAID) %>% 
  select(STAID, imperv1974est, imperv1982est, 
         imperv1992est, imperv2001, imperv2002est,
         imperv2006, imperv2011, imperv2012est) 


# Timber: One table with percent of watershed affected 
# by timber or forest cutting, annually from 1999-2012.
timber <- read.table("Timber_1999-2012.txt",sep=",",header=T) %>% 
  filter(STAID %in% sites_trendAnalysis$STAID)

# forest canopy
forestCanopy <- read.table("forest_canopy_watershed_and_mainstem_riparian_2011.txt",
                           sep=",",header=T) %>% 
  filter(STAID %in% sites_trendAnalysis$STAID)


# landuse
landUse_NLCD_2001 <- read.table("LandUse_NLCD_2001.txt",sep=",",header=T)
landUse_NWALT_2002 <- read.table("LandUse_NWALT_2002.txt",sep=",",header=T)






#basinID <- read.table("BasinID.txt",sep=",",header=T)
#timber <- read.table("Timber_1999-2012.txt",sep=",",header=T)
#waterUse <- read.table("WaterUse_1985-2010.txt",sep=",",header=T)
#peakFlow <- read.table("PeakFlow_Codes-AnthroInfl.txt",sep=",",header=T)
#ppltnHousing <- read.table("PopulationHousing.txt",sep=",",header=T)
#agric_census <- read.table("CensusOfAgriculture_1950-2012.txt",sep=",",header=T)