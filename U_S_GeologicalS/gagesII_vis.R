library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(ggthemr)
library(ggthemes)
library(ggsci)
library(ggpubr)
library(maps)
library(tidyverse)
library(sf)
library(sp)
library("rnaturalearth")
library("rnaturalearthdata")
library(ggalt)
library(rgeos)     
library(ggspatial)
library(usmap)
library(ggsn) 
library(ggsci)
library(patchwork)
library(cowplot)


### first run gagesII_analysis.R ###




# What are the number of removed dams each year

# There is an upward trend in dam removals
# least dams removed between 1971 and 1999 ( 1- 10 dams)
# from 2000 there's an increase in dam removals (dam removals > 10, except for 2005 - dam removals = 5)
# from 2010 and 2011 had the highest dam removals (55 and 56)
# this was followed by a sharp drop between 2011 and 2012 from 56 to 34 dam removals

ggplot(damRemoval_analysisYear, aes(x = YearDamRemoved, y = num_dams))+
  geom_point()+
  geom_smooth()+
  labs(x = "Year dam removed",
       y = "Number of removed dams")+
  theme_bw()+
  theme_clean()+
  scale_x_continuous(breaks = seq(from = 1970, to = 2020, by = 5))+
  scale_y_continuous(breaks = seq(from = -30, to = 60, by = 10))





# What are the trends at sites where dams were removed?/ is there a pattern between trends and when dams are removed?
ggplot(damRemoval_trends, aes(x = YearDamRemoved, y = estimates, color = highlight_flag))+
  geom_point()+
  labs(x = "Year Dam Removed",
       y = "Theil Sen Slope")+
  theme_bw()+
  theme_clean()+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq(from = 1970, to = 2020, by = 5))+
  scale_y_continuous(breaks = seq(from = -.005, to = .005, by = .001))





# Compare trends at sites where dams were removed and sites where there is no dam removal
ggplot(damsRemovedAtSites, aes(x = damRemoved, y = estimates, fill = damRemoved)) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width=0.1) +
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "black"
  )+
  geom_jitter(alpha = 0.9, size = 0.09, width=0.3, color = "black") +
  scale_fill_manual(values = safe_colorblind_palette)+
  theme_clean()+
  theme(legend.position = "none",
        axis.title.x = element_blank())+
  labs(y = "Theil sen slope")





# Do changes in imperviousness (changes between 1974 and 2012) vary with drainage size? 

# most development (higher imperv percent diff) occuring in areas with smaller watersheds
# most of the areas with higher drainage area are rural
ggplot(imperv, aes(x = DRAIN_SQKM, y = imperv_percent_diff))+
  geom_point(alpha = 0.6) + 
  #geom_smooth()+
  theme_clean()+
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  )+
  scale_x_log10() +
  scale_y_continuous(labels = function(y) paste0(y, "%"), limits = c(0,8,1))+
  labs(y = "Change in
  Imperviousness ",
       x = "Drainage Area (sqkm)")





# Is there a change in number of urban and rural areas between 1974 - 2012?
ggplot(imperv_pivoted, aes(x = imperv_value, fill = highlight_flag))+
  geom_histogram()+
  scale_fill_manual(values = c("grey60", "red"))+
  facet_grid(year~STATE)+
  theme_bw()+
  theme_few()+
  theme(axis.text.y = element_text(size = 7, face = "bold"),
    legend.text = element_text(size = 9),
    axis.text.x = element_text( size = 7, face = "bold"), 
    strip.text.x = element_text(size = 10, color = "white"),
    strip.text.y = element_text(size = 10, color = "white"),
    strip.background = element_rect(fill="black", size=1, color = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(x = "Percent Imperviousness",
       y = "Number of sites")
  




# Is there a change in number of urban and rural areas between 2001 - 2011? (using the LandUse data)
ggplot(developedLandUse_NLCDpivoted, aes(x = type, fill = type))+
  geom_bar(stat = "count") + 
  geom_text(stat='count', aes(label=..count..), vjust=-.25, size = 3.5)+
  scale_y_continuous(limits = c(0,100,10))+
  facet_grid(year~STATE)+
  scale_fill_manual(values = safe_colorblind_palette, labels = c("rural", "urban"))+
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(size = 7, face = "bold"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle=90, size = 7, face = "bold"), 
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(size = 10),
    strip.background = element_rect(fill="grey90", size=2, color = "grey90"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_text(size = 8)
  )+
  labs(y = "Number of sites")






# What is the relationship between trend and drainage area for reference sites?
ggplot(reference_sites, aes(x = DRAIN_SQKM, y = estimates)) +
  geom_point(alpha = 0.9)+
  geom_smooth(method = "lm")+
  scale_x_log10()+
  labs(y = "Theil sen slope",
       x = "Drainage Area (sqkm)")+
  theme_bw()+
  theme_clean()





# What is the spatial distribution of reference sites?
world <- ne_countries(scale = "medium", returnclass = "sf")


ggplot(data = world) +
  geom_sf(fill = "white") +
  borders("state", colour = "black", fill = "grey90") +
  geom_point(data = reference_sitesMap, aes(x = long, y = lat), size = 1, shape = 21, fill = "blue4", alpha = .9) + 
  coord_sf(xlim = c(-79.67697,-64.7688), ylim = c(34.22892, 49.23739), expand = FALSE)+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  theme_map()#+
  #with(states, 
       #annotate(geom="text", x = long, y=lat, label = STATE, 
                #size = 3,color="black",family="Times")
  #)





# What is the spatial distribution of high and low trends for sites with significant p values?
trends_rbiMap <- ggplot(data = world) +
  geom_sf(fill = "white") +
  #annotation_map_tile(zoom = 6, type = "osm") + 
  borders("state", colour = "black", fill = "grey90") +
  #geom_text(data = trends_rbi, aes(label = STATE)) +
  with(states, 
       annotate(geom="text", x = long, y=lat, label = STATE, 
                size = 4,color="black",family="Times")
  )+
  geom_point(data = trends_rbi, aes(x = long, y = lat, color = trend_type), size = 3.5, shape = 18, alpha = .9) + 
  scale_color_manual(values = c("#000000", "#56B4E9"), labels = c("High Trends", "Low Trends"))+
  #scale_fill_gradient2(limits = c(-1.2,1.2),midpoint = 0, low = "#1d1369", mid = "white", high = "#691313", breaks = c(-1.2,-0.5,0,0.5,1.2)) +
  coord_sf(xlim = c(-79.67697,-64.7688), ylim = c(34.22892, 49.23739), expand = FALSE)+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  labs(title = "Distribution of Trends")+
  theme_map()+
  theme(legend.position = c(1,0),
        legend.title = element_blank(),
        plot.title = element_text(hjust = .5),
        legend.background = element_rect(fill="white", size=0.5, linetype="solid"),
        legend.justification=c(1,0))





# What is the spatial distribution of urban and rural areas from 2001 - 2011
developedLand_2001Map <- ggplot(data = world) +
  geom_sf(fill = "white") +
  #annotation_map_tile(zoom = 6, type = "osm") + 
  borders("state", colour = "black", fill = "grey90") +
  #geom_text(data = trends_rbi, aes(label = STATE)) +
  with(states, 
       annotate(geom="text", x = long, y=lat, label = STATE, 
                size = 4,color="black",family="Times")
  )+
  geom_point(data = developedLandUse_NLCD_types, aes(x = long, y = lat, color = type2001), size = 2, alpha = .7) + 
  scale_color_manual(values = c("#800000FF", "#FFB547FF"))+
  #scale_fill_gradient2(limits = c(-1.2,1.2),midpoint = 0, low = "#1d1369", mid = "white", high = "#691313", breaks = c(-1.2,-0.5,0,0.5,1.2)) +
  coord_sf(xlim = c(-79.67697,-64.7688), ylim = c(34.22892, 49.23739), expand = FALSE)+
  
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  labs(title = "2001")+
  theme_map()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))


developedLand_2006Map <- ggplot(data = world) +
  geom_sf(fill = "white") +
  #annotation_map_tile(zoom = 6, type = "osm") + 
  borders("state", colour = "black", fill = "grey90") +
  #geom_text(data = trends_rbi, aes(label = STATE)) +
  with(states, 
       annotate(geom="text", x = long, y=lat, label = STATE, 
                size = 4,color="black",family="Times")
  )+
  geom_point(data = developedLandUse_NLCD_types, aes(x = long, y = lat, color = type2006), size = 2, alpha = .7) + 
  scale_color_manual(values = c("#800000FF", "#FFB547FF"))+
  #scale_fill_gradient2(limits = c(-1.2,1.2),midpoint = 0, low = "#1d1369", mid = "white", high = "#691313", breaks = c(-1.2,-0.5,0,0.5,1.2)) +
  coord_sf(xlim = c(-79.67697,-64.7688), ylim = c(34.22892, 49.23739), expand = FALSE)+
  
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  labs(title = "2006")+
  theme_map()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))


developedLand_2011Map <- ggplot(data = world) +
  geom_sf(fill = "white") +
  #annotation_map_tile(zoom = 6, type = "osm") + 
  borders("state", colour = "black", fill = "grey90") +
  #geom_text(data = trends_rbi, aes(label = STATE)) +
  with(states, 
       annotate(geom="text", x = long, y=lat, label = STATE, 
                size = 4,color="black",family="Times")
  )+
  geom_point(data = developedLandUse_NLCD_types, aes(x = long, y = lat, color = type2011), size = 2, alpha = .7) + 
  scale_color_manual(values = c("#800000FF", "#FFB547FF"), labels = c("Rural", "Urban"))+
  #scale_fill_gradient2(limits = c(-1.2,1.2),midpoint = 0, low = "#1d1369", mid = "white", high = "#691313", breaks = c(-1.2,-0.5,0,0.5,1.2)) +
  coord_sf(xlim = c(-79.67697,-64.7688), ylim = c(34.22892, 49.23739), expand = FALSE)+
  
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  labs(title = "2011")+
  theme_map()+
  theme(legend.position = c(1,0),
        legend.title = element_blank(),
        plot.title = element_text(hjust = .5),
        legend.background = element_rect(fill="white", size=0.5, linetype="solid"),
        legend.justification=c(1,0))

# using developedLandUse_NLCD, there is not much change in area type (urban or rural) from 2001 to 2011 
ggarrange(developedLand_2001Map, developedLand_2006Map, developedLand_2011Map, ncol = 3, common.legend = FALSE) + 
  theme_clean()+
  theme_bw()




# use the 2011 developed land and trend maps to see if urban areas tend to be flashier than rural areas
modified_2011Map <- developedLand_2011Map +
  labs(title = "Site Classifications") # add different title to the developedLand_2011Map

ggarrange(modified_2011Map, trends_rbiMap)+
  theme_clean()+
  theme_bw()



# REFERENCES
#https://rstudio-pubs-static.s3.amazonaws.com/446253_b0059ba404ec418db41bbd5517bf818d.html

