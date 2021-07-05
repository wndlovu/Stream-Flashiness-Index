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
### first run gagesII_analysis.R ###



# QUE 1. WHEN WERE MOST DAMS REMOVED?

# There is an upward trend in dam removals
# least dams removed between 1971 and 1999 ( 1- 10 dams)
# from 2000 there's an increase in dam removals (dam removals > 10, except for 2005 - dam removals = 5)
# from 2010 and 2011 had the highest dam removals (55 and 56)
# this was followed by a sharp drop between 2011 and 2012 from 56 to 34 dam removals

# high dam removal == higher rbi??
ggplot(damRemoval_analysisYear, aes(x = YearDamRemoved, y = num_dams))+
  geom_point()+
  geom_smooth()

# is there a pattern between trends and when dams are removed
ggplot(damRemoval_trends, aes(x = YearDamRemoved, y = estimates, color = highlight_flag))+
  geom_point()+
  labs(x = "Year dam removed",
       y = "Theil sen slope")+
  theme_bw()+
  theme_clean()+
  theme(legend.position = "none")


# still have to change this and factor the year
ggplot(damRemoval_trends2, aes(x = damRemoved, y = estimates, fill = damRemoved)) +
  geom_violin(trim = FALSE) + 
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "black"
  )+
  geom_jitter(alpha = 0.3, size = 0.5, width=0.3) +
  scale_fill_manual(values = safe_colorblind_palette)+
  #facet_wrap(~YearDamRemoved)+
  theme_clean()+
  theme(legend.position = "none",
        axis.title.x = element_blank())+
  labs(y = "Theil sen slope")



# most development (higher imperv percent diff) occuring in areas with smaller watersheds
# most of the areas with higher drainage area are rural
ggplot(imperv, aes(x = DRAIN_SQKM, y = percent_diff))+
  geom_point(alpha = 0.3) + 
  theme_clean()+
  #theme_bw() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )+
  scale_x_log10() +
  scale_y_continuous(labels = function(y) paste0(y, "%"), limits = c(0,8,1))+
  #theme(axis.text.x = element_text(angle=90))+
  labs(y = "Imperviousness Difference",
       x = "Drainage Area (sqkm)")


# most devpt - MA, PA, NJ
# most areas have less than 10% of imperv surfaces
ggplot(imperv_pivoted, aes(x = imperv_value, fill = highlight_flag))+
  geom_histogram()+
  #scale_x_continuous(labels = function(x) paste0(x, "%"))+
  scale_fill_manual(values = c("grey60", "red"))+
  facet_grid(year~STATE)+
  theme_bw()+
  theme(
    #axis.title.y = element_blank(),
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 9),
    #legend.title = element_blank(),
    #legend.position = "none",
    #plot.title = element_text(size = 12),
    axis.text.x = element_text(angle=90, size = 6), 
    strip.text.x = element_text(size = 7),
    strip.text.y = element_text(size = 5.5),
    #strip.background = element_blank(),
    #panel.background = element_rect(color = "white"),
    strip.background = element_rect(fill="grey90", size=1, color = "grey90"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "bottom",
    legend.title = element_blank()
  )+
  labs(x = "Percent Imperviousness",
       y = "Number of sites")



# using developed land cover of > 20%
ggplot(developedLandUse_NLCDpivoted, aes(x = type, fill = type))+
  #geom_bar()+
  geom_bar(stat = "count") + 
  #stat_count(geom = "text",
  #aes(label = ..count..))+
  geom_text(stat='count', aes(label=..count..), vjust=-.25, size = 2.5)+
  scale_y_continuous(limits = c(0,100,10))+
  facet_grid(year~STATE)+
  #facet_grid(type~STATE)+
  scale_fill_manual(values = safe_colorblind_palette, labels = c("rural", "urban"))+
  #theme_classic()+
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(size = 6),
    axis.ticks.x = element_blank(),
    #legend.position = "none",
    #plot.title = element_text(size = 12),
    axis.text.x = element_text(angle=90, size = 6), 
    strip.text.x = element_text(size = 7),
    strip.text.y = element_text(size = 5.5),
    #legend.position = "bottom",
    strip.background = element_rect(fill="grey90", size=2, color = "grey90"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = "bottom",
    legend.title = element_blank(),
    #lege = element_rect(size = 7),
    #legend.title = element_blank(),
    axis.title = element_text(size = 8)
  )+
  labs(y = "Number of sites")





ggplot(damRemoval_trends2, aes(x = damRemoved, y = estimates, fill = damRemoved)) +
  geom_violin(trim = FALSE) + 
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "black"
  )+
  geom_jitter(alpha = 0.3, size = 0.5, width=0.3, color = "black") +
  scale_fill_manual(values = safe_colorblind_palette)+
  theme_clean()+
  theme(legend.position = "none")+
  labs(y = "Theil sen slope")


# reference sites 
ggplot(reference_sites, aes(x = DRAIN_SQKM, y = estimates)) +
  geom_point(alpha = 0.3)+
  scale_x_log10()+
  labs(y = "Theil sen slope",
       x = "Drainage Area (sqkm)")+
  theme_bw()+
  theme_clean()

# bbox for reference_sites xmin: -78.67697 ymin: 38.22892 xmax: -67.7688 ymax: 47.23739

# data(us.cities)
# capitals <- subset(us.cities, capital == 2)

world <- ne_countries(scale = "medium", returnclass = "sf")

# all reference sites
ggplot(data = world) +
  geom_sf(fill = "white") +
  #annotation_map_tile(zoom = 6, type = "osm") + 
  borders("state", colour = "black", fill = "grey90") +
  geom_point(data = reference_sites, aes(x = long, y = lat), size = .9, shape = 21, fill = "red") + 
  #scale_fill_gradient2(limits = c(-1.2,1.2),midpoint = 0, low = "#1d1369", mid = "white", high = "#691313", breaks = c(-1.2,-0.5,0,0.5,1.2)) +
  coord_sf(xlim = c(-79.67697,-64.7688), ylim = c(34.22892, 49.23739), expand = FALSE)+
  
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  theme_map()



# is there a geographic organisation of the trends?


ggplot(data = world) +
  geom_sf(fill = "white") +
  #annotation_map_tile(zoom = 6, type = "osm") + 
  borders("state", colour = "black", fill = "grey90") +
  geom_text(trends_map, aes(label = , x = Longitude, y = Latitude)) +
  geom_point(data = trends_map, aes(x = dec_long_va, y = dec_lat_va, color = trend_type), size = 1.5) + 
  scale_color_calc()+
  #scale_fill_gradient2(limits = c(-1.2,1.2),midpoint = 0, low = "#1d1369", mid = "white", high = "#691313", breaks = c(-1.2,-0.5,0,0.5,1.2)) +
  coord_sf(xlim = c(-79.67697,-64.7688), ylim = c(34.22892, 49.23739), expand = FALSE)+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  theme_map()


ggplot(imperv_developed, aes(x = percent_diff, y = estimates))+
  geom_point()+
  facet_grid(~type)


