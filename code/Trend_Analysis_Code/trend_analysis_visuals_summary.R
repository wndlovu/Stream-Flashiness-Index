library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(patchwork)
library(cowplot)
library(ggthemes)
library("ggsci")
options(scipen = 999)


# add rbi_kendall df
rbi_kendall <- read_csv("results/tables/rbi_kendall_senSlope_1971_2020.csv")

outlierdf <- rbi_kendall %>% 
  filter(highlight == "false")

# create color blind palette
safe_colorblind_palette <- c("#88CCEE",  "grey", "navy", "#117733", "#999933", "#AA4499", "black", 
                             "#44AA99", "#999933", "#882255", "#661100", "orange")
legend_title <- "Type"

# make plots
plot1971 <- ggplot(rbi_kendall, aes(x = rbiValue_1970, y = `estimates.Sen's slope`, color = binary_pvalue))+
  geom_point(aes(shape = highlight), size = 2, show.legend = FALSE)+
  labs(x = "RBI 1971", 
       y = "Theil sen slope")+
  scale_shape_manual(values = c(8, 16))+
  scale_color_manual(legend_title, values = safe_colorblind_palette)+
  theme_classic()+
  #theme_minimal()+
  theme(
        legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 9),
        )+
  annotate(
    geom = "curve", xend = 1.5, yend = -0.027, x = 1, y = -.02, 
    curvature = .2, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = .4, y = -.019, label = "Valley Stream at Valley Stream NY", hjust = "left", size = 3)
  


plot2020 <- ggplot(rbi_kendall, aes(x = rbiValue_2020, y = `estimates.Sen's slope`, color = binary_pvalue))+
  geom_point(aes(shape = highlight), size = 2, show.legend = FALSE)+
  labs(x = "RBI 2020")+
  scale_shape_manual(values = c(8, 16))+
  scale_color_manual(legend_title, values = safe_colorblind_palette, labels = c("non-significant p value", "significant p value"))+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(size = 9),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 15, face = "bold")
    #legend.title = element_blank(),
    #legend.position = "bottom"
  )+
  annotate(
    geom = "curve", xend = 0.4, yend = -0.027, x = 1, y = -.02, 
    curvature = .2, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = .6, y = -.019, label = "Cooper River at Haddonfield NJ", hjust = "left", size = 3)

  

rbi1971_2020_outliers <- plot1971 + plot2020 +  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        plot.tag.position = c(0.2, -0.1))


        
# remove outlier in the data to see the trends at the top and plot results
plot_without_outlier <- rbi_kendall %>% 
  filter(`estimates.Sen's slope` > -0.02)

plot1971_noOutlier <- ggplot(plot_without_outlier, aes(x = rbiValue_1970, y = `estimates.Sen's slope`, color = binary_pvalue))+
  geom_point()+
  labs(x = "RBI 1971", 
       y = "Theil sen slope")+
  scale_color_manual(legend_title, values = safe_colorblind_palette)+
  theme_classic()+
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    #axis.text = element_text(size = 9),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 15, face = "bold")
  )



plot2020_noOutlier <- ggplot(plot_without_outlier, aes(x = rbiValue_2020, y = `estimates.Sen's slope`, color = binary_pvalue))+
  geom_point()+
  labs(x = "RBI 2020")+
  scale_color_manual(legend_title, values = safe_colorblind_palette, labels = c("non-significant p value", "significant p value"))+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 15, face = "bold")
  )



rbi1971_2020_Nooutliers <- plot1971_noOutlier + plot2020_noOutlier +  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 10, r = 10, b = 40, l = 10),
        plot.tag.position = c(0.2, -0.1))


# code for saving graphs
img_rbi <- list(rbi1971_2020_Nooutliers = rbi1971_2020_Nooutliers,
            rbi1971_2020_outliers = rbi1971_2020_outliers)


to_print <- tibble(
  name = names(img_rbi), 
  img = img_rbi,
  filename = paste0(name, ".jpg"),
  path = fs::path(here::here('results/visuals', filename))
)


walk2(
  to_print$img, to_print$path,
  ~ggsave(filename = .y, plot = .x,  # width 12 and height 8
          width = 10, height = 8, dpi = 300, limitsize = FALSE) # used width 25 and height 27 for graph x 
)

## end of code with removed outlier


#table containing the number of sites with trends > 0 and < 0, number of significant trends > 0 and < 0
# results show that there is no obvious trend across the entire region as significant true/false are almost equal
slopes_2020 <- rbi_kendall %>% 
  select(-rbiValue_1970) %>% 
  group_by(binary_pvalue, slope_greater0) %>% 
  tally() %>% 
  dplyr::rename(num_sites2020 = n) 

slopes_1971_2020 <- rbi_kendall  %>% 
  select(-rbiValue_2020) %>% 
  group_by(binary_pvalue, slope_greater0) %>% 
  tally() %>% 
  dplyr::rename(num_sites1971 = n) %>% 
  left_join(slopes_2020)
