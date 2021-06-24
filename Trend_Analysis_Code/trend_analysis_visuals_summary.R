library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(patchwork)
library(cowplot)
library(ggthemes)
library(ggthemr)
library("ggsci")
options(scipen = 999)

# create trend analysis df with the important variables
trend_analysis_df <- read_csv("results/mannKendall_analysis_df.csv") %>% 
  left_join(read_csv("results/sensSlope_analysis_df.csv")) %>% 
  select(site_no, tau, estimates,  S, sl, p.value)

# export df as .csv (file already provided)
#write_csv(trend_analysis_df, "trend_analysis_df.csv")

#Other part of results to export into a CSV: RBI value
#for each site -> 1971 & 2020
rbiWy_1971_2020 <- read_csv("Trend_Analysis_Code/rbiWy_dfAll.csv") %>% 
  filter(waterYear == c(1971, 2020))

# transform df from series and include site_no and rbi values for 1971 and 2020
rbiWy_1971_2020 <- data.frame(t(rbiWy_1971_2020))  

rbiWy_1971_2020 <-  setDT(rbiWy_1971_2020, keep.rownames = TRUE)[] %>% 
  row_to_names(row_number = 1) %>% 
  dplyr::rename(site_no = waterYear,
                rbiValue_1970 = '1971',
                rbiValue_2020 = '2020')
  
# join trend analysis df with the rbi data
plot_data <- rbiWy_1971_2020 %>% 
  left_join(trend_analysis_df) %>% 
  mutate(binary_pvalue = as.factor(ifelse(p.value < 0.05, 'significant', 'non-significant')), # when p.value is <0.05, binary_pvalue = significant
         slope_greater0 = as.factor(ifelse(estimates > 0, 'true', 'false'))) # when slope is greater than 0, slope_greater0 = true

# create color blind palette
safe_colorblind_palette <- c("#88CCEE",  "grey", "navy", "#117733", "#999933", "#AA4499", "black", 
                             "#44AA99", "#999933", "#882255", "#661100", "orange", "orange")

# make plots
plot1971 <- ggplot(plot_data, aes(x = rbiValue_1970, y = estimates, color = binary_pvalue))+
  geom_point()+
  labs(x = "RBI 1971", 
       y = "Theil sen slope")+
  scale_color_manual(legend_title, values = safe_colorblind_palette)+
  theme_clean()+
  theme_bw() +
  theme(
        legend.title = element_blank(),
        legend.position = "none",
        #axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        )
  
legend_title <- "Type"

plot2020 <- ggplot(plot_data, aes(x = rbiValue_2020, y = estimates, color = binary_pvalue))+
  geom_point()+
  labs(x = "RBI 2020")+
  scale_color_manual(legend_title, values = safe_colorblind_palette, labels = c("non-significant p value", "significant p value"))+
  theme_clean()+
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(size = 9),
    #legend.title = element_blank(),
    #legend.position = "bottom"
  )

plot1971 + plot2020 +  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 10, r = 10, b = 40, l = 10),
        plot.tag.position = c(0.2, -0.1))

        
        
# remove outlier in the data to see the trends at the top and lot results
plot_without_outlier <- plot_data %>% 
  filter(estimates > -0.02)

plot1971_2 <- ggplot(plot_without_outlier, aes(x = rbiValue_1970, y = estimates, color = binary_pvalue))+
  geom_point()+
  labs(x = "RBI 1971", 
       y = "Theil sen slope")+
  scale_color_manual(legend_title, values = safe_colorblind_palette)+
  theme_clean()+
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 9)
  )

plot2020_2 <- ggplot(plot_without_outlier, aes(x = rbiValue_2020, y = estimates, color = binary_pvalue))+
  geom_point()+
  labs(x = "RBI 2020")+
  scale_color_manual(legend_title, values = safe_colorblind_palette, labels = c("non-significant p value", "significant p value"))+
  theme_clean()+
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(size = 9),
  )

plot1971_2 + plot2020_2 +  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 10, r = 10, b = 40, l = 10),
        plot.tag.position = c(0.2, -0.1))
## end of code with removed outlier


#table containing the number of sites with trends > 0 and < 0, number of significant trends > 0 and < 0
# results show that there is no obvious trend across the entire region as significant true/false are almost equal
slopes_2020 <- plot_data %>% 
  select(-rbiValue_1970) %>% 
  group_by(binary_pvalue, slope_greater0) %>% 
  count() %>% 
  dplyr::rename(num_sites2020 = n) 

slopes_1971_2020 <- plot_data %>% 
  select(-rbiValue_2020) %>% 
  group_by(binary_pvalue, slope_greater0) %>% 
  count() %>% 
  dplyr::rename(num_sites1971 = n) %>% 
  left_join(slopes_2020)
