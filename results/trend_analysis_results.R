library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(patchwork)

# create trend analysis df with the important variables
trend_analysis_df <- read_csv("mannKendall_analysis_df.csv") %>% 
  left_join(read_csv("sensSlope_analysis_df.csv")) %>% 
  select(site_no, tau, estimates,  S, sl, p.value)

# export df as .csv (file already provided)
#write_csv(trend_analysis_df, "trend_analysis_df.csv")

#Other part of results to export into a CSV: RBI value
#for each site, 1970, 2020
rbiWy_1971_2020 <- read_csv("Trend_Analysis_Code/rbiWy_dfAll.csv") %>% 
  filter(waterYear == c(1971, 2020))


rbiWy_1971_2020 <- data.frame(t(rbiWy_1971_2020)) 
rbiWy_1971_2020 <- setDT(rbiWy_1971_2020, keep.rownames = TRUE)[] %>% 
  row_to_names(row_number = 1) %>% 
  dplyr::rename(site_no = waterYear,
                rbiValue_1970 = '1971',
                rbiValue_2020 = '2020')
  
#write_csv(rbiWy_1971_2020, "rbiWy_1971_2020.csv")

plot_data <- rbiWy_1971_2020 %>% 
  left_join(trend_analysis_df) %>% 
  mutate(binary_pvalue = as.factor(ifelse(p.value < 0.05, 1, 0)))

plot1971 <- ggplot(plot_data, aes(x = rbiValue_1970, y = estimates, color = binary_pvalue))+
  geom_point()

plot2020 <- ggplot(plot_data, aes(x = rbiValue_2020, y = estimates, color = binary_pvalue))+
  geom_point()

plot1971 + plot2020
