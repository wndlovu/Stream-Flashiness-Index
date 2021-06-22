library(readr)
library(tidyverse)
library(lubridate)

# create trend analysis df with the important variables
trend_analysis_df <- read_csv("mannKendall_analysis_df.csv") %>% 
  left_join(read_csv("sensSlope_analysis_df.csv")) %>% 
  select(site_no, tau, estimates,  S, sl, p.value)

# export df as .csv
write_csv(trend_analysis_df, "trend_analysis_df.csv")
