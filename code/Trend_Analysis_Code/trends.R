remotes::install_github("amutaya/easyrbi")

library(tidyverse)
library(readr)
library(easyrbi)
options(scipen = 999)

# use Mann Kendall to check if there are changes in the rbi values
rbiWy_dfAll <- read_csv("results/tables/rbiWy_dfAll.csv")

kendall_senSlope_df = trends(rbiWy_dfAll)

# export df as .csv (file already provided)
#write_csv(trend_analysis_df, "trend_analysis_df.csv")

#Other part of results to export into a CSV: RBI value
#for each site -> 1971 & 2020
rbiWy_1971_2020 <- rbiWy_dfAll%>% 
  filter(waterYear == c(1971, 2020))



# transform df from series and include site_no and rbi values for 1971 and 2020
rbiWy_1971_2020 <- data.frame(t(rbiWy_1971_2020))  

rbiWy_1971_2020 <-  data.table::setDT(rbiWy_1971_2020, keep.rownames = TRUE)[] %>% 
  janitor::row_to_names(row_number = 1) %>% 
  dplyr::rename(site_no = waterYear,
                rbiValue_1970 = '1971',
                rbiValue_2020 = '2020')


# join trend analysis df with the rbi data
rbi_kendall_senSlope <- rbiWy_1971_2020 %>% 
  left_join(kendall_senSlope_df, by ='site_no') %>% 
  mutate(binary_pvalue = as.factor(ifelse(p.value < 0.05, 'significant', 'non-significant')), # when p.value is <0.05, binary_pvalue = significant
         slope_greater0 = as.factor(ifelse(`estimates.Sen's slope` > 0, 'true', 'false')), # when slope is greater than 0, slope_greater0 = true
         highlight = as.factor(ifelse(`estimates.Sen's slope` > -0.02, 'true', 'false'))) # when slope is greater than 0, slope_greater0 = true

# save df to results
#write_csv(rbi_kendall_senSlope, "results/tables/rbi_kendall_senSlope_1971_2020.csv")
