install.packages("dataRetrieval")

library(ggplot2)
library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(trend)
library(Kendall)
library(plyr)
library(purrr)
library(zoo)


# first import rbiWy_dfAll.csv file from the folder

mannKendall_analysis_df <- data.frame()
sensSlope_analysis_df <- data.frame()
timeSeries <- data.frame(matrix(ncol = 2, nrow = nrow(rbiWy_dfAll)))
# loop that calculates the trends 
for (i in 2:length(rbiWy_dfAll)){
  timeSeries$X1 <- rbiWy_dfAll$waterYear # populate timeSeries with waterYear and rbiValue
  timeSeries$X2 <- rbiWy_dfAll[i]
  trend_timeSeries <- as.ts(read.zoo(timeSeries)) # tranform df to time-series
  
  # mannKendall analysis - working
  mannKendall_analysis <- MannKendall(trend_timeSeries)# perform MannKendall Analysis
  resultsMK_all <- data.frame(t(ldply(mannKendall_analysis, data.frame))) # tranform data and make column 1 values the variable names
  colnames(resultsMK_all) <- resultsMK_all[1, ] # make row 1 values the headers
  resultsMK_all <- resultsMK_all[- 1, ] # remove row 1
  mannKendall_analysis_df <- rbind(mannKendall_analysis_df,resultsMK_all)
  
  # sensSlope analysis
  # sensSlope stuff not working because some values are stored as lists of lists, so we only have 98 obs instead of 304 
  sensSlope_analysis <- sens.slope(trend_timeSeries)
  resultsSS_all <- data.frame(t(ldply(sensSlope_analysis, data.frame)))
  colnames(resultsSS_all) <- resultsSS_all[1, ]
  resultsSS_all <- resultsSS_all[- 1, ]
  sensSlope_analysis_df <- rbind(sensSlope_analysis_df,resultsSS_all)
}

# mannKendall
mannKendall_analysis_df[,6 ] <- colnames(rbiWy_dfAll)[-1] # add 6th column = variable names from rbiWy_dfAll beginning at column 2
colnames(mannKendall_analysis_df)[6] <- "site_no"
rownames(mannKendall_analysis_df) <- NULL

mannKendall_analysis_df <- mannKendall_analysis_df %>% 
  select(site_no, tau, sl, S, D, varS)

# sens.slope - rename the variables??
sensSlope_analysis_df[,11 ] <- colnames(rbiWy_dfAll)[-1] # add 11th column = variable names from rbiWy_dfAll beginning at column 2
colnames(sensSlope_analysis_df)[10] <- "conf.int2"
colnames(sensSlope_analysis_df)[11] <- "site_no"
colnames(sensSlope_analysis_df)[10]
rownames(sensSlope_analysis_df) <- NULL

sensSlope_analysis_df <- sensSlope_analysis_df %>% 
  select(site_no, estimates, statistic, p.value, null.value, alternative, 
         data.name, method, parameter, conf.int, conf.int2)





