remotes::install_github("amutaya/easyrbi")

library(tidyverse)
library(readr)
library(easyrbi)

site_nums <- read_csv("data/nesites.csv")%>% # read sites list into R
  mutate(STAID = as.character(paste0("0", STAID))) # add a 0 to start of all sites

site_nums <- as.character(site_nums$STAID) # re-save data as array

# only run if interested in the following gauge station info
# station_nm, lat_va, long_va, dec_lat_va, dec_long_va, coord_datum_cd,
# dec_coord_datum_cd, district_cd, state_cd, county_cd, country_cd, alt_va,
# drain_area_va, X_00060_00003


#sites_data_df = easyrbi::sitedata(site_num = site_nums, startDate = "1970-10-01", endDate = "2020-09-30")

# calculate rbi values for given sites
rbi_values_df = rbi_df(site_num = site_nums, startDate = "1970-10-01", endDate = "2020-09-30")

# save df as .csv
#write_csv(rbiWy_dfAll, 'results/tables/rbiWy_dfAll.csv')