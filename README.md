---
output:
  pdf_document: default
  html_document: default
---

# Effects of land use and precipitation patterns on streamflow flashiness trends in the northeast USA

### What is stream flashiness?

- Stream flashiness is the rapidity and frequency of short term changes
  in the streamflow especially during runoff events.
- Calculated as the ratio between the sum of daily changes in discharge
  and sum of daily discharge

### Abstract

Examining long term trends in streamflow signatures is useful for
understanding how changes in land use and precipitation patterns impact
streamflow regimes. One particular metric of streamflow regimes that
encapsulates flow variations is the Richards-Baker flashiness index.
High flashiness is commonly associated with imperviousness, agricultural
land management, water management and dam removal, and other human
activities. Therefore, examining trends in this signature may provide a
lens to examine how streamflow regimes in both flashy and stable
watersheds are responding to human activity. In this study, we evaluate
the effects of human activities and precipitation pattern changes on
streamflow flashiness for 304 watersheds across the northeastern US. We
quantified trends in annual Richards-Baker Flashiness Indices for the
period of 1970 - 2020 using publicly available streamflow estimates from
the USGS. To interpret drivers of these trends, we used land use data
from the GAGESII dataset to 1) identify sites where dams were removed or
where dam storage increased and 2) classify watersheds into urban ( \>
20% imperviousness values) and rural (\< 20% imperviousness values).
From the 1970s, there has been little change in the proportion of urban
and rural watersheds. However, dam removals increased between 1980 and
2012, though we detected no difference in trends between sites where
dams were removed and those where no dams were removed. There is also
evidence that urban watersheds and watersheds in high snowfall states
(Vermont and Maine) exhibited positive trends in flashiness indices.
Overall, there are an equal number of positive and negative trends,
indicating that flashiness indices across the NE are both increasing and
decreasing. These results give us insight into how watershed regimes
respond to changing climate, infrastructure, and other associated human
impacts.

Procedure: To reproduce this analysis you will need to install the
following packages:

Available on CRAN: \* `tidyverse` \* `lubridate` \* `ggx` \* `geosphere`
\* `ggthemes` \* `ggsci` \* `ggpubr` \* `maps` \* `sf` \* `sp` \*
`rnaturalearth` \* `rnaturalearthdata` \* `ggalt` \* `usmap` \* `ggsn`
\* `ggsci` \* `patchwork` \* `cowplot` \* `readr`

Available on Github: \* `easyrbi`

To install packages run the following:

``` r
# install.packages("remotes") 
install.packages(c("tidyverse", "dataRetrieval", "lubridate", 
"ggx", "geosphere", "ggthemes", "ggsci", "ggpubr", "maps", "sf",
"sp", "rnaturalearth", "rnaturalearthdata",  "ggalt", "usmap",
"ggsn", "ggsci", "patchwork", "cowplot",  "readr"))

remotes::install_github("amutaya/easyrbi")
```
