
pacman::p_load(dplyr, ggplot2, magrittr, reshape2, stringr)

# set working directory
setwd("~/Documents/WAUSP TRT/wateruse-trt")

# set "select" function to dplyr's default
select = dplyr::select

# Load population data 
# I don't use the pop data because it is still census data
pa.pop <- read.csv("~/Documents/WAUSP TRT/data/public supply/PA/pa_pop_served.csv") %>%
  mutate(siteid = as.character(WUDS_FACILITY_ID), pop_served = TtlPop2010) %>% 
  select(siteid,pop_served)

# Load water use data
pa.data <- read.csv("~/Documents/WAUSP TRT/data/public supply/PA/pa_quantity_data.csv") %>%
  select(siteid=From.Station.Name,lat=From.Latitude,lon=From.Longitude,year=Year,
         gallons=Annual.Value,source=From.Site.Type.Code) %>%
  mutate(MG = gallons/1e6,
         source=ifelse(source=="FA-DV","SW","GW"),
         lon = (as.numeric(substring(lon, first=1, last=2)) + 
                  as.numeric(substring(lon, first=3, last=4))/60 + 
                  as.numeric(substring(lon, first=5, last=7))/3600)*-1,
         siteid = str_sub(siteid, start= -5),
         lat = as.numeric(substring(lat, first=1, last=2)) + 
           as.numeric(substring(lat, first=3, last=4))/60 + 
           as.numeric(substring(lat, first=5, last=7))/3600,
         state = "PA") %>%
  select(siteid,state,lon,lat,year,MG,source)

save(pa.data, file="pa_ps_trt.rda")

# days in month code
# days = days_in_month(ymd(paste(year,sprintf("%02d",month),"01", sep="")))

## plot monthly values for every site in PA  


