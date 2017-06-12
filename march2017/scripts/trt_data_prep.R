
# load packages
pacman::p_load(feather,dplyr,ggplot2,lubridate,geoknife,SPEI,lme4,tidyr,data.table)
select <- dplyr::select

# set working directory
setwd("~/Documents/WAUSP TRT/wateruse-trt/march2017")

# load data
d <- read_feather("data/WaterUse_aggregated_zipcode_pop.feather") %>%
  select(id, state, lat=Y_WGS, lon=X_WGS,  
         year, mg=Mg, pop=Population, source) %>%
  mutate(lat2=lat,
         lon2=lon,
         lat=ifelse(state=="FL",lon2,lat),
         lon=ifelse(state=="FL",lat2,lon)) %>%
  filter(lat < 60) %>%
  filter(lon > -150 & lon < -50) %>%
    dplyr::select(-lat2,-lon2)

# extrapolate
lm1 <- lmer(pop ~  (1 + year | id), data=d)
d$pred_pop <- predict(lm1, newdata=d, allow.new.levels=T)

dfull <- d %>%
  mutate(id = paste0(state,id)) %>%
  mutate(pop=ifelse(is.na(pop),pred_pop,pop)) %>%
  filter(pop > 0) %>%
  select(-pred_pop) %>%
  group_by(id) %>%
  mutate(n_years=n_distinct(year)) %>%
  ungroup() %>%
  filter(n_years>3) %>%
  mutate(year=as.character(year))

# extract coords
coords <- select(dfull, id, lon, lat, state) %>%
  unique()

#write.csv(coords,"trt_all_coords_march_23.csv",row.names = F)

# PRISM precip and temp
id_link <- read.csv("data/wolock_id_link.csv", stringsAsFactors = F)

## column classes
classes <- c("character","character","character","integer","numeric")

## precip
file_list <- paste0("data/pptfiles/",list.files("data/pptfiles"))

precip = rbindlist(lapply(file_list, fread, colClasses = classes)) %>%
  setNames(c("siteno","date","year","month","precip")) %>%
  filter(year >= 1970)

## temp
file_list <- paste0("data/tavfiles/",list.files("data/tavfiles"))

temp = rbindlist(lapply(file_list, fread, colClasses = classes)) %>%
  setNames(c("siteno","date","year","month","tmean")) %>%
  filter(year >= 1970)

## combine temp and precip
climate <- precip %>%
  left_join(temp, by=c("siteno","year","month","date")) %>%
  left_join(id_link,by="siteno") %>%
  mutate(month=month.abb[month]) %>%
  group_by(siteno,month) %>%
  mutate(p_month45yr = mean(precip),
         t_month45yr = mean(tmean)) %>% 
  ungroup() %>%
  data.frame()

# site_sub <- unique(climate$siteno)[1:500]
# climate2 <- filter(climate, siteno %in% site_sub)

# calculate SPEI
## columns=site, rows=months, values=tmean
tmean.wide <- select(climate,siteno,date,tmean) %>%
  spread(siteno,tmean) %>%
  select(-date)

## lat vector, each row associated with column in tmean.wide
lat <- select(climate, siteno, lat) %>%
  unique() %>% 
  select(lat)

## pet using thornthwaite
pet <- data.frame(thornthwaite(tmean.wide,lat$lat)) %>%
  gather(id,pet) 

## Standardized Precipitation-Evapotranspiration Index
ptm <- proc.time()
spei1 <- spei(climate$precip-pet$pet,1)
proc.time() - ptm

## add to climate dataset
climate$pet <- pet$pet
climate$spei1 <- as.numeric(spei1$fitted)

# Combine everything
library(feather)

data_all <- climate %>%
  select(-date,-lat,-lon) %>%
  right_join(dfull,by=c("id","year","state")) %>%
  select(id,siteno,state,lat,lon,year,month,mg,source,pop,
         n_years,precip,tmean,p_month45yr,t_month45yr,pet,spei1)

write_feather(data_all,"data/wutrt_data_full.feather")

# summarize climate data
# d2 <- d %>%
#   mutate(id = paste0(state,id)) %>%
#   select(id,source) %>%
#   distinct(id,source)

data_all <- read_feather("data/wutrt_data_full.feather")

model_data <- data_all %>%
  mutate(t_anom = tmean-t_month45yr,
         p_anom = precip-p_month45yr,
         summer = ifelse(month %in% c("Jun","Jul","Aug"),1,0)) %>%
  group_by(siteno,summer,year) %>%
  mutate(p_summer_mean = mean(p_month45yr),
         t_summer_mean = mean(t_month45yr),
         p_summer_anom = mean(p_anom),
         t_summer_anom = mean(t_anom),
         spei1_min = min(spei1)) %>%
  distinct(siteno,year,.keep_all=T) %>%
  filter(summer==1) %>%
  ungroup() %>%
  #left_join(d2,by="id") %>%
  select(-month,-precip,-tmean,-spei1,-p_month45yr,
         -t_month45yr,-t_anom,-p_anom,-summer,-pet) 

write_feather(model_data,"data/wutrt_model_data.feather")



