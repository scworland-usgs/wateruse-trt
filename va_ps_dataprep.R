
pacman::p_load(dplyr, ggplot2, magrittr, reshape2, stringr, ggmap)

# set working directory
setwd("~/Documents/WAUSP TRT/wateruse-trt")

# set "select" function to dplyr's default
select = dplyr::select

# Load withdrawal data
va.data <- read.csv("~/Documents/WAUSP TRT/data/public supply/VA/PWSWells_WaterUse_06172016.csv") %>%
  rbind(read.csv("~/Documents/WAUSP TRT/data/public supply/VA/PWSIntakes_WaterUse_06282016.csv")) %>%
  select(siteid=VA.Hydro.ID.of.Source,lon = Longitude, lat = Latitude, year=Year, 
         MG=Million.Gallons.per.Year, source=Source.Type) %>%
  mutate(source=ifelse(source=="Well","GW","SW"),
         state="VA",
         siteid=as.character(siteid)) %>%
  filter(lat < 45 & lat > 36.5, lon < -50) %>%
  select(siteid,state,lon,lat,year,MG,source)

save(va.data, file="va_ps_trt.rda")
 
# map
va = subset(map_data('state'), region=="virginia")

ggplot(va.data) + geom_polygon(data=va,aes(long,lat, group=group), 
                               fill="white",color="black") +
  geom_point(aes(lon,lat, color=Source.Type)) + 
  scale_color_manual(values=c("darkblue","orange"), 
                     labels=c("GW","SW"),name="Source") +
  theme_classic()

# dataframes for plotting
va.plots <- va.data %>% group_by(Year) %>%
  summarize(p60=quantile(value, probs=0.6),
            p50=quantile(value, probs=0.5),
            p40=quantile(value, probs=0.4)) 

ggplot(va.plots)  + geom_ribbon(aes(Year,ymin=p40, ymax=p60), alpha=0.5) +
  geom_line(aes(Year,p50), color="blue", size=1) + ylab("Mil. Gallons/year") +
  geom_point(aes(Year,p50), fill="dodgerblue", color="black",size=3, shape=21) +
  theme_bw()+ scale_x_continuous(breaks = 2000:2014) +
  ggtitle("Time series for VA Public Supply")



va.data2 <- va.data %>%
  select(uid=siteid, year, value, lat, lon) %>%
  mutate(month=NA, days = NA, state= "VA", mgd=value/365) %>%
  select(uid,year, month, days, mgd, lat, lon, state)




