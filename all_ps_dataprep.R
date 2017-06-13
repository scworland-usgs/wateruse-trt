

pacman::p_load(dplyr, ggplot2, magrittr, reshape2, stringr, ggmap, mapdata,
               choroplethr, choroplethrMaps, lubridate, gridExtra, ggthemes)

# set working directory
setwd("~/Documents/WAUSP TRT/wateruse-trt")

# load data
load("fl_ps_trt.rda")
load("va_ps_trt.rda")
load("pa_ps_trt.rda")

# join
ps.all <- rbind(fl.data,pa.data,va.data) 

new_id <- ps.all %>%
  group_by(siteid) %>%
  group_indices()

ps.all <- ps.all %>%
  mutate(id=as.character(new_id),
         id=paste0(state,id)) %>%
  select(id,old_id=siteid,state,lon,lat,year,MG,source) 
  
write.csv(ps.all,file="public_supply_data_pa_va_fl.csv")

ggplot(ps.all) + geom_density(aes(MG, fill=state), alpha=0.5) + 
  scale_x_log10() + theme_bw()  + 
  labs(x="log10(mgd)") +
  geom_vline(xintercept=1,linetype="dashed") +
  ggtitle("log10(MG) densities for states", 
          subtitle="Dashed line is = 1 million gallons")

  


# subset unique coords for plotting
ps.all.coords <- ps.all %>%
  distinct(uid) %>%
  select(lat,lon,state)

# map
nat <- map_data('state')
sub = subset(map_data('state'), region %in% 
              c("pennsylvania","virginia","florida",
                "georgia","south carolina",
                "north carolina","west virginia",
                "delaware","maryland","new jersey",
                "alabama","tennessee","kentucky",
                "ohio"))

ggplot() + geom_polygon(data=sub,aes(long,lat, group=group), fill="white", color="black", size=1) +
  geom_point(data=ps.all.coords, aes(lon,lat,color=state),  size=0.5, alpha=0.7) + 
  coord_fixed(1.3) + theme_nothing() + xlim(-92,-70) +
  annotate("text",x=-77, y=43, label="N = 5061", color="green4") + #pa
  annotate("text",x=-78, y=28, label="N = 1792", color="coral1") + #fl
  annotate("text",x=-73, y=38, label="N = 1646", color="dodgerblue")  #va
  

# distributions
ggplot(ps.all) + geom_density(aes(mgd, fill=state), alpha=0.5) + 
  scale_x_log10() + theme_bw()  + 
  geom_vline(xintercept=1e-4,linetype="dashed") +
  facet_wrap(~year) +
  labs(x="",y="") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank())

  




