
pacman::p_load(dplyr, ggplot2, magrittr, reshape2, stringr, ggmap, mapdata,
               choroplethr, choroplethrMaps, lubridate, gridExtra, ggthemes)

# set working directory
setwd("~/Documents/WAUSP TRT/wateruse-trt")

# set "select" function to dplyr's default
select = dplyr::select

# load coordinates
fl.coords <- read.csv("~/Documents/WAUSP TRT/data/public supply/FL/FL_WS_permit_site_locations.csv") %>%
  mutate(permit_tx=as.character(permit_tx),
         agency_cd=as.character(agency_cd)) %>%
  group_by(permit_tx) %>%
  dplyr::summarize(lat=mean(lat),
            lon=mean(lon)) 

# Load withdrawal data
fl.data <- read.csv("~/Documents/WAUSP TRT/data/public supply/FL/FloridaPublicSupplyData.csv") %>%
  select(permit_tx,year=cn_qnty_yr,gallons=cn_qnty_mo_va,source=water_cd) %>%
  mutate(permit_tx=as.character(permit_tx),
         source=as.character(source)) %>%
  group_by(year,permit_tx) %>%
  dplyr::summarize(MG = sum(gallons)/1e6) %>%
  ungroup() %>%
  left_join(fl.coords, by="permit_tx") %>%
  filter(complete.cases(lat)) %>% # remove NA lat values
  filter(lat!=0) %>% # remove lat == 0 values
  arrange(permit_tx,year) %>%
  mutate(diff_year = year-lag(year)) %>%
  filter(abs(diff_year) != 5) %>%
  filter(abs(diff_year) != 10) %>%
  filter(abs(diff_year) != 15) %>%
  filter(abs(diff_year) != 20) %>%
  select(-diff_year) %>%
  mutate(state="FL",
         source="GW") %>%
  select(siteid=permit_tx,state,lon,lat,year,MG,source)
  
save(fl.data, file="fl_ps_trt.rda")
  

  
  
  
  


fl.yr.total <- fl.data2 %>%
  group_by(year) %>%
  summarize(total = sum(total,na.rm = T)) %>%
  mutate(cum.total = cumsum(total)) %>%
  mutate(cum.norm = cum.total/max(cum.total))


# make map
fl = subset(map_data('state'), region %in% c("florida"))

ggplot() + geom_polygon(data=fl,aes(long,lat, group=group), fill="grey45", color="black", size=1) +
  geom_point(data=fl.data2, aes(lon,lat, fill=total), shape=21, color="black", size=2) + 
  theme_nothing() + coord_fixed(1.3) +
  scale_fill_gradient2(low="red", mid="yellow", high="blue",trans="log10") 



# work on mosaic plot
hold <-table(fl.data2$region, fl.data2$year)
mosaicplot(t(hold))

# foor loop for plot
yrs <- unique(fl.data2$year)
pth = "C:/Users/scworlan/Documents/WAUSP TRT/wateruse-trt/fl_figures"


for (i in 1:length(yrs)) {
  fl.yr <- fl.data2 %>% filter(year==yrs[i])
  
  m1 <- ggplot() + geom_polygon(data=fl,aes(long,lat, group=group), fill="grey45", color="black", size=1) +
    geom_point(data=fl.yr, aes(lon,lat, fill=total), shape=21, color="black", size=2) + 
    theme_nothing() + coord_fixed(1.3) +
    scale_fill_gradient2(low="red", mid="yellow", high="blue",trans="log10") +
    annotate("text",x=-84, y=32, label=paste("Public supply withdrawals for",yrs[i]," "))
  
  p1 <- ggplot(fl.yr.total[1:i,],aes(year,total/365)) + 
    geom_line(size=1) + geom_point(size=2) +
    ylab("millions of gallons") + theme_tufte(base_size=15) + 
    geom_rangeframe(data=fl.yr.total,aes(year,cum.norm)) +
    xlim(1978,2012) + 
    ylim(min(fl.yr.total$total/365),max(fl.yr.total$total/365))
  
  #grid.arrange(p1, m1, heights = unit(0.7, "npc"),nrow=1)
  
  out <- arrangeGrob(p1, m1, heights = unit(0.7, "npc"),nrow=1)
  
  ggsave(out,filename=paste("FL_public_supply",i,".png",sep=""),path=pth)
}
       

fl.data3 <- fl.data %>%
  select(uid = permit_tx, year, month, mgd, lat, lon) %>%
  mutate(state = "FL")




