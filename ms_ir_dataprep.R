
pacman::p_load(dplyr, ggplot2, magrittr, reshape2, stringr, ggmap, mapdata,
               choroplethr, choroplethrMaps, lubridate, gridExtra)

# set working directory
setwd("~/WAUSP TRT/wateruse-trt")

# set "select" function to dplyr's default
select = dplyr::select

# Load withdrawal data exclusion codes 
ms.ir.ex <- read.csv("~/WAUSP TRT/data/irrigation/MS/MDEQ_DeltaWU2015_exclusion_codes.csv")
ms.ir <- read.csv("~/WAUSP TRT/data/irrigation/MS/MDEQ_DeltaWU2015_excluded.csv") %>%
  filter(!Exclusion_Code %in% ms.ir.ex$code)

# count frequencies of crop types
crop.count <- data.frame(table(ms.ir$Crop1Type)) %>% 
  select(crop.type=Var1, crop.freq=Freq)

# caclulate days between start and end
dates <- data.frame(start = strptime(as.character(ms.ir$StartReadingDate),format = "%m/%d/%Y"),
                    end = strptime(as.character(ms.ir$EndReadingDate),format = "%m/%d/%Y"))

dates$dif <- as.numeric(difftime(dates$end, dates$start, units = c("days")))

# modify data
ms.ir <- ms.ir %>%
  rename(lat = Latitude, lon = Longitude, crop.type=Crop1Type) %>%
  mutate(volume = EndReading-StartReading) %>%
  mutate(days = dates$dif) %>%
  mutate(AF.days = AF.Acre/days) %>%
  inner_join(crop.count, by="crop.type") %>%
  mutate(rel.freq = crop.freq/sum(unique(crop.freq)))


# boxplot
ms.ir.plot <- ms.ir %>% 
  filter(crop.freq > 15) %>%
  select(crop.type, rel.freq, AF.days, AF.Acre, lat, lon, days) %>%
  filter(complete.cases(.)) 
  
ggplot(ms.ir.plot) + geom_boxplot(aes(reorder(crop.type, AF.days, FUN=median),AF.days, fill=rel.freq), 
                                  outlier.shape = NA, alpha=0.7) + 
  scale_fill_gradient2(low="blue", mid="khaki1", high="orange",midpoint=0.35) + ylim(0,0.03) +
  labs(y="Acre feet/Acre/days", x="", fill="rel. freq") + theme_bw() + coord_flip()

ggplot(ms.ir.plot) + geom_point(aes(days,AF.Acre)) + 
  facet_wrap(~crop.type) + labs(y="Total Acre Feet per Acre", x = "Days of Record")

# map
us.rivers <- filter(map_data("rivers"), lat > 27 & lat < 36 & long > -95 & long < -89) %>%
  distinct(lat, long)

ms = subset(map_data('state'), region %in% c("mississippi","arkansas", "louisiana"))

m1 <- ggplot(ms.ir) + 
  geom_path(data=us.rivers, aes(x=long, y=lat, group=group),color="blue", size=2) +
  geom_point(aes(lon,lat, color=AF.Acre)) + coord_fixed(0.8) +
  coord_cartesian(ylim=c(32.5, 35),xlim=c(-91.5, -89.7)) + theme_bw() +
  scale_color_gradient2(low="blue", mid="orange", high="red",midpoint=3) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  labs(color="AF/acre")


m2 <- ggplot(ms.ir) + geom_polygon(data=ms,aes(long,lat, group=group), fill="grey80", color="black", size=1) +
  geom_path(data=us.rivers, aes(x=long, y=lat, group=group),color="blue", size=1) +
  geom_point(aes(lon,lat, color=AF.Acre), alpha=0.6) + coord_fixed(1.2) + theme_nothing() +
  scale_color_gradient2(low="blue", mid="orange", high="red",midpoint=3)
  
grid.arrange(m2,m1, ncol=2)






