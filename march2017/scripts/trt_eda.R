
# load packages
pacman::p_load(feather,dplyr,ggplot2,lme4,tidyr,rethinking)
select <- dplyr::select

# set working directory
setwd("~/Documents/WAUSP TRT/wateruse-trt/march2017")

# load data
model_data <- read_feather("data/wutrt_model_data.feather")

# map of sites
coords <- select(model_data, id, lon, lat, state) %>%
  unique()

state.poly <- map_data("state")

ggplot() + 
  geom_polygon(data=state.poly, aes(x=long, y=lat, group=group), fill="grey100", color="black") +
  geom_point(data=coords, aes(x=lon, y=lat, color=state), alpha=0.6, size=0.2) +
  theme_void() + 
  coord_fixed(1.3) 

# mgd density plot by state
ggplot(model_data) + geom_density(aes(mg, fill=state), alpha=0.5) + 
  scale_x_log10() + theme_bw()  + 
  labs(x="log10(mg)") +
  geom_vline(xintercept=0.1,linetype="dashed") +
  ggtitle("log10(MG) densities for states", 
          subtitle="Dashed line is = 1 MGD")

# mgd density plot by state ~ year
ggplot(model_data) + geom_density(aes(mg, fill=state), alpha=0.5) + 
  scale_x_log10() + theme_bw()  + 
  geom_vline(xintercept=0.1,linetype="dashed") +
  facet_wrap(~year) +
  labs(x="",y="") +
  theme(axis.text=element_blank(),
        axis.ticks=element_blank()) +
  ggtitle("Annual log10(MG) densities for states", 
          subtitle="Dashed line is = 1 MGD")

# summarize data by state
state_mgd <- model_data %>%
  group_by(state) %>%
  summarize(num_sites=length(unique(id)),
            num_years=length(unique(year)),
            prop_zero=round(sum(mgd == 0)/n(),2),
            p95_mgd=round(quantile(mgd,0.95),2),
            max_mgd=round(max(mgd),2))

