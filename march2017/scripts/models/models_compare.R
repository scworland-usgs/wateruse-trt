
# load packages
pacman::p_load(feather,dplyr,ggplot2,lme4,tidyr,rethinking,stringr,viridis)
select <- dplyr::select

# set working directory
setwd("~/Documents/WAUSP TRT/wateruse-trt/march2017")

# load data
model_data <- read_feather("data/wutrt_model_data.feather") %>%
  mutate(aridity = p_summer_mean/(t_summer_mean + 33),
         spei1_min = ifelse(spei1_min==-Inf,0,spei1_min),
         log_mg = log(mg + 0.001),
         state_index = coerce_index(state),
         source_index = ifelse(source=="SW",0,1)) %>%
  mutate_each(funs(scaled=scale), c(pop,t_summer_mean,p_summer_mean,
                                    p_summer_anom,t_summer_anom)) %>%
  filter(mg < quantile(mg,0.99)) %>%
  na.omit() %>%
  data.frame() 

# link for state
state_link <- model_data %>%
  distinct(state,state_index)

# read in models
m0 <- readRDS("scripts/models/m0.rds")
m1 <- readRDS("scripts/models/m1.rds")
m2 <- readRDS("scripts/models/m2.rds")
m3 <- readRDS("scripts/models/m3.rds")
m4 <- readRDS("scripts/models/m4.rds")
m5 <- readRDS("scripts/models/m5.rds")
m6 <- readRDS("scripts/models/m6.rds")
m7 <- readRDS("scripts/models/m7.rds")

# compare
compare(m0,m1,m2,m3,m4,m5,m6,m7)

# plot parameters for best model
pars = names(m7@stanfit@par_dims)[6:12]

params_plot <- precis(m7,depth=2,pars=pars)@output %>% 
  setNames(c("mean","std","lower","upper","n_eff","rhat")) %>%
  mutate(params=row.names(.),
         state_index=as.numeric(gsub("\\D", "", params)),
         params=gsub("_.*$", "", params)) %>%
  left_join(state_link) %>%
  data.frame() %>%
  na.omit() %>%
  mutate(state=factor(state, levels = state[order(mean)]))

ggplot(params_plot) + 
  geom_pointrange(aes(x=state,y=mean,ymin=lower,ymax=upper),size=0.3) +
  facet_wrap(~params, scales="free_x") + 
  geom_hline(yintercept=0, linetype="dashed") +
  coord_flip() + 
  theme_bw()

# retrodictive predictions
m7.link <- link(m7, data=model_data)

# summarize
mu <- apply(m7.link,2,mean)
pi <- apply(m7.link,2,PI, prob=0.95)

# pred data set
preds <- model_data %>%
  select(siteno,lat,lon,state,year,mg) %>%
  mutate(pred = mu,
         low = pi[1,],
         high = pi[2,],
         resid = pred-mg,
         in_pi = ifelse(mg > low & mg < high,1,0)) 

# custom function to select entire groups
sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  grps = tbl %>% 
    groups %>% 
    lapply(as.character) %>% 
    unlist
  
  keep = tbl %>% 
    summarise() %>% 
    ungroup() %>% 
    sample_n(size, replace, weight)
  
  tbl %>% 
    right_join(keep, by=grps) %>% 
    group_by_(.dots = grps)
}


# randomly plot sites for each state
dsub <- preds %>%
  filter(state=="PA") %>%
  mutate(year=as.integer(year)) %>%
  group_by(siteno) %>%
  mutate(n_years=n_distinct(year)) %>%
  filter(n_years > 5) %>%
  sample_n_groups(16) %>%
  ggplot() +
  geom_line(aes(year,mg)) +
  geom_line(aes(year,pred),color="red") +
  geom_line(aes(year,low),color="red",linetype="dashed") +
  geom_line(aes(year,high),color="red",linetype="dashed") +
  facet_wrap(~siteno,scales="free_y") +
  theme_bw()


# subset and plot "good" and "bad" sites for each state
good <- paste0("site",c("8293","14833","7712","9487","3654","6154","642"))
bad <- paste0("site",c("8289","14897","7559","10376","2452","6128","311"))

good_sites <- preds %>%
  filter(siteno %in% good) %>%
  mutate(year = as.integer(year),
         type = "Well predicted sites") 

bad_sites <- preds %>%
  filter(siteno %in% bad) %>%
  mutate(year = as.integer(year),
         type = "Poorly predicted sites")

all_sites <- rbind(good_sites,bad_sites)

ggplot(all_sites) + geom_line(aes(year,mg)) +
  geom_line(aes(year,pred,color=state)) +
  geom_line(aes(year,low,color=state),linetype="dashed") +
  geom_line(aes(year,high,color=state),linetype="dashed") +
  facet_grid(state~type,scales="free") +
  theme_bw() + 
  labs(y="millions of gallons") +
  guides(color=F)


# state MG for all years 
state <- preds %>%
  group_by(year,state) %>%
  summarize_each(funs(sum),mg,pred,high,low) %>%
  ungroup() %>%
  mutate(year=as.integer(year),
         in_pi = ifelse(mg > low & mg < high,1,0))

ggplot(state) + geom_line(aes(year,mg)) +
  geom_line(aes(year,pred,color=state)) +
  geom_line(aes(year,low,color=state),linetype="dashed") +
  geom_line(aes(year,high,color=state),linetype="dashed") +
  facet_grid(state~.,scales="free_y") +
  theme_bw() + scale_y_continuous(labels = comma) +
  guides(color=F)

# total MG for all years 
total <- preds %>%
  group_by(year) %>%
  summarize_each(funs(sum),mg,pred,high,low) %>%
  ungroup() %>%
  mutate(year=as.integer(year),
         in_pi = ifelse(mg > low & mg < high,1,0))

ggplot(total) + geom_line(aes(year,mg)) +
  geom_line(aes(year,pred),color="dodgerblue") +
  geom_line(aes(year,low),color="dodgerblue",linetype="dashed") +
  geom_line(aes(year,high),color="dodgerblue",linetype="dashed") +
  theme_bw() + scale_y_continuous(labels = comma) +
  labs(y="millions of gallons")


# map of residuals
pred.mean <- preds %>%
  group_by(siteno) %>%
  summarize(resid=mean(resid),
            lat=mean(lat),
            lon=mean(lon),
            state=unique(state))

state.poly <-  subset(fortify(map_data('state')),
                     region %in% c("florida", "idaho","maryland",
                                   "new jersey","oregon","pennsylvania",
                                   "virginia")) %>%
  mutate(state=state.abb[match(region,tolower(state.name))])

ggplot() + 
  geom_polygon(data=state.poly, aes(x=long, y=lat, group=group), fill="grey100", color="black") +
  geom_point(data=pred.mean, aes(x=lon, y=lat, size=abs(resid), alpha=abs(resid)),shape=21,fill="dodgerblue") +
  facet_wrap(~state,scales="free") +
  scale_size_continuous(range = c(0.1,6), breaks=c(300,1000,3000)) +
  scale_alpha_continuous(range=c(0.4,0.8),breaks=c(300,1000,3000)) +
  theme_bw() + 
  coord_fixed(1.3)  +
  theme(aspect.ratio = 1)

ggplot() + 
  geom_polygon(data=state.poly, aes(x=long, y=lat, group=group), fill="grey100", color="black") +
  geom_point(data=pred.mean, aes(x=lon, y=lat),size=0.1,alpha=0.3) +
  theme_void() + 
  coord_fixed(1.3) 

states <- c("florida", "idaho","maryland",
            "new jersey","oregon","pennsylvania",
            "virginia")

gg_list <- list()

for(i in 1:7) {
state.poly <- subset(fortify(map_data('state')),
                      region %in% states[i]) %>%
  mutate(state=state.abb[match(region,tolower(state.name))])

pred.mean2 <- filter(pred.mean, state==unique(state.poly$state))

gg_list[[i]] <- ggplot() + 
  geom_polygon(data=state.poly, aes(x=long, y=lat, group=group), fill="grey100", color="black") +
  geom_point(data=pred.mean2, aes(x=lon, y=lat, size=abs(resid), alpha=abs(resid)),shape=21,fill="dodgerblue") +
  facet_wrap(~state,scales="free") +
  scale_size_continuous(range = c(0.1,6), breaks=c(300,1000,3000)) +
  scale_alpha_continuous(range=c(0.4,0.8),breaks=c(300,1000,3000)) +
  theme_bw() + 
  coord_fixed(1.3)  +
  theme(aspect.ratio = 1, legend.position="none")
}

grid.arrange(gg_list[[1]],gg_list[[2]],gg_list[[3]],
             gg_list[[4]],gg_list[[5]],gg_list[[6]],
             gg_list[[7]], ncol=7)

