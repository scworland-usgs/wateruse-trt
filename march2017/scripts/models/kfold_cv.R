
# load packages
pacman::p_load(feather,dplyr,ggplot2,lme4,tidyr,rethinking)
select <- dplyr::select

# set working directory
setwd("~/Documents/WAUSP TRT/wateruse-trt/march2017")

# load data
model_data <- read_feather("data/wutrt_model_data.feather") %>%
  mutate(aridity = p_summer_mean/(t_summer_mean + 33),
         spei1_min = ifelse(spei1_min==-Inf,0,spei1_min),
         state_index = coerce_index(state),
         source_index = ifelse(source=="SW",0,1)) %>%
  mutate_each(funs(scaled=scale), c(pop,t_summer_mean,p_summer_mean,
                                    p_summer_anom,t_summer_anom)) %>%
  filter(mg < quantile(mg,0.99)) %>%
  na.omit() %>%
  data.frame() %>%
  split(.$id) %>% 
  sample(length(.)) %>% 
  bind_rows() %>%
  data.frame() 

# randomly shuffles groups
test_data <- data.frame(id = rep(letters[1:10], 3)) %>% 
  arrange(id) %>% 
  split(.$id) %>% 
  sample(length(.)) %>% 
  bind_rows() 

