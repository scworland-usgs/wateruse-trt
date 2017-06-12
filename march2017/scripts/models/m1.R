
#'/Users/scworlan/Documents/WAUSP TRT/wateruse-trt/march2017/scripts/models/m1.R'

# load packages
pacman::p_load(feather,dplyr,ggplot2,lme4,tidyr,rethinking)
select <- dplyr::select

# set working directory
setwd("~/Documents/WAUSP TRT/wateruse-trt/march2017")

# load data
model_data <- read_feather("data/wutrt_model_data.feather") %>%
  mutate(aridity = p_summer_mean/(t_summer_mean + 33),
         log_mg = log(mg + 0.001),
         state_index = coerce_index(state)) %>%
  filter(mg < quantile(mg,0.99)) %>%
  na.omit() %>%
  data.frame() 

## pooled varying intercept model
m1.0 <- map2stan(
  alist(
    mg ~ dnorm(mu, sigma),
    mu <- a_state[state_index],
    a_state[state_index] ~ dnorm(a, sigma_state),
    sigma ~ dcauchy(0,100),
    a ~ dnorm(50,30),
    sigma_state ~ dcauchy(0,50)
  ),
  data=model_data, iter=2)

m1 <- resample(m1.0, warmup=1000, iter=2000, chains=2, cores=2)

saveRDS(m1, "scripts/models/m1.rds")



