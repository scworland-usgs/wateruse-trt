
#'/Users/scworlan/Documents/WAUSP TRT/wateruse-trt/march2017/scripts/models/m4.R'

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
  mutate_each(funs(scaled=scale), c(pop,t_summer_mean,p_summer_mean,
                                    p_summer_anom,t_summer_anom)) %>%
  filter(mg < quantile(mg,0.99)) %>%
  na.omit() %>%
  data.frame() 

## m1 + varying aridity parameter
m4.0 <- map2stan(
  alist(
    mg ~ dnorm(mu, sigma),
    mu <- a_state[state_index] + ba_state[state_index]*aridity +
      bpop_state[state_index]*pop_scaled + bpa_state[state_index]*p_summer_anom_scaled,
    c(a_state,ba_state,bpop_state,bpa_state)[state_index] ~ dmvnormNC(sigma_state, rho_state),
    sigma ~ dcauchy(0,100),
    sigma_state ~ dcauchy(0,50),
    rho_state ~ dlkjcorr(2)
  ),
  data=model_data, iter=2)

m4 <- resample(m4.0, warmup=1000, iter=3000, chains=2, cores=2)

saveRDS(m4, "scripts/models/m4.rds")



