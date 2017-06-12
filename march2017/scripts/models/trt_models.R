
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


state_link <- model_data %>%
  distinct(state,state_index)


# Gaussian models
## no pooling varying intercept only model
m0 <- map2stan(
  alist(
    mg ~ dnorm(mu, sigma),
    mu <- a_state[state_index],
    a_state[state_index] ~ dnorm(50,30),
    sigma ~ dcauchy(0,100)
  ),
  data=model_data,
  chains=2, cores=2,
  iter=1000, warmup=500)

## pooled varying intercept model
m1_prep <- map2stan(
  alist(
    mg ~ dnorm(mu, sigma),
    mu <- a_state[state_index],
    a_state[state_index] ~ dnorm(a, sigma_state),
    sigma ~ dcauchy(0,100),
    a ~ dnorm(50,30),
    sigma_state ~ dcauchy(0,50)
  ),
  data=model_data, iter=2)

m1 <- resample(m1_prep, warmup=1000, iter=2000, chains=3, cores=3)

## m1 + global aridity parameter
m2.1 <- map2stan(
  alist(
    mg ~ dnorm(mu, sigma),
    mu <- a_state[state_index] + ba*aridity,
    a_state[state_index] ~ dnorm(a, sigma_state),
    ba ~ dnorm(50,30),
    sigma ~ dcauchy(0,100),
    a ~ dnorm(50,30),
    sigma_state ~ dcauchy(0,50)
  ),
  data=model_data,
  chains=2, cores=2,
  iter=1000, warmup=500)

## m1 + varying aridity parameter
m2.2 <- map2stan(
  alist(
    mg ~ dnorm(mu, sigma),
    mu <- a_state[state_index] + ba_state[state_index]*aridity,
    c(a_state,ba_state)[state_index] ~ dmvnormNC(sigma_state, rho_state),
    sigma ~ dcauchy(0,100),
    sigma_state ~ dcauchy(0,50),
    rho_state ~ dlkjcorr(2)
  ),
  data=model_data,
  chains=2, cores=2,
  iter=1000, warmup=500)

## m2.2 + varying pop parameter
m3 <- map2stan(
  alist(
    mg ~ dnorm(mu, sigma),
    mu <- a_state[state_index] + ba_state[state_index]*aridity + 
      bpop_state[state_index]*pop,
    c(a_state,ba_state,bpop_state)[state_index] ~ dmvnormNC(sigma_state, rho_state),
    sigma ~ dcauchy(0,100),
    sigma_state ~ dcauchy(0,50),
    rho_state ~ dlkjcorr(2)
  ),
  data=model_data,
  chains=2, cores=2,
  iter=1000, warmup=500)

compare(m1,m2,m3,m4)

m1234 <- ensemble(m1,m2,m3,m4)
pred.p <- apply(m1234$link,2,mean)
pred.p.PI <- apply(m1234$link,2,PI)

preds <- data_sub %>%
  select(siteno,year,state,lat,lon,mg) %>%
  mutate(average = pred.p,
         p5 = pred.p.PI[1,],
         p95 = pred.p.PI[2,],
         year=as.integer(year))

ggplot(preds) + 
  geom_pointrange(aes(x=mg,y=average,ymin=p5,ymax=p95,color=state),alpha=0.7) +
  geom_abline(slope=1,intercept=0,linetype="dashed") +
  theme_bw()

# save models to load later
saveRDS(m0, "data/models/m0.rds")
saveRDS(m1, "data/models/m1.rds")
saveRDS(m2.1, "data/models/m2.1.rds")
saveRDS(m2.2, "data/models/m2.2.rds")

m3 <- readRDS("data/models/m3.rds")







