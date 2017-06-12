# What's here?
This repo contains the model scripts for estimating site specifc public supply withdrawals for 7 states in the U.S. The model files located in the "models" folder generate stan code and MCMC samples of a posterior distribution for several models. The models are large, run several chains in parallel, and it is wise to run several at the same time from the terminal using `Rscript directory/model.R`. The stan scripts are created using the `map2stan` functions from the `rethinking` R package. This choice was made as it allows for the most direct translation from the mathematical set up to the code itself. For example, a simple varying intercept model with zero pooling:

![equation](http://www.sciweavers.org/tex2img.php?eq=%5Cbegin%7Baligned%7D%0Amg_i%20%26%5Csim%20Normal%28%5Cmu_i%2C%5Csigma_i%29%20%5C%5C%0A%5Cmu%20%26%3D%20%5Calpha%5Bstate%5D_i%20%5C%5C%0A%5Calpha%5Bstate%5D%20%26%5Csim%20Normal%2850%2C30%29%20%5C%5C%0A%5Csigma%20%26%5Csim%20HalfCauchy%280%2C100%29%20%0A%5Cend%7Baligned%7D&bc=White&fc=Black&im=jpg&fs=12&ff=concmath&edit=0)

Is coded as:

```r
## no pooling varying intercept only model
m0.0 <- map2stan(
  alist(
    mg ~ dnorm(mu, sigma),
    mu <- a_state[state_index],
    a_state[state_index] ~ dnorm(50,30),
    sigma ~ dcauchy(0,100)
  ),
  data=model_data,
  iter=2)

m0 <- resample(m0.0, warmup=1000, iter=2000, chains=2, cores=2)
```

The `resample` line just prevents recompiling the stan code each time, and passes some MCMC details to stan (chains, iterations, etc). The resulting stan code is:

```stan
data{
    int<lower=1> N;
    int<lower=1> N_state_index;
    real mg[N];
    int state_index[N];
}
parameters{
    vector[N_state_index] a_state;
    real<lower=0> sigma;
}
model{
    vector[N] mu;
    sigma ~ cauchy( 0 , 100 );
    a_state ~ normal( 50 , 30 );
    for ( i in 1:N ) {
        mu[i] = a_state[state_index[i]];
    }
    mg ~ normal( mu , sigma );
}
generated quantities{
    vector[N] mu;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        mu[i] = a_state[state_index[i]];
    }
    dev = dev + (-2)*normal_lpdf( mg | mu , sigma );
}
```

Where the stuff in the `generated quantities` block is for calculating fit metrics. 
