---
title: Estimating latent Gaussian variable with maximum likelihood of Gaussian-product
  integral
author: ~
date: '2018-12-22'
slug: estimating-latent-gaussian-variable-with-maximum-likelihood-of-gaussian-product-integral
categories: []
tags: []
---

Suppose we have a population with a Normal distribution of some property, `$t$`, i.e. `$t_i \sim N(\mu, \sigma_t$`. For each member of the population we make a noisy measurement of the value, where the error distribution is Normal, `$\epsilon_i \sim N(0, \sigma_i)$`. The probability to make an observation `$x_i$` is then, 

$$ P(x_i | t) ~ 
\sim
\frac{1}{2 \pi \sigma_i \sigma_t}  
\int d\mu 
\exp{(-\frac{1}{2} \frac{(x_i - \mu)^2}{\sigma_i^2})}
\exp{(-\frac{1}{2} \frac{(\mu - t)^2}{\sigma_t^2})}
$$

In the [last post](../../../../2018/12/10/normalization-of-product-of-two-gaussians/) I verified that the value of the integral is a Normal distribution, 

$$P(x_i|t) \sim N(t, \sqrt{\sigma_i^2 + \sigma_t^2})$$

$$P(x_i|t) = \frac{1}{\sqrt{2 \pi (\sigma_i^2 + \sigma_t^2)}}
\exp{(-\frac{1}{2} \frac{(x_i - t)^2}{\sigma_i^2 +\sigma_t^2})}
$$

## Estimate of population parameters with maximum likelihood

Given a set of observations of `$X$`, we can use the above expression to estimate the population parameters, `$t$` and `$\sigma_t$`. In this case the overall likelihood is the product of the individual likelihoods,

$$ L = \Pi_i P(x_i | t)$$
Then the log-likelihood (actually -2 times the log-likelihood) is,

$$ l = - 2 \ln{L} = 
\sum_i 
\frac{(x_i - t)^2}{\sigma_i^2 +\sigma_t^2} + \sum_i \ln{(\sigma_i^2 +\sigma_t^2)}$$

The maximum likelihood values are determined by setting the first derivatives to 0 and solving for `$t$` and `$\sigma_t$`,

## value of t

$$ \frac{\partial l}{\partial t} = 2 \sum_i \frac{(t - x_i)}{\sigma_i^2 +\sigma_t^2}$$

$$ t = 
\frac{
\sum_i {x_i / (\sigma_i^2 +\sigma_t^2})
}
{\sum_i {1 / (\sigma_i^2 +\sigma_t^2)}
}$$

## value of `$\sigma_t$`

$$ 
\frac{\partial l}{\partial \sigma_t} = 
2 \sigma_t (
- \sum_i \frac{(t - x_i)^2}{(\sigma_i^2 +\sigma_t^2)^2} 
+ 
\sum_i {\frac{1}{\sigma_i^2 +\sigma_t^2}})$$

## solving the equations

These equations are coupled and non-linear so there's no closed form solution. However, we can investigate the limiting behavior in two interesting scenarios.

### All `$\sigma_i$` equal 

If `$\sigma_i = \sigma_x$` for all `$i$`, then these simplify to 

$$ t = \sum_i x_i $$

$$\frac{1}{(\sigma_x^2 +\sigma_t^2)^2} \sum_i (t - x_i)^2 = 
\frac{N}{\sigma_x^2 +\sigma_t^2}$$

$$ \sigma_t^2 = \frac{1}{N} \sum_i (t - x_i)^2 - \sigma_x^2$$

where `$N$` is the number of observations of `$X$`.

The result for `$\sigma_t$` says that the variance of the latent population is the sample variance minus the noise variance.

### `$\sigma_i \ll \sigma_t$`

In this limit the noise is negligible - each measurement very precisely measures the latent value, `$t$`. Then

$$ t = \sum_i x_i$$

$$ \sigma_t^2 = \frac{1}{N} \sum_i (t - x_i)^2$$, 

i.e. the variance of the latent variable, `$t$` is equal to the sample variance.

## multi-dimensional version

In more than 1-dimension, `$t$` becomes a vector and `$\sigma_i$` and `$\sigma_t$` covariance matrices. The log-likelihood is then,

$$ l = - 2 \ln{L} = 
\sum_i 
(x_i - t) (\Gamma_i + \Gamma_t)^{-1} (x_i - t)^{T} + \sum_i \ln{|\Gamma_i + \Gamma_t|}$$


where `$\Gamma$` represents a covariance matrix and `$|M|$` the determinant of the matrix `$M$`.

## numerical example - 1-dimension

Let's say we have a population with mean `$\mu=1$` and standard deviation `$\sigma_t = 2$`.





```r
set.seed(101)
N = 10000
mu = 1
sigma_t = 2
population = rnorm(N, mu, sigma_t) 
```

The noise distribution for each datum is drawn from a gamma distribution


```r
noise_sd_shape = 20
noise_sd_rate = 10
noise_sd = rgamma(N, shape = noise_sd_shape, rate = noise_sd_rate)
data.frame(noise_sd=noise_sd) %>% 
  ggplot(aes(x=noise_sd)) + 
  geom_histogram()
```

![plot of chunk unnamed-chunk-3](/post/2018-12-22-estimating-latent-gaussian-variable-with-maximum-likelihood-of-gaussian-product-integral_files/figure-html/unnamed-chunk-3-1.png)

Now we can generate a synthetic set of measurements, applying this noise level.


```r
noise = sapply(1:N, function(i) {rnorm(1, 0, noise_sd[i])})
measurements = population + noise
```

This sets up a log-likelihood function


```r
log_likelihood_factory = function(measurements, noise_sd) {
  
  function(params) {
    t = params[1]
    sigma_t = params[2]
    logl_x = sum((measurements - t)**2 / (noise_sd ** 2 + sigma_t ** 2))
    logl_c = sum(log(noise_sd ** 2 + sigma_t ** 2))
    logl_x + logl_c
  } 
}
```

This finds the maximum likelihood values


```r
logl_fun = log_likelihood_factory(measurements, noise_sd)
max_likelihood_solution = optim(c(1,1), logl_fun)
max_likelihood_solution$par
#> [1] 1.038575 1.953789
```

On the other hand if we look at the standard deviation of the measurements we get,


```r
sd(measurements)
#> [1] 2.840141
```

If we take the mean of the noise standard deviation as an effective value and apply the result for the case where all `$\sigma_i$` are equal we get


```r
sqrt(var(measurements)  - mean(noise_sd**2))
#> [1] 1.969332
```

which is not a bad approximation.


## numerical example - 2-dimensions

The following defines the parameters for a 2-dimensional latent population variable, with a correlation of `$\rho = 0.5$`


```r
N = 10000
mu1 = 1
mu2 = 2
s1 = 1
s2 = 2
rho = 0.5
```

To simulate a correlated 2-d random variable we generate an uncorrelated variable and then multiply by the right-factor of a Cholesky decomposition.


```r
set.seed(101)
D = 2
population_uncorr = matrix(rnorm(N * D, 0, 1), ncol=D)
cor(population_uncorr)
#>             [,1]        [,2]
#> [1,] 1.000000000 0.005650561
#> [2,] 0.005650561 1.000000000
```


```r
cov_mat = matrix(c(s1*s1, rho*s1*s2, rho*s1*s2, s2*s2), ncol=2)
right_chol = chol(cov_mat)
population = population_uncorr %*% right_chol
population[,1] = population[,1] + mu1
population[,2] = population[,2] + mu2
cor(population)
#>           [,1]      [,2]
#> [1,] 1.0000000 0.5036932
#> [2,] 0.5036932 1.0000000
```

The noise distribution for each datum is drawn from uncorrelated gamma distributions


```r
noise_sd_shape = 20
noise_sd_rate = 10
noise_sd = matrix(rgamma(N * D, shape = noise_sd_shape, rate = noise_sd_rate), ncol=2)
```

Now we can generate a synthetic set of measurements, applying this noise level.


```r
noise = cbind(
  sapply(1:N, function(i) {rnorm(1, 0, noise_sd[i,1])}),
  sapply(1:N, function(i) {rnorm(1, 0, noise_sd[i,2])})
)
measurements = t( t(population) + t(noise))
```

We see that the noise applied to the population weakens the correlation


```r
df = data.frame(x1=measurements[,1], x2=measurements[,2], 
                t1 = population[,1], t2=population[,2])
df %>% ggplot(aes(x=t1, y=t2)) + geom_point() + geom_point(aes(x=x1, y=x2), color='orange1', alpha=0.2)
```

![plot of chunk unnamed-chunk-14](/post/2018-12-22-estimating-latent-gaussian-variable-with-maximum-likelihood-of-gaussian-product-integral_files/figure-html/unnamed-chunk-14-1.png)

This sets up a log-likelihood function


```r
log_likelihood_factory_2d = function(measurements, noise_sd) {
  
  function(params) {
    tvec = matrix(c(params[1], params[2]), ncol=2)
    s1 = params[3]
    s2 = params[4]
    rho = params[5]
    cov_t = matrix(c(s1*s1, rho * s1*s2, rho*s1*s2, s2*s2), ncol=2)
    tmp = lapply(1:nrow(measurements), function(i) {
        cov_i = matrix(c(noise_sd[i,1]**2, 0, 0, noise_sd[i,2]**2), ncol=2)
        cov_mat = cov_i + cov_t
        right_chol = chol(cov_mat)
        right_chol_inv = solve(right_chol)
        w = matrix(measurements[i,] - tvec, nrow=1) %*% right_chol_inv
        logl_x = crossprod(t(w))
        logl_c = determinant(cov_mat, logarithm = T)$modulus
        list(logl_x=logl_x, logl_c = logl_c)
    }) %>% bind_rows(
      
    )

    sum(tmp)
    
  } 
}
```

This finds the maximum likelihood values. I use the "L-BFGS-B" optimization method so I can explicitly put bounds on `$\rho$`.


```r
logl_fun = log_likelihood_factory_2d(measurements, noise_sd)
max_likelihood_solution = optim(c(1, 1, 1, 1 , 0.5), logl_fun, 
                                method = "L-BFGS-B", 
                                lower = c(-Inf, -Inf, -Inf, -Inf, -1),
                                upper = c(Inf, Inf, Inf, Inf, 1)
                                )
max_likelihood_solution$par
#> [1] 1.0128679 2.0263088 0.9756733 1.9411863 0.5152542
```

The maximum likelihood method recovers the latent correlation of the population. On the other hand if we subtract a representative value for the data noise from the measurement sample covariance, we get a good approximation, as in the 1d case.


```r
v1_x = var(noise[,1])
v2_x = var(noise[,2])
pop_covar_estimate = var(measurements) - matrix(c(v1_x, 0, 0, v2_x), ncol=2)
pop_covar_estimate
#>           [,1]      [,2]
#> [1,] 0.9472655 0.9734004
#> [2,] 0.9734004 3.8450709
```

