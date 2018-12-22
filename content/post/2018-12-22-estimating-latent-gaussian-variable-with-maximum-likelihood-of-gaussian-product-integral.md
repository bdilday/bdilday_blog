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
\exp{(-\frac{1}{2} \frac{(x_i - \mu)}{\sigma_i^2})}
\exp{(-\frac{1}{2} \frac{(\mu - t)}{\sigma_t^2})}
$$

In the last post I confirmed that the 
