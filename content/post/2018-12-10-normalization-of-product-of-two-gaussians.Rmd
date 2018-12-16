---
title: Normalization of product of two Gaussians
author: ~
date: '2018-12-10'
slug: normalization-of-product-of-two-gaussians
categories: []
tags: []
---

In Bayesian statistical analysis it is common to encounter the product of two Gaussian functions, representing the likelihood and the prior for a Normally distributed random variable. As articulated by [John D. Cook here](https://www.johndcook.com/blog/2012/10/29/product-of-normal-pdfs/), it's usually the case that one doesn't need the normalization factor of the posterior distribution and so it's difficult to find a clear reference for what it is. The post by John D. Cook mentioned above does show the result, but the purpose of my post here is to flesh out the math a little bit more, and to state the result i a way I think is more intuitive. An additional references that covers this topic is [*Products and Convolutions of Gaussian Probability Density Functions*](http://www.tina-vision.net/docs/memos/2003-003.pdf).

The application of knowing the normalization factor that I'm interested in is applying maximum-likelihood to make an inference about the parameters of the prior distribution. I'll expand on this in the next post.

## TL;DR 

The result, borrowing from John D. Cook's formulation, is going to be, 

$$ \phi(x | \mu_1, \sigma_1) \phi(x | \mu_2, \sigma_2) = 
\phi(\mu_1 | \mu_2, \sigma_c) \phi(x | \mu, \sigma)$$, 

where `$\phi$` is a standard Gaussian, 

$$ \phi(x | \mu, \sigma) = \frac{1}{\sqrt{(2 \pi)} \sigma} \exp{(-\frac{1}{2}\frac{(x-\mu)^2}{\sigma^2})}$$

and, 
$$\sigma_c = \sqrt{\sigma_1^2 + \sigma_2^2}$$
$$\mu = \frac{\mu_1/\sigma_1^2 + \mu_2/\sigma_2^2}{1/\sigma_1^2 + 1/\sigma_2^2}$$

$$\sigma^2 = \frac{1}{1/\sigma_1^2 + 1/\sigma_2^2}$$

## Product of Gaussians

The product of Gaussians is

$$\phi(x | \mu_1, \sigma_1) \phi(x | \mu_2, \sigma_2) = 
\frac{1}{2 \pi \sigma_1 \sigma_2} \exp{(-\frac{1}{2}\frac{(x-\mu_1)^2}{\sigma_1^2})} \exp{(-\frac{1}{2}\frac{(x-\mu_2)^2}{\sigma_2^2})}$$

If I look at the arguments of the exponentials, suppressing the `$-\frac{1}{2}$` factor, I can expand out the terms to get

$$\frac{(x-\mu_1)^2}{\sigma_1^2} \frac{(x-\mu_2)^2}{\sigma_2^2} \\
= \frac{x^2}{\sigma_1^2} + 
\frac{x^2}{\sigma_2^2} + 
\frac{\mu_1^2}{\sigma_1^2} +
\frac{\mu_2^2}{\sigma_2^2} - \frac{2 x \mu_1}{\sigma_1^2} - \frac{2 x \mu_2}{\sigma_2^2}$$

which can be rewritten 

$$(\frac{1}{\sigma_1^2} + \frac{1}{\sigma_2^2})(x^2) + 
(-\frac{2 \mu_1}{\sigma_1^2} -\frac{2 \mu_2}{\sigma_2^2})(x) + (\frac{\mu_1^2}{\sigma_1^2} + \frac{\mu_2^2}{\sigma_2^2})$$

Now, generically, if I have an expression `$A x^2 + B x + C$`, I can complete the square by adding and subtracting `$\frac{B^2}{4 A} - C$`. To see this, note that if we have a perfect square of `$(x-k)$` for some `$k$`, then, 

$$ (x-k)^2 = x^2 - 2 x k + k^2$$ 

and write

$$ A x^2 + B x + C = A (x^2 + \frac{B}{A} x + \frac{C}{A})$$

We need the inner part to be equal a perfect square, i.e., 

$$\frac{B}{A} = -2 k$$
$$ k = \frac{-B}{2 A}$$

and we require that 

$$\frac{C}{A} + \delta = \frac{B^2}{4 A^2}$$, 
i.e.,

$$\delta = \frac{B^2}{4 A^2} - \frac{C}{A}$$

so that finally we have, 

$$A x^2 + B x + C = A (x - k)^2 - A \delta$$.

From above we have,

$$ A = \frac{1}{\sigma_1^2} + \frac{1}{\sigma_2^2}$$
$$ B = -2 (\frac{\mu_1}{\sigma_1^2} + \frac{\mu_2}{\sigma_2^2})$$
$$ C = \frac{\mu_1^2}{\sigma_1^2} + \frac{\mu_2^2}{\sigma_2^2}$$

so that 

$$ k = \frac{-B}{2 A} = \frac{\mu_1/\sigma_1^2 + \mu2/\sigma_2^2}{1/\sigma_1^2 + 1/\sigma_2^2}$$

$$ A \delta = \frac{B^2}{4 A} - C$$

To evaluate this term by term we have,

$$\frac{B^2}{4 A} = \frac{4 (\mu_1^2/\sigma_1^4 + \mu_2^2/\sigma_2^4 + 2\mu_1 \mu_2 / \sigma_1^2 \sigma_2^2)}{4 (1/\sigma_1^2 + 1/\sigma_2^2)}$$

$$ = \frac{\sigma_1^2 \sigma_2^2}{\sigma_1^2 + \sigma_2^2}(\mu_1^2/\sigma_1^4 + \mu_2^2/\sigma_2^4 + 2\mu_1 \mu_2 / \sigma_1^2 \sigma_2^2)$$
$$ = \frac{1}{\sigma_1^2 + \sigma_2^2}(\mu_1^2 \sigma_2^4/\sigma_1^2 \sigma_2^2 + \mu_2^2 \sigma_1^4/\sigma_1^2 \sigma_2^2 + 2\mu_1 \mu_2 \sigma_1^2 \sigma_2^2/ \sigma_1^2 \sigma_2^2)$$

On the other hand, 

$$C = \mu_1^2/\sigma_1^2 + \mu_2^2\sigma_2^2$$
$$ = \mu_1^2 \sigma_2 ^2 / \sigma_1^2 \sigma_2^2 + \mu_2^2 \sigma_1 ^2 / \sigma_1^2 \sigma_2^2$$
$$ = \frac{1}{\sigma_1^2 \sigma_2^2}\frac{1}{\sigma_1^2 + \sigma_2^2}(\mu_1^2 \sigma_2^2 + \mu_2^2 \sigma_1^2)(\sigma_1^2 + \sigma_2^2)$$
$$ = \frac{1}{\sigma_1^2 \sigma_2^2}\frac{1}{\sigma_1^2 + \sigma_2^2}
(\mu_1^2 \sigma_1^2 \sigma_2^2 + \mu_1^2 \sigma_2^4 + \mu_2^2 \sigma_1^4 + \mu_2^2 \sigma_1^2 \sigma_2^2) 
$$

So in `$B^2/4 A - C$` the terms `$\mu_1 \sigma_2^4$` and `$\mu_2 \sigma_1^4$` cancel out and the term `$\sigma_1^2 \sigma_2^2$` divides out, leaving,

$$ \frac{1}{\sigma_1^2 + \sigma_2^2}(\mu_1^2 + \mu_2^2 - 2 \mu_1 \mu_2)$$
$$ A \delta = \frac{1}{\sigma_1^2 + \sigma_2^2}(\mu_1 - \mu_2)^2$$

## back-substituting

To back substitute this in to the original product of Gaussians, we see

$$\frac{1}{2 \pi \sigma_1 \sigma_2} \exp{(-\frac{1}{2}\frac{(x-\mu_1)^2}{\sigma_1^2})} \exp{(-\frac{1}{2}\frac{(x-\mu_2)^2}{\sigma_2^2})}$$

$$ = \frac{1}{2 \pi \sigma_1 \sigma_2} \exp({A \delta / 2}) \exp(-\frac{1}{2} \frac{(x-\mu)^2}{\sigma^2})
$$
$$ = \frac{1}{\sqrt{2 \pi}} \frac{1}{(1/\sigma_1^2 + 1/\sigma_2^2)^{-1 / 2}} \exp(-\frac{1}{2} \frac{(x-\mu)^2}{\sigma^2}) \times \frac{1}{\sqrt{2 \pi}} \frac{1}{\sqrt{\sigma_1^2 + \sigma_2^2}}  \exp({A \delta / 2}) $$

i.e. 

$$ \phi(\mu_1 | \mu_2, \sigma_c) \phi(x | \mu, \sigma) $$
as expected.
