Assignment 9
================
Zack Wixom
3/27/2021

``` r
# Packages
library(tidyverse)
library(rethinking)
library(dagitty)
library(ellipse)

# Set Seed
set.seed(42)

# Load Data
data("bangladesh")
d <- bangladesh
```

### Question 1

First to make the varying slopes model, with centered parameterization.

``` r
# Standardize Vars
data <- tibble(
  contra = d$use.contraception,
  district_id = as.integer(as.factor(d$district)),
  urban = d$urban
)

# Fit Model
m1.1 <- ulam(
  alist(
    contra ~ bernoulli(p),
    logit(p) <- a[district_id] + b[district_id]*urban,
    c(a, b)[district_id] ~ multi_normal(c(abar, bbar), Rho, Sigma),
    abar ~ normal(0, 1),
    bbar ~ normal(0, 0.5),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
  ),
  data = data,
  chains = 4,
  cores = 4,
  cmdstan = TRUE
)
```

    ## This is cmdstanr version 0.3.0

    ## - Online documentation and vignettes at mc-stan.org/cmdstanr

    ## - CmdStan path set to: /Users/zwixom/.cmdstanr/cmdstan-2.25.0

    ## - Use set_cmdstan_path() to change the path

    ## 
    ## A newer version of CmdStan is available. See ?install_cmdstan() to install it.
    ## To disable this check set option or environment variable CMDSTANR_NO_VER_CHECK=TRUE.

    ## Compiling Stan program...

    ## Running MCMC with 4 parallel chains, with 1 thread(s) per chain...
    ## 
    ## Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup)

    ## Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 1 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 1

    ## Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 1 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 1

    ## Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 1 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 1

    ## Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 1 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 1

    ## Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 1 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 1

    ## Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup)

    ## Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 2 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 2

    ## Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup)

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup)

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d38864cd5691.stan', line 17, column 4 to column 24)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 1 finished in 23.9 seconds.
    ## Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 2 finished in 25.9 seconds.
    ## Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 4 finished in 27.6 seconds.
    ## Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 3 finished in 28.3 seconds.
    ## 
    ## All 4 chains finished successfully.
    ## Mean chain execution time: 26.4 seconds.
    ## Total execution time: 29.0 seconds.

``` r
plot(precis(m1.1))
```

    ## 126 vector or matrix parameters hidden. Use depth=2 to show them.

![](../Figures/Week%2011/m1.1%20plot%201-1.png)<!-- -->

This is telling us that urban areas are using more contraceptives. But
we will look at distribution of varying effects.

``` r
plot(precis(m1.1, depth = 3, pars = c("Rho", "Sigma")))
```

![](../Figures/Week%2011/m1.1%20plot%202-1.png)<!-- -->

**Can’t remember what differeence of looking at Rho does, Check in
Book**

Now onto plotting the intercpets and slopes with the ellipse plot

``` r
post <- extract.samples(m1.1)

a <- apply(post$a, 2, mean)
b <- apply(post$b, 2, mean)

plot(a, b, xlab = "a intercept", ylab = "b urban slope")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

R <- apply(post$Rho, 2:3, mean)
s <- apply(post$Sigma, 2, mean)
S <- diag(s) %*% R %*% diag(s)
ll <- c(0.5, 0.67, 0.89, 0.97)
for(l in ll){
  el <- ellipse(S, contre = c(mean(post$abar), mean(post$bbar)), level = l)
  lines(el, col = "black", lwd = 0.5)
}
```

![](../Figures/Week%2011/m1.1%20ellipse-1.png)<!-- -->

So we can see a negative correlation with districts with higher use
outside urban areas have smaller slopes.

Overall this can mean that perhaps urban areas are similar to all
districts but once you go to rural areas there is variation, But to
check we will plot urban areas against rural areas.

``` r
u0 <- inv_logit(a)
u1 <- inv_logit(a + b)

plot(u0, u1, xlim = c(0,1), ylim = c(0,1), xlab = "rural", ylab = "urban")
abline(h = 0.5, lty = 2)
abline(v = 0.5, lty = 2)
```

![](../Figures/Week%2011/m1.1%20outcome%20scale-1.png)<!-- -->

This shows how the spread is more tight in the urban areas while the
rural as a large spread and more variety.

### Question 2

First to draw my DAG with `A` as age, `K` as number of children, and `C`
as contraception.

``` r
# Create DAG
dag1 <- dagitty("dag{A -> K; K -> C; A -> C}")

# DAG coordinates
coordinates(dag1) <- list(x = c(A = 0, C = 1, K = 2), y = c(A = 0, C = 1, K = 0))

# Draw DAG
drawdag(dag1)
```

![](../Figures/Week%2011/dag-1.png)<!-- -->

Now onto the modeling

``` r
# Standardize Vars
data <- tibble(
  contra = d$use.contraception,
  district_id = as.integer(as.factor(d$district)),
  urban = d$urban,
  children = standardize(d$living.children),
  age = standardize(d$age.centered)
)

# Fit Model
m2.1 <- ulam(
  alist(
    contra ~ bernoulli(p),
    logit(p) <- a[district_id] + b[district_id]*urban + bA*age,
    c(a, b)[district_id] ~ multi_normal(c(abar, bbar), Rho, Sigma),
    abar ~ normal(0, 1),
    bbar ~ normal(0, 0.5),
    bA ~ normal(0, 0.5),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
  ),
  data = data,
  chains = 4,
  cores = 4,
  cmdstan = TRUE
)
```

    ## Running MCMC with 4 parallel chains, with 1 thread(s) per chain...
    ## 
    ## Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
    ## Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
    ## Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
    ## Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
    ## Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 1 finished in 33.3 seconds.
    ## Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 2 finished in 33.6 seconds.
    ## Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 3 finished in 35.6 seconds.
    ## Chain 4 finished in 35.7 seconds.
    ## 
    ## All 4 chains finished successfully.
    ## Mean chain execution time: 34.5 seconds.
    ## Total execution time: 36.1 seconds.

``` r
plot(precis(m2.1))
```

    ## 126 vector or matrix parameters hidden. Use depth=2 to show them.

![](../Figures/Week%2011/m2.1%20plot-1.png)<!-- -->

So it looks like age doesn’t give us a really big impact, it is slightly
positive though.

So let’s add in `K` as number of children.

``` r
# Fit Model
m2.2 <- ulam(
  alist(
    contra ~ bernoulli(p),
    logit(p) <- a[district_id] + b[district_id]*urban + bA*age + bK*children,
    c(a, b)[district_id] ~ multi_normal(c(abar, bbar), Rho, Sigma),
    abar ~ normal(0, 1),
    bbar ~ normal(0, 0.5),
    c(bA,bK) ~ normal(0, 0.5),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1)
  ),
  data = data,
  chains = 4,
  cores = 4,
  cmdstan = TRUE
)
```

    ## Compiling Stan program...

    ## Running MCMC with 4 parallel chains, with 1 thread(s) per chain...
    ## 
    ## Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup)

    ## Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 1 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 1

    ## Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 1 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 1

    ## Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 1 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 1

    ## Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 1 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 1

    ## Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup)

    ## Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 2 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 2

    ## Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 2 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 2

    ## Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 2 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 2

    ## Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 2 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 2

    ## Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 2 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 2

    ## Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 2 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 2

    ## Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 2 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 2

    ## Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 2 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 2

    ## Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup)

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 3 Exception: multi_normal_lpdf: LDLT_Factor of covariance parameter is not positive definite.  last conditional variance is 0. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 31, column 4 to column 58)

    ## Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 3

    ## Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup)

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: multi_normal_lpdf: Covariance matrix is not symmetric. Covariance matrix[1,2] = -2.21927e+11, but Covariance matrix[2,1] = -2.21927e+11 (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 31, column 4 to column 58)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: lkj_corr_lpdf: y is not positive definite. (in '/var/folders/t4/xnrqyf8n73330vth4334d_y80000gn/T/RtmpQ4x4It/model-d3887e0ae3cb.stan', line 21, column 4 to column 24)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 4 finished in 36.2 seconds.
    ## Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 3 finished in 37.9 seconds.
    ## Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 2 finished in 42.4 seconds.
    ## Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 1 finished in 44.4 seconds.
    ## 
    ## All 4 chains finished successfully.
    ## Mean chain execution time: 40.2 seconds.
    ## Total execution time: 44.9 seconds.

``` r
plot(precis(m2.2))
```

    ## 126 vector or matrix parameters hidden. Use depth=2 to show them.

![](../Figures/Week%2011/m2.2%20plot-1.png)<!-- -->

Now we can see there is a negative effect of age once number of children
is conditioned and that has a positive effect on it. I guess that means
that people use more contraceptives once they have more children and the
older they are the less likely the are to use contraceptives. I guess
the older the parents the more likely they will have more children over
their life time.

### Question 3

No we will add the ordered categorical variables.

``` r
# Standardize Vars
data <- list(
  contra = d$use.contraception,
  district_id = as.integer(as.factor(d$district)),
  urban = d$urban,
  children = d$living.children,
  age = standardize(d$age.centered)
)

data$alpha <- rep(2,3)

# Fit model
m3.1 <- ulam(
  alist(
    contra ~ bernoulli(p),
    logit(p) <- a[district_id] + b[district_id]*urban + bA*age + bK*sum(delta_shell[1:children]),
    c(a, b)[district_id] ~ multi_normal(c(abar, bbar), Rho, Sigma),
    abar ~ normal(0, 1),
    bbar ~ normal(0, 0.5),
    c(bA,bK) ~ normal(0, 0.5),
    Rho ~ lkj_corr(2),
    Sigma ~ exponential(1),
    vector[4]: delta_shell <<- append_row(0, delta),
    simplex[3]: delta ~ dirichlet(alpha)
  ),
  data = data,
  chains = 4,
  cores = 4,
  cmdstan = TRUE
)
```

    ## Running MCMC with 4 parallel chains, with 1 thread(s) per chain...
    ## 
    ## Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
    ## Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
    ## Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
    ## Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
    ## Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
    ## Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
    ## Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
    ## Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
    ## Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
    ## Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
    ## Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
    ## Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
    ## Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 3 finished in 60.5 seconds.
    ## Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 1 finished in 64.4 seconds.
    ## Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
    ## Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 4 finished in 66.2 seconds.
    ## Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
    ## Chain 2 finished in 68.1 seconds.
    ## 
    ## All 4 chains finished successfully.
    ## Mean chain execution time: 64.8 seconds.
    ## Total execution time: 68.7 seconds.

``` r
plot(precis(m3.1))
```

    ## 129 vector or matrix parameters hidden. Use depth=2 to show them.

![](../Figures/Week%2011/m3.1%20plot-1.png)<!-- -->

So we can see that it is similiar to `m2.2`. Age slightly reduces use of
contraceptives while the number of kids increases use.

let’s plot with `pars = "delta"`.

``` r
plot(precis(m3.1, 3, pars = "delta"))
```

![](../Figures/Week%2011/m3.1%20plot%20delta-1.png)<!-- -->

Looks like the change from having 1 to 2 kids greatly increases the use
of contraceptives.
