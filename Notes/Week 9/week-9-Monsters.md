Monsters & Mixtures
================
Zack Wixom

![owlimage](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/owl.png)

## Generalized Linear Madness

Bad Model:

  - Make anomalies
  - Intercepts don’t pass through origin
  - Zero Population = Zero Tools

We can do better by thinking *scientifically* instead of
*statistically*.

## Scientific model

Change in tools per unit time:

![owlimage](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/Scientific_model.png)

Three parameters: Alpha, Beta, and Gamma

![scienceimage](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/scientific_model2.png)

  - No link function needed.

  - intercept is fixed.

### Poisson Exposure

Observation windows are *exposures*.

Poisson outcome: events per unit time/distance

**Question:** What if time/distance varies across cases?

**Answer:** Use an *exposure* aka *offset*

Offset –\> log of the amount of observation

![scienceimage](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/offset.png)

### Survival Analysis

*Count the stuff that wasn’t counted* –\> Censoring

Count models are fundamentally about rates - Rate of heads per coin toss
- Rate of tools per person

Can also estimate rates by modeling time-to-event

Tricky, because cannot ignore censored cases - Left-censored: Don’t know
when time started - Right-censored: Something cut observation off before
event occurred

Ignoring censored cases leads to inferential error - Imagine estimating
time-toPHD but ignoring people who drop out

**Cat Adoptions**

Look at two events: 1. Adopted 2. Something else (death, escape,
censored)

Hypothesis: *People don’t like black cats*

Exponential distributions for observed events

Censored Cats need Cumulative distribution: Probability event before or
at time X

Complementary Cumulative distribution: Probability not-event-yet.

First model with multiple choice.

![catimage](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/cat.png)

![cat2image](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/cat2.png)

## Monsterous Models

**Monsters**: Specialized, complex distributions

  - Ordered categories, ranks

**Mixtures**: blends of stochastic processes

  - Varying means, probabilities, rates
  - Varying processes

<!-- end list -->

1.  Over-Dispersion

2.  Zero-inflated and Zero-augmented

3.  Ordered Categorical

These models help us transform our modeling to cope with inconvenient
realities of measurement, rather than transforming measurements to cope
with constraints of our models.

## Over-dispered counts

Models based on normal distributions can be overly sensitive to extreme
observations

**Over-Dispersion** –\> When counts are more variable than pure process.

The variance of a variable is called **dispersion**.

Variance as a function:

  - Expected Value of binomial = *Np*

  - Variance = *Np*(1 - *p*)

When observed variance *exceed* the expected value of the binomial then
some omitted variable is producing additional **dispersion** in observed
counts.

Ignoring over-dispersion can be just as bad as ignoring a predictor
variable.

Heterogeneity in counts can be a confound and hide effects or make
spurious inferences. (Waffles)

So we need to find the source of dispersion.

### Two Strategies

**Continuous Mixture Models**

Linear model is attached to distribution of observations instead of the
observations themselves.

**Multilevel Models**

estimate both residuals of each observation and the distribution of the
residuals. These are flexible and can handle over-dispersion and
heterogeneity at same time.

### Beta-Binomals

Estimates the distribution of probabilities of success instead of a
single probability of success.

Mixture of binomial distributions that uses each binomial count as its
own probability of success.

beta distribution has two parameters:

  - Average probability

  - Shape parameter
    
      - describes how spread out the distribution is.

Shape = 2 –\> probability from 0 to 1 equally likely

``` r
pbar <- 0.5
theta <- 2

curve(dbeta2(x, pbar, theta), from = 0, to = 1, xlab = "probability", ylab = "Density")
```

![](../Figures/Week%209/shape%202-1.png)<!-- -->

Shape \> 2 –\> distribution of probabilities grows more concentrated

``` r
pbar <- 0.5
theta <- 5

curve(dbeta2(x, pbar, theta), from = 0, to = 1, xlab = "probability", ylab = "Density")
```

![](../Figures/Week%209/shape%20greater%20than%202-1.png)<!-- -->

Shape \< 2 –\> extreme probabilities near 0 and 1 more likely than mean

``` r
pbar <- 0.5
theta <- 1

curve(dbeta2(x, pbar, theta), from = 0, to = 1, xlab = "probability", ylab = "Density")
```

![](../Figures/Week%209/shape-1.png)<!-- -->

### Fitting beta-binomial model

``` r
library(rethinking)
data("UCBadmit")
d <- UCBadmit

# Standardize
d$gid <- ifelse(d$applicant.gender == "male", 1L, 2L)

dat <- tibble(
  A = d$admit,
  N = d$applications,
  gid = d$gid
)

# Fit Model
m12.1 <- ulam(
  alist(
    A ~ dbetabinom(N, pbar, theta),
    logit(pbar) <- a[gid],
    a[gid] ~ dnorm(0, 1.5),
    transpars> theta <<- phi + 2.0,
    phi ~ dexp(1)
  ), data = dat, 
  chains = 4
)
```

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppParallel/include/"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DBOOST_NO_AUTO_PTR  -include '/Library/Frameworks/R.framework/Versions/4.0/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include   -fPIC  -Wall -g -O2  -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.0/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.0/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1
    ## 
    ## SAMPLING FOR MODEL '7524ef78bc3d11bac148cfd5e80323bf' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 6.2e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.62 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
    ## Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 2.89427 seconds (Warm-up)
    ## Chain 1:                3.07891 seconds (Sampling)
    ## Chain 1:                5.97319 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '7524ef78bc3d11bac148cfd5e80323bf' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 1.6e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.16 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:   1 / 1000 [  0%]  (Warmup)
    ## Chain 2: Iteration: 100 / 1000 [ 10%]  (Warmup)
    ## Chain 2: Iteration: 200 / 1000 [ 20%]  (Warmup)
    ## Chain 2: Iteration: 300 / 1000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 400 / 1000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 500 / 1000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 501 / 1000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 600 / 1000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 700 / 1000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 800 / 1000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 900 / 1000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 1000 / 1000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 2.93738 seconds (Warm-up)
    ## Chain 2:                3.01229 seconds (Sampling)
    ## Chain 2:                5.94968 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '7524ef78bc3d11bac148cfd5e80323bf' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 1.5e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.15 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:   1 / 1000 [  0%]  (Warmup)
    ## Chain 3: Iteration: 100 / 1000 [ 10%]  (Warmup)
    ## Chain 3: Iteration: 200 / 1000 [ 20%]  (Warmup)
    ## Chain 3: Iteration: 300 / 1000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 400 / 1000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 500 / 1000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 501 / 1000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 600 / 1000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 700 / 1000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 800 / 1000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 900 / 1000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 1000 / 1000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 2.85382 seconds (Warm-up)
    ## Chain 3:                3.01139 seconds (Sampling)
    ## Chain 3:                5.86521 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '7524ef78bc3d11bac148cfd5e80323bf' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 1.4e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.14 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:   1 / 1000 [  0%]  (Warmup)
    ## Chain 4: Iteration: 100 / 1000 [ 10%]  (Warmup)
    ## Chain 4: Iteration: 200 / 1000 [ 20%]  (Warmup)
    ## Chain 4: Iteration: 300 / 1000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 400 / 1000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 500 / 1000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 501 / 1000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 600 / 1000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 700 / 1000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 800 / 1000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 900 / 1000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 1000 / 1000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 2.87094 seconds (Warm-up)
    ## Chain 4:                2.99044 seconds (Sampling)
    ## Chain 4:                5.86138 seconds (Total)
    ## Chain 4:

    ## Warning: There were 1997 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
    ## http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded

    ## Warning: Examine the pairs() plot to diagnose sampling problems

    ## Warning: The largest R-hat is 3.04, indicating chains have not mixed.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#r-hat

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#bulk-ess

    ## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#tail-ess

``` r
# Extract Samples
post <- extract.samples(m12.1)
post$da <- post$a[,1] - post$a[,2]
precis(post, depth = 2)
```

    ##              mean        sd       5.5%     94.5%      histogram
    ## a[1]  -0.33294956 0.9842797 -1.6593526 0.9632629       ▁▂▇▁▁▃▇▁
    ## a[2]  -0.03004246 1.4533116 -2.0447543 2.2040150     ▂▃▃▃▅▃▁▁▁▇
    ## phi    0.94864232 0.6963220  0.1838768 2.3605573        ▇▃▃▂▁▁▁
    ## theta  2.94864232 0.6963220  2.1838768 4.3605573        ▇▃▃▂▁▁▁
    ## da    -0.30290710 2.1273820 -3.2923703 2.9536927 ▇▁▁▁▃▂▃▂▂▁▂▁▃▁

`a[1]` is the log-odds of admission for male applicants lower than
female.

*??? Wasn’t getting same results as book*

## Zero-inflated Outcomes

We sometimes measure mixtures of multiple process. And when there are
different causes for the same observation we use a **Mixture Model**
that uses more than one likelihood for the same outcome variable.

Zero means nothing happened, because rate of events is low or rather
because the process that generates events failed to get started.

![jayimage](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/jay.jpg)

### Drunk Monks

Monk example was a binomial process, but with large number of trials and
low probability we used a Poisson distribution.

New problem: *how often do monks take a break from writing and how often
do they drink?*

Hidden state: *drunk or sober*

**The Mixture**

Zeros in data?

  - Monks spent the day drinking

  - they worked but failed to complete any manuscripts

Parameters: *p* is probability the monks spend day drinking and *lambda*
is mean number of manuscripts completed when monks work.

**The Monster**

Define a likelihood function that mixes these two zero processes.
Simulated as a coin flip to decide whether they drink or work.

Here is that function:

![formulaimage](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/monks_drink_formula.png)

![chartimage](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/chart.png)

![chart2image](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/chart2.png)

> The probability of observing a zero is the probability of the monks
> drinking OR (+) the probability of the monks working AND (x) them not
> finishing a manuscript.

**ZIPoisson or Zero Inflated Poisson regression**

![ZIPformulaimage](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/ZIPoisson.png)

![chart3image](/Users/zwixom/School/Marketing/Quant%20Analytics/Repo/zack-wixom/Figures/Week%209/chart3.png)

``` r
## Define Parameters

# 20% of days
prob_drink <- 0.2

# Average 1 manuscript per day
rate_work <- 1

# sample one year of production
N <- 365

set.seed(365)

# simulate days monks drink
drink <- rbinom(N, 1, prob_drink)

# simulate manuscripts completed
y <- (1 - drink) * rpois(N, rate_work)
```

Result is *y* as a list of counts of completed manuscripts for each day
of year.

Zeros produced by drinking shown in blue and zeros from working shown in
black.

``` r
simplehist(y, xlab = "manuscripts complete", lwd = 4)
#calculate zero instances
zeros_drink <- sum(drink)
zeros_work <- sum(y == 0 & drink == 0)
zeros_total <- sum(y == 0)
lines(c(0, 0), c(zeros_work, zeros_total), lwd = 4, col = rangi2)
```

![](../Figures/Week%209/monk%20plot-1.png)<!-- -->

Now onto fitting the model:

``` r
m12.3 <- ulam(
  alist(
    y ~ dzipois(p, lambda),
    logit(p) <- ap,
    log(lambda) <- al,
    ap ~ dnorm(-1.5, 1),
    al ~ dnorm(1, 0.5)
  ), 
  data = list(y = y),
  chains = 4
)
```

    ## Trying to compile a simple C file

    ## Running /Library/Frameworks/R.framework/Resources/bin/R CMD SHLIB foo.c
    ## clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/Rcpp/include/"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/unsupported"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/BH/include" -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/StanHeaders/include/src/"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/StanHeaders/include/"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppParallel/include/"  -I"/Library/Frameworks/R.framework/Versions/4.0/Resources/library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DBOOST_NO_AUTO_PTR  -include '/Library/Frameworks/R.framework/Versions/4.0/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include   -fPIC  -Wall -g -O2  -c foo.c -o foo.o
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.0/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/Core:88:
    ## /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:1: error: unknown type name 'namespace'
    ## namespace Eigen {
    ## ^
    ## /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/src/Core/util/Macros.h:628:16: error: expected ';' after top level declarator
    ## namespace Eigen {
    ##                ^
    ##                ;
    ## In file included from <built-in>:1:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.0/Resources/library/StanHeaders/include/stan/math/prim/mat/fun/Eigen.hpp:13:
    ## In file included from /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/Dense:1:
    ## /Library/Frameworks/R.framework/Versions/4.0/Resources/library/RcppEigen/include/Eigen/Core:96:10: fatal error: 'complex' file not found
    ## #include <complex>
    ##          ^~~~~~~~~
    ## 3 errors generated.
    ## make: *** [foo.o] Error 1
    ## 
    ## SAMPLING FOR MODEL 'd1e812d40c58a935495385d618556a25' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 8.4e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.84 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
    ## Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
    ## Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
    ## Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
    ## Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.289864 seconds (Warm-up)
    ## Chain 1:                0.2177 seconds (Sampling)
    ## Chain 1:                0.507564 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL 'd1e812d40c58a935495385d618556a25' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 6e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.6 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:   1 / 1000 [  0%]  (Warmup)
    ## Chain 2: Iteration: 100 / 1000 [ 10%]  (Warmup)
    ## Chain 2: Iteration: 200 / 1000 [ 20%]  (Warmup)
    ## Chain 2: Iteration: 300 / 1000 [ 30%]  (Warmup)
    ## Chain 2: Iteration: 400 / 1000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 500 / 1000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 501 / 1000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 600 / 1000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 700 / 1000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 800 / 1000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 900 / 1000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 1000 / 1000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.258558 seconds (Warm-up)
    ## Chain 2:                0.251139 seconds (Sampling)
    ## Chain 2:                0.509697 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL 'd1e812d40c58a935495385d618556a25' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 5.7e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.57 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:   1 / 1000 [  0%]  (Warmup)
    ## Chain 3: Iteration: 100 / 1000 [ 10%]  (Warmup)
    ## Chain 3: Iteration: 200 / 1000 [ 20%]  (Warmup)
    ## Chain 3: Iteration: 300 / 1000 [ 30%]  (Warmup)
    ## Chain 3: Iteration: 400 / 1000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 500 / 1000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 501 / 1000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 600 / 1000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 700 / 1000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 800 / 1000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 900 / 1000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 1000 / 1000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.249425 seconds (Warm-up)
    ## Chain 3:                0.227693 seconds (Sampling)
    ## Chain 3:                0.477118 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL 'd1e812d40c58a935495385d618556a25' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 7.9e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.79 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:   1 / 1000 [  0%]  (Warmup)
    ## Chain 4: Iteration: 100 / 1000 [ 10%]  (Warmup)
    ## Chain 4: Iteration: 200 / 1000 [ 20%]  (Warmup)
    ## Chain 4: Iteration: 300 / 1000 [ 30%]  (Warmup)
    ## Chain 4: Iteration: 400 / 1000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 500 / 1000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 501 / 1000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 600 / 1000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 700 / 1000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 800 / 1000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 900 / 1000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 1000 / 1000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.277378 seconds (Warm-up)
    ## Chain 4:                0.220536 seconds (Sampling)
    ## Chain 4:                0.497914 seconds (Total)
    ## Chain 4:

``` r
precis(m12.3)
```

    ##           mean        sd       5.5%      94.5%    n_eff    Rhat4
    ## ap -1.29083107 0.3784356 -1.9535842 -0.7790829 543.2908 1.003459
    ## al  0.01137333 0.0914561 -0.1365572  0.1589068 647.3281 1.000952

On natural scale the posterior means are below:

``` r
post <- extract.samples(m12.3)

# probability drink
mean(inv_logit(post$ap))
```

    ## [1] 0.2223358

``` r
# rate finish manuscripts when not drinking
mean(exp(post$al))
```

    ## [1] 1.015661

## Honorable Mentions

More Mixture Models

  - ZIBinomial
  - Zero-Augmented
  - Gamma-Poisson
  - Multilevel Models

## Ordered Categories

Common in social sciences (Marketing) are ranking or ordered questions
that created ordered categories.
