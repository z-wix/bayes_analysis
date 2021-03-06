Assignment 5
================

``` r
# Packages
library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(rethinking)

# Load Data
data("Wines2012")

d <- Wines2012
```

## Question 1

``` r
# Standardize  and Index Variables
wines <- list(
    score = standardize(d$score),
    judge_id = as.integer(d$judge),
    wine_id = as.integer(d$wine)
)
```

Not sure if I need to also do the log and std of the outcome variable or
not. I tried and it wasn’t working so I will keep with just the original
standardized `score` variable.

``` r
# Fit model
m1.1 <- quap(
  alist(
    score ~ dnorm(mu, sigma),
    mu <- a[judge_id] + b[wine_id],
    a[judge_id] ~ dnorm(0, 0.5),
    b[wine_id] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = wines
)
```

``` r
plot(precis(m1.1, 2))
```

![](../Figures/q1.1%20precis-1.png)<!-- -->

``` r
# Fit Model
m1.2 <- ulam(
  alist(
    score ~ dnorm(mu, sigma),
    mu <- a[judge_id] + b[wine_id],
    a[judge_id] ~ dnorm(0, 0.5),
    b[wine_id] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ),
  data=wines, 
  chains=4, 
  cores = 4
)
```

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

``` r
plot(precis(m1.2, depth = 2))
```

![](../Figures/q1.2%20plot-1.png)<!-- -->

**How do you interpret the variation among individual judges and
individual wines?**

I am assuming that different judges have different preferences for
certain wines.

**Do you notice any patterns, just by plotting the differences?**

I wasn’t able to get to get the plot to work, perhaps my priors are off
as well. But I do see there are a lot of differences between the judges
and not the wines, maybe suggesting that there isn’t much difference
between them.

**Which judges gave the highest/lowest ratings?**

``` r
d %>% 
  group_by(judge) %>% 
  summarize(
    id = max(as.integer(judge))
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 9 x 2
    ##   judge              id
    ##   <fct>           <int>
    ## 1 Daniele Meulder     1
    ## 2 Francis Schott      2
    ## 3 Jamal Rayyis        3
    ## 4 Jean-M Cardebat     4
    ## 5 John Foy            5
    ## 6 Linda Murphy        6
    ## 7 Olivier Gergaud     7
    ## 8 Robert Hodgson      8
    ## 9 Tyler Colman        9

*Lowest*: Robert Hodgson, Jean-M Cardebat, and Tyler Colman.

*Highest*: John Foy and Linda Murphy.

**Which wines were rated worst/best on average?**

``` r
d %>% 
  group_by(wine) %>% 
  summarize(
    id = max(as.integer(wine))
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 20 x 2
    ##    wine     id
    ##    <fct> <int>
    ##  1 A1        1
    ##  2 A2        2
    ##  3 B1        3
    ##  4 B2        4
    ##  5 C1        5
    ##  6 C2        6
    ##  7 D1        7
    ##  8 D2        8
    ##  9 E1        9
    ## 10 E2       10
    ## 11 F1       11
    ## 12 F2       12
    ## 13 G1       13
    ## 14 G2       14
    ## 15 H1       15
    ## 16 H2       16
    ## 17 I1       17
    ## 18 I2       18
    ## 19 J1       19
    ## 20 J2       20

*Highest wines:* B2 and J2

*Lowest wines:* I2 and C2

## Question 2

``` r
# Standardize  and Index Variables
wines2 <- list(
    score = standardize(d$score),
    wine_id = d$wine.amer + 1L,
    judge_id = d$judge.amer + 1L,
    flight_id = as.integer(d$flight)
)
```

``` r
# Fit Model
m1.2 <- ulam(
  alist(
    score ~ dnorm(mu, sigma),
    mu <- w[wine_id] + j[judge_id] + f[flight_id],
    w[wine_id] ~ dnorm(0, 0.5),
    j[wine_id] ~ dnorm(0, 0.5),
    f[wine_id] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), 
  data = wines2, 
  chains = 4, 
  cores = 4
)
```

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

``` r
plot(precis(m1.2, depth = 2))
```

![](../Figures/q2%20plot-1.png)<!-- -->

**What do you conclude about the differences among the wines and
judges?**

It looks like there aren’t differences between the red and white wine
scoring but there is a noticeable difference between where the judge and
wine are from. American judges will score higher than French. However,
the French wines score higher than American wines.

## Question 3

For this question I am doing a different approach. Not sure which one is
the best in general but I think it is better for an indicator approach
to the predictor variables, than mbefore I was using index variables.

``` r
# Standardize and M<ake Indicator Variables
wines3 <- list(
    score = standardize(d$score),
    wine = d$wine.amer,
    judge = d$judge.amer,
    flight = ifelse(d$flight == "red", 1, 0)
)
```

For the interactions I am doing three different ones: *wine & judge,
wine & flight,* and *judge & flight.*

I saw something that suggested to do interaction prior tighter so that’s
why I chose these priors.

``` r
# Fit Model
m1.3 <- ulam(
  alist(
    score ~ dnorm(mu, sigma),
    mu <- a + 
      bW*wine + 
      bJ*judge + 
      bF*flight + 
      bWJ*wine*judge + 
      bWF*wine*flight + 
      bJF*judge*flight,
    a ~ dnorm(0, 0.2),
    bW ~ dnorm(0, 0.5),
    bJ ~ dnorm(0, 0.5),
    bF ~ dnorm(0, 0.5),
    bWJ ~ dnorm(0, 0.25),
    bWF ~ dnorm(0, 0.25),
    bJF ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ), 
  data = wines3, 
  chains = 4, 
  cores = 4
)
```

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

``` r
plot(precis(m1.3))
```

![](../Figures/q3%20plot-1.png)<!-- -->

Now to use `link()` to interpret the models predictions

``` r
# Predictive data
pred_data <- tibble(
  wine = rep(0:1, times = 4),
  judge = rep(0:1, each = 4),
  flight = rep(c(0,0,1,1), times = 2)
)

# Link
mu <- link(m1.3, data = pred_data)

# Row names for plot easier interpretation (found a resource to add these feature made it easier)
row_labels <- paste(ifelse(pred_data$wine == 1, "A", "F"),
                    ifelse(pred_data$judge == 1, "A", "F"),
                    ifelse(pred_data$flight == 1, "R", "W"), 
                    sep = "")

# Plot precis()
plot(precis(list(mu = mu), 2), labels = row_labels)
```

![](../Figures/q3%20link-1.png)<!-- -->

**What do you conclude about the features and the scores? Can you relate
the results of your model(s) to the individual judge and wine inferences
from Problem 1?**

Now with labels I can read this better and see that American Red Wine
that scores low from French judge. While French Red Wine scores high
from American Judge.

In general this is similar to my inferences from problem one. It looks
like French judges score low on average and american score higher.
French wines score higher compared to American ones, but there isn’t
much difference between Red and White wines that I can realy tell here.
