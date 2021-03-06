Smash Data Analysis
================
Zack Wixom

![Super Smash
Logo](/Project/Figures/Smash/super-smash-bros-ultimate.png)

Now that I have data and a success metric that I want to model on. I can
start to build out some models. I am going to start with a simple model
using quadratic approximation before moving onto a `ulam()`.

### Set Up

First I am going to be loading some packages I will need and import my
data from `01_smash_data.Rmd`.

``` r
# Packages
library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(rethinking)
library(dagitty)

# Set Seed
set.seed(42)

# Load Data
smash <- read_csv(here::here("Project/Data", "smash_data.csv")) %>% 
  select(-contains("rank")) # Don't need these variables

smash
```

    ## # A tibble: 94 x 52
    ##    character score total_players per_played string1 string2 string3 base_accel
    ##    <chr>     <dbl>         <dbl>      <dbl>   <dbl>   <dbl>   <dbl> <chr>     
    ##  1 Cloud       449           190       3.66     102      55      33 0.01      
    ##  2 Bayonetta   290           151       2.91      26      87      38 0.01      
    ##  3 Terry       276           135       2.6       46      49      40 <NA>      
    ##  4 Joker       236           114       2.2       44      34      36 <NA>      
    ##  5 Ganondorf   223           110       2.12      35      43      32 0.01      
    ##  6 Young Li…   213           129       2.48      26      32      71 0.02      
    ##  7 Snake       199            97       1.87      39      24      34 0.01      
    ##  8 Roy         194            96       1.85      26      46      24 0.02      
    ##  9 Marth       194            93       1.79      31      39      23 0.01      
    ## 10 Inkling     194            95       1.83      35      29      31 0.01      
    ## # … with 84 more rows, and 44 more variables: additional_accel <chr>,
    ## #   max_accel <chr>, Air.speed <dbl>, Regular.Fall <dbl>, Fast.Fall <dbl>,
    ## #   X..Increase <chr>, Gravity <dbl>, fullhop_height <chr>,
    ## #   shorthop_height <chr>, airjump_height <chr>, shorthop_dur <dbl>,
    ## #   fullhop_dur <dbl>, SH.Fast.Fall <dbl>, FH.Fast.Fall <dbl>, Weight <dbl>,
    ## #   Hard.Land <dbl>, Soft.Land..Universal. <dbl>, Walk.Speed <dbl>,
    ## #   Initial.Dash <dbl>, Run.Speed <dbl>, Dash.Frames <chr>,
    ## #   Pivot.Dash.Frames <chr>, Fast.Initial.Dash <chr>,
    ## #   Normal.Buffer.Window <chr>, Slow.Full.Dash <chr>, Grab.Range <chr>,
    ## #   Attack.Range <chr>, Attack.Frames <chr>, Neutral.Getup <chr>, Roll <chr>,
    ## #   Jump <chr>, X.1 <chr>, Fastest.Move.s. <chr>, X.2 <dbl>,
    ## #   X2nd.Fastest.Move.s. <chr>, X.3 <dbl>, X3rd.Fastest.Move.s. <chr>,
    ## #   Grab <chr>, Grab..Post.Shieldstun <chr>, Item.Throw.Forward. <chr>,
    ## #   Item.Throw.Back. <chr>, Jump.Z.Drop..Front. <chr>,
    ## #   Jump.Z.Drop.Behind. <chr>, Key <chr>

Some of the characters do not have all the frame stats available. I
might need to take care of this later if having NA’s present cause an
issue, but for now I will not worry about them. However, I really don’t
need the rankings per frame stat, so I will just drop those columns.

### Clean Up Data

**The Outcome Variable**

The variable I will be modeling on is `per_played`. This is a ratio for
how often this character is chosen and I want to know what influences
peoples choice of each character.

**The Explanatory Variables**

There are a lot of variables in this dataset. So I want to make it
simple and just use the most realistic variables that might go into a
person’s choice of a character. Here is a list of the variables that are
most important to gameplay and are used in strategic moves:

  - `string1`: character is first string choice
  - `base_accel`: base acceleration
  - `Air.speed`: speed in air
  - `Fast.Fall`: fast fall speed
  - `fullhop_height`: height of full hop
  - `shorthop_height`: height of short hop
  - `airjump_height`: height of midair jump
  - `Weight`: weight
  - `Run.Speed`: \# of frames to pivot dash
  - `Grab.Range`: range of grab
  - `Fastest.Move.s.`: Fastest move out of shield (I need to clean up
    this variable a little)

> Note: I do not have damage data as of right now. It might be good to
> incorporate damage data for certain moves as this could be an
> indicator of character preference, however, the real reason players
> are good are because they understand the mechanics of their preferred
> character and know how to use it. So for a pro the actual damage
> output might not be as important as knowing how to move and time
> combos, leading to rampping up damage, and delivering a kill.

``` r
vars <- c(1, 3, 4, 5, 8, 11, 13, 16:18, 23, 28, 34, 41)

smash_tidy <- smash %>% 
  select(vars) %>% 
  # Clean up Fastest Move Variable
  rename(oos_move = Fastest.Move.s.) %>% 
  separate_rows(oos_move, sep = ",")

unique(smash_tidy$oos_move[order(smash_tidy$oos_move)])

# Recode some of the character values that are different spellijngs
smash_tidy$oos_move[smash_tidy$oos_move == "Up B"] <- "UpB"
smash_tidy$oos_move[smash_tidy$oos_move == " Bair"] <- "Bair"
smash_tidy$oos_move[smash_tidy$oos_move == " Fair"] <- "Fair"
smash_tidy$oos_move[smash_tidy$oos_move == " Uair"] <- "Uair"
smash_tidy$oos_move[smash_tidy$oos_move == " Usmash"] <- "Usmash"
smash_tidy$oos_move[smash_tidy$oos_move == "USmash"] <- "Usmash"
smash_tidy$oos_move[smash_tidy$oos_move == "NAir"] <- "Nair"
smash_tidy$oos_move[smash_tidy$oos_move == " NeutralB (Air)"] <- "B"
smash_tidy$oos_move[smash_tidy$oos_move == " DownB (Air)"] <- "DownB"
smash_tidy$oos_move[smash_tidy$oos_move == "DownB (Air)"] <- "DownB"
smash_tidy$oos_move[smash_tidy$oos_move == "UpB(3)"] <- "UpB"
smash_tidy$oos_move[smash_tidy$oos_move == "UpB(2) or Echo Reflector (air)"] <- "UpB"
smash_tidy$oos_move[smash_tidy$oos_move == "UpB (air)"] <- "UpB"
smash_tidy$oos_move[smash_tidy$oos_move == "UpB (Air)"] <- "UpB"
smash_tidy$oos_move[smash_tidy$oos_move == "Uair or Fair"] <- "Fair"
smash_tidy$oos_move[is.na(smash_tidy$oos_move)] <- "none"
  

write.csv(smash_tidy, here::here("Project", "Data", "smash_tidy.csv"), row.names = FALSE)
```

## Round 1

Just for fun, let’s put in all the variables and see what happens.

  - `string1`: character is first string choice
  - `base_accel`: base acceleration
  - `Air.speed`: speed in air
  - `Fast.Fall`: fast fall speed
  - `fullhop_height`: height of full hop
  - `shorthop_height`: height of short hop
  - `airjump_height`: height of midair jump
  - `Weight`: weight
  - `Run.Speed`: \# of frames to pivot dash
  - `Grab.Range`: range of grab
  - `oos_move`: Fastest move out of shield

<!-- end list -->

``` r
# Load Data
smash_tidy <- read_csv(here::here("Project", "Data", "smash_tidy.csv"))
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   character = col_character(),
    ##   total_players = col_double(),
    ##   per_played = col_double(),
    ##   string1 = col_double(),
    ##   base_accel = col_character(),
    ##   Air.speed = col_double(),
    ##   Fast.Fall = col_double(),
    ##   fullhop_height = col_character(),
    ##   shorthop_height = col_character(),
    ##   airjump_height = col_character(),
    ##   Weight = col_double(),
    ##   Run.Speed = col_double(),
    ##   Grab.Range = col_character(),
    ##   oos_move = col_character()
    ## )

``` r
# Standardize and Index Variables
smash1 <- tibble(
  played = standardize(smash_tidy$per_played),
  string1 = standardize(smash_tidy$string1),
  accel = standardize(as.numeric(smash_tidy$base_accel)),
  air = standardize(smash_tidy$Air.speed),
  fall = standardize(smash_tidy$Fast.Fall),
  fullhop = standardize(as.numeric(smash_tidy$fullhop_height)),
  shorthop = standardize(as.numeric(smash_tidy$shorthop_height)),
  airjump = standardize(as.numeric(smash_tidy$airjump_height)),
  weight = standardize(smash_tidy$Weight),
  run = standardize(smash_tidy$Run.Speed),
  grab = standardize(as.numeric(smash_tidy$Grab.Range)),
  oosmove = as.integer(factor(smash_tidy$oos_move))
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.0 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + 
      b1 * string1 +
      bA * accel + 
      bAr * air + 
      bF * fall + 
      bH * fullhop + 
      bS * shorthop + 
      bAj * airjump + 
      bW * weight + 
      bR * run + 
      bG * grab,
    a ~ dnorm(0, 0.2),
    c(b1, bA, bAr, bF, bH, bS, bAj, bW, bR, bG) ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = smash1
)

# Plot Precis
plot(precis(m1.0))
```

![](../Figures/Smash/m1.0-1.png)<!-- -->

So this is pretty interesting. There are a few variables that are really
influencing the model. Namely, `fullhop_height`, `shorthop_height`,
`Fast.Fall`, and `weight`. Perhaps this is because aerials attacks and
movement is key for competitive play.

`string1` is not surprising. This variable is an indicator of if this
character is their first choice and so this directly effects
`per_played`. I think that perhaps I won’t use this variable in my
analysis, but in other iterations use `string2` and `string3.` My logic
being, if a character is someones second or third choice then it might
be an indicator of per\_played. But this might give us the same result.

I haven’t tried putting in the categorical variable of `oos_move`. I
will deal with adding that later.

Let’s do a quick prior predicitve check.

``` r
# Extract the prior (first set.seed).
set.seed(42)
prior0 <- extract.prior(m1.0)
# Use link() to compute mu without having to write out the linear function.
mu <- link(m1.0, post = prior0, data = list(
  string1 = c(-2, 2),
  accel = c(-2, 2),
  air = c(-2, 2),
  fall = c(-2, 2),
  fullhop = c(-2, 2),
  shorthop = c(-2, 2),
  airjump = c(-2, 2),
  weight = c(-2, 2),
  run = c(-2, 2),
  grab = c(-2, 2)
))
# Base plot plus "layers".
plot(NULL, xlim = c(-2, 2), ylim = c(-2,2))
for (i in 1:50) {
  lines(c(-2, 2), mu[i,], col = col.alpha("black",0.4))
}
```

![](../Figures/Smash/prior0%20check-1.png)<!-- -->

I can’t really remember how to interpret this chart. It doesn’t look too
crazy but is still not really reasonable.

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.0)
# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.0, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)
# Plot posterior predictions.
plot(
  mu_mean ~ smash1$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.0 Posterior Predictive Check"
)
abline(a = 0, b = 1, lty = 2)
for (i in 1:nrow(smash1)) {
  lines(rep(smash1$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/pred%20check-1.png)<!-- -->

## Round 2

I want to see if I just use the variables that were sticking out if they
will still be significant on their own.

``` r
# Fit Model
m0 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + 
      b1 * string1 +
      bF * fall + 
      bH * fullhop + 
      bS * shorthop + 
      bW * weight,
    a ~ dnorm(0, 0.2),
    c(b1, bF, bH, bS, bW) ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = smash1
)

# Plot Precis
plot(precis(m0))
```

![](../Figures/Smash/m0-1.png)<!-- -->

So they are still pretty significant. And if we do a predicitve check

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m0)
# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m0, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)
# Plot posterior predictions.
plot(
  mu_mean ~ smash1$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M0 Posterior Predictive Check"
)
abline(a = 0, b = 1, lty = 2)
for (i in 1:nrow(smash1)) {
  lines(rep(smash1$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/m0%20pred%20check-1.png)<!-- -->

It looks like these variables are accounting for most of the variation
happening in the data. But I want to see if I take out `string1` if this
changes. Since `string1` is so connected to `played` I need to do a
correlation check.

### Correlation Check

``` r
# Check Correlation of model
diag(vcov(m0))
```

    ##           a          b1          bF          bH          bS          bW 
    ## 0.002218283 0.002514752 0.002472422 0.004903095 0.004738594 0.002343512 
    ##       sigma 
    ## 0.001189993

``` r
cov2cor(vcov(m0))
```

    ##                   a           b1          bF          bH          bS
    ## a      1.0000000000 -0.009228959 -0.03845522  0.02111885 -0.02145840
    ## b1    -0.0092289595  1.000000000 -0.12465180  0.13419033 -0.06990221
    ## bF    -0.0384552248 -0.124651800  1.00000000 -0.16842952  0.03612219
    ## bH     0.0211188479  0.134190327 -0.16842952  1.00000000 -0.74122836
    ## bS    -0.0214584016 -0.069902214  0.03612219 -0.74122836  1.00000000
    ## bW     0.0056020499  0.011085443 -0.28473553  0.09557086  0.03188035
    ## sigma -0.0007660315 -0.136814611 -0.00932384  0.04037139 -0.04624211
    ##                bW         sigma
    ## a      0.00560205 -0.0007660315
    ## b1     0.01108544 -0.1368146108
    ## bF    -0.28473553 -0.0093238400
    ## bH     0.09557086  0.0403713908
    ## bS     0.03188035 -0.0462421130
    ## bW     1.00000000  0.0187601348
    ## sigma  0.01876013  1.0000000000

So `fullhop` and `shorthop` are fairly correlated. Let’s do a further
check, we might not need both, since intuitively if we know the shorthop
height is low, then the full hop must also be lower. It is similar to
the right and left leg example from our textbook.

``` r
post <- extract.samples(m0)
plot(bH ~ bS,
     post,
     col=col.alpha(rangi2,0.1),
     pch=16,
    main = "M0 Correlation between Short Hop and Full Hop")
```

![](../Figures/Smash/corr%20plot-1.png)<!-- -->

So there is a general correlation. Perhaps an interaction between the
two would be best for the model. Either that, or I will just take
Fullhop out.

``` r
# Fit Model
m0.2 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + 
      b1 * string1 +
      bF * fall + 
      bH * fullhop +
      bS * shorthop +
      bW * weight +
      bHS * fullhop * shorthop,
    a ~ dnorm(0, 0.2),
    c(b1, bF, bH, bS, bW, bHS) ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = smash1
)

# Plot Precis
plot(precis(m0.2))
```

![](../Figures/Smash/m0.2-1.png)<!-- -->

The interaction didn’t work. So let’s just do Shorthop.

``` r
# Fit Model
m0.3 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + 
      b1 * string1 +
      bF * fall + 
      bS * shorthop +
      bW * weight,
    a ~ dnorm(0, 0.2),
    c(b1, bF, bS, bW) ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = smash1
)

# Plot Precis
plot(precis(m0.3))
```

![](../Figures/Smash/m0.3-1.png)<!-- -->

Looks like once I get rid of full hop I see a decrease in significance
for shorthop and even with some other variables. So perhaps keeping
fullhop and getting rid of shorthop we will see something happen.

``` r
# Fit Model
m0.4 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + 
      b1 * string1 +
      bF * fall + 
      bH * fullhop +
      bW * weight,
    a ~ dnorm(0, 0.2),
    c(b1, bF, bH, bW) ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = smash1
)

# Plot Precis
plot(precis(m0.4))
```

![](../Figures/Smash/m0.4-1.png)<!-- -->

I will keep both variables. I think that the correlation is not bad, but
they do influence each other and need to both be conditioned on the
model.

I am going to do the same thing with weight so I can see if it
influencing short and full hops and fast fall

``` r
# Fit Model
m0.5 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + 
      b1 * string1 +
      bF * fall + 
      bH * fullhop +
      bW * weight,
    a ~ dnorm(0, 0.2),
    c(b1, bF, bH, bW) ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = smash1
)

# Plot Precis
plot(precis(m0.5))
```

![](../Figures/Smash/m0.5-1.png)<!-- -->

### DAG

So let’s make a dag for this set of variables.

  - X = `per_played` *the outcome variable*
  - 1 = `string1`
  - W = `Weight`
  - S = `shorthop_height`
  - H = `fullhop_height`
  - F = `Fast.fall`

<!-- end list -->

``` r
# Create DAG
dag0 <- dagitty("dag{
    W -> X; 
    S -> X;
    H -> X;
    F -> X;
    W -> S;
    W -> H;
    W -> F;
    H -> S;
    S -> H;
    H -> W;
    H -> F
}")

# DAG coordinates
coordinates(dag0) <- list(
  x = c(X = 2, W = 2, S = 3, H = 3, F = 1),
  y = c(X = 2, W = 0, S = 0, H = 2, F = 0)
)

# Draw DAG
drawdag(dag0)
```

![](../Figures/Smash/dag0-1.png)<!-- -->

Our DAG shows how fullhop and shorthop are influencing each other but
also has the fullhop influence on fast fall and weight.

## Other Rounds

This section shows other iterations that I was doing to see what
variables were best used. Ultimately I think the most telling are from
the previous models so that is what my findings are based on.

Now I will create a DAG that I can use to determine my models. here are
my variables that I am going to start with:

  - X = `per_played` the outcome variable
  - W = `Weight`
  - S = `shorthop_height`
  - G = `Grab.Range`
  - R = `Run.Speed`

These are pretty basic components that should be considered when picking
a character. These are *Grab Range*, *Run Speed*, *Short Hop*, and
*Weight*. Weight is related to a lot of the other variables, besides
Grab.

``` r
# Create DAG
dag1 <- dagitty("dag{
    W -> X; 
    S -> X;
    G -> X;
    R -> X;
    W -> S;
    W -> R
}")

# DAG coordinates
coordinates(dag1) <- list(
  x = c(X = 2, W = 2, S = 3, G = 1, R = 1),
  y = c(X = 2, W = 0, S = 0, G = 2, R = 0)
)

# Draw DAG
drawdag(dag1)
```

![](../Figures/Smash/dag1-1.png)<!-- -->

There are a few interactions that we need to watch. Since it is a fork
from W to X, S, and R we will condition all the variables in the model.

### Standardize Variables

We are going to standardize each variable, there are a couple that need
to be converted to numeric because of they are character variables.

Here are the variables I will standardize

  - played = `per_played` the outcome variable
  - weight = `Weight`
  - shorthop = `shorthop_height` make numeric to get rid of \~’s
  - grab = `Grab.Range` make numeric to get rid of \~’s
  - run = `Run.Speed`

<!-- end list -->

``` r
# Initial Density Graphs
dens(smash1$played)
```

![](../Figures/Smash/unnamed-chunk-1-1.png)<!-- -->

``` r
dens(smash1$weight)
```

![](../Figures/Smash/unnamed-chunk-1-2.png)<!-- -->

``` r
dens(smash1$shorthop)
```

![](../Figures/Smash/unnamed-chunk-1-3.png)<!-- -->

``` r
dens(smash1$grab)
```

![](../Figures/Smash/unnamed-chunk-1-4.png)<!-- -->

``` r
dens(smash1$run)
```

![](../Figures/Smash/unnamed-chunk-1-5.png)<!-- -->

There are a couple of outliers happening in grab and run. I think this
is because there are a few characters that are known to have this
extreme stats. So it is not surprising, but I might need to log these
two variables.

I also do not have any discrete varables at the moment. In a later
iteration this may change especially as I group by character.

### Build Model

``` r
# Standardize and Index Variables
smash1.1 <- tibble(
  played = standardize(smash_tidy$per_played),
  shorthop = standardize(as.numeric(smash_tidy$shorthop_height)),
  weight = standardize(smash_tidy$Weight),
  run = standardize(smash_tidy$Run.Speed),
  grab = standardize(as.numeric(smash_tidy$Grab.Range))
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.1 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bW * weight + bS * shorthop + bG * grab + bR * run,
    a ~ dnorm(0, .2),
    bW ~ dnorm(0, .5),
    bS ~ dnorm(0, .5),
    bG ~ dnorm(0, .5),
    bR ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = smash1.1
)

# Plot Precis
plot(precis(m1.1))
```

![](../Figures/Smash/m1-1.png)<!-- -->

So we can see that there the variables are straddling 0 but there is
some pull for *Grab* and *Run Speed*. However, I think that I still need
to fix this to see if there is something else influencing this.

``` r
# Extract the prior (first set.seed).
set.seed(42)
prior1 <- extract.prior(m1.1)
# Use link() to compute mu without having to write out the linear function.
mu <- link(m1.1, post = prior1, data = list(
  shorthop = c(-2, 2),
  weight = c(-2, 2),
  run = c(-2, 2),
  grab = c(-2, 2)
))
# Base plot plus "layers".
plot(NULL, xlim = c(-2, 2), ylim = c(-2,2))
for (i in 1:50) {
  lines(c(-2, 2), mu[i,], col = col.alpha("black",0.4))
}
```

![](../Figures/Smash/unnamed-chunk-2-1.png)<!-- -->

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.1)
# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.1, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)
# Plot posterior predictions.
plot(
  mu_mean ~ smash1.1$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.1 Posterior Predictive Check"
)
abline(a = 0, b = 1, lty = 2)
for (i in 1:nrow(smash1.1)) {
  lines(rep(smash1.1$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-3-1.png)<!-- -->

Alright so it looks like that without a lot of the variables we can’t
get a good prediction. There must be an optimal set of variables that
will be useful and these are not the most useful. Let’s dive into the
variables more to get a better idea.

### Iteration 2

I want to see if I don’t include the `weight` variable if it changes
things.

There are a couple of outliers happening in grab and run. I think this
is because there are a few characters that are known to have this
extreme stats. So it is not surprising, but I might need to log these
two variables.

I also do not have any discrete varables at the moment. In a later
iteration this may change especially as I group by character.

### Build Model

``` r
# Standardize and Index Variables
smash1.2 <- tibble(
  played = standardize(smash_tidy$per_played),
  shorthop = standardize(as.numeric(smash_tidy$shorthop_height)),
  run = standardize(smash_tidy$Run.Speed),
  grab = standardize(as.numeric(smash_tidy$Grab.Range))
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.2 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bS * shorthop + bG * grab + bR * run,
    a ~ dnorm(0, .2),
    bS ~ dnorm(0, .5),
    bG ~ dnorm(0, .5),
    bR ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = smash1.2
)

# Plot Precis
plot(precis(m1.2))
```

![](../Figures/Smash/m1.2-1.png)<!-- -->

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.2)
# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.2, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)
# Plot posterior predictions.
plot(
  mu_mean ~ smash1.2$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.2 Posterior Predictive Check"
)
abline(a = 0, b = 1, lty = 2)
for (i in 1:nrow(smash1.2)) {
  lines(rep(smash1.2$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-4-1.png)<!-- -->

No difference. So let’s look at some new variables.

### Iteration 3

Let’s try with just `weight` variable.

``` r
# Standardize and Index Variables
smash1.3 <- tibble(
  played = standardize(smash_tidy$per_played),
  weight = standardize(smash_tidy$Weight)
) %>% 
  drop_na()

# Fit Model
m1.3 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bW * weight,
    a ~ dnorm(0, .2),
    bW ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = smash1.3
)

# Plot Precis
plot(precis(m1.3))
```

![](../Figures/Smash/m1.3-1.png)<!-- -->

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.3)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.3, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.3$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.3 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.3)) {
  lines(rep(smash1.3$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-5-1.png)<!-- -->

### Iteration 4

Let’s look at just the `shorthop` variable.

``` r
# Standardize and Index Variables
smash1.4 <- tibble(
  played = standardize(smash_tidy$per_played),
  shorthop = standardize(as.numeric(smash_tidy$shorthop_height))
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.4 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bS * shorthop,
    a ~ dnorm(0, .2),
    bS ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = smash1.4
)

# Plot Precis
plot(precis(m1.4))
```

![](../Figures/Smash/m1.4-1.png)<!-- -->

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.4)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.4, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.4$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.4 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.4)) {
  lines(rep(smash1.4$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-6-1.png)<!-- -->

### Iteration 5

Since I saw that `grab` was significant, let’s just condition on that
variable.

``` r
# Standardize and Index Variables
smash1.5 <- tibble(
  played = standardize(smash_tidy$per_played),
  grab = standardize(as.numeric(smash_tidy$Grab.Range))
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.5 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bG * grab,
    a ~ dnorm(0, .2),
    bG ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = smash1.5
)

# Plot Precis
plot(precis(m1.5))
```

![](../Figures/Smash/m1.5-1.png)<!-- --> So with `grab` all alone we see
that it is consistently negative. Looks like perhaps people do not like
characters with long grabs.

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.5)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.5, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.5$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.5 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.5)) {
  lines(rep(smash1.5$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-7-1.png)<!-- -->

*Grab* alone is showing a difference in the posterior check. So perhaps
this variable will for sure stay in the final model.

### Iteration 6

Since I saw that `run` also was slightly significant, let’s just
condition on that variable.

``` r
# Standardize and Index Variables
smash1.6 <- tibble(
  played = standardize(smash_tidy$per_played),
  run = standardize(smash_tidy$Run.Speed)
) %>% 
  drop_na()

# Fit Model
m1.6 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bR * run,
    a ~ dnorm(0, .2),
    bR ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = smash1.6
)

# Plot Precis
plot(precis(m1.6))
```

![](../Figures/Smash/m1.6-1.png)<!-- -->

So it looks like it has a slightly more positive effect on percentage
played.

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.6)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.6, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.6$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.6 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.6)) {
  lines(rep(smash1.6$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-8-1.png)<!-- -->

### Iteration 7

However, I do need to check if `weight` influences `run`. So I will do
one more model conditioning on `weight` and `run`.

``` r
# Standardize and Index Variables
smash1.7 <- tibble(
  played = standardize(smash_tidy$per_played),
  weight = standardize(smash_tidy$Weight),
  run = standardize(smash_tidy$Run.Speed)
) %>% 
  drop_na()

# Fit Model
m1.7 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bW * weight + bR * run,
    a ~ dnorm(0, .2),
    bW ~ dnorm(0, 0.5),
    bR ~ dnorm(0, .5),
    sigma ~ dexp(1)
  ), data = smash1.7
)

# Plot Precis
plot(precis(m1.7))
```

![](../Figures/Smash/m1.7-1.png)<!-- -->

Weight does increase slightly here. Lets add an interaction between the
two variables.

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.7)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.7, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.7$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.7 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.7)) {
  lines(rep(smash1.7$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-9-1.png)<!-- -->

``` r
# Standardize and Index Variables
smash1.8 <- tibble(
  played = standardize(smash_tidy$per_played),
  weight = standardize(smash_tidy$Weight),
  run = standardize(smash_tidy$Run.Speed)
) %>% 
  drop_na()

# Fit Model
m1.8 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bW * weight + bR * run + bWR * weight * run,
    a ~ dnorm(0, .2),
    bW ~ dnorm(0, 0.5),
    bR ~ dnorm(0, .5),
    bWR ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = smash1.8
)

# Plot Precis
plot(precis(m1.8))
```

![](../Figures/Smash/m1.8-1.png)<!-- -->

Well it doesn’t look like we need to include this interaction.

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.8)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.8, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.8$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.8 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.8)) {
  lines(rep(smash1.8$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-10-1.png)<!-- -->

### Iteration 9

``` r
# Standardize and Index Variables
smash1.9 <- tibble(
  played = standardize(smash_tidy$per_played),
  weight = standardize(smash_tidy$Weight),
  run = standardize(smash_tidy$Run.Speed),
  grab = standardize(as.numeric(smash_tidy$Grab.Range))
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.9 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bW * weight + bR * run + bG * grab,
    a ~ dnorm(0, .2),
    bW ~ dnorm(0, 0.5),
    bR ~ dnorm(0, .5),
    bG ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = smash1.9
)

# Plot Precis
plot(precis(m1.9))
```

![](../Figures/Smash/m1.9-1.png)<!-- -->

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.9)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.9, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.9$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.9 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.9)) {
  lines(rep(smash1.9$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-11-1.png)<!-- -->

### Comparing Models

Let’s plot each model (`m1.5`, `m1.6`, `m1.7`, `m1.8`, `m1.9`) next to
each other so we can see if there are any hidden effects

``` r
# WAIC.
# plot(compare(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, m1.8, m1.9, func = WAIC))
# # PSIS.
# plot(compare(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, m1.8, m1.9, func = PSIS))

# Plot all models together
plot(coeftab(m1.5, m1.6, m1.7, m1.8, m1.9))
```

![](../Figures/Smash/unnamed-chunk-12-1.png)<!-- -->

Well it looks like perhaps`shorthop` will not be useful for me at this
moment. We should try new variables but keep `weight`, `grab` and `run`.

So I saw that some of the variables I chose to analyze were not
effective. So let’s look at some new combinations.

  - X = `per_played` the outcome variable
  - H = `fullhop_height`: height of full hop
  - W = `Weight`: weight
  - R = `Run.Speed`: \# of frames to pivot dash
  - G = `Grab.Range`: range of grab

Now I will create a DAG that I can use to determine my models. This DAG
is actually very similar to the other one, just adding in *full hop
height* instead of short hop.

``` r
# Create DAG
dag2 <- dagitty("dag{
    W -> X; 
    H -> X;
    G -> X;
    R -> X;
    W -> H;
    W -> R
}")

# DAG coordinates
coordinates(dag2) <- list(
  x = c(X = 2, W = 2, H = 3, G = 1, R = 1),
  y = c(X = 2, W = 0, H = 0, G = 2, R = 0)
)

# Draw DAG
drawdag(dag2)
```

![](../Figures/Smash/dag2-1.png)<!-- -->

So far our best model was the one with `weight`, `grab` and `run`. So
let’s just add in `fullhop_height`.

### Iteration 10

``` r
# Standardize and Index Variables
smash1.10 <- tibble(
  played = standardize(smash_tidy$per_played),
  fullhop = standardize(as.numeric(smash_tidy$fullhop_height)),
  weight = standardize(smash_tidy$Weight),
  run = standardize(smash_tidy$Run.Speed),
  grab = standardize(as.numeric(smash_tidy$Grab.Range))
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.10 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bW * weight + bR * run + bG * grab + bH * fullhop,
    a ~ dnorm(0, .2),
    bW ~ dnorm(0, 0.5),
    bR ~ dnorm(0, .5),
    bG ~ dnorm(0, 0.5),
    bH ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = smash1.10
)

# Plot Precis
plot(precis(m1.10))
```

![](../Figures/Smash/m1.10-1.png)<!-- -->

So we can se that `weight` is now pushed to straddle 0 with `fullhop` in
play. Let’s take out `weight`.

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.10)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.10, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.10$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.10 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.10)) {
  lines(rep(smash1.10$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-13-1.png)<!-- -->

### Iteration 11

``` r
# Standardize and Index Variables
smash1.11 <- tibble(
  played = standardize(smash_tidy$per_played),
  fullhop = standardize(as.numeric(smash_tidy$fullhop_height)),
  run = standardize(smash_tidy$Run.Speed),
  grab = standardize(as.numeric(smash_tidy$Grab.Range))
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.11 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bR * run + bG * grab + bH * fullhop,
    a ~ dnorm(0, .2),
    bR ~ dnorm(0, .5),
    bG ~ dnorm(0, 0.5),
    bH ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = smash1.11
)

# Plot Precis
plot(precis(m1.11))
```

![](../Figures/Smash/m1.11-1.png)<!-- -->

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.11)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.11, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.11$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.11 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.11)) {
  lines(rep(smash1.11$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-14-1.png)<!-- -->

### Compare the models

``` r
plot(coeftab(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, m1.8, m1.9, m1.10, m1.11), pars = c("bW", "bG", 'bR', 'bH'))
```

![](../Figures/Smash/unnamed-chunk-15-1.png)<!-- -->

I was questionable about `weight` before so perhaps I just leave it out.
What is interesting is that `fullhop_height` was very influential in the
all variable model. So there must be an interaction between this
variable and another that gives it this effect.

But the biggest story I am seeing here is the `grab` is influenced by
`fullhop`. Once conditioned on `fullhop` we see a big pull towards 0. I
thin that `grab` is going to be a variable I will continue to condition
in future rounds.

## Round 3

I am interested in the aerial variables. So let’s just look at those and
nothing else.

  - X = `per_played` the outcome variable
  - As = `Air.speed`: speed in air
  - F = `Fast.Fall`: fast fall speed
  - H = `fullhop_height`: height of full hop
  - S = `shorthop_height`: height of short hop
  - Ah = `airjump_height`: height of midair jump

Now I will create a DAG that I can use to determine my models. This DAG
is actually very similar to the other one, just adding in *full hop
height* instead of short hop.

``` r
# Create DAG
dag3 <- dagitty("dag{
    H -> X; 
    S -> X;
    F -> X;
    As -> F;
    Aj -> H;
    Aj -> S
}")

# DAG coordinates
coordinates(dag3) <- list(
  x = c(X = 2, H = 1, S = 3, F = 1, As = 1, Aj = 2),
  y = c(X = 2, H = 0, S = 0, F = 2, As = 1, Aj = 0)
)

# Draw DAG
drawdag(dag3)
```

![](../Figures/Smash/dag3-1.png)<!-- -->

So here we see a semi-complex DAG. We have the relationship of *Air
Speed* to *Fast Falling* which influence our outcome since it is a
common thing to look for how your character moves in the air when
performing combos. Separately there is the relationship of different
types of jumps; short, full, and air (or double jump). I see these of
short and full individually influencing character choice

``` r
# Standardize and Index Variables
smash1.12 <- tibble(
  played = standardize(smash_tidy$per_played),
  air = standardize(smash_tidy$Air.speed),
  fall = standardize(smash_tidy$Fast.Fall),
  fullhop = standardize(as.numeric(smash_tidy$fullhop_height)),
  shorthop = standardize(as.numeric(smash_tidy$shorthop_height)),
  airjump = standardize(as.numeric(smash_tidy$airjump_height)),
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.12 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bAs * air + bF * fall + bH * fullhop + bS * shorthop + bAj * airjump,
    a ~ dnorm(0, .2),
    c(bAs, bF, bH, bS, bAj) ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = smash1.12
)

# Plot Precis
plot(precis(m1.12))
```

![](../Figures/Smash/m1.12-1.png)<!-- -->

Glancing at this it looks like `fullhop`, `shorthop`, and `fall` have
more impact on model.

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.12)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.12, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.12$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.12 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.12)) {
  lines(rep(smash1.12$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-16-1.png)<!-- -->

Just to check I need to take out those variables that didn’t influence
the outcome to see if there are relationships there.

``` r
# Standardize and Index Variables
smash1.13 <- tibble(
  played = standardize(smash_tidy$per_played),
  fall = standardize(smash_tidy$Fast.Fall),
  fullhop = standardize(as.numeric(smash_tidy$fullhop_height)),
  shorthop = standardize(as.numeric(smash_tidy$shorthop_height))
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.13 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bF * fall + bH * fullhop + bS * shorthop,
    a ~ dnorm(0, .2),
    c(bF, bH, bS) ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = smash1.13
)

# Plot Precis
plot(precis(m1.13))
```

![](../Figures/Smash/m1.13-1.png)<!-- -->

Glancing at this it looks like `fullhop`, `shorthop`, and `fall` have
more impact on model.

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.13)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.13, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.13$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.13 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.13)) {
  lines(rep(smash1.13$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-17-1.png)<!-- -->

Looks like no change, so I should be good to take those variables out of
the model.

## Round 4

Combining what I have learned from the last two rounds we can use the
best set of variables and see what kind of a model we can make.

  - X = `per_played` the outcome variable

*From Round 2* - W = `Weight`: weight - R = `Run.Speed`: \# of frames to
pivot dash - G = `Grab.Range`: range of grab

*From Round 3* - F = `Fast.Fall`: fast fall speed - H =
`fullhop_height`: height of full hop - S = `shorthop_height`: height of
short hop

``` r
# Create DAG
dag4 <- dagitty("dag{
    W -> X; 
    R -> X;
    G -> X;
    W -> R;
    W -> F;
    
    F -> X;    
    H -> X; 
    S -> X
}")

# DAG coordinates
coordinates(dag4) <- list(
  x = c(X = 1, W = 1, S = 2, G = 1, R = 2, F = 0, H = 0),
  y = c(X = 1, W = 0, S = 2, G = 2, R = 0, F = 0, H = 2)
)

# Draw DAG
drawdag(dag4)
```

![](../Figures/Smash/dag4-1.png)<!-- -->

So this is a pretty interesting DAG. We basically have all these
variables that do not pipe into each other and are independent in their
influence over the outcome. The reason why I think this is because I
have seen from previous models that the interactions did not make a
difference. I am however including a new relationship between `weight`
and `fall`. The logic is that the heavier characters would fall faster,
but this may not be the case always. I mean it is a game afterall…

``` r
# Standardize and Index Variables
smash1.14 <- tibble(
  played = standardize(smash_tidy$per_played),
  weight = standardize(smash_tidy$Weight),
  run = standardize(smash_tidy$Run.Speed),
  grab = standardize(as.numeric(smash_tidy$Grab.Range)),
  fall = standardize(smash_tidy$Fast.Fall),
  fullhop = standardize(as.numeric(smash_tidy$fullhop_height)),
  shorthop = standardize(as.numeric(smash_tidy$shorthop_height))
) %>% 
  drop_na()
```

    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion
    
    ## Warning in scale(x): NAs introduced by coercion

``` r
# Fit Model
m1.14 <- quap(
  alist(
    played ~ dnorm(mu, sigma),
    mu <- a + bW * weight + bR * run + bG * grab + bF * fall + bH * fullhop + bS * shorthop,
    a ~ dnorm(0, .2),
    c(bW, bR, bG, bF, bH, bS) ~ dnorm(0, 0.2),
    sigma ~ dexp(1)
  ), data = smash1.14
)

# Plot Precis
plot(precis(m1.14))
```

![](../Figures/Smash/m1.14-1.png)<!-- -->

``` r
# Extract the prior (first set.seed).
set.seed(42)
prior <- extract.prior(m1.14)

# Use link() to compute mu without having to write out the linear function.
mu <- link(m1.14, post = prior, data = list(
  fall = c(-2, 2),
  fullhop = c(-2, 2),
  shorthop = c(-2, 2),
  weight = c(-2, 2),
  run = c(-2, 2),
  grab = c(-2, 2)
))

# Base plot plus "layers".
plot(NULL, xlim = c(-2, 2), ylim = c(-2,2), main = "M1.14 Priors")
for (i in 1:50) {
  lines(c(-2, 2), mu[i,], col = col.alpha("black",0.4))
}
```

![](../Figures/Smash/prior1%20check-1.png)<!-- -->

Glancing at this it looks like `fullhop`, `shorthop`, and `fall` have
more impact on model.

``` r
# Call link without specifying new data so it uses the original data.
mu <- link(m1.14)

# Summarize samples across cases.
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# Simulate observations (again no new data, so it uses original data).
smash_sim <- sim(m1.14, n = 10000)
smash_PI <- apply(smash_sim, 2, PI)

# Plot posterior predictions.
plot(
  mu_mean ~ smash1.14$played, 
  col = rangi2, 
  ylim=range(mu_PI),
  xlab = "Observed Player Choice", ylab = "Predicted Player Choice",
  main = "M1.14 Posterior Predictive Check"
)

abline(a = 0, b = 1, lty = 2)

for (i in 1:nrow(smash1.14)) {
  lines(rep(smash1.14$played[i], 2), mu_PI[,i], col = rangi2)
}
```

![](../Figures/Smash/unnamed-chunk-18-1.png)<!-- -->

Let’s compare this model with the all variable model

``` r
# Plot all models together
plot(coeftab(m1.0, m1.14), pars = c("bF", "bH", "bS", "bW", "bR", "bG"))
```

![](../Figures/Smash/unnamed-chunk-19-1.png)<!-- -->

I think I will move onto some more advanced models now in
`03_smash_mcmc.Rmd`
