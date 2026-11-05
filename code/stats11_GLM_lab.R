## stats 11 GLM Lab
pacman::p_load(tidyverse, patchwork)

# GLM exercise -----------------------------------------------------
url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

# (1) Use fish species richness n_sp as a response variable of the model with an appropriate probability distribution 
# (family). Identify which predictor variable(s) significantly influence the response variable.

m_fish <- glm(n_sp ~ distance + cat_area + hull_area, data=df_fish, family="poisson")
summary(m_fish) #distance to the sea significantly impacts number of fish

# (2) Which variables significantly influence the probability that a car has a manual transmission? 
# Choose an appropriate probability distribution (family). Consider including the following predictors: 
# miles per gallon (mpg), horsepower (hp), and weight (wt).
df_car <- mtcars

m_car <- glm(am ~ mpg + hp + wt, data=df_car, family="binomial")
summary(m_car) #weight significantly influences probability of a car having manual transmission

# Develop the same GLM but with a normal distribution (family = "gaussian").
# Compare the results to the model with an appropriate probability distribution.
m_car_gaus <- glm(am ~ mpg + hp + wt, data=df_car, family="gaussian")
summary(m_car_gaus)
# if you assume a normal distribution, both weight and horsepower significantly impact the prob. of having manual transmission

# effect size -------------------------------------------------------------
# want to standardize the effect of predictors to make them comparable (i.e., when units are different)
## The function scale() will perform standardization. 
## Create columns of standardized distance (std_dist), cat_area (std_cat), and hull_area (std_hull).
df_fish <- df_fish %>% 
  mutate(std_dist = c(scale(distance)),
         std_cat = c(scale(cat_area)),
         std_hull = c(scale(hull_area)))
df_fish
## Perform a GLM analysis of fish species richness with the standardized variables, and identify the most 
## influential variable among them in terms of effect size.
m_fish_std <- glm(n_sp ~ std_dist + std_cat + std_hull, data=df_fish, family="poisson")
summary(m_fish_std) #in terms of effect size, distance from the sea is the most influential var. on fish spp. richness
# the output is a lot easier to interpret because the estimates are no longer extremely small numbers
# between the raw numbers and the standardized, z- and p-values remain the same, as do the null and residual deviances and degrees of freedom 

# offset term -------------------------------------------------------------
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_offset.csv"
df_offset <- read_csv(url)

# (1) Using df_offset, perform the following:
# (a) Plot the relationship between count and nitrate.
g_nitrate <- df_offset %>% 
  ggplot(aes(
  x=nitrate,
  y=count)) + 
  geom_point()
# (b) Plot the relationship between count and area.
g_area <- df_offset %>%
  ggplot(aes(
  x=area,
  y=count)) + 
  geom_point()

g_area | g_nitrate

# (c) Plot the relationship between count/area and nitrate.
g_density <- df_offset %>%
  ggplot(aes(
  x=nitrate,
  y=(count/area))) + 
  geom_point()

g_area | g_nitrate | g_density

## confirm that the following code throws a warning because density is not a discrete variable:
df_offset <- df_offset %>% 
  mutate(density = count / area)

# glm(density ~ nitrate,
#     data = df_offset,
#     family = "poisson") confirmed, model runs with errors

# A better approach is to incorporate sampling effort as an offset term in glm(), which allows the model to
# account for differences in effort while preserving the count nature of the response variable. 
# Perform the following exercise:

## Develop a GLM with the response variable count and the predictor nitrate.
m_ct_nitrate <- glm(count ~ nitrate, data=df_offset, family="poisson")
summary(m_ct_nitrate)
## Develop a GLM with the response variable count, the predictor nitrate, and the offset term offset(log(area)) (~ nitrate + offset(log(area)) will work).
m_off <- glm(count ~ nitrate + offset(log(area)), data=df_offset, family="poisson")
summary(m_off)
## Compare the results of the two models: it changes the sign for the estimate from negative to positive

# overdispersion ----------------------------------------------------------
## Poisson and binomial distributions (and some others) don't have a parameter that defines the variance independently
## poisson: Y ~ Poisson(lambda); lambda is the sole parameter 
## - mean = lambda, variance = lambda 

## Binomial: Y ~ binomial (N,p); p s the sole parameter (N is the data you supply)
## -mean = N * p, variance = N*p*(1-p)

## normal: y ~ normal(mu, sigma^2); sigma^2 is independent of mu and captures the variance in your data
## - mean = mu, variance = sigma^2

## For poisson and binomial, once the mean is determined, the variance is also defined. 
## As such, there is an "expected" variance given the mean. 
## If the observed variance goes beyond the expected variance, that will result in "over-dispersion"
## and cause serious consequences for statistical inference 
 
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_tadpole.csv"
df_tadpole <- read_csv(url)
 
## (1) Plot the relationships between (a) tadpole and aqveg and (b) tadpole and permanence.
df_tadpole %>% 
  ggplot(aes(
    x=aqveg,
    y=tadpole
  )) + geom_point()

df_tadpole %>% 
  ggplot(aes(
    x=permanence,
    y=tadpole
  )) + geom_point()

## (2) Discuss in a group that which variable may have significant influence.
## -- we cannot really tell given the strange shape of the data when graphed...

## (3) Develop a GLM (choose an appropriate distribution) explaining tadpole with aqveg and permanence

m_tad <- glm(tadpole ~ aqveg + permanence, data=df_tadpole, family="poisson")
summary(m_tad)
#if you just run it with a poisson dist., it does not make sense given the shape of the data when graphed

# compare the summary stats: notice that the variance is much larger than the mean
mean(df_tadpole$tadpole)
var(df_tadpole$tadpole)
sd(df_tadpole$tadpole)
## problem-- poisson assumes mean and variance are the same, so we have over-dispersion here (variable with almsot no relationship with y will show as very significant)

## in this case, negative binomial works better 
MASS::glm.nb(tadpole ~ aqveg + permanence, data=df_tadpole) %>% 
  summary()
