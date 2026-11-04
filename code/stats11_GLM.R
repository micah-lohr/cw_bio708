# stast 11 GLM
pacman::p_load(tidyverse, patchwork)
## GLM: Poisson, negative-binomial, beta-binomial, etc. distributions (rather than normal dist.)
## Poisson- usually count data (e.g. species richness) where we work with whole numbers

df_count <- read_csv("data_src/data_garden_count.csv") #read data
print(df_count) #plant count and nitrate level per plot


# Poisson Model -----------------------------------------------------------


# draw a figure relating nitrate to plant count
df_count %>% ggplot(aes(x=nitrate, y=count)) + geom_point()

# fit lm() to this data, and draw a predicted line using geom_abline()
m_lm <- lm(count~nitrate, data=df_count)
summary(m_lm)

df_count %>% 
  ggplot(aes(x=nitrate, y=count)) + 
  geom_point() + 
  geom_abline(intercept= coef(m_lm)[1], slope=coef(m_lm)[2])

# alternative to ordinary regression: 
# "Poisson" regression for non-negative count data
m_pois <- glm(count ~ nitrate, family="poisson", data=df_count)
summary(m_pois)

## use intercept and slope from m_pois as is to draw prediction with geom_abline()
df_count %>% 
  ggplot(aes(x=nitrate, y=count)) + 
  geom_point() + 
  geom_abline(intercept= coef(m_pois)[1], slope=coef(m_pois)[2]) #doesn't make sense!

## link function, log(lambda) = alpha + beta * x for Poisson
x <- seq(min(df_count$nitrate),
         max(df_count$nitrate),
         length=100)

a <- coef(m_pois)[1] #intercept in a log-scale
b <- coef(m_pois)[2] #slope in a log-scale

log_y_pred <- a + b * x
y_pred <- exp(log_y_pred)

df_pred <- tibble(y_pred = y_pred,
                  nitrate = x)

df_count %>% 
  ggplot(aes(x=nitrate, y=count)) +
  geom_point() +
  geom_line(data=df_pred,
            aes(y=y_pred))

## compare results between lm() and glm(). any difference?
summary(m_lm)
summary(m_pois)

# model selection- look at range of values in your y variable
# can they take decimal points or only discrete data? negative?


# binomial model -------------------------------------------------------

df_mussel <- read_csv("data_src/data_mussel.csv") #read in dataset
print(df_mussel) #fertilized eggs per mussel, examined eggs for the study, density of mussels in a quadrat

## draw figure 
df_mussel %>% 
  mutate(p = n_fertilized / n_examined) %>% 
  ggplot(aes(
    x=density,
    y=p)) +
  geom_point()

#for proportion data, it must be between 0-1, so poisson log/exp does not work/
#we have to use logit function, which is exp(alpha + beta*x / 1+exp(alpha + beta*x)
#logit link function limits y to be between 0 and 1 

## binomial regression
## n_examined is the possible maximum success
## n_fertilized is the success number
## binomial distributions take two parameters 
## Bin(N, p), where N is number of trials, p is success probability

# cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized)
# ^ you can see success - failure 
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density, 
    data=df_mussel,
    family="binomial")

summary(m_binom) #large reduction in null to residual deviance, the relationship is strong in x explaining y

## draw prediction...?
z <- seq(min(df_mussel$density),
         max(df_mussel$density),
         length=100)

theta <- coef(m_binom)
logit_p <- theta[1] + theta[2] * z
p_pred <- exp(logit_p) / (1 + exp(logit_p))

df_p <- tibble(p_pred = p_pred, density=z)

df_mussel %>% 
  mutate(p = n_fertilized/n_examined) %>% 
  ggplot(aes(x=density, y=p)) +
  geom_point() +
  geom_line(data=df_p,
            aes(y=p_pred))


