# Probability Lab
pacman::p_load(tidyverse,
               patchwork,
               ggplot2)

# PDF Lab -----------------------------------------------------------------
#The function rnorm() produces a random variable that follows a Normal distribution with a specified mean and SD. 
# (1) Generate a variable with 50 observations.
# (2) Create a figure similar to Figure 9.3

# generate 50 observations
x<-rnorm(50, mean=10, sd=1.68)

# get sample mean and SD
mu <- mean(x)
sigma <- sd(x)
pd <- dnorm(x, mean = mu, sd = sigma)

# define bins
x_min <- floor(min(x)) 
x_max <- ceiling(max(x)) 
bin <- seq(x_min, x_max, by=1) 

# calculate probability for each bin
p <- NULL
for (i in 1:(length(bin) - 1)) {
  xu <- pnorm(bin[i+1], mean=mu, sd=sigma)

  xl <- pnorm(bin[i], mean=mu, sd=sigma)

  p[i] <- xu-xl
}

# get frequency - p*50 since we have 50 observations, or length(x)
df_prob <- tibble(p=p, 
                  bin=bin[-length(bin)] + 0.5
                  ) %>% 
  mutate(
    freq=p*length(x))

# make rnorm(x) a tibble for graph
df_x <- tibble(x=x)

# combined graph
df_x %>% 
  ggplot(aes(x = x)) + 
  geom_histogram(binwidth = 1, 
                 center = 0.5) + 
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "orchid") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "orchid",
            linetype="dotted")


# PMF Lab -----------------------------------------------------------------
# The function rpois() produces a random variable that follows a Poisson distribution with a specified mean. 
# (1) Generate a variable with 1000 observations.
# (2) Create a figure similar to Figure 9.7

x2 <- rpois(1000, 21.876)

# calculate probability mass
lambda_hat <- mean(x2)
pm <- dpois(x2, lambda = lambda_hat)

df_prob_pmf <- tibble(x = x2, y = pm) %>% 
  mutate(freq = y * 1000)

tibble(y=pm, x=x2) %>% 
  ggplot(aes(x = x2)) +
  geom_histogram(binwidth = 0.5, 
                 center = 0) +
  geom_line(data = df_prob_pmf,
            aes(x = x2,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob_pmf,
             aes(x = x2,
                 y = freq))
