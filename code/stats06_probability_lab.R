# Probability Lab
pacman::p_load(tidyverse,
               patchwork,
               ggplot2)

# Normal distribution -----------------------------------------------------------------
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


# Poisson distribution -----------------------------------------------------------------
# The function rpois() produces a random variable that follows a Poisson distribution with a specified mean. 
# (1) Generate a variable with 1000 observations.
# (2) Create a figure similar to Figure 9.7

# generate poisson-dist numbers
z <- rpois(n=1000, lambda=10)

# sample mean
lambda <- mean(z)

#bins 
zbin <-seq(min(z), max(z), by=1)

#probability
pm <- dpois(x=zbin, lambda = lambda)

# prepare dataframes
df_z <- tibble(z=z)

df_prob_z <- tibble(pm = pm, zbin = zbin) %>% 
  mutate(freq = pm * nrow(df_z))

df_z %>% 
  ggplot(aes(x=z)) +
  geom_histogram(
    binwidth=0.5, 
    center = 1) +
  geom_point(
    data=df_prob_z,
    aes(x=zbin,
        y=freq),
    color="hotpink"
  ) +
  geom_line(
    data=df_prob_z,
    aes(x=zbin, y=freq), color="hotpink"
  )
