# Sampling 

#rm(list=ls()) clears environment
#install.packages("pacman")

library(tidyverse)
pacman::p_load(tidyverse, patchwork, ggplot2)


# 10 individual samples ---------------------------------------------------

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h, # height
                unit = "cm") # unit

df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.)) # while piping, "." refers to the df inherited

print(df_h1)

# do the same thing again to show how sampling differs every time 
h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)

# PARAMETER = an unmeasurable constant that represents the population of interest- we have to make an inference via sampling
# Mean and variance are parameters that we try to estimate from sampling data

# Parameter inference ---------------------------------------

# load csv into R with base R read_csv():
df_h0 <- read_csv("data_src/data_plant_height.csv") 

# show the first 10 rows
print(df_h0)

# calculate true mean and variance
mu <- mean(df_h0$height) #true mean

sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0) #true variance

# We can simulate the sampling of 10 plant individuals by randomly selecting 10 rows from df_h0:
df_i <- df_h0 %>% 
  sample_n(size = 10) # size specifies the number of rows to be selected randomly

print(df_i)

# sample_n() selects randomly, so it will look different every time: 
df_i <- df_h0 %>% 
  sample_n(size = 10)

print(df_i)

# We can use the for loop to repeat sampling (e.g., 100 sets of 10 randomly selected plants)
## Randomly sample 10 individuals 
df_i <- df_h0 %>% 
  sample_n(size = 10)

## Mean for a subset
mu_i <- mean(df_i$height)

## Variance for a subset
var_i <- sum((df_i$height - mu_i)^2)/nrow(df_i)

# for loop
mu_i <- var_i <- NULL

for(i in 1:1000) {
  df_i <- df_h0 %>% 
    sample_n(size = 10)
  
  ## Mean for a subset
  mu_i[i] <- mean(df_i$height)
  
  ## Variance for a subset
  var_i[i] <- sum((df_i$height - mu_i[i])^2)/nrow(df_i)
}

print(mu_i)
print(var_i)

# visualization ---------------------------------------------------------------
#install.packages(patchwork)
df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)  #mu is the true mean we assigned above


# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)+ 
  scale_x_continuous(limits= c(min(c(var_i, var_i)),
                               max(c(var_i, var_i)))) #sigma2 is the true variance that we assigned above

# lay out vertically-- possible only if "patchwork" is loaded
g_mu / g_var #we could replace the / with + to lay out horizontally
g_hor <- g_mu + g_var #can also name these and save as new objects

#samples are distributed symmetrically around the true mean, while sample variance are skewed a bit because of the outliers
#this shows that variance is underestimated when we do sampling because variance is biased

# bias-corrected version --------------------------------------------------
mu_i <- var_ub_i <- NULL

for(i in 1:1000) {
  df_i <- df_h0 %>% 
    sample_n(size = 10)
  
  ## Mean for a subset
  mu_i[i] <- mean(df_i$height)
  
  ## Variance for a subset
  var_ub_i[i] <- var(df_i$height) #var() function divides by N-1 instead of N
}

print(mu_i)
print(var_ub_i)

df_sample <- df_sample %>% 
  mutate(var_ub_hat=var_ub_i)

g_var_ub <- df_sample %>% 
  ggplot(aes(x=var_ub_hat)) +
  geom_histogram() + 
  geom_vline(xintercept=sigma2) + 
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
            max(c(var_i, var_ub_i))))

g_var_ub

## combined figure
g_mu / g_var / g_var_ub

