# Sampling Lab 
pacman::p_load(tidyverse, patchwork, ggplot2)

# (1) We used 10 plants to estimate sample means and variances. 
# Obtain 100 sub-datasets with 50 or 100 measures each, and draw histograms of sample means and unbiased variances (use var()).
df_h0 <- read_csv("data_src/data_plant_height.csv")

true_mu_i <- mean(df_h0$height)
true_var_ub_i <- var(df_h0$height)

mu_i <- var_ub_i <- NULL

for(i in 1:100) {
  df_i <- df_h0 %>% 
    sample_n(size = 50)
  
  ## Mean for a subset
  mu_i[i] <- mean(df_i$height)
  
  ## Variance for a subset
  var_ub_i[i] <- var(df_i$height)
}

df_sample <- tibble(mu_hat = mu_i, var_ub_hat = var_ub_i)

g_mu <- df_sample %>% 
  ggplot(aes(x=mu_hat)) + 
  geom_histogram() +
  geom_vline(xintercept=true_mu_i) +
  theme_classic()

g_var_ub <- df_sample %>% 
  ggplot(aes(x=var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept=true_var_ub_i) +
  theme_classic()

g_mu / g_var_ub

# (2) Sample means and unbiased variances are unbiased if samples are randomly selected. 
# What happens if samples are non-random? Suppose the investigator was unable to find plants less than 10 cm in height – 
# the following code excludes those less than 10 cm in height:

df_h10 <- df_h0 %>% 
  filter(height >= 10)

# Repeat step 1 with df_h10 instead of df_h0 and compare the results.

true_mu_i2 <- mean(df_h10$height)
true_var_ub_i2 <- var(df_h10$height)

mu_i2 <- var_ub_i2 <- NULL

for(i in 1:100) {
  df_i2 <- df_h10 %>% 
    sample_n(size = 50)
  
  ## Mean for a subset
  mu_i2[i] <- mean(df_i2$height)
  
  ## Variance for a subset
  var_ub_i2[i] <- var(df_i2$height)
}

df_sample2 <- tibble(mu_hat = mu_i2, var_ub_hat = var_ub_i2)

g_mu2 <- df_sample2 %>% 
  ggplot(aes(x=mu_hat)) + 
  geom_histogram() +
  geom_vline(xintercept=true_mu_i2) +
  theme_classic()

g_var_ub2 <- df_sample2 %>% 
  ggplot(aes(x=var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept=true_var_ub_i2) +
  theme_classic()

g_mu2 / g_var_ub2

(g_mu / g_var_ub) + (g_mu2 / g_var_ub2)

# When samples are non-random, there is less skew in the distributions of the samples, and they appear to be clustered closer to the true population