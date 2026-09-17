# Sampling Lab 
pacman::p_load(tidyverse, patchwork, ggplot2)

# (1) We used 10 plants to estimate sample means and variances. 
# Obtain 100 sub-datasets with 50 or 100 measures each, and draw histograms of sample means and unbiased variances (use var()).
df_h0 <- read_csv("data_src/data_plant_height.csv")

true_mu_i <- mean(df_h0$height)
true_var_ub_i <- var(df_h0$height)
# sample size 50
mu_50 <- var_ub_50 <- NULL
for(i in 1:100) {
  df_50 <- df_h0 %>% 
    sample_n(size = 50)
  mu_50[i] <- mean(df_50$height)
  var_ub_50[i] <- var(df_50$height)
}

#sample size 100
mu_100 <- var_ub_100 <- NULL
for(i in 1:100) {
  df_100 <- df_h0 %>% 
    sample_n(size = 100)
  mu_100[i] <- mean(df_100$height)
  var_ub_100[i] <- var(df_100$height)
}

#graphing
df_sample <- tibble(mu_hat50 = mu_50, var_ub_hat50 = var_ub_50, mu_hat100=mu_100, var_ub_hat100 = var_ub_100)

g_mu50 <- df_sample %>% 
  ggplot(aes(x=mu_hat50)) + 
  geom_histogram() +
  geom_vline(xintercept=true_mu_i) +
  theme_classic()

g_var_ub50 <- df_sample %>% 
  ggplot(aes(x=var_ub_hat50)) +
  geom_histogram() +
  geom_vline(xintercept=true_var_ub_i) +
  theme_classic()

g_mu50 / g_var_ub50

g_mu100 <- df_sample %>% 
  ggplot(aes(x=mu_hat100)) + 
  geom_histogram() +
  geom_vline(xintercept=true_mu_i) +
  theme_classic()

g_var_ub100 <- df_sample %>% 
  ggplot(aes(x=var_ub_hat100)) +
  geom_histogram() +
  geom_vline(xintercept=true_var_ub_i) +
  theme_classic()

g_mu100 / g_var_ub100

(g_mu50 / g_var_ub50) | (g_mu100 / g_var_ub100)

# (2) Sample means and unbiased variances are unbiased if samples are randomly selected. 
# What happens if samples are non-random? Suppose the investigator was unable to find plants less than 10 cm in height – 
# the following code excludes those less than 10 cm in height:

df_h10 <- df_h0 %>% 
  filter(height >= 10)

# Repeat step 1 with df_h10 instead of df_h0 and compare the results.
true_mu_i2 <- mean(df_h10$height)
true_var_ub_i2 <- var(df_h10$height)

#sample size 50
mu_50_2 <- var_ub50_2 <- NULL
for(i in 1:100) {
  df_50_2 <- df_h10 %>% 
    sample_n(size = 50)
  mu_50_2[i] <- mean(df_50_2$height)
  var_ub50_2[i] <- var(df_50_2$height)
}
#sample size 50
mu_100_2 <- var_ub100_2 <- NULL
for(i in 1:100) {
  df_100_2 <- df_h10 %>% 
    sample_n(size = 100)
  mu_100_2[i] <- mean(df_100_2$height)
  var_ub100_2[i] <- var(df_100_2$height)
}


df_sample2 <- tibble(mu_hat50 = mu_50_2, var_ub_hat50 = var_ub50_2, mu_hat100 = mu_100_2, var_ub_hat100 = var_ub100_2)

g_mu50_2 <- df_sample2 %>% 
  ggplot(aes(x=mu_hat50)) + 
  geom_histogram() +
  geom_vline(xintercept=true_mu_i2) +
  theme_classic()

g_var_ub50_2 <- df_sample2 %>% 
  ggplot(aes(x=var_ub_hat50)) +
  geom_histogram() +
  geom_vline(xintercept=true_var_ub_i2) +
  theme_classic()

g_mu50_2 / g_var_ub50_2

g_mu100_2 <- df_sample2 %>% 
  ggplot(aes(x=mu_hat100)) + 
  geom_histogram() +
  geom_vline(xintercept=true_mu_i2) +
  theme_classic()

g_var_ub100_2 <- df_sample2 %>% 
  ggplot(aes(x=var_ub_hat100)) +
  geom_histogram() +
  geom_vline(xintercept=true_var_ub_i2) +
  theme_classic()

g_mu100_2 / g_var_ub100_2

# When samples are non-random, there is less skew in the distributions of the samples, and they appear to be clustered closer to the true population

# (3) Make a dataframe with all four using pivot_long

df_long <- pivot_longer(df_sample, cols= everything(), names_to="type", values_to="value")

df_long %>% ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~type) 
