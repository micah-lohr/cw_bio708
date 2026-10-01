# T-test lab 
library(tidyverse); library(ggplot2)
# central limit theorum: normal dist. can be approached as the mean increases 


# 10.5.1 Influence of Sample Size -----------------------------------------

xs <- rnorm(10, mean=10, sd=5)
ys <- rnorm(10, mean=12, sd=5)
xl <- rnorm (100, mean=10, sd=5)
yl <- rnorm(100, mean=12, sd=5)

# (1) Perform t-test for xs vs. ys, and xl vs. yl.
t.test(xs, ys, var.equal=TRUE)
t.test(xl, yl, var.equal=TRUE)

# (2) Compare (a) the degree of freedom, and (b) p-values.
# xs vs ys df is 18, p-value is 0.362, which is not statistically significant. xl vs yl df is 198, p-value is 0.2827
# although this p-value is smaller, it is also not statistically significant. 


# 10.5.2 Effects of Uncertainty -------------------------------------------

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

# (1) Estimate sample means and SDs for each vector. 
# To do so, create a tibble() object with group (consist of characters a1, a2, b1, b2) and value columns (consist of values above). 
# Then use group_by() and summarize() functions to estimate means and SDs for each group.

ab_tibble <- tibble(a1, a2, b1, b2) %>% 
  pivot_longer(cols= c(a1,a2,b1,b2), names_to="group", values_to="value")

ab_tibble_mu <- ab_tibble %>% 
  group_by(group) %>% 
  summarize(mu_ab=mean(value), sd_ab = sd(value))

# (2) Create a figure similar to Figure 10.1 using data from a1 and a2.
ab_tibble %>% 
  filter(group %in% c("a1", "a2")) %>% 
  ggplot(aes(x = group,
             y = value)) +
  geom_jitter(width = 0.1, 
              height = 0, 
              alpha = 0.25) + 
  geom_segment(data = ab_tibble_mu %>% filter(group %in% c("a1", "a2")), 
               aes(x = group,
                   xend = group,
                   y = mu_ab - sd_ab,
                   yend = mu_ab + sd_ab)) +
  geom_point(data = ab_tibble_mu %>%  
             filter(group %in% c("a1", "a2")), 
             aes(x = group,
                 y = mu_ab),
             size = 3) +
  labs(x = "Group",
       y = "Value") 

# (3) Perform Welch’s t-test for each pair (a1 vs. a2 AND b1 vs. b2) and compare results.
t.test(a1, a2, var.equal=FALSE)
t.test(b1, b2, var.equal=FALSE)
# the degrees of freedom are almost the same for the two tests, but not identical.
# There is a statistically significant difference in the means of a1 and a2, as indicated by the p-value of 2.714 x 10^-8.
# The p-value for b1 and b2 is much larger (0.1082) and thus indicates no statistically significant difference between the means.


# 10.5.3 Simulate Null Hypothesis -----------------------------------------

df_fl <- read_csv("data_src/data_fish_length.csv") #load data in again

# (1) Assign mean(df_fl$length) to mu and sd(df_fl$length) to sig.
mu <- mean(df_fl$length)
sig <- sd(df_fl$length) 

# (2) Create x and y with rnorm, run t.test with var.equal set to TRUE
x <- rnorm(50, mean=mu, sd=sig)
y <- rnorm(50, mean=mu, sd=sig)

v <- t.test(x,y,var.equal=TRUE)$statistic

# (3) Use for loop to repeat this process 100 times, generating a distribution of 100 simulated t-values.

v <- NULL
R <- 100
for (i in 1:R) {
  df_x <- rnorm(50, mu, sig) 
  df_y <- rnorm(50, mu, sig)
  v[i] <- t.test(df_x, df_y, var.equal=TRUE)$statistic
}

# (4) Draw a histogram of the simulated t-values
# add vertical lines indicating the observed t-value for the comparison of length between lakes a and b in the df_fl dataset.
# Include vertical lines for both the positive and negative values of the observed t-value to show the two-tailed comparison.

a <- df_fl %>% 
  filter(lake=="a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake=="b") %>% 
  pull(length)

t_obs <- t.test(a,b,var.equal=TRUE)$statistic

tibble(v=v) %>% 
  ggplot(aes(x=v)) +
  geom_histogram() +
  labs(x="t-statistic", y="count")  +
  geom_vline(xintercept=t_obs, color="red") +
  geom_vline(xintercept=abs(t_obs), color="red")

# (5) Calculate the proportion of simulated t-values whose absolute values are > the absolute value of the observed t-value for the difference in length between lakes a and b in the df_fl dataset. 
# Compare this proportion with the p.value obtained from t.test() function.

t_prop <- mean(abs(v) > abs(t_obs)) 
t_prop

t.test(a,b,var.equal=TRUE)
#the proportion of t-values greater than the absolute value from the t-test are very similar, nearly identical, meaning that we simulated the null hypothesis with this exercise.

