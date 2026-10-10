# To complete the following exercises, you must use either base R or tidyverse functions.  
# Manual searching or hard-coding answers will not be graded.

library(tidyverse)

# sampling ----------------------------------------------------------------

# GUIDE:
# The original `iris` dataset has 150 observations, with 50 samples per species.
# Convert the base R `iris` dataframe to a tibble for easier manipulation.
df_iris <- as_tibble(iris)

# Using `nrow(df_iris)` will confirm there are 150 rows.
nrow(df_iris) # return 150

# Using `group_by()` will confirm there are 50 rows for each species.
df_iris %>% 
  group_by(Species) %>% 
  summarize(n_sample = n()) # return n_sample = 50 each

# The function `sample_n(size = 3)` randomly selects 3 observations from the dataset. 
# When applied after `filter(Species == XX)`, it samples 3 observations from a given species.
df_s3 <- df_iris %>% 
  filter(Species == "versicolor") %>% 
  sample_n(size = 3)

# You can take the mean of a selected column using `pull()` and `mean()`
df_s3 %>% 
  pull(Petal.Width) %>% 
  mean()

# Questions:
# 1. Create a `for` loop (with loop index `i`) to perform the following steps for 100 iterations:
#    a. Randomly sample 5 individuals of "versicolor".
#    b. Calculate the mean of their "Petal.Width" values.
#    c. Assign the result to `mu5[i]`. 
#       (Hint: initialize an empty object `mu5` before starting the loop.)
#    This procedure will yield 100 estimates of the mean Petal.Width from 5 individuals.
mu5 <- NULL
for(i in 1:100) {
  df_i <- df_iris %>% 
    filter(Species=="versicolor") %>% 
    sample_n(size = 5)
  mu5[i] <- mean(df_i$Petal.Width)
}

# 2. Calculate the standard deviation of `mu5` and assign it to `s_mu5`.
s_mu5 <- sd(mu5)

# 3. Repeat step 1 but sample 20 individuals in each iteration, and assign the results to `mu20`.
mu20 <- NULL
for(i in 1:100) {
  df_i <- df_iris %>% 
    filter(Species=="versicolor") %>% 
    sample_n(size = 20)
  mu20[i] <- mean(df_i$Petal.Width)
}

# 4. Calculate the standard deviation of `mu20` and assign it to `s_mu20`.
s_mu20 <- sd(mu20)
#    Verify that `s_mu20` is smaller than `s_mu5` by printing the comparison (`s_mu20 < s_mu5`)
s_mu20 < s_mu5

# probability density -----------------------------------------------------

# GUIDE:
# For continuous variables (e.g., body length), we use a probability density function (PDF) 
# to describe how values are distributed and how likely different values are.
# R provides built-in functions to work with probability distributions:
# - Probability Density Functions (PDF) for evaluating how likely a value is.
# - Cumulative Distribution Functions (CDF) for evaluating the probability 
#   that a value is less than or equal to a given number.
# These functions are available for many common distributions (e.g., normal).

# `dnorm(x = 1, mean = 0, sd = 1)` calculates the probability density of x = 1 
# for a normal distribution with mean = 0 and standard deviation = 1.
# Probability density tells us how "likely" a value is relative to other values.
# In this example, x = 1 is more likely than x = 2 under this distribution.

dnorm(x = 1, mean = 0, sd = 1)  # returns 0.2419707
dnorm(x = 2, mean = 0, sd = 1)  # returns 0.05399097

# `pnorm(q = 1, mean = 0, sd = 1)` calculates the probability (not probability density!) of x <= 1
# for a normal distribution with mean = 0 and standard deviation = 1.
pnorm(q = 1, mean = 0, sd = 1) # returns 0.8413447
pnorm(q = 0, mean = 0, sd = 1) # returns 0.5

# Questions:
# 1. Calculate the probability that 0 < x <= 10 for a normal distribution with mean = 5 and sd = 3.
#    Hint: Use `qnorm()` function and subtract P(x <= 0) from P(x <= 10).
p0 <- pnorm(0, mean=5, sd=3)
p10 <- pnorm(10, mean=5, sd=3)
p_result <- p10-p0
print(p_result)

# 2. Calculate the probability that 0 < x <= 10 for a normal distribution with mean = -5 and sd = 3.
p0_2 <- pnorm(0, mean=-5, sd=3)
p10_2 <- pnorm(10, mean=-5, sd=3)
p_result2 <- p10_2-p0_2
p_result2

# t-test ------------------------------------------------------------------

# GUIDE:
# The `t.test()` function in R can perform a t-test with two options for handling variances: `var.equal = TRUE` or `FALSE`.
# - If the equal-variance assumption holds, using `var.equal = TRUE` can increase the power to detect a true difference.
# - If the assumption is seriously violated, using `var.equal = TRUE` may produce incorrect or misleading results.
# Even though both options can give similar results in many cases, it is important to understand the assumptions behind each test.

# Questions:
# 1. Using the `df_iris` dataset, calculate the variance of "Petal.Length" for each species.
#    (Hint: use `group_by()` and `summarize()` from dplyr.)
df_iris %>% 
  group_by(Species) %>% 
  summarize(var=var(Petal.Length))

# 2. Perform a t-test comparing "Petal.Length" between "setosa" and "versicolor".
#    Choose the appropriate `var.equal` option depending on whether the SDs appear equal.
setosa <- df_iris %>% 
  filter(Species=="setosa") %>% 
  pull(Petal.Length)

versicolor <- df_iris %>% 
  filter(Species=="versicolor") %>% 
  pull(Petal.Length)

t.test(setosa, versicolor, var.equal=FALSE) #p-value indicates a significant difference!

# anova -------------------------------------------------------------------

# GUIDE:
# The `aov()` function in R can be used to perform ANOVA.
# The built-in dataset `InsectSprays` provides a useful example.
# It contains insect count data for different types of insect sprays.

df_insect <- as_tibble(InsectSprays)

# Questions:
# 1. Using the `df_insect` dataset, create a plot to visualize the distribution of insect counts for each spray type.
#    - Use a violin plot to show the distribution and include the median (y = count, x = spray).
#    - Add individual data points with a small horizontal jitter for clarity.
df_insect %>% 
  ggplot(aes(x = spray,
           y = count)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) +
  labs(x="Spray", y="Count")

# Question:
# 2. Using the `df_insect` dataset, perform a one-way ANOVA to test whether insect counts differ among spray types.
#    - Use the `aov()` function and display the summary of the model with `summary()`.
#    - Report if there was a significant difference between spray groups.

summary(aov(count ~ spray, data=df_insect))

#Yes, there is a statistically significant difference between spray groups.

insect_aov <- aov(count~spray, data=df_insect) 
(insect_tukey <- TukeyHSD(insect_aov))
