# ANOVA- multiple comparisons 
pacman::p_load(tidyverse, ggplot2)

# load data
df_anova <- read_csv("data_src/data_fish_length_anova.csv")
distinct(df_anova, lake) #see unique elements from lake column

#visualize how the data is distributed w/ a violin plot 
df_anova %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_violin(draw_quantiles = 0.5, 
              alpha = 0.2) + 
  geom_jitter(alpha = 0.2) +
  theme_classic()

#If we observe a greater between-group variability relative to within-group variability, 
#it suggests that the differences among the groups are substantial. 

## anova 
aov(length~lake,
    data=df_anova)
# variable on the left (y) is the one where the variation is what you want to explain
# variable ob the right is the explanatory/predictor variable that you use to explain the variation in y

# 11.1.1 Between-group variability ----------------------------------------

# first, estimate overall mean
mu <- mean(df_anova$length)

# estimate group means and sample size for each group
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group (group mean - overall mean)^2
            n = n()) # sample size for each group
print(df_g)

# calculate between-group variability:
ss_b <- df_g %>% 
  mutate(ss_g = dev_g * n) %>% 
  pull(ss_g) %>% 
  sum()

aov(length~lake, data=df_anova) #see that ss_b is the sum of squares for the "lake" column

# 11.1.2 Within-group variability -----------------------------------------

#calculate the sum of squares  
ss_w <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) %>% # deviation from group mean for each fish
  pull(dev_i) %>% 
  sum()

print(ss_w)

aov(length~lake, data=df_anova) #ss_w is the sum of squares for the "Residuals" column

#if your residual variability is reduced after accounting for group structure, there is something meaningful in that grouping. If it is large, then maybe nothing is going on.


# overall variability -----------------------------------------------------

ss_o <- sum((df_anova$length -mu)^2)

# you can get the same result by summing the between-group and within-group variability
ss_b + ss_w

summary(aov(length~lake, data=df_anova))
# between group df- number of groups -1
# within group df- number of individuals - number of groups 

# 11.1.3 Variability to Variance ------------------------------------------
sig_b <- ss_b/2 #this will watch the Mean Sq error value from anova for lake 
sig_w <- ss_w / (nrow(df_anova) - n_distinct(df_anova$lake)) #this will match the Mean Sq error from anova for Residuals

# calculate the test statistic
f_value <- sig_b/sig_w # ratio of variability after accounting for degrees of freedom 

# if any pair of groups is sig. different from each other, the aov will be sig. You need a post-hoc to see WHERE the difference actually lies 


# 11.2 Null Hypothesis ----------------------------------------------------

f <- seq(0, 10, by = 0.01)
pd <- df(f, df1=2, df2=147)
        
tibble(x = f, y = pd) %>% 
  ggplot(aes(x = x,
             y = y)) + 
  geom_line() + # F distribution
  geom_vline(xintercept = f_value,
             color = "chocolate") + # observed F-statistic
  labs(x="F-statistic", y = "Probability Density")

#in this plot, the p-value is the area under the curve to the right of the f-value vertical line 

#probability BEYOND the value observed 

p_value <- 1-pf(f_value, df1=2, df2=147)
print(p_value)

#check that the p-value matches what the anova calculates:
summary(aov(length~lake, data=df_anova))

#for fun: run a Tukey's HSD post-hoc on the data: 

m <- aov(length~lake, data=df_anova)
tukey_m <- TukeyHSD(m)
print(tukey_m)
