#stats10 Linear Models

pacman::p_load(tidyverse)
df_fl <- read_csv("data_src/data_fish_length.csv")
print(df_fl)

# categorical -------------------------------------------------------------

## t.test versus lm()

### lm() approach for fish length by lake
m_fl <- lm(length~lake, data=df_fl)
summary(m_fl)

### calculate mean fish lengths for lake a & b
### take the difference
v_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu=mean(length)) %>% 
  pull(mu)

v_mu[2] - v_mu[1] #difference is the same as the lm output for lake b estimate

### t-test for a comparison bwteen lake a and b
t.test(length ~ lake, data=df_fl, var.equal=TRUE) #t-value is same here and in lm()

## anova vs lm()
df_anova <- read_csv("data_src/data_fish_length_anova.csv")

### lm approach
m_anova <- lm(length~lake, data=df_anova)
summary(m_anova)

### calculate means for each group; differences between a and b, a and c
v_mu_anova <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  pull(mu)

v_mu_anova[2] - v_mu_anova[1] # difference between a and b
v_mu_anova[3] - v_mu_anova[1] # difference between a and c

### try anova for comparison 
fish_aov <- aov(length~lake, data=df_anova)
summary(fish_aov) #F-statistic is 5.305, which matches F-stat from the lm()

### how to modify the order of groups 
df_anova <- df_anova %>% 
  mutate(lake_factor = factor(lake, levels = c("c", "a", "b")))

unique(df_anova$lake)
unique(df_anova$lake_factor)

### lm() with lake_factor
m_anova_factor <- lm(length~lake_factor, data=df_anova)
summary(m_anova_factor)

# mix of continuous and categorical in predictors -------------------------
iris <- as_tibble(iris)
iris
distinct(iris, Species) #three species in this dataset (categorical)

m_iris <- lm(Petal.Length ~ Petal.Width + Species, data=iris)
summary(m_iris)

# visualization -----------------------------------------------------------
## generate prediction
n_rep <- 100
df_pred <- iris %>% 
  group_by(Species) %>% 
  reframe(
    Petal.Width = seq(min(Petal.Width),
                      max(Petal.Width),
                      length=n_rep)
  )

y_pred <- predict(m_iris,
                  newdata=df_pred) #produces predicted values of petal length based on petal width/based on model we made

df_pred <- df_pred %>% 
  mutate(y_pred=y_pred)

df_pred

## figure 
iris %>%  
  ggplot(aes(
    x=Petal.Width,
    y=Petal.Length, 
    color=Species
  )) +
  geom_point(alpha=0.5) +
  geom_line(
    data=df_pred,
    aes(y= y_pred)
  )
#notice that there is no change in slope between species, but each species has different average petal length

# interaction -------------------------------------------------------------

## manual interaction syntax
#lm(Petal.Length ~ Petal.Width + Species, Petal.Width:Species, data=iris)

## short-cut formula 
m_iris_int <- lm(Petal.Length ~ Petal.Width*Species, data=iris)
summary(m_iris_int)

y_pred_int <- predict(m_iris_int,
        newdata=df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred_int = y_pred_int)

iris %>% 
  ggplot(aes(
         x=Petal.Width,
         y=Petal.Length,
         color=Species)) +
  geom_point(alpha=0.5) +
  geom_line(
    data=df_pred, 
    aes(y=y_pred_int)
  )

#interaction term is difference in slope value from the baseline

