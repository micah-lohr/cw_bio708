# Two-group comparison: t-test
pacman::p_load(tidyverse)

# read in fish length data
df_fl <- read_csv("data_src/data_fish_length.csv")
print(df_fl)

unique(df_fl$lake) # unique() returns unique values as a vector

distinct(df_fl, lake) # distinct() returns unique values as a tibble

# get mean and sd body size 
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length),
            sd_l = sd(length))

# visualize 
df_fl %>% 
  ggplot(
    aes(x= lake, 
    y= length
  )) +
  geom_jitter(
    width=0.1,
    height=0,
    alpha=0.25) +
  geom_segment(
    data= df_fl_mu,
    aes(x = lake,
        xend=lake,
        y=mu_l - sd_l,
        yend=mu_l+sd_l)) +
  geom_point(
    data = df_fl_mu,
    aes(x=lake,
      y=mu_l),
    size=2
  ) +
  labs(x="Lake", y="Fish Body Length (cm)")


# t-test in R -------------------------------------------------------------
# t-test with fish length data- compare between the two lakes
x <- df_fl %>%
  filter(lake == "a") %>%  # subset lake a
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>% # subset lake b
  pull(length)

t.test(x, y, var.equal = TRUE)

# get t-value
v_mu <- df_fl_mu %>% 
  pull(mu_l)

v_mu

v_mu[1] - v_mu[2]

df_t <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length),
            var_l = var(length),
            n=n())

#mean vector
v_mu <- pull(df_t, mu_l)
#variance vector
v_var <- pull(df_t, var_l)
#sample size vector
v_n <- pull(df_t, n)

# calculate weighted variance between two groups- important especially when sample sizes are uneven
# also called pooled variance
var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] -1) / (sum(v_n) - 2)) * v_var[2]

# calculate t-value manually
# ratio of difference to variation. larger variation -> smaller t-value, larger diff in mean -> larger t-value
t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p*((1/v_n[1]) + (1/v_n[2])))
t_value

# null hypothesis- get p-value ---------------------------------------------------------
# get p-value- produce 500 values from -5 to 5 with equal interval
x <- seq(-5, 5, length = 500)

# probability density of t-statistics with df = 98
y <- dt(x, df = sum(v_n) - 2)
y1 <- dt(x, df=10-2) # change df to 8 to see how sample size changes the t-statistic distribution 

# draw figure
tibble(x, y, y1) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_line(aes(y=y1), color="red") +
  labs(y = "Probability density",
       x = "t-statistic") +
  theme_classic()

# draw entire range
tibble(x, y, y1) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_line(aes(y=y1), color="red") +
  geom_vline(xintercept = t_value,
             color = "darkgray") + # t_value is the observed t_value
  geom_vline(xintercept = abs(t_value),
             color = "darkgray") + # t_value is the observed t_value
  labs(y = "Probability density",
       x = "t-statistic") +
  theme_classic()


# t-test under unequal variance -------------------------------------------
x <- df_fl %>%  
  filter(lake=="a") %>% 
  pull(length)

y <- df_fl %>%  
  filter(lake=="b") %>% 
  pull(length)

t.test(x, y, var.equal=FALSE) 
t.test(x,y) #this is the default for t.test in R- if you don't add var.equal it runs a Welch's test
t.test(x, y, var.equal=TRUE) #just running again to compare
