# Regression
pacman::p_load(tidyverse)

df_algae <- read_csv("data_src/data_algae.csv")
print(df_algae)

df_algae %>% 
  ggplot(
    aes(x= conductivity,
        y= biomass)) +
  geom_point()

# lm() takes a formula as the first argument; lm(y~ x, data=df)
m <- lm(biomass ~ conductivity,
        data = df_algae)
summary(m)

# extract intercept and slope
coef(m) # coef() extracts estimated coefficients, e.g., coef(m)[1] is (Intercept)
alpha <- coef(m)[1]
beta <- coef(m)[2]

#draw the "fittest line" on the graph
df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) #use geom_abline as a shortcut to draw relationship between x and y


# standard errors and t-values --------------------------------------------

#x-intercept divided by x standard error = t-value:
0.50355/0.02568

# extract coef
theta <- coef(m)
se <- sqrt(diag(vcov(m)))
(t_value <- theta / se)


# unexplained variation ---------------------------------------------------

## extract residuals (deviation from fitted line)
eps0 <- df_algae$biomass - (alpha + beta * df_algae$conductivity) #epsilon is symbol for residuals
eps0

eps <- resid(m) #built in function for calculating residuals from a model
eps

plot(eps0 ~ eps) #they are identical

## visualize error
df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(
    x= conductivity, 
    y=biomass)) +
  geom_point() +
  geom_abline(intercept=alpha,
              slope=beta) +
  geom_segment(
    aes(x=conductivity,
        xend=conductivity, 
        y=biomass,
        yend=biomass - eps), 
    linetype="dashed")

## original variation in y
y <- df_algae$biomass
ss0 <- sum((y-mean(y))^2) #sum of squares 
ss0

ss1 <- sum(eps^2) #sum of squares of the residuals
ss1

ss1/ss0 # how much variation remains unexplained even after accounting for effect of x on y
rsq <- 1 - (ss1/ss0) # how much variation is explained by the model (R^2)
rsq

summary(m) #R-squared is also stated in the model summary ("multiple R-squared")