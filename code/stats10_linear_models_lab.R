#stas10 linear models lab
pacman::p_load(tidyverse, patchwork)


# 13.4.1 Normality Assumption ---------------------------------------------
df_tg <- ToothGrowth

# (1) Develop a linear model examining the effect of supplement type (supp), dose (dose), and their interaction 
#     on tooth length (len). Assign the developed model to m_tooth.
m_tooth <- lm(len ~ supp*dose, df_tg)
summary(m_tooth)

# (2) The Linear Model framework assumes normality. To verify the validity of the normality assumption,
#     we typically employ the Shapiro-Wilk test (shapiro.test() in R). Discuss whether the test should be 
#     applied to (1) the response variable or (2) model residuals. Then, apply the Shapiro-Wilk. 
#     Type ?shapiro.test in the R console to learn more about its usage.
?shapiro.test
## This test should be applied to the residuals. There is not stochasticity in the response variable term (alpha + beta(x)),
## so we need to check the residuals (epsilon)
eps <- resid(m_tooth)
shapiro.test(eps) #test if residuals are significantly deviating from a normal dist. Since p>0.05, the residuals follow the normal dist.


# 13.4.2 Model Interpretation ---------------------------------------------
# Using m_tooth, create a figure illustrating the relationship between tooth length (len; y-axis) and 
# dose (dose; x-axis), with color indicating supplement type (supp). Include both the observed and predicted values, 
# similar to Figure 13.1. Use the following data frame for prediction:
df_pred <- df_tg %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_pred <- predict(m_tooth,
                  newdata = df_pred) 

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

## Visualization:
df_tg %>% 
  ggplot(aes(
    x=dose,
    y=len,
    color=supp
  )) +
  geom_point(alpha=0.5) +
  geom_smooth(
    data=df_pred,
    aes(y=y_pred)
  ) +
  theme_bw()


# 13.4.3 Multicolinearity -------------------------------------------------

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))

# (1) Create Figures of Response vs. Predictors.
# Using df_y, create the following figures:
## (1a) A scatter plot of y (y-axis) versus x1 (x-axis)
gx1 <- df_y %>% ggplot(aes(x=x1, y=y)) + geom_point()

## (1b) A scatter plot of y (y-axis) versus x2 (x-axis)
gx2 <- df_y %>% ggplot(aes(x=x2, y=y)) + geom_point()

gx1 + gx2

## (1c) Based on these plots, interpret the effects of x1 and x2 on y.
## Do x1 and x2 appear to have positive or negative influences on y?

### Given the direction of the data, it seems both x1 and x2 have a strong positive effect on y.

# (2) Examining statistical effects of x1 and x2 on y. Examine the statistical effects of x1 and x2 on y using the lm() function.
#     Suppose that x1 and x2 are factors that vary across samples. In this case, you want to account for the influence 
#     of one variable while assessing the effect of the other. To do so, include both predictors in a single linear model
#     using the + operator. Discuss whether the statistical estimates of x1 and x2’s effects make sense.

m_xy <- lm(y ~ x1 + x2, df_y)
summary(m_xy)

# given the visual relationship between x1 and y, the model results don't appear to make sense. This presents a case of 
# multicolinearity, where x1 and x2 are strongly correlated with each other, making the relationship between the explanatory and 
# predictor variables look visually different than what the linear model tells us (i.e., x1 and x2 are correlated with each other).

# (3) Create a Figure of Predictor vs. Predictor. Using df_y: 
# (3a) Create a scatter plot of x1 and x2.
df_y %>%  ggplot(aes(x=x1, y=x2)) + geom_point()

# (3b) Examine the Pearson’s correlation between x1 and x2 using a function cor(). (?cor() for its usage).
?cor()
cor(df_y$x1, df_y$x2) #super high correlation coefficient


# 13.4.4 Experimental Design ----------------------------------------------

#You are designing an experiment to examine the effects of nutrient level and predation on prey fish growth.
#The proposed treatment combinations are:

# (1) Low nutrient and predator present

# (2) High nutrient and predator present

# (3) High nutrient and predator absent

#In addition, you plan to record the initial length of the prey fish (a continuous variable), as it may influence subsequent growth

# Question: Select the most appropriate statement regarding what this experiment can assess:

# I think that option b is the most appropriate statement. This experiment can capture the main effects of nutrients and predation.
# However, the nutrient effect may be limited to the presence of predators, and the predator effect is limited to the high nutrient condition.

# I think it would be better to have a factorial design that includes four treatment combinations: 
# (1) Low nutrient and predator present

# (2) Low nutrient and predator absent

# (3) High nutrient and predator present

# (4) High nutrient and predator absent

# This would allow a more comprehensive and sound experiment.

