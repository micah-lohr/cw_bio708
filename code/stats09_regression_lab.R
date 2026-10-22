#stats 09 regression lab 

pacman::p_load(tidyverse)

# 12.4.1 develop regression models ----------------------------------------
iris
df_iris <- iris

## Split this data set by species (create three separate data frames)
df_setosa <- df_iris %>% filter(Species=="setosa")

df_versicolor <- df_iris %>% filter(Species=="versicolor")

df_virginica <- df_iris %>% filter(Species=="virginica")

## Perform regression for each species to analyze the relationship between Sepal.Width (response variable) and Petal.Width (explanatory variable).
m_setosa <- lm(Sepal.Width ~ Petal.Width, data= df_setosa)
summary(m_setosa)

m_versicolor <- lm(Sepal.Width ~ Petal.Width, data= df_versicolor)
summary(m_versicolor)

m_virginica <- lm(Sepal.Width ~ Petal.Width, data= df_virginica)
summary(m_virginica)

## for loop version 
v_sp <- unique(df_iris$Species)

fit <- list(NULL)
for (i in 1:length(v_sp)) {
  fit[[i]] <- iris %>% 
    filter(Species==v_sp[i]) %>% 
    lm(Sepal.Width ~ Petal.Width, data = .)
}

fit

# 12.4.2 multiple explanatory variables -----------------------------------
## Regression analysis can involve multiple explanatory variables.
## To explore this, consider utilizing Petal.Length as an additional explanatory variable for each species.
fit2 <- list(NULL)
for (i in 1:length(v_sp)) {
  fit2[[i]] <- iris %>% 
    filter(Species==v_sp[i]) %>% 
    lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)
}

fit2

## Investigate (1) the variations in estimates of regression coefficients and
fit2[1] #for setosa, petal width coefficient is larger than petal length
fit2[2] #for versicolor, petal width is much larger than petal length
fit2[3] #for virginica, petal width is also a larger coefficient than petal length

## overall, it appears that petal width is a stronger predictor of sepal width compared to petal length.

## (2) the differences in the coefficients of determination compared to the model with only a single explanatory variable.
fit[1]
fit[2]
fit[3]

# the intercept valuyes are smaller when you use multiple explanatory variables, as are the coefficients. 
# for example, for virginica, the petal.width coefficient is 0.5353 when there are multiple explanatory variables, and 0.6314 when there is a single explanatory variable.

## (3) Compare R^2
### For setosa, the R^2 value is 0.06522, meaning very little of the variation in sepal width is explained by petal width and petal length
### For versicolor, R^2 is 0.4447 and for virginica it is 0.3471, so more variation, though still less than half, is explained by petal width/length.

## When you add more explanatory variables, R^2 increases no matter what. Looking at the adjusted R^2 is better because it accounts for this, but you need to be aware when you interpret your summary!

iris_x <- iris %>% 
  mutate(useless = rnorm(150))

m0 <- lm(Sepal.Width ~ Petal.Width, data=iris_x)
m1 <- lm(Sepal.Width ~ Petal.Width + useless, data=iris_x)
summary(m0) # multiple R^2 is 0.134
summary(m1) # multiple R^2 is now 0.1507, even though the var. is useless!

