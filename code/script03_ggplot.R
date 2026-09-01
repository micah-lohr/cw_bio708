# 9/1/2026 script03_
library(tidyverse) #need to call every time that you open RStudio

# hot key for tidyverse pipe on Mac is command/shift/M (%>%)
# when you load tidyverse, ggplot2 is loaded and you don't need to call it separately
# can look at "from data to viz" on Dr. Terui's github textbook for more info
# hot key for making sections is command/shift/R (see point figure below)
# shortcut for commenting out is command/shift/C while selecting lines

# Point Figure ------------------------------------------------------------
# basic point figure 
iris %>% ggplot(
  mapping = aes(x = Sepal.Length,
                y= Sepal.Width)
) +
  geom_point() #if you don't include aes() then it will not work

# alternative syntax for basic point figure
ggplot(
  data=iris,
  mapping = aes(x=Sepal.Length,
                y=Sepal.Width)) + geom_point()

# color by species
iris %>% ggplot(
  aes(x=Sepal.Length, 
      y= Sepal.Width, 
      color=Species) 
) + geom_point() #color argument must be inside aes() function

# put color argument within geom_point to make all dots the same color
iris %>% ggplot(
  aes(x=Sepal.Length, 
      y= Sepal.Width) 
) + geom_point(color="darkorchid3") 


# Line Figure -------------------------------------------------------------
# make a sample dataframe
df0 <- tibble(
  x=rep(1:50, 3),
  y=x*2
)

#basic line graph 
df0 %>% ggplot(
  mapping=aes(x=x,
              y=y) 
) + geom_line()


# Histogram ---------------------------------------------------------------
# basic plot; bins = 30 is the default
iris %>% 
  ggplot(mapping = aes(x = Sepal.Length)) +
  geom_histogram() #specify only one variable

# change bin width within geom_histogram()
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5)

# change bin number within geom_histogram()
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram(bins = 50)


# Boxplot -----------------------------------------------------------------
# graph a categorical variable on x axis, continuous on y axis
# basic plot
iris %>% 
  ggplot(mapping = aes(x = Species,
             y = Sepal.Length)) +
  geom_boxplot()

# change fill/color code by "Species" ("color=" argument just changes border color)
iris %>% 
  ggplot(mapping = aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot() 

# change fill by "Species" but change border color consistently for plot
iris %>% 
  ggplot(mapping = aes(x = Species,
             y = Sepal.Length,
             fill = Species)) +
  geom_boxplot(color = "darkgrey")


# Fun Plot 1----------------------------------------------------------------
# Goal: make a violin plot 
iris %>% ggplot(
  mapping = aes(x= Species, 
                y= Sepal.Length, 
                fill=Species)) + 
  labs(title="Variation in Sepal Length by Iris Species", y="Sepal Length") +
  geom_violin() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


# Fun Plot 2 --------------------------------------------------------------
#Goal: make a bubble plot 
# Libraries for a bubbleplot- you need gapminder
install.packages("gapminder")
library(gapminder)
bubble_data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Most basic bubble plot
ggplot(bubble_data, 
       aes(x=gdpPercap,
           y=lifeExp,
           size = pop)) +
  geom_point(alpha=0.7)

# Making it fun 
bubble_data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap,
             y=lifeExp,
             size=pop,
             color=continent)) +
  labs(x= "GDP Per Capita", y= "Life Expectancy") +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24),
             name="Population (M)") +
  theme_classic()
