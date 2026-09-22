# Probabilistic View
pacman::p_load(tidyverse,
               patchwork,
               ggplot2)

df_h0 <- read_csv("data_src/data_plant_height.csv") # load csv data on R

# Continuous variable = probability density function; discrete variable = probability mass function 

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean


# Probability Density Function --------------------------------------------

# draw probability distribution 
x <- seq(min(df_h0$height),
    max(df_h0$height),
    length=100)

# calculate probability density 
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma) #function dnorm() calculates probability density with arguments sequence, mean, sd

# figure
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density") 

# The y-axis represents “probability density.” To convert it into actual “probability,” we need to calculate the area under the curve. 
# pnorm() calculates the probability of a variable being < the specified value, which is provided in the first argument q.
# convert probability density to frequency:

# probability of observing a value in x that is < 10
p10 <- pnorm(q = 10, mean = mu, sd = sigma)
print(p10) #calculating area under the curve

# probability of x < 20
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
print(p20)

# probability of 10 < x < 20
p20_10 <- p20 - p10
print(p20_10)

x_min <- floor(min(df_h0$height)) #floor rounds down to nearest integer
x_max <- ceiling(max(df_h0$height)) #ceiling rounds up to nearest integer 
bin <- seq(x_min, x_max, by=1) #where length above eas for number of elements, by=1 determines the interval

p <- NULL
for (i in 1:(length(bin) - 1)) {
  ## p_up - probability up to bin [i+1]
  p_up <- pnorm(bin[i+1], mean=mu, sd=sigma)
  ## p_low - probability up to bin [i]
  p_low <- pnorm(bin[i], mean=mu, sd=sigma)
  ## difference represents probability between bin[i] and bin[i+1] 
  p[i] <- p_up-p_low
}

# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin
df_prob <- tibble(p, bin=bin[-length(bin)] + 0.5) %>% 
  mutate(freq=p*nrow(df_h0))

# combine data and PDF
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, 
                 center = 0.5) + 
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")

# Probability Mass Function -----------------------------------------------
df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)

#Poisson distributions assume mean and variance are equal

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # define bin width
                 center = 0) # relative position of each bin

## Poisson fit
x <- seq(0, 10, by = 1)

# calculate probability mass
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat) #dpois() returns probability, not probability density since it's for discrete data

# figure
tibble(y = pm, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label

# overlay
 df_prob <- tibble(x=x,
                   y=pm) %>% 
   mutate(freq= y*nrow(df_count))

 df_count %>% 
   ggplot(aes(x = count)) +
   geom_histogram(binwidth = 0.5, 
                  center = 0) +
   geom_line(data = df_prob,
             aes(x = x,
                 y = freq),
             linetype = "dashed", 
             color="steelblue") +
   geom_point(data = df_prob,
              aes(x = x,
                  y = freq),
              color="steelblue")
 