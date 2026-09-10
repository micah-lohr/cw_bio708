# Descriptive Statistics Lab
library(tidyverse)

# central tendency --------------------------------------------------------
# (1) Create a new vector z with length 100 as exp(rnorm(n = 100, mean = 0, sd = 0.1)), and calculate the arithmetic mean, geometric mean, and median of z.
z <- exp(rnorm(n = 100, mean = 0, sd = 0.1))

(z_mu <- mean(z)) #arithmetic mean 

(mu_z_geom <- prod(z)^(1/length(z))) #geometric mean

(med_z <- median(z)) #median 

# (2) Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram().
z_for_histo <- tibble(z=z)

z_for_histo %>% 
  ggplot(mapping= aes(x=z)) + 
  geom_histogram() + 
  theme_classic()

# (3) Draw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors using a function geom_vline() .
z_for_histo %>% 
  ggplot(mapping= aes(x=z)) + 
  geom_histogram() + 
  geom_vline(xintercept=z_mu, color="orchid") +
  geom_vline(xintercept=mu_z_geom, color="cyan")+
  geom_vline(xintercept=med_z, color="hotpink")+
  theme_classic()
# could also make a separate df for the central tendencies and then call them in geom_vline

# (4) Visually compare the values of the central tendency measures with the vertical lines drawn by geom_vline().

#The arithmetic mean is higher/a larger number than than the geometric mean, biased towards the higher values. 
#The median is in between the geometric and arithmetic means. 

# (5) Create a new vector z_rev as -z + max(z) + 0.5, and repeat step 1 – 4.
z_rev <- -z +max(z) + 0.5

(z_rev_mu <- mean(z_rev)) #arithmetic mean 

(mu_z_rev_geom <- prod(z_rev)^(1/length(z_rev))) #geometric mean

(med_z_rev <- median(z_rev)) #median 

z_rev_for_histo <- tibble(z_rev=z_rev)

z_rev_for_histo %>% 
  ggplot(mapping= aes(x=z_rev)) + 
  geom_histogram() + 
  theme_classic()

z_rev_for_histo %>% 
  ggplot(mapping= aes(x=z_rev)) + 
  geom_histogram() + 
  geom_vline(xintercept=z_rev_mu, color="orchid") +
  geom_vline(xintercept=mu_z_rev_geom, color="cyan")+
  geom_vline(xintercept=med_z_rev, color="hotpink")+
  theme_classic()

#Visually compare: on this one, the median was the highest value, and the geometric mean was the lowest. 
#The arithmetic mean was in the middle, and both of the means seemed skewed towards the small outliers on the right.

# variation ---------------------------------------------------------------
# Why do we have absolute (variance, SD, MAD, IQR) and relative measures (CV, MAD/Median) of variation? 
# To understand this, suppose we have 100 measurements of fish weight in unit “gram.” (w in the following script)
w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

# Using this data, perform the following exercise:
# (1) Convert the unit of w to “milligram” and create a new vector m.
m <- 1000 * w

# (2) Calculate SD and MAD for w and m.
(w_sd <- sqrt(sum((w- mean(w))^2) / length(w)))
(m_sd <- sqrt(sum((m- mean(m))^2) / length(m)))

(w_MAD <- median(abs(w - median(w))))
(m_MAD <- median(abs(m - median(m))))

# (3) Calculate CV and MAD/Median for w and m.
(w_CV <- w_sd/mean(w))
(m_CV <- m_sd/mean(m))

(mm_w <- w_MAD/median(w))
(mm_m <- m_MAD/median(m))

#Notice that these two values are scaled so that you can compare between units (grams and milligrams)
