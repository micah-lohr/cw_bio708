# Descriptive Statistics
library(tidyverse)

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
x
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)
y


# central tendency --------------------------------------------------------
# in ecology, geometric mean might be used for temporal avg, while arithmetic is used for spatial avg 
# arithmetic mean
mu_x <- sum(x) / length(x) #calculate manually
mean(x) #built-in R function

mu_y <- sum(y)/length(y)
mean(y)

#added () to print value when you assign it a name 
(mu_y <- sum(y)/length(y))

# geometric mean
(mu_x_ge <- prod(x)^(1/length(x))) #method 1

exp(sum(log(x)) / length(x)) #method 2, more stable

exp(sum(log(y))/ length(y)) 

(mu_y_ge <- prod(y)^(1/length(y)))

c(mu_x, mu_y) #arithmetic mean is very sensitive to outliers 
c(mu_x_ge, mu_y_ge) #geometric mean is always smaller than arithmetic mean

# median
(med_x<-median(x))
(med_y<-median(y))


# variation ---------------------------------------------------------------

# variance
sum((x- mean(x))^2) #sum of squares
(var_x <- sum((x- mean(x))^2) / length(x)) #variance is the arithmetic mean of deviations from the mean

(var_y <- sum((y-mean(y))^2) / length(y)) #shows that variance is still sensitive to outliers 

# standard deviation- square root of variance 
sqrt(var_x)
sqrt(var_y) #SD puts data back into original units, variance is units-squared

# interquartile range (IQR)- more robust to outliers
(x_l <- quantile(x, 0.25)) #first argument is vector, second is percentile you want to use
(x_h <- quantile(x, 0.75))
(iqr_x <- x_h - x_l) #IQR is higher percentile (75%) minus lower (25%)

(y_q <- quantile(y, c(0.25, 0.75))) #can put higher and lower quartiles together as a vector
(iqr_y<- y_q[2] - y_q[1]) #subtract second minus first element using brackets

# Median Absolute Deviation (MAD)- another robust measure of variation
ad_x <- abs(x-median(x))
median(ad_x) #median of the absolute deviation from the median

ad_y <- abs(y-median(y))
median(ad_y)

# coefficient of variation (CV)- no unit, for when you want to compare degree of variation across data sets of different measures
sd_x <- sqrt(var_x)
(cv_x <- sd_x/mu_x)

sd_y <- sqrt(var_y)
(cv_y <- sd_y/mu_y)


# extra -------------------------------------------------------------------
# function() used to define our own functions
mycv <- function(v) {
  mu <- mean(v)
  s <- sd(v)
  cv <- s/mu
  return(cv)
}

mycv(x)
mycv(y)
