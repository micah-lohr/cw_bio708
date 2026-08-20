## data structure
## scalar
a <- 2.0 #numeric
b<- 4L #integer
d <- "aquatic"
a
b
d

## vector
va <- c(1.0, 2.3, 3) #numeric vector
vb <- c("a", "b", "c") #character vector
vc <- c("1", "2", "3.5") #still character
#vd <- c(1.0, "b") #made both characters- don't do this, use the same type of data

va
vb
vc

## matrix- everything must be the same data type
va <- c(1.0, 2.3, 3) # numeric vector
vb <- c(3,2,5.6) # numeric vector

ma <- cbind(va,vb) # vectors must be the same length by column
ma

mb <- rbind(va, vb) #vectors bind by row
mb

mc <- matrix(c(1,2,3,4), nrow=2, ncol=2)
mc
