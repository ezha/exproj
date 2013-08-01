# Annual maximum data, give thres (x.thres)
# PIII fit to the logs of the discharges
GwLP3 <- function(x,x.thres){

# load necessary packages:
  library("e1071")
  library("fitdistrplus")
  
# throw out all the NAs out
  x<-x[!is.na(x)]
  x.thres=1.35 # remove this in function
  x.POT<-x[x>=x.thres]
  GwLP3.lgd <- log(x.POT*100)

m <- mean(GwLP3.lgd)
v <- var(GwLP3.lgd)
s <- sd(GwLP3.lgd)
g <- e1071::skewness(GwLP3.lgd, type=1)

# Correct the sample skew for bias using the recommendation of 
# Bobee, B. and R. Robitaille (1977). "The use of the Pearson Type 3 and 
# Log Pearson Type 3 distributions revisited." 
# Water Resources Reseach 13(2): 427-443, as used by Kite

n <- length(x.POT)
g <- g*(sqrt(n*(n-1))/(n-2))*(1+8.5/n)

# We will use method of moment estimates as starting values for the MLE search

my.shape <- (2/g)^2
my.scale <- sqrt(v)/sqrt(my.shape)
my.location <- m-sqrt(v * my.shape)

my.param <- list(shape=my.shape, scale=my.scale, location=my.location)

  dPIII<-function(x, shape, location, scale) PearsonDS::dpearsonIII(x, shape, location, scale, log=FALSE)
  pPIII<-function(q, shape, location, scale) PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
  qPIII<-function(p, shape, location, scale) PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
  
  
  
LP3.fit<-fitdist(GwLP3.lgd, distr="PIII", method="mle", start=my.param)

return(LP3.fit)
}