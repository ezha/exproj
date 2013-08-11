# Autocorrelation
library(plyr)
library(ggplot2)

# yearly autocorr -------------------------------------------------------------

acYabs <- vector("list",length(rLDfY))
acYnorm <- vector("list",length(rLDfY))

rLDfY <- lapply(rLDfY, function(x) 
                        ddply(x, .(Omr_stn),mutate, 
                        norm = (m_o_h - mean(m_o_h))/sd(m_o_h)))
for(j in 1:143){ 
  acYabs[[j]] <- acf(rLDfY[[j]]$m_o_h, plot=F)
  acYnorm[[j]] <- acf(rLDfY[[j]]$norm, plot=F)
}


acYnabs <- sapply(acYabs, "[[", "acf")
acYnnor <- sapply(acYnorm, "[[", "acf")
acYnlag <- sapply(acYabs, "[[", "lag")

acYdFabs <- lapply(acYnabs,as.data.frame)
acYdFnor <- lapply(acYnnor,as.data.frame)
acYdFlag <- lapply(acYnlag,as.matrix)

acYdFl <- ldply(acYnabs,length)
for(j in 1:143) {
  acYdFabs[[j]]$rör <- rep(names(rLDfY[j]),acYdFl[j,])
  acYdFnor[[j]]$rör <- rep(names(rLDfY[j]),acYdFl[j,])
}

acYdFabs <- rbind.fill(acYdFabs)
acYdFabs$lag <- factor(do.call(rbind,acYdFlag))
acYdFnor <- rbind.fill(acYdFnor)
acYdFnor$lag <- factor(do.call(rbind,acYdFlag))

ggplot(acYdFabs, aes(x=lag, y=V1)) +
  geom_boxplot() +
  geom_hline(yintercept=0.2, linetype=2, colour = "#7A0177") +
  geom_hline(yintercept=-0.2, linetype=2, colour = "#7A0177") +
  ylab("ACF absoluta värden")

ggplot(acYdFnor, aes(x=lag, y=V1)) +
  geom_boxplot() +
  geom_hline(yintercept=0.2, linetype=2, colour = "#7A0177") +
  geom_hline(yintercept=-0.2, linetype=2, colour = "#7A0177") +
  ylab("ACF")


# monthly autocorr -----------------------------------------------------------

acQ <- vector("list",length(rLDfQ))
for(j in 1:143) acQ[[j]] <- acf(rLDfQ[[j]]$m_o_h, plot=F, na.action=na.pass)
acQn1 <- sapply(acQ, "[[", "acf")
acQn2 <- sapply(acQ, "[[", "lag")
acQdF1 <- lapply(acQn1,as.data.frame)
acQdF2 <- lapply(acQn2,as.matrix)

acQdFl <- ldply(acQn,length)
for(j in 1:143) acQdF1[[j]]$rör <- rep(names(rLDfQ[j]),acQdFl[j,])

acQdF <- rbind.fill(acQdF1)
acQdF$lag <- factor(do.call(rbind,acQdF2))

ggplot(acQdF, aes(x=lag, y=V1)) +
  geom_boxplot() +
  geom_hline(yintercept=0.2, linetype=2, colour = "#7A0177") +
  geom_hline(yintercept=-0.2, linetype=2, colour = "#7A0177") +
  ylab("autocorrelation")


# turning point statistics ----------------------------------------------------

turning.point.test <- function(ts)
{
  n <- length(ts)
  mu <- 2*(n-2)/3
  var <- (16*n-29)/90
  x <- embed(ts,3)
  test.sum <- sum((x[,2] > x[,1] & x[,2] > x[,3]) | (x[,2] < x[,1] & x[,2] < x[,3]))
  test <- abs(test.sum-mu)/sqrt(var)
  p.value <- 2*(1-pnorm(test))
  structure(list(test.sum=test.sum,test=test,p.value=p.value,mu=mu,var=var))
}

tpY <- vector("list",length(rLDfY))
for(j in 1:143) tpY[[j]] <- as.numeric(rLDfY[[j]]$m_o_h)
tpYa <- lapply(tpY,turning.point.test)
ggplot2::qplot(sapply(tpYa, "[[", "p.value"))

# TODO EXTREMOGRAMM!!!

# TODO 