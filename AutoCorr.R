# Autocorrelation
library(plyr)
library(ggplot2)

# yearly autocorr -------------------------------------------------------------

acY <- vector("list",length(rLDfY))
for(j in 1:143) acY[[j]] <- acf(rLDfY[[j]]$m_o_h, plot=F)
acYn1 <- sapply(acY, "[[", "acf")
acYn2 <- sapply(acY, "[[", "lag")
acYdF1 <- lapply(acYn1,as.data.frame)
acYdF2 <- lapply(acYn2,as.matrix)

acYdFl <- ldply(acYn,length)
for(j in 1:143) acYdF1[[j]]$rör <- rep(names(rLDfY[j]),acYdFl[j,])

acYdF <- rbind.fill(acYdF1)
acYdF$lag <- factor(do.call(rbind,acYdF2))

ggplot(acYdF, aes(x=lag, y=V1)) +
  geom_boxplot() +
  geom_hline(yintercept=0.2, linetype=2)+
  geom_hline(yintercept=-0.2, linetype=2) +
  ylab("autocorrelation")

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
  geom_hline(yintercept=0.2, linetype=2) +
  geom_hline(yintercept=-0.2, linetype=2) +
  ylab("autocorrelation")
