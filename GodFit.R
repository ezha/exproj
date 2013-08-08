# GOF testing

lmrd(samlmu(rLDfY[[9]]$m_o_h), distributions="GEV ALL.LB PE3", xaxs="i", yaxs="i", las=1)
library("ADGofTest", lib.loc="C:/Program Files/R/R-2.15.3/library")
library(MASS)
library(lmom)
library(VGAM)

parGEV <- vector("list",length(rLDfY))
adGEV <- vector("list",length(rLDfY))
ksGEV <- vector("list",length(rLDfY))

parNor <- vector("list",length(rLDfY))
adNor <- vector("list",length(rLDfY))

parWei <- vector("list",length(rLDfY))
adWei <- vector("list",length(rLDfY))

parEV1 <- vector("list",length(rLDfY))
adEV1 <- vector("list",length(rLDfY))

parLP3 <- vector("list",length(rLDfY))
adLP3 <- vector("list",length(rLDfY))


for(l in 1:length(rLDfY)){
  parGEV[[l]] <- pelgev(samlmu(rLDfY[[l]]$m_o_h))
  parNor[[l]] <- pelnor(samlmu(rLDfY[[l]]$m_o_h))
  parWei[[l]] <- tryCatch(pelwei(samlmu(rLDfY[[l]]$m_o_h)), 
                          error=function(e) e,finally = c(0, 1, 1))
  parEV1[[l]] <- tryCatch(pelgum(samlmu(rLDfY[[l]]$m_o_h)), 
                          error=function(e) e,finally = c(0, 1, 1))
  par[[l]] <- tryCatch(pelgum(samlmu(rLDfY[[l]]$m_o_h)), 
                          error=function(e) e,finally = c(0, 1, 1))
}


# GEV parameters --------------------------------------------------------------
location <- sapply(parGEV, "[[", "xi")
scale <- sapply(parGEV, "[[", "alpha")
shape <- sapply(parGEV, "[[", "k")

# Normal parameters -----------------------------------------------------------
mean <- sapply(parNor, "[[", "mu")
sd <- sapply(parNor, "[[", "sigma")

# Weibull parameters ----------------------------------------------------------
zeta <- tryCatch(sapply(parWei, "[[", "zeta"), error=function(e) NULL)
beta <- tryCatch(sapply(parWei, "[[", "beta"), error=function(e) NULL)
delta <- tryCatch(sapply(parWei, "[[", "delta"), error=function(e) NULL)

# EV1 parameters --------------------------------------------------------------
xi <- tryCatch(sapply(parEV1, "[[", "xi"), error=function(e) NULL)
alpha <- tryCatch(sapply(parEV1, "[[", "alpha"), error=function(e) NULL)


# GOF tests -------------------------------------------------------------------
for(l in 1:length(rLDfY)){
  adGEV[[l]] <- ad.test(rLDfY[[l]]$m_o_h,cdfgev, c(location[l], 
                                                   scale[l], shape[l]))
  #ksGEV[[l]] <- ks.test(rLDfY[[l]]$m_o_h,cdfgev, c(location[l], 
  #scale[l], shape[l]))
  adNor[[l]] <- ad.test(rLDfY[[l]]$m_o_h,cdfnor, c(mean[l], sd[l]))
  adWei[[l]] <- tryCatch(ad.test(rLDfY[[l]]$m_o_h, cdfwei, 
                                 c(zeta[[l]], beta[[l]], delta[[l]])),
                                error=function(e) e=1)
  adEV1[[l]] <- tryCatch(ad.test(rLDfY[[l]]$m_o_h, cdfgum, 
                               c(xi[[l]], alpha[[l]])),
                       error=function(e) e=1)
}


ADGEV <- data.frame(AD= sapply(adGEV, "[[", "statistic"), FIT = "GEV")
ADGEV$rör <- names(rLDfY)

ADNor <- data.frame(AD= sapply(adNor, "[[", "statistic"), FIT = "Normal")
ADNor$rör <- names(rLDfY)

names(adWei) <- names(rLDfY)
adWei <- adWei[adWei != "1"]
ADWei <- data.frame(AD= sapply(adWei, "[[", "statistic"),FIT = "Weibull")
# TODO improve here
ADWei$rör <- names(adWei)

names(adEV1) <- names(rLDfY)
adEV1 <- adEV1[adEV1 != "1"]
ADEV1 <- data.frame(AD= sapply(adEV1, "[[", "statistic"),FIT = "EV1")
# TODO improve here
ADEV1$rör <- names(adEV1)


ADGEVok <- ADGEV[which(ADGEV$AD <=2.492),]
ADNorok <- ADNor[which(ADNor$AD <=2.492),]
ADWeiok <- ADWei[which(ADWei$AD <=2.492),]
ADEV1ok <- ADEV1[which(ADEV1$AD <=2.492),]

ADFits = rbind.fill(ADNorok, ADGEVok, ADWeiok, ADEV1ok)

ggplot(ADFits, aes(FIT,AD)) +
  geom_boxplot() +
#   geom_jitter(position = position_jitter(width = .1), size=0.1) +
  ylab(expression(A^2))


# plot to look at cdf vs ecdf -------------------------------------------------
ggg <- data.frame(CDF=sort(cdfgev(rLDfY[[l]]$m_o_h, c(location[l], 
                                                      scale[l], shape[l]))),
                  val=sort(rLDfY[[l]]$m_o_h),
                  ecdf=sort(rLDfY[[l]]$m_o_h))

ggplot(data=ggg, aes(x=val, y=CDF)) +
  geom_step(aes(colour= 'fitted')) +
  stat_ecdf(aes(x=ecdf, colour = 'Empirical'), geom = "step")

