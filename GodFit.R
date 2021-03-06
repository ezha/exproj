# GOF testing

lmrd(samlmu(rLDfY[[9]]$norm), distributions="GEV ALL.LB PE3", xaxs="i", yaxs="i", las=1)
library("ADGofTest")
library(MASS)
library(lmom)
library(VGAM)
library(plyr)
library(xts)

# Create monthly time series longer than 10 years -----------------------------

rLDfYmin <- ldply(rLDfY, summarise, min= min(m_o_h))

rXts10Y <- vector("list", length(rXtsM))
names(rXts10Y) <- names(rXts)
Df10Y <- vector("list", length(rXtsM))

for(i in 1:length(rXtsM)){
  if(length(rXtsM[[i]]) >= 120){
    rXts10Y[[i]] <- apply.yearly(lag(rXtsM[[i]],3),max)
    #rXts10Y[[i]] <- rXts10Y[[i]][!is.na(rXts10Y[[i]])] 
    Df10Y[[i]] <- data.frame(tid = index(rXts10Y[[i]]), 
                              m_o_h = rXts10Y[[i]]$m_o_h, 
                              Omr_stn = names(rXts[i]))
    # get rid of NA values
    
    
  }
}

rXts10Y <- rXts10Y[rXts10Y != "NULL"]
Df10Y <- rDf10Y[Df10Y != "NULL"]

# turn yearly xts to data.frame for further manipulation
Df10Y <- lapply(Df10Y, function(x) 
  ddply(x, .(Omr_stn), transform, 
        norm = (m_o_h - mean(m_o_h, na.rm=T))/sd(m_o_h, na.rm=T)))


# Choose length of series -----------------------------------------------------
# at least 40 years:
Df40Y
# at least 10 years:
Df10Y

rLDfY <- Df10Y

# Prepare GOF and fitting of series -------------------------------------------

parGEV <- vector("list",length(rLDfY))
adGEV <- vector("list",length(rLDfY))
ksGEV <- vector("list",length(rLDfY))

parNor <- vector("list",length(rLDfY))
adNor <- vector("list",length(rLDfY))
ksNor <- vector("list",length(rLDfY))

parWei <- vector("list",length(rLDfY))
adWei <- vector("list",length(rLDfY))
ksWei <- vector("list",length(rLDfY))

parEV1 <- vector("list",length(rLDfY))
adEV1 <- vector("list",length(rLDfY))
ksEV1 <- vector("list",length(rLDfY))

parLP3 <- vector("list",length(rLDfY))
adLP3 <- vector("list",length(rLDfY))
ksLP3 <- vector("list",length(rLDfY))

Lmom <- vector("list", length(rLDfY))

# Estimate parameters and L-moments -------------------------------------------

for(l in 1:length(rLDfY)){
  parNor[[l]] <- pelnor(samlmu(rLDfY[[l]]$m_o_h))
  parGEV[[l]] <- pelgev(samlmu(rLDfY[[l]]$m_o_h))
  parEV1[[l]] <- tryCatch(pelgum(samlmu(rLDfY[[l]]$m_o_h)), 
                          error=function(e) e,finally = c(0, 1, 1))
  parWei[[l]] <- tryCatch(pelwei(samlmu(rLDfY[[l]]$m_o_h)), 
                          error=function(e) e,finally = c(0, 1, 1))
  parLP3[[l]] <- tryCatch(pelpe3(samlmu(log(rLDfY[[l]]$m_o_h))), 
                          error=function(e) e,finally = c(0, 1, 1))
  Lmom[[l]] <- data.frame(lskew = samlmu(rLDfY[[l]]$m_o_h)[[3]],
                          lkurt = samlmu(rLDfY[[l]]$m_o_h)[[4]],
                          Omr_stn = names(rLDfY[l]),
                          obs = "obs",
                          stlk = factor(2))
}
Lmom <- rbind.fill(Lmom)



# Normal parameters -----------------------------------------------------------
mean <- sapply(parNor, "[[", "mu")
sd <- sapply(parNor, "[[", "sigma")
mlenorm <- lapply(rLDfY, function(x) fitdist(x$norm, distr='norm'))

# GEV parameters --------------------------------------------------------------
location <- sapply(parGEV, "[[", "xi")
scale <- sapply(parGEV, "[[", "alpha")
shape <- sapply(parGEV, "[[", "k")
mlegev <- lapply(rLDfY, function(x) fitdist(x$norm, distr='gev', 
                start=c()))

# EV1 parameters --------------------------------------------------------------
xi <- tryCatch(sapply(parEV1, "[[", "xi"), error=function(e) NULL)
alpha <- tryCatch(sapply(parEV1, "[[", "alpha"), error=function(e) NULL)


# Weibull parameters ----------------------------------------------------------
zeta <- tryCatch(sapply(parWei, "[[", "zeta"), error=function(e) NULL)
beta <- tryCatch(sapply(parWei, "[[", "beta"), error=function(e) NULL)
delta <- tryCatch(sapply(parWei, "[[", "delta"), error=function(e) NULL)

# LP3 parameters --------------------------------------------------------------
mu <- tryCatch(sapply(parLP3, "[[", "mu"), error=function(e) NULL)
sigma <- tryCatch(sapply(parLP3, "[[", "sigma"), error=function(e) NULL)
gamma <- tryCatch(sapply(parLP3, "[[", "gamma"), error=function(e) NULL)

# GOF tests Lmom --------------------------------------------------------------
for(l in 1:length(rLDfY)){
  adNor[[l]] <- ad.test(rLDfY[[l]]$m_o_h, cdfnor, c(mean[l], sd[l]))
  ksNor[[l]] <- ks.test(x=rLDfY[[l]]$m_o_h, y=cdfnor, c(mean[l], sd[l]))
  
 
  adGEV[[l]] <- ad.test(rLDfY[[l]]$m_o_h, cdfgev, 
                        c(location[l], scale[l], shape[l]))
  ksGEV[[l]] <- ks.test(x=rLDfY[[l]]$m_o_h, y=cdfgev, 
                        c(location[l], scale[l], shape[l]))
  
  
  adEV1[[l]] <- tryCatch(ad.test(rLDfY[[l]]$m_o_h, cdfgum, 
                               c(xi[[l]], alpha[[l]])),
                       error=function(e) e=1)
  ksEV1[[l]] <- tryCatch(ks.test(rLDfY[[l]]$m_o_h, cdfgum, 
                                 c(xi[[l]], alpha[[l]])),
                         error=function(e) e=1)
  
  
  adWei[[l]] <- tryCatch(ad.test(rLDfY[[l]]$m_o_h, cdfwei, 
                                 c(zeta[[l]], beta[[l]], delta[[l]])),
                         error=function(e) e=1)
  ksWei[[l]] <- tryCatch(ks.test(x=rLDfY[[l]]$m_o_h, y=cdfwei, 
                                 c(zeta[[l]], beta[[l]], delta[[l]])),
                         error=function(e) e=1)
  
  
  adLP3[[l]] <- tryCatch(ad.test(log(rLDfY[[l]]$m_o_h), cdfpe3, 
                                 c(mu[[l]], sigma[[l]], gamma[[l]])),
                         error=function(e) e=1)
  ksLP3[[l]] <- tryCatch(ks.test(x = log(rLDfY[[l]]$m_o_h), y = cdfpe3, 
                                 c(mu[[l]], sigma[[l]], gamma[[l]])),
                         error=function(e) e=1)
}

# GOF tests MLE ---------------------------------------------------------------
for(l in 1:length(rLDfY)){
  adNor[[l]] <- ad.test(rLDfY[[l]]$m_o_h, pnorm, 
                        mlenorm[[l]]$estimate[[1]], 
                        mlenorm[[l]]$estimate[[2]])
  ksNor[[l]] <- ks.test(x=rLDfY[[l]]$m_o_h, y=pnorm, 
                        mlenorm[[l]]$estimate[[1]], 
                        mlenorm[[l]]$estimate[[2]])
  
  
#   adGEV[[l]] <- ad.test(rLDfY[[l]]$m_o_h, cdfgev, 
#                         c(location[l], scale[l], shape[l]))
#   ksGEV[[l]] <- ks.test(x=rLDfY[[l]]$m_o_h, y=cdfgev, 
#                         c(location[l], scale[l], shape[l]))
#   
#   
#   adEV1[[l]] <- tryCatch(ad.test(rLDfY[[l]]$m_o_h, cdfgum, 
#                                  c(xi[[l]], alpha[[l]])),
#                          error=function(e) e=1)
#   ksEV1[[l]] <- tryCatch(ks.test(rLDfY[[l]]$m_o_h, cdfgum, 
#                                  c(xi[[l]], alpha[[l]])),
#                          error=function(e) e=1)
#   
#   
#   adWei[[l]] <- tryCatch(ad.test(rLDfY[[l]]$m_o_h, cdfwei, 
#                                  c(zeta[[l]], beta[[l]], delta[[l]])),
#                          error=function(e) e=1)
#   ksWei[[l]] <- tryCatch(ks.test(x=rLDfY[[l]]$m_o_h, y=cdfwei, 
#                                  c(zeta[[l]], beta[[l]], delta[[l]])),
#                          error=function(e) e=1)
#   
#   
#   adLP3[[l]] <- tryCatch(ad.test(log(rLDfY[[l]]$m_o_h), cdfpe3, 
#                                  c(mu[[l]], sigma[[l]], gamma[[l]])),
#                          error=function(e) e=1)
#   ksLP3[[l]] <- tryCatch(ks.test(x = log(rLDfY[[l]]$m_o_h), y = cdfpe3, 
#                                  c(mu[[l]], sigma[[l]], gamma[[l]])),
#                          error=function(e) e=1)
}


# prepare GOF results for data frame normal -----------------------------------

# Anderson Darling
ADNor <- data.frame(AD = sapply(adNor, "[[", "statistic"), FIT = "Normal")
ADNor$Omr_stn <- names(rLDfY)

# Kolmogorov
KSNor <- data.frame(KS = sapply(ksNor, "[[", "statistic"), FIT = "Normal")
KSNor$Omr_stn <- names(rLDfY)


# prepare GOF results for data frame GEV --------------------------------------

# Anderson Darling
ADGEV <- data.frame(AD = sapply(adGEV, "[[", "statistic"), FIT = "GEV")
ADGEV$Omr_stn <- names(rLDfY)

# Kolmogorov
KSGEV <- data.frame(KS = sapply(ksGEV, "[[", "statistic"), FIT = "GEV")
KSGEV$Omr_stn <- names(rLDfY)


# prepare GOF results for data frame Weibull ----------------------------------

# Anderson Darling
names(adWei) <- names(rLDfY)
adWei <- adWei[adWei != "1"]
ADWei <- data.frame(AD = sapply(adWei, "[[", "statistic"),FIT = "Weibull")
ADWei$Omr_stn <- names(adWei)

# Kolmogorov
names(ksWei) <- names(rLDfY)
ksWei <- ksWei[ksWei != "1"]
KSWei <- data.frame(KS = sapply(ksWei, "[[", "statistic"),FIT = "Weibull")
KSWei$Omr_stn <- names(ksWei)


# prepare GOF results for data frame EV1 --------------------------------------

# Anderson Darling
names(adEV1) <- names(rLDfY)
adEV1 <- adEV1[adEV1 != "1"]
ADEV1 <- data.frame(AD = sapply(adEV1, "[[", "statistic"),FIT = "EV1")
ADEV1$Omr_stn <- names(adEV1)

# Kolmogorov
names(ksEV1) <- names(rLDfY)
ksEV1 <- ksEV1[ksEV1 != "1"]
KSEV1 <- data.frame(KS = sapply(ksEV1, "[[", "statistic"),FIT = "EV1")
KSEV1$Omr_stn <- names(ksEV1)


# prepare GOF results for data frame LP3 --------------------------------------

# Anderson Darling
names(adLP3) <- names(rLDfY)
adLP3 <- adLP3[adLP3 != "1"]
ADLP3 <- data.frame(AD = sapply(adLP3, "[[", "statistic"),FIT = "LP3")
ADLP3$Omr_stn <- names(adLP3)

# Kolmogorov
names(ksLP3) <- names(rLDfY)
ksLP3 <- ksLP3[ksLP3 != "1"]
KSLP3 <- data.frame(KS = sapply(ksLP3, "[[", "statistic"),FIT = "LP3")
KSLP3$Omr_stn <- names(ksLP3)


# Data frame with all AD-GOF results --> plotting -----------------------------

ADNorok <- ADNor[which(ADNor$AD <= 2.492),]
ADGEVok <- ADGEV[which(ADGEV$AD <= 2.492),]
ADEV1ok <- ADEV1[which(ADEV1$AD <= 2.492),]
ADWeiok <- ADWei[which(ADWei$AD <= 2.492),]
ADLP3ok <- ADLP3[which(ADLP3$AD <= 2.492),]

ADFits = rbind.fill(ADNorok, ADGEVok, ADEV1ok, ADWeiok, ADLP3ok)

ggplot(ADFits, aes(FIT,AD)) +
  geom_boxplot() +
#   geom_jitter(position = position_jitter(width = .1), size=0.1) +
  ylab(expression(A^2)) +
  geom_text(stat="bin", color="black", vjust=1,
            aes(y=2.5, label=..count..))

# Best fit AD ----------------------------------------------------------------
ADBFex <- ddply(ADFits, .(Omr_stn), summarise, AD = min(AD))
ADBF <- join(ADBFex, ADFits, by = c("Omr_stn","AD"), type = "inner")

ggplot(ADBF, aes(FIT)) +
  geom_bar(stat="bin") +
  xlab("fördelning") + ylab("antal") +
  theme_minimal()


# Data frame with all KS-GOF results --> plotting -----------------------------

KSNorok <- KSNor[which(KSNor$KS <= 0.12555),]
KSGEVok <- KSGEV[which(KSGEV$KS <= 0.12555),]
KSEV1ok <- KSEV1[which(KSEV1$KS <= 0.12555),]
KSWeiok <- KSWei[which(KSWei$KS <= 0.12555),]
KSLP3ok <- KSLP3[which(KSLP3$KS <= 0.12555),]

KSFits = rbind.fill(KSNorok, KSGEVok, KSEV1ok, KSWeiok, KSLP3ok)

ggplot(KSFits, aes(FIT,KS)) +
  geom_boxplot() +
  #   geom_jitter(position = position_jitter(width = .1), size=0.1) +
  ylab(expression(D)) +
  geom_text(stat="bin", color="black", vjust=1,
            aes(y=0.13, label=..count..))

# Best fit KS ----------------------------------------------------------------
KSBFex <- ddply(KSFits, .(Omr_stn), summarise, KS = min(KS))
KSBF <- join(KSBFex, KSFits, by = c("Omr_stn","KS"), type = "inner")

ggplot(KSBF, aes(FIT)) +
  geom_bar(stat="bin") +
  xlab("fördelning") + ylab("antal") +
  theme_minimal()


# plot to look at cdf vs ecdf -------------------------------------------------
ggg <- data.frame(CDF=sort(cdfgev(rLDfY[[l]]$m_o_h, c(location[l], 
                                                      scale[l], shape[l]))),
                  val=sort(rLDfY[[l]]$m_o_h),
                  ecdf=sort(rLDfY[[l]]$m_o_h))

ggplot(data=ggg, aes(x=val, y=CDF)) +
  geom_step(aes(colour= 'fitted')) +
  stat_ecdf(aes(x=ecdf, colour = 'Empirical'), geom = "step")
  

# L-moments diagram -----------------------------------------------------------

# create necessary data:

tau3 = seq(-0.1, 0.6, length.out= 400)

EV1Lmom <- data.frame(par2 = "EV1", lskew = 0.1699, lkurt = 0.1504)
NorLmom <- data.frame(par2 = "Normal", lskew = 0, lkurt = 0.1226)
WeiLmom <- data.frame(par3 = "Weibull", 
                      lskew = (sapply(seq(-0.1,0.6, length.out = 400), 
                        function(t3) lmrwei(pelwei(c(0,1,t3)), nmom=4)[[3]])),
                      lkurt = (sapply(seq(-0.1,0.6, length.out = 400),                                           
                        function(t3) lmrwei(pelwei(c(0,1,t3)), nmom=4)[[4]])))

LP3Lmom <- data.frame(par3 = rep("LP3",length(tau3)),
                      lskew = tau3, 
                      lkurt = 0.1224 + 0.30115 * tau3^2 + 0.95812 * tau3^4 + 
                              0.57488 * tau3^6 +  0.19383 * tau3^8)

GEVLmom <- data.frame(par3 = rep("GEV",length(tau3)),
                      lskew = tau3, 
                      lkurt = 0.10701 + 0.1109 * tau3 + 0.84838 * tau3^2 + 
                        0.06669 * tau3^3 + 0.00567 * tau3^4 + 
                        0.04208 * tau3^5 + 0.03673 * tau3^6)
L3par <- rbind.fill(LP3Lmom,GEVLmom, WeiLmom)
L2par <- rbind.fill(NorLmom,EV1Lmom)

# Sample average
LmomSampAvg <- data.frame(lskew = mean(Lmom$lskew),
                          lkurt = mean(Lmom$lkurt),
                          Omr_stn = factor("00_00"),
                          obs = "Medelvärde",
                          stlk = factor(5))
Lmom10 <- rbind.fill(Lmom, LmomSampAvg)
# Lmom40 <- Lmom

# plot l-moment diagram with ggplot

ggplot(data = L3par, aes(x=lskew, y=lkurt)) +
  geom_line(aes(colour = par3), size = 1.2) +
  geom_point(data = L2par, aes(colour= par2), size = 5, shape = 15) + 
  geom_point(data = Lmom, aes(fill = obs, shape = obs, size=stlk)) +
  scale_size_manual(values=c(2,4), guide=FALSE) +
#   geom_point(data = LmomSampAvg, aes(colour = dis), size = 5, shape = 12) +
  xlab(expression(italic(L) * "-skewness")) +
  ylab(expression(italic(L) * "-kurtosis")) +
#   geom_text(data=Lmom, aes(label = Omr_stn, y = lkurt + 0.02, size=0.3)) +
  ylim(0,0.4) + xlim(-0.1,0.6)

# TODO 
Line of Best fit