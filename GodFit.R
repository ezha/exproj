# GOF testing

# skräp!
lmrd(samlmu(rLDfY[[9]]$m_o_h), distributions="GEV ALL.LB PE3", xaxs="i", yaxs="i", las=1)
library("ADGofTest", lib.loc="C:/Program Files/R/R-2.15.3/library")
library(MASS)
library(VGAM)
ad.test(rLDfY[[81]]$m_o_h,pgev, location=317.2433716,scale=0.1412652,shape=0.2364874)
pelnor(samlmu(rLDfY[[81]]$m_o_h)))
ad.test(rLDfY[[81]]$m_o_h,pnorm,317.2975556,0.1455561)


library(lmom)
library(VGAM)

paramY <- vector("list",length(rLDfY))
adY <- vector("list",length(rLDfY))

for(l in 1:length(rLDfY)){
  paramY[[l]] <- pelgev(samlmu(rLDfY[[l]]$m_o_h))
}

location <- sapply(paramY, "[[", "xi")
scale <- sapply(paramY, "[[", "alpha")
shape <- sapply(paramY, "[[", "k")

for(l in 1:length(rLDfY)){
  adY[[l]] <- ad.test(rLDfY[[l]]$m_o_h,pgev,location[l],scale[l],shape[l])
}

ADvalues <- data.frame(AD= sapply(adY, "[[", "statistic"))

ggplot(ADvalues, aes(AD)) +
  geom_histogram()


#ADvalues[(seq(1,to=nrow(ADvalues),2)),]