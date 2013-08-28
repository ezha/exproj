<<<<<<< HEAD
# SGU Data import/analysis

## Find all options in part Series 10
# change according to data
# setwd("~/RProjects/exjobb")
xRaw <- file.path("./data/grvniva2.txt")
xMeta <- file.path("./data/grvn_stninfo_nivaaktiv.txt")
xMeta2 <- file.path("./data/all_metadata_nivastn.txt")

# 1.    Get metadata in good form ------------------------------------------------

# 1.1   Load metadata active stations --------------------------------------------
metadata <- read.delim(xMeta, encoding="ISO-8859-1")

# check if enough data available in TACKT
m <- data.frame(Omr_stn = metadata$ID,
  Omr = metadata$OMR,
                stn = metadata$STN,
                       Lnr = factor(metadata$LNR),
                       Northing = as.numeric(metadata$N),
                       Easting = as.numeric(metadata$E),
                       Top = factor(metadata$TOP),
                       Akvifertyp = factor(metadata$AKVIFER),
                       Geohylag = factor(metadata$GEOHYLAG),
                       Jordart = factor(metadata$JORDART),
                       Taeckt = factor(metadata$TACKT),
                       Bergart = factor(metadata$BERGART),
                       MagasinStl = factor(metadata$MAGSTLK))


# m<- m[with(m, order(Omr, stn)), ]
# 
# is.na(m)[NAs] <- TRUE

# TODO
# map other names for easier understanding: mapvalues(tempmeta$Akvifertyp, c())
# rm(tempmeta)
write.table(m,file="./data/SGUmeta_active.csv",sep="\t",row.names=F, quote=F)

# 1.2   Load metadata all stations -----------------------------------------------

metadata2 <- read.delim(xMeta2, encoding="ISO-8859-1")

# check if enough data available in TACKT
m2 <- data.frame(Omr_stn = metadata2$ID,
                Omr = metadata2$OMR,
                stn = metadata2$STN,
                Lnr = factor(metadata2$LNR),
                Northing = as.numeric(metadata2$N),
                Easting = as.numeric(metadata2$E),
                Top = factor(metadata2$TOP),
                Akvifertyp = factor(metadata2$AKVIFER),
                Geohylag = factor(metadata2$GEOHYLAG),
                Jordart = factor(metadata2$JORDART),
                Taeckt = factor(metadata2$TACKT),
                Bergart = factor(metadata2$BERGART),
                MagasinStl = factor(metadata2$MAGSTLK))

library("plyr")
# m2$Akvifertyp <- revalue(m2$Akvifertyp, c("J?"="JÖ", "B?"="BÖ","K?"="KÖ"))
m2$MagasinStl <- revalue(m2$MagasinStl, c("L"="Liten", "S"="Stor"))

setequal(levels(m2$Omr_stn), levels(m$Omr_stn))

m <- rbind(m,m2)
m<- m[!duplicated(m$Omr_stn), ]

# 1.3    Remaping of metadata -------------------------------------------------
library("plyr")
m$Akvifertyp <- revalue(m$Akvifertyp, c("J?"="JÖ", "B?"="BÖ","K?"="KÖ"))

m$Geohylag <- revalue(m$Geohylag, c("I"="Inströmningsområde",
                                    "M"="Intermediärt område",
                                    "U"="Utströmingsområde",
                                    "V"="Vattendelarläge"))

m$MagasinStl <- revalue(m$MagasinStl, c("L"="Liten", "S"="Stor"))

!##############################################################################

# 1.4    Compute which rows are missing ----------------------------------------

m2 <- data.frame(Omr_stn = m$Omr_stn)

r2 <- rMonthly$Omr_stn[!duplicated(rMonthly$Omr_stn)]

m2r2 <- setdiff(levels(r2), levels(m2$Omr_stn))

# Sort dataframe after columns
#m2r2 <- m2r2[with(m2r2, order(Omr, stn)), ]



# 2.     Get rawdata in good form ---------------------------------------------
library("xts")
library("plyr")


## load rawdata
rawdata <- read.table(xRaw, header=TRUE,strip.white=TRUE
                ,colClasses=c(rep("factor",3),"Date","NULL","numeric","NULL"))

# split wells to list object for aggregation purposes
r <- split(rawdata,rawdata$Omr_stn)
tempdata1 <- lapply(r, function(y) cbind(rnam=row.names(y), y))
tempdata2 <- lapply(r, function(y) cbind(rnam=row.names(y), y))
names(r) <- paste('r', names(tempdata1[]),sep="")


# Preallocate list for the new xts list
r.xts <- vector("list", length(r))
names(r.xts) <- names(r)

# Preallocate list for the new monthly aggregated xts
r.xts.m <- vector("list", length(r))
rDfM <- vector("list", length(r))

# Preallocate list for the new monthly aggregated regular ts
r.ts.m <- vector("list", length(r))
names(r.ts.m) <- names(r)

# 2.1    Aggregate to monthly max ---------------------------------------------

for(i in 1:length(r)){ 
  # make list items to xts objects
  r.xts[[i]] <- xts(r[[i]]$m_o_h,as.Date(r[[i]]$Datum))
  colnames(r.xts[[i]]) <- 'm_o_h'
  
  # aggregated monthly values  
  r.xts.m[[i]] <- apply.monthly(r.xts[[i]],max)
  
  # transform to data.frame that can be used for further analysis and plotting
  rDfM[[i]] <- data.frame(tid = index(r.xts.m[[i]]), 
                            m_o_h = r.xts.m[[i]]$m_o_h, 
                            Omr_stn = names(r[i]))
  
  # transform to regular ts, currently not working
  r.ts.m[[i]] <- ts(r.xts.m[[i]]$m_o_h,
                    start = start(r.xts.m[[i]]), 
                    end=end(r.xts.m[[i]]),frequency=12)
}


# rowbind data to get into dataframe
rMonthly <- rbind.fill(r.df.m)


# get rid of unnecessary temporary variables
rm(r.df.m, tempdata1, i)


# make new rows with "Omr", "Stn" from roer column
rMonthly["Omr_stn"] <- gsub("r","",rMonthly$roer[])
rMonthly[c("Omr","stn")] = matrix(unlist(strsplit(rMonthly$Omr_stn[],
                      split='_', fixed=TRUE)), nrow=nrow(rMonthly), byrow=T)

rMonthly[,c('Omr_stn','Omr','stn')] <- lapply(rMonthly[,c('Omr_stn',
                                                        'Omr','stn')], factor)

# 2.2    Aggregate to yearly max ----------------------------------------------

# Preallocate list for the new monthly aggregated xts
rXtsY <- vector("list", length(r))
rDfY <- vector("list", length(r))
rXts <- vector("list", length(r))

for(i in 1:length(r)){ 
  # make list items to xts objects
  rXts[[i]] <- xts(r[[i]]$m_o_h,as.Date(r[[i]]$Datum))
  colnames(r.xts[[i]]) <- 'm_o_h'
  
  # aggregated yearly values  
  rXtsY[[i]] <- apply.yearly(r.xts[[i]],max)
  
  # transform to data.frame that can be used for further analysis and plotting
  rDfY[[i]] <- data.frame(tid = index(rXtsY[[i]]), 
                            m_o_h = rXtsY[[i]]$m_o_h, roer = names(r[i]))

}

rYearly <- rbind.fill(rDfY)
rm(r.xts, i,rXtsY, rDfY)

rYearly["Omr_stn"] <- gsub("r","",rYearly$roer[])
rYearly[c("Omr","stn")] = matrix(unlist(strsplit(rYearly$Omr_stn[],
                                                  split='_', fixed=TRUE)), 
                                                  nrow=nrow(rYearly), byrow=T)

rYearly[,c('Omr_stn','Omr','stn')] <- lapply(rYearly[,c('Omr_stn',
                                                      'Omr','stn')], factor)



!##############################################################################

# 2.3    Summary table ---------------------------------------------------------
library("plyr")
library("xts")

# Create table with entries: names, length, start, end
rSumTable <- cbind(index(r),names(tempdata2),ldply(r.xts.m,length),
                   ldply(r.xts.m,start),ldply(r.xts.m,end))

colnames(rSumTable) <- c('Index','Omr_stn', 'Laengd', 'Start', 'End')


# Playing to only get values with a certain length
RoerL40 <- droplevels(rSumTable[which(rSumTable$Laengd>40),]$Omr_stn)
RoerL180 <- droplevels(rSumTable[which(rSumTable$Laengd>180),]$Omr_stn)
# 
# rSumTable.roer <- rSumTable$Roer[yu]
# 
attach(rSumTable)

rSumTableL40 <- rSumTable[which(Omr_stn %in% RoerL40),]
rSumTableL180 <- rSumTable[which(Omr_stn %in% RoerL180),]

detach(rSumTable)

ggplot(rSumTableL40, aes(x=Laengd/12))+
  geom_histogram(colour="grey90", fill="black") +
  xlab("År") + ylab("Antal") 
#   theme_minimal()

ggplot(rSumTableL180, aes(x=Laengd/12))+
  geom_bar(stat="bin", colour="grey90", fill="black") +
  xlab("År") + ylab("Antal") +
  geom_text(stat="bin", color="white", hjust=1, size=3,
            aes(y=..count.., label=..count..))





# Add column with Period length
rSumTableL40 <- ddply(rSumTableL40, .(Omr_stn), mutate, Period = End-Start)
rSumTableL180 <- ddply(rSumTableL180, .(Omr_stn), mutate, Period = End-Start)

#output to csv file for print (col.names=NA if row.names=T)
write.table(rSumTableL40,file="./data/sum_month_max.csv",sep=";",row.names=F)




write.table(m2r2, file="./data/SGUmeta.csv", sep="\t",row.names=F, quote=F)


# 3.     Compute Summary statistics add join with metadata -----------------------
# Compute variance for all objects in data.frame 


# TODO build in this: psych::describe(x)


# use mutate to add column to long format data frame
rMonthlySum <- ddply(rMonthly, .(Omr_stn), summarise, Variance = var(m_o_h))

rMonthlySum[c("Omr","stn")] = matrix(unlist(strsplit(as.character(rMonthlySum$Omr_stn),
                                                 split='_', fixed=TRUE)), 
                                 nrow=nrow(rMonthlySum), byrow=T)

rMonthlySum[,c('Omr_stn','Omr','stn')] <- lapply(rMonthlySum[,c('Omr_stn',
                                                        'Omr','stn')], factor)

rMonthlySum["Median"] <- ddply(rMonthly, .(Omr_stn), summarise, 
                               Med = median(m_o_h))$Med

rMonthlySum["Std"] <- ddply(rMonthly, .(Omr_stn), summarise, 
                            Std = sd(m_o_h))$Std

rMonthlySum["Count"] <- ddply(rMonthly, .(Omr_stn), summarise, 
                              Count = length(m_o_h))$Count

rMonthlySum["Average"] <- ddply(rMonthly, .(Omr_stn), summarise,
                              Average = mean(m_o_h))$Average

rMonthlySum["Median"] <- ddply(rMonthly, .(Omr_stn), summarise,
                                Median = median(m_o_h))$Median

rMonthlySum <- ddply(rMonthlySum, .(Omr_stn), 
                     transform, ConvEV = Average+Std*2.6)


head(rMonthlySum)

m <- merge(m,rMonthlySum, by="Omr_stn")


!##############################################################################


# 4.     Summary statistics of intresting metadata ------------------------------

library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape2)

# 4.1    Barchart for each Variable -------------------------------------------
m[m==""]  <- NA ; 

m$Geohylag <- revalue(m$Geohylag, c("I"="Inströmningsområde",
                                    "M"="Intermediärt område",
                                    "U"="Utströmingsområde",
                                    "V"="Vattendelarläge"))

# Create label for plot
m = ddply(m, .(Var2), transform, lab= value)


ak <- ggplot(m, aes(x=Akvifertyp, fill=Akvifertyp))  + 
        geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
        theme(legend.position="none") +
        ylab("Antal") +
        geom_text(stat="bin", color="black", hjust=1, size=3,
            aes(y=..count.., label=..count..))


gh <- ggplot(m, aes(x=Geohylag, fill=Geohylag))  + 
  geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),legend.position="none") +
  ylab("Antal")


ja <- ggplot(m, aes(x=Jordart, fill=Jordart))  + 
  geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
  theme(legend.position="none") +
  ylab("Antal")


ta <- ggplot(m, aes(x=Taeckt, fill=Taeckt))  + 
  geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
  theme(legend.position="none") +
  ylab("Antal") 


ba <- ggplot(m, aes(x=Bergart, fill=Bergart))  + 
  geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1),legend.position="none") +
  ylab("Antal")

ms <- ggplot(m, aes(x=MagasinStl, fill=MagasinStl))  + 
  geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
  theme(legend.position="none") +
  ylab("Antal")

source("./functions/multiplot.R")
multiplot(ak,gh,ja,ta,ba,ms, cols=3)

# 4.2    Stacked bar for each Variable ----------------------------------------

out <- by(data = mSumSum, INDICES = mSumSum$Var2, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(x=Var2, y=value, fill=var))  + 
    geom_bar(stat="identity") +
#     geom_text(aes(label = lab, y = pos), size = 3) +
    ylab("Antal") + xlab("") +
#     coord_polar() +
    guides(fill = guide_legend(reverse = TRUE)) 
})

do.call(grid.arrange, out)


# DEPRECATED
# mSumSum = ddply(mSumSum, .(Var2), transform, pos = cumsum(value) - 0.5*value)
# 
# mSumSum$var <- revalue(mSumSum$var, c("!NA's"="NA's"))
# 
# 
# 
# ggplot(mSumSum, aes(x=Var2, y=value)) + 
#   geom_bar(aes(fill=var)) +
# #   facet_wrap(~Var2, scales="free_x") +
#   geom_text(aes(label = lab, y = pos), size = 3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   # Rotate tick mark labels
#   theme(legend.position="none")+
#   coord_polar()
# #   guides(fill = guide_legend(reverse = TRUE)) 


!##############################################################################


# 5.     Skewness testing, heatmap, OLS ----------------------------------------
library(e1071)
library(ggplot2)

tempSkewN <- with(rMonthly, tapply(m_o_h, Omr_stn, function(x) skewness(x, type=2)))
xSkew <- data.frame(value=as.numeric(tempSkewN))
xSkew[c("Omr","stn")] = matrix(unlist(strsplit(names(tempSkewN), split='_', 
                                               fixed=TRUE)), nrow=nrow(tempSkewN), 
                               byrow=T)

# remove failed skewtests
xSkew = xSkew[complete.cases(xSkew),]

rescal <- function(jj) if (jj>0) jj=1 else jj=0

xSkew["resca"] <- factor(with(xSkew, tapply(value, rownames(xSkew), rescal)))

# plot heatmap
p <- ggplot(xSkew, aes(stn, Omr)) + 
  geom_tile(aes(fill = resca),colour = "white") + 
  scale_fill_gradient(low = "darkgreen",high = "red")

base_size <- 9
p + theme_minimal(base_size = base_size) + labs(x = "Station",y = "Område") + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position = "none",axis.ticks = element_blank(), 
        axis.text.x = element_text(size = base_size *0.8, 
                                   angle = 60, hjust = 0, colour = "grey40"))


# Join metadata and Skewtable

SkewMetaJoin <- merge(xSkew, tempmeta, by=c("Omr","stn"))
pairs(SkewMetaJoin)

# OLS analysis, factanal(), 
xSkew$Omr = sapply(xSkew$Omr, as.factor)
xSkew$stn = sapply(xSkew$stn, as.factor)


# some data exploration with pairs plots
panel.hist <- function(x, ...) {
  # Set user coordinates of plotting region
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  par(new=TRUE)
  # Do not start new plot
  hist(x, prob=TRUE, axes=FALSE, xlab="", ylab="",
       main="", col="lightgrey")
  lines(density(x, na.rm=TRUE))
  curve( dnorm(x, mean(log(x)), sd(log(x))), col='red', add=T)
  # Add density curve
}
# Create function to compute and print R^2
panel.r2 <- function(x, y, digits=2, cex.cor, ...) {
  # Set user coordinates of plotting region
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="complete.obs")**2 # Compute R^2
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  if(missing(cex.cor)) cex.cor <- 1/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)#*r for diff size
}

pairs(~resca+Akvifertyp+Geohylag, data=SkewMetaJoin,
      lower.panel=panel.smooth, upper.panel=panel.r2,
      diag.panel=panel.hist)

xSkew.LM(lm(formula = value ~ Omr + stn, data = xSkew))

factanal(~Omr+stn+resca)




# 6      Trend testing ---------------------------------------------------------

larger3 <- which(rSumTable$Laengd>3)

rTsLar3 <- vector("list", length(larger3))
names(rTsLar3) <- names(r[larger3])
rTsLar3 <- r.ts.m[larger3]

#TODO use a for loop instead
options(warn=-1)
rTrend <- do.call(rbind,lapply(rTsLar3,function(x) unlist(SeasonalMannKendall(x))))
options(warn=0)

rTrend <- data.frame(rTrend)

rTrendConf <- subset(ty, tau > 0.0025 | tau < -0.0025)

# tauPos <- subset(rTrend, tau > -0.0025 & tau >
tauNeg <- subset(rTrend, tau < 0.0025)



# 7.     Subsetting and plotting ----------------------------------------------

library(ggplot2)
library(plyr)

# 7.1    Subsetting after Omr -------------------------------------------------

# Monthly subset
attach(rMonthly)
x <- droplevels(rMonthly[ which(Omr==85),])
# x <- rMonthly[ which(Omr==11 & stn %in% c(1,10,100,102,12,124,125,126,127,128,129)),]
detach(rMonthly)

z = x

# Yearly subset
attach(rYearly)
y <- rYearly[ which(Omr==31 & stn %in% c(2)),]
detach(rYearly)

z = x

# plot regular lineplot
x.line <- ggplot(z,aes(x = tid,y = m_o_h, colour = stn)) + 
              geom_line(size=1) +
              geom_smooth(method="loess") +
              facet_wrap(~stn, scales="free_y") +
#             geom_point(colour="black",size=3.5,shape=21,fill="grey80") +
              geom_hline(yintercept=10.92, linetype=2) +
#               geom_text(x="1992-03-15", y=67.3, label="67,59") +
              ylab("m.ö.h.") + xlab("Tid") +
              theme_minimal()

ggsave(x.line, file="./img/line.png", dpi=600)

# plot density plot

x.dens <- ggplot(z,aes(x = m_o_h, colour = Omr_stn)) + 
          geom_density(alpha=0.55) +
          facet_wrap(~stn, scales="free") +
          xlab("m.ö.h. [m]") + ylab("frekvens") +
          theme_bw()
  



# plot ecdf plot
x2 <- ddply(z,.(stn),transform, ecd = ecdf(m_o_h)(m_o_h))

x.ecdf <- ggplot(x2,aes(x = m_o_h, y = ecd, colour = stn, fill = stn)) + 
          geom_step(alpha=0.55, size=1.1) +
          facet_wrap(~stn, scales="free_x") +
          theme_minimal()




# 7.2    Subsetting after Omr_stn Monthly -------------------------------------
attach(rMonthly)
rMonthly40 <- droplevels(rMonthly[which(Omr_stn %in% RoerL40),])
detach(rMonthly)

# x <- rMonthly[ which(Omr==13 & stn %in% c(101,102,103,107,2,3,4,5)),]
OmrFunct <- c(1,10,1400,15,17,1720,1730,18,20,21,2300,24,25,2510,31,
              35,41,44,5,51,52,53,56,57,59,6,60,64,65,67,69,7,71,75,
              8,85,86,90,93,94,96)


# Monthly subset
attach(rMonthly40)
x <- droplevels(rMonthly40[ which(Omr %in% OmrFunct ),])
detach(rMonthly40)
testdistfit(x)


source('./functions/testdistfit.R')

# 10.    Some distribution fitting ---------------------------------------------

library(fitdistrplus)
library(MASS)
library(VGAM)

# 10.1   Model two parameter distribution --------------------------------------
# Distributions left to fit:  "bimodal" "Pareto" "truncuated pareto"



# All distributions with bootstrap CI
# define number of bootstrap simulations
# nrit = 1001

## NORMAL DIST
fitNorm <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'norm'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitNorm <-fitNorm[fitNorm != "NULL"]
# bootNorm <- lapply(fitNorm,function(u) bootdist(u, bootmethod='nonparam',nrit))


## LOGNORMAL DIST
fitLnorm <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'lnorm'),error=function(e) NULL)))
# trim list to get rid of empty elements, left in the dataframe
fitLnorm <-fitLnorm[fitLnorm != "NULL"]
# bootLnorm <- lapply(fitLnorm,function(u) bootdist(u, bootmethod='nonparam',nrit))


## LOGISTIC DIST
fitLogis <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'logis'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitLogis <-fitLogis[fitLogis != "NULL"]
# bootLogis <- lapply(fitLogis,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GAMMA
fitGamma <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'gamma'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitGamma <-fitGamma[fitGamma != "NULL"]
# bootGamma <- lapply(fitGamma,function(u) bootdist(u, bootmethod='nonparam',nrit))


## WEIBULL DIST
fitWeibull <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'weibull'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitWeibull <-fitWeibull[fitWeibull != "NULL"]
# bootWeibull <- lapply(fitWeibull,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GUMBEL DIST
fitGumbel <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'gumbel', start = list(location = 0, scale = 1)),
           error=function(e) NULL)))
# trim list to get rid of empty elements, left in the dataframe
fitGumbel <-fitGumbel[fitGumbel != "NULL"]
# bootGumbel <- lapply(fitGumbel,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GEV DIST
fitGev <- with(x, tapply(m_o_h, Omr_stn, function(u) tryCatch(gev(u),error=function(e) NULL ))) 
# trim list to get rid of empty elements, left in the dataframe
fitGev <-fitGev[fitGev != "NULL"]



# 10.2    Fitting GEV Monthly--------------------------------------------------

GevFitsEst <- vector("list", length(fitGev))

for(i in 1:length(fitGev)){
    GevFitsEst[[i]] <- data.frame(ests=fitGev[[i]]$par.ests)
    GevFitsEst[[i]]["Omr_stn"] <-  names(fitGev[i])
    
}
GevFitsEstList <-GevFitsEst
GevFitsEst <- rbind.fill(GevFitsEst)
GevFitsEst["par"] <- rep(factor(c("shape","scale","location")),length(fitGev))

PlotGevFit <- ggplot(GevFitsEst[which(GevFitsEst$par=="shape"),], 
                     aes(x=ests, fill=par))+
                geom_bar(stat="bin", fill="black") +
                geom_vline(xintercept = 0, colour= "red", size=2,linetype = 2) +
                coord_flip() +
                xlab(expression(xi)) + ylab("antal") +
                geom_text(stat="bin", color="white", hjust=1, size=4,
                aes(y=..count.., label=..count..))




# 10.1.1 Sort Fits -------------------------------------------------------------

# preallocate list for fits
rFits <- vector("list", length(fitNorm))
# rBoots <- vector("list", length(fitNorm))

for(i in 1:length(fitNorm)){
  rFits[[i]] <- list(fitNorm[[i]],
#              fitCauchy[[i]],
             fitLnorm[[i]],
             fitLogis[[i]],
             fitGamma[[i]],
             fitWeibull[[i]],
             fitGumbel[[i]])
#   rBoots[[i]]<- list(bootNorm[[i]]$CI,
#                      bootCauchy[[i]]$CI,
#                      bootLnorm[[i]]$CI,
#                      bootLogis[[i]]$CI,
#                      bootGamma[[i]]$CI,
#                      bootWeibull[[i]]$CI,
#                      bootGumbel[[i]]$CI)
}

fitObjs <- length(fitGumbel)

rm(fitNorm,fitLnorm,fitLogis,fitGamma,fitWeibull,i,fitGumbel)
# rm(bootNorm, bootCauchy,bootLnorm, bootLogis,bootGamma,bootWeibull,bootGumbel)
# denscomp(rFits[[2]])

                  

# 10.1.2 Extract Estimates -----------------------------------------------------

library(plyr)

# preallocate list
rFitsEst <- vector("list", length(rFits))
xOmr_stns <- as.character(x$Omr_stn[!duplicated(x$Omr_stn)])

# get estimates in good form
for(i in 1:length(rFits)){
  tempFit <- rFits[[i]]
  rFitsEst[[i]] <- data.frame(t(data.frame(sapply(tempFit,"[[", "estimate"))))
  colnames(rFitsEst[[i]]) <- c("par1","par2")
  rFitsEst[[i]]["Omr_stn"] <- rep(xOmr_stns[i],nrow(rFitsEst[[i]]))
  rFitsEst[[i]]["FIT"] <- c("norm","lnorm","logis","gamma","weibull","gumbel")
}

rm(tempFit)

rFitsEst <- rbind.fill(rFitsEst)

# 10.1.3 Sort GOF --------------------------------------------------------------

library(fitdistrplus)
# preallocate list for gofs
rGofs <- vector("list", fitObjs)


for(i in 1:fitObjs){
    tempGof <- lapply(rFits[[i]], gofstat)
    temprGofs <- lapply(tempGof, function(x){
                        data.frame(x[c("cvm", "ad", "ks")])})
    rGofs[[i]] <- rbind.fill(temprGofs)
    names(rGofs[[i]]) <- c("CVM", "AD", "KS")
    rGofs[[i]]["Omr_stn"] <- rep(xOmr_stns[i],length(fitObjs))
    rGofs[[i]]["FIT"] <- c("norm","lnorm","logis","gamma","weibull","gumbel")
}
rm(tempGof,temprGofs)

# make list to data frame long
rGofs <- rbind.fill(rGofs)


# 10.2    Find best fitting distribution --------------------------------------

# find minmum in Anderson Darling test
minAD <- with(rGofs, tapply(AD, Omr_stn, min))
ADfits <- rGofs$FIT[which(rGofs$AD %in% minAD)]
ADfits <- data.frame(Omr_stn=xOmr_stns, FIT=ADfits)

# find minmum in Kolmogorov Smirnof test
minKS <- with(rGofs, tapply(KS, Omr_stn, min))
KSfits <- rGofs$FIT[which(rGofs$KS %in% minKS)]

# find minmum in Kolmogorov Cramer von-Mises test
minCVM <- with(rGofs, tapply(CVM, Omr_stn, min))
CVMfits <- rGofs$FIT[which(rGofs$CVM %in% minCVM)]

ADfitsEsts <- merge(rFitsEst,ADfits,by=c("Omr_stn","FIT"))

# 10.2.1 Plot cdf against ecdf -----------------------------------------------



ADfitsDist <- ADfitsEsts[which(ADfitsEsts$FIT=="norm"),]

Distfits <- droplevels(merge(ADfitsDist, rMonthly40, by="Omr_stn"))

source("./functions/cdf_comp.R")
DistCDfs <- with(Distfits, tapply(m_o_h, Omr_stn, function(u) cdf_comp(u,'weibull'))) 

lapply(names(DistCDfs), function(x)ggsave(filename=paste("./img/norm_CDF_",x,".png",sep=""), 
                                            plot=DistCDfs[[x]], dpi=300))

source("./functions/hist_with_density.R")
multiplot(CauchyCDfs, cols=4)



# 10.3    Get return levels of best fits monthly ------------------------------

# 10.3.1  Get return levels of fits monthly non GEV ---------------------------

kvantil <- c(0.98,0.99,0.995)
retLevMon <- vector("list",fitObjs)

# loop to get return values
for(i in 1:fitObjs){
  if(ADfitsEsts$FIT[[i]] == "norm"){
    retLevMon[[i]] <- data.frame(qnorm(kvantil, 
                                ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
#   if(ADfitsEsts$FIT[[i]] == "cauchy"){
#     retLevMon[[i]] <- data.frame(qcauchy(kvantil,
#                                 ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
#   } 
  if(ADfitsEsts$FIT[[i]] == "lnorm"){
    retLevMon[[i]] <- data.frame(qlnorm(kvantil,
                                ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "logis")
  {
    retLevMon[[i]] <- data.frame(qlogis(kvantil,
                                ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "gamma"){
    retLevMon[[i]] <- data.frame(qgamma(kvantil, 
                                ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "weibull"){
    retLevMon[[i]] <- data.frame(qweibull(kvantil,
                      ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "gumbel"){
    retLevMon[[i]] <- data.frame(qgumbel(kvantil,
                                          ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  # make data readable
  colnames(retLevMon[[i]]) <- c("ReturnLevel")
  retLevMon[[i]]["ReturnPeriod"] <- c("50","100","200")
  retLevMon[[i]]["Omr_stn"] <- ADfitsEsts$Omr_stn[i]
  retLevMon[[i]]["FIT"] <- ADfitsEsts$FIT[i]
}

retLevMon <- rbind.fill(retLevMon)
retLevMon$ReturnPeriod <- factor(retLevMon$ReturnPeriod)
retLevMon$Omr_stn <- factor(retLevMon$Omr_stn)
retLevMon$FIT <- factor(retLevMon$FIT)

# 10.3.2  Get return levels of fits monthly GEV ------------------------------

retLevMonGEV <- vector("list",length(GevFitsEstList))

# loop to get return values
for(i in 1:length(GevFitsEstList)){
    retLevMonGEV[[i]] <- data.frame(qgev(kvantil, GevFitsEstList[[i]]$ests[1],
                                         GevFitsEstList[[i]]$ests[3],
                                         GevFitsEstList[[i]]$ests[2]))
    colnames(retLevMonGEV[[i]]) <- c("ReturnLevel")
    retLevMonGEV[[i]]["ReturnPeriod"] <- c("50","100","200")
    retLevMonGEV[[i]]["Omr_stn"] <- GevFitsEstList[[i]]$Omr_stn

}

retLevMonGEV <- rbind.fill(retLevMonGEV)



# Difference between GEV and regular distributions

retLevMonDiff <- retLevMon[which(retLevMon$Omr_stn %in% retLevMonGEV$Omr_stn),]
retLevMonDiff["ReturnLevelGEV"] <- retLevMonGEV$ReturnLevel

retLevMonDiff <- ddply(retLevMonDiff, .(ReturnPeriod), 
                       mutate, diff=ReturnLevel-ReturnLevelGEV)
dd = -3
retLevMonDiff[which(retLevMonDiff$diff < dd),]

retLevMonDiffss <- retLevMonDiff[which(retLevMonDiff$ReturnPeriod == 100 & retLevMonDiff$diff > dd),]

ggplot(retLevMonDiffss, aes(x=Omr_stn, y=diff, colour=FIT)) +
  geom_point() +
  geom_hline(yintercept=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Differens [m]")

diffFitM <- ggplot(retLevMonDiffss, aes(x=FIT, y=diff, colour=FIT)) +
  geom_jitter() +
#   coord_flip() +
#   geom_hline(yintercept=0)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Differens FIT-GEV [m]") + xlab("Anpassning") +
  ggtitle("Månadsmax")



# 10.4    Get return levels of best fits monthly/12 ------------------------------

kvantil <- c(0.998333,0.999167,0.9995455)

retLevMon2 <- vector("list",fitObjs)

# loop to get return values
for(i in 1:fitObjs){
  if(ADfitsEsts$FIT[[i]] == "norm"){
    retLevMon2[[i]] <- data.frame(qnorm(kvantil, 
                                       ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  #   if(ADfitsEsts$FIT[[i]] == "cauchy"){
  #     retLevMon[[i]] <- data.frame(qcauchy(kvantil,
  #                                 ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  #   } 
  if(ADfitsEsts$FIT[[i]] == "lnorm"){
    retLevMon2[[i]] <- data.frame(qlnorm(kvantil,
                                        ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "logis")
  {
    retLevMon2[[i]] <- data.frame(qlogis(kvantil,
                                        ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "gamma"){
    retLevMon2[[i]] <- data.frame(qgamma(kvantil, 
                                        ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "weibull"){
    retLevMon2[[i]] <- data.frame(qweibull(kvantil,
                                          ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "gumbel"){
    retLevMon2[[i]] <- data.frame(qgumbel(kvantil,
                                         ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  # make data readable
  colnames(retLevMon2[[i]]) <- c("ReturnLevel")
  retLevMon2[[i]]["ReturnPeriod"] <- c("50","100","200")
  retLevMon2[[i]]["Omr_stn"] <- ADfitsEsts$Omr_stn[i]
  retLevMon2[[i]]["FIT"] <- ADfitsEsts$FIT[i]
}

retLevMon2 <- rbind.fill(retLevMon2)
retLevMon2$ReturnPeriod <- factor(retLevMon2$ReturnPeriod)
retLevMon2$Omr_stn <- factor(retLevMon2$Omr_stn)
retLevMon2$FIT <- factor(retLevMon2$FIT)



# 11.   Regression Analysis Monthly -------------------------------------------

library(ggplot2)
library(plyr)

attach(retLevMon)
names(retLevMon)<- c("ReturnLevel","ReturnPeriod","Omr_stn","FIT")
detach(retLevMon)

attach(rMonthlySum)
rMonthlySum40 <- droplevels(rMonthlySum[which(Omr_stn %in% RoerL40),])
detach(rMonthlySum)
attach(rMonthlySum40)
regMonthlySum <- droplevels(rMonthlySum40[ which(Omr %in% OmrFunct),])
detach(rMonthlySum40)


# 11.1    Statistics on fitted data -------------------------------------------


# 11.1.1  Plot Count for each distribution ------------------------------------
retLevMonCount <- droplevels(retLevMon[which(retLevMon$ReturnPeriod==100),])

ggplot(retLevMonCount, aes(x=FIT, fill=FIT))+
  geom_bar(stat="bin") +
  ylab("Antal") + xlab("Fördelning") +
  theme_minimal() +
  theme(legend.position="none") +
  coord_flip() +
  scale_x_discrete(limits=c("gamma","lnorm","gumbel","norm","logis","weibull")) +
  geom_text(stat="bin", color="black", hjust=1, size=4,
            aes(y=..count.., label=..count..))


# 11.1.2  Plot Correlation between intervall and distribution -----------------

rSumCorr <- join(rSumTableL40, ADfits, by="Omr_stn",type = "right")
rSumCorr$Period <- as.numeric(rSumCorr$Period)



x.corrFoerdel <- ggplot(rSumCorr, aes(x=Period, y=FIT, colour=FIT))+
                    geom_boxplot() + 
                    geom_jitter(alpha=.4, width=10) +
                    xlab("Dagar mätperiod") + ylab("Fördelning") +
                    theme_minimal() +
                    theme(legend.position="none") +
#                     coord_flip() +
                    scale_y_discrete(limits=c("gamma","lnorm","gumbel",
                                              "norm","logis","weibull"))

ggsave(x.corrFoerdel, file="./img/maetper-foerdel.png", dpi=600)




# 11.2  Compute Kest ----------------------------------------------------------





regMonth <- data.frame(Med=rep(regMonthlySum$Median,each=3),
                       Avg=rep(regMonthlySum$Average,each=3),
                       Std=rep(regMonthlySum$Std, each=3), 
                       Återkomstid=retLevMon$ReturnPeriod,
                       Åter = retLevMon$ReturnLevel,
                       Omr_stn = retLevMon$Omr_stn,
                       FIT = retLevMon$FIT)



# attach a column with k est
regMonth <- ddply(regMonth, .(Återkomstid), mutate, kest_med=(Åter-Med)/Std)
regMonth <- ddply(regMonth, .(Återkomstid), mutate, kest_avg=(Åter-Avg)/Std)


# Compute summary of kest_median
kestSum <- ddply(regMonth, .(Återkomstid), summarise, Std_med=sd(kest_med))
kestSum["Mean_med"] <- ddply(regMonth, .(Återkomstid), 
                                 summarise, Mean_med=mean(kest_med))$Mean_med
kestSum["Median_med"] <- ddply(regMonth, .(Återkomstid), 
                                 summarise, Median_med=median(kest_med))$Median_med

# Compute summary of kest_avg
kestSum["Std_avg"] <- ddply(regMonth, .(Återkomstid), 
                                 summarise, Std_avg=mean(kest_avg))$Std_avg
kestSum["Mean_avg"] <- ddply(regMonth, .(Återkomstid), 
                                 summarise, Mean_avg=mean(kest_avg))$Mean_avg
kestSum["Median_avg"] <- ddply(regMonth, .(Återkomstid), 
                                 summarise, Median_avg=median(kest_avg))$Median_avg



kestL4 <- (regMonthMed[which(regMonthMed$kest>6),])

# k_est plotted(median) ~ return period and distribution
ggplot(regMonth, aes(x=Återkomstid, y=kest_med)) +
  geom_jitter(alpha=.55,aes(fill=FIT,colour=FIT)) + 
  geom_boxplot(alpha=.55, aes(fill=FIT)) +
  ylab(expression(k[est])) +
  scale_x_discrete(limits=c("50","100","200"))


# k_est plotted(average) ~ return period and distribution
ggplot(regMonth, aes(x=Återkomstid, y=kest_avg)) +
  geom_jitter(alpha=.55,aes(fill=FIT,colour=FIT)) + 
  geom_boxplot(alpha=.55, aes(fill=FIT)) +
  ylab(expression(k[est])) +
  scale_x_discrete(limits=c("50","100","200"))


ggplot(regMonth, aes(x=Återkomstid, y=kest)) +
  geom_jitter(alpha=.55,aes(colour=FIT)) + 
  geom_boxplot(alpha=.55, aes(fill=Återkomstid)) +
  ylab(expression(k[est])) +
  scale_x_discrete(limits=c("50","100","200"))

# TODO: variance, average vs median


# 11.3  Combine Regression in plot ---------------------------------------------

attach(rMonthlySum)
rMonthlySum40 <- rMonthlySum[which(Omr_stn %in% RoerL40),]
regMonth <- rMonthlySum40[ which(Omr==13 & stn %in% c(101,102,103,2,3,4,5)),]
detach(rMonthlySum)

regMonth <- data.frame(Avg=rep(regMonth$Average,each=3),
                       Std=regMonth$Std, Återkomstid=retLevMon$ReturnPeriod,
                       Åter = retLevMon$ReturnLevel,
                       Omr_stn=retLevMon$Omr_stn)


# attach a column with k est
regMonth <- ddply(regMonth, .(Återkomstid), mutate, kest=(Åter-Avg)/Std)


ggplot(regMonth, aes(x=Återkomstid, y=kest, colour=Återkomstid)) +
        geom_jitter() + geom_boxplot(alpha=.7) +
  scale_x_discrete(limits=c("50","100","200"))
  

ggplot(regMonth, aes(x=kest)) +
  geom_point() 










regPlot <- ggplot(regMonth, aes(x=indVar, y=depVar, colour=Återkomstid)) + 
              geom_point(size=4) + 
  #           stat_smooth(method=lm, level=0.95) +
              stat_smooth(method=lm, se=F, fullrange=F) +
              xlab("Std(data)") + ylab("BestFit(data) - Mean(data)")


# 5.     Multiple correspondence Analysis ---------------------------------------

library(FactoMineR)
# Create factor table ready 
mSum <- m[, c(1,8:16)]

mSum <- merge(mSum, retLevMonCount, by="Omr_stn")
drops <- c("Omr_stn","ReturnLevel","ReturnPeriod")
mSum <- droplevels(mSum[,!names(mSum) %in% drops])

droplevels(mSum[which(mSum$FIT==cauchy),])

mSumCA <- CA(mSum)
mSumMCA <- MCA(mSum)
par(bty='n')
plot(mSumMCA, cex=0.7, selectMod="contrib 20", unselect="grey60", select="contrib 10")
plot(mSumMCA, choix="var")
HCPC(mSumMCA)

plot(mca(mSum, nf=2, abbrev=T),rows=F)

mSum1 <- mSum[complete.cases(mSum),]

catdes

# 12     Comparison etc. ------------------------------------------------------

# Select only metadata of stations that has been fitted
compDistMeta <- droplevels(m[which(m$Omr_stn %in% retLevMonCount$Omr_stn),])
# Select only fitted data that has metadata
compDistMeta <- droplevels(m[which(retLevMonCount$Omr_stn %in% m$Omr_stn),])
retLevMonCountX <-droplevels(retLevMonCount[which(retLevMonCount$Omr_stn %in% m$Omr_stn),])

compDistMeta["FIT"] <- retLevMonCountX$FIT
# comp_ok <- comp[which(comp$FIT == comp$FITY),]

comp.ak<- ggplot(compDistMeta, aes(x=FIT, fill=Akvifertyp))+
              geom_bar(stat="bin") +
              coord_polar()

comp.gh<- ggplot(compDistMeta, aes(x=FIT, fill=Geohylag))+
  geom_bar(stat="bin") +
  coord_polar()


comp.ja<- ggplot(compDistMeta, aes(x=FIT, fill=Jordart))+
  geom_bar(stat="bin") +
  coord_polar()


comp.ta<- ggplot(compDistMeta, aes(x=FIT, fill=Taeckt))+
  geom_bar(stat="bin") +
  coord_polar()


comp.ba<- ggplot(compDistMeta, aes(x=FIT, fill=Bergart))+
  geom_bar(stat="bin") +
  coord_polar()


comp.ms<- ggplot(compDistMeta, aes(x=FIT, fill=MagasinStl))+
  geom_bar(stat="bin") +
  coord_polar()

library(gridExtra)
source("./functions/multiplot.R")
multiplot(comp.ak,comp.gh,comp.ja,comp.ta,comp.ba,comp.ms, cols=3)

# YEARLY OPERATIONS -----------------------------------------------------------


# 1    Subsetting after Omr_stn Yearly --------------------------------------
attach(rYearly)
rYearly180 <- droplevels(rYearly[which(Omr_stn %in% RoerL180),])
detach(rYearly)

# x <- rMonthly[ which(Omr==13 & stn %in% c(101,102,103,107,2,3,4,5)),]
OmrFunct <- c(1,10,1400,15,17,1720,1730,18,20,21,2300,24,25,2510,31,
              35,41,44,5,51,52,53,56,57,59,6,60,64,65,67,69,7,71,75,
              8,85,86,90,93,94,96)


# Monthly subset
attach(rYearly180)
x <- droplevels(rYearly180[ which(Omr %in% OmrFunct ),])
detach(rYearly180)

# 2    Model two parameter distribution --------------------------------------
# Distributions left to fit:  "bimodal" "Pareto" "truncuated pareto"

x <- droplevels(x[which(x$Omr_stn %in% names(fitLnorm)),])


## NORMAL DIST
fitNorm <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'norm'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitNorm <-fitNorm[fitNorm != "NULL"]
# bootNorm <- lapply(fitNorm,function(u) bootdist(u, bootmethod='nonparam',nrit))


## LOGNORMAL DIST
fitLnorm <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'lnorm'),error=function(e) NULL)))
# trim list to get rid of empty elements, left in the dataframe
fitLnorm <-fitLnorm[fitLnorm != "NULL"]
# bootLnorm <- lapply(fitLnorm,function(u) bootdist(u, bootmethod='nonparam',nrit))


## LOGISTIC DIST
fitLogis <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'logis'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitLogis <-fitLogis[fitLogis != "NULL"]
# bootLogis <- lapply(fitLogis,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GAMMA
fitGamma <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'gamma'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitGamma <-fitGamma[fitGamma != "NULL"]
# bootGamma <- lapply(fitGamma,function(u) bootdist(u, bootmethod='nonparam',nrit))


## WEIBULL DIST
fitWeibull <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'weibull'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitWeibull <-fitWeibull[fitWeibull != "NULL"]
# bootWeibull <- lapply(fitWeibull,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GUMBEL DIST
fitGumbel <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'gumbel', start = list(location = 0, scale = 1)),
           error=function(e) NULL)))
# trim list to get rid of empty elements, left in the dataframe
fitGumbel <-fitGumbel[fitGumbel != "NULL"]
# bootGumbel <- lapply(fitGumbel,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GEV DIST
fitGev <- with(x, tapply(m_o_h, Omr_stn, function(u) tryCatch(gev(u),error=function(e) NULL ))) 
# trim list to get rid of empty elements, left in the dataframe
fitGev <-fitGev[fitGev != "NULL"]
# All distributions with bootstrap CI
# define number of bootstrap simulations
# nrit = 1001


# 3    Fitting GEV Yearly --------------------------------------------------

GevFitsEstY <- vector("list", length(fitGev))

for(i in 1:length(fitGev)){
  GevFitsEstY[[i]] <- data.frame(ests=fitGev[[i]]$par.ests)
  GevFitsEstY[[i]]["Omr_stn"] <-  names(fitGev[i])
  
}
GevFitsEstListY <-GevFitsEstY
GevFitsEstY <- rbind.fill(GevFitsEstY)
GevFitsEstY["par"] <- rep(factor(c("shape","scale","location")),length(fitGev))

PlotGevFitY <- ggplot(GevFitsEstY[which(GevFitsEstY$par=="shape"),], 
                      aes(x=ests, fill=par)) +
  geom_bar(stat="bin", fill="black") +
  geom_vline(xintercept = 0, colour= "red", size=2,linetype = 2) +
  coord_flip() +
  xlab(expression(xi)) + ylab("antal") +
  geom_text(stat="bin", color="white", hjust=1, size=2,
            aes(y=..count.., label=..count..))

# 4    Sort Fits -------------------------------------------------------------

# preallocate list for fits
rFitsY <- vector("list", length(fitNorm))
# rBootsY <- vector("list", length(fitNorm))

for(i in 1:length(fitNorm)){
  rFitsY[[i]] <- list(fitNorm[[i]],
                     #              fitCauchy[[i]],
                     fitLnorm[[i]],
                     fitLogis[[i]],
                     fitGamma[[i]],
                     fitWeibull[[i]],
                     fitGumbel[[i]])
  #   rBoots[[i]]<- list(bootNorm[[i]]$CI,
  #                      bootCauchy[[i]]$CI,
  #                      bootLnorm[[i]]$CI,
  #                      bootLogis[[i]]$CI,
  #                      bootGamma[[i]]$CI,
  #                      bootWeibull[[i]]$CI,
  #                      bootGumbel[[i]]$CI)
}

fitObjsY <- length(fitNorm)

rm(fitNorm,fitLnorm,fitLogis,fitGamma,fitWeibull,i,fitGumbel)
# rm(bootNorm, bootCauchy,bootLnorm, bootLogis,bootGamma,bootWeibull,bootGumbel)
# denscomp(rFits[[2]])

# 5    Extract Estimates -----------------------------------------------------

library(plyr)

# preallocate list
rFitsEstY <- vector("list", length(rFitsY))
xOmr_stns <- as.character(x$Omr_stn[!duplicated(x$Omr_stn)])

# get estimates in good form
for(i in 1:length(rFitsY)){
  tempFit <- rFitsY[[i]]
  rFitsEstY[[i]] <- data.frame(t(data.frame(sapply(tempFit,"[[", "estimate"))))
  colnames(rFitsEstY[[i]]) <- c("par1","par2")
  rFitsEstY[[i]]["Omr_stn"] <- rep(xOmr_stns[i],nrow(rFitsEstY[[i]]))
  rFitsEstY[[i]]["FIT"] <- c("norm","lnorm","logis","gamma","weibull","gumbel")
}

rm(tempFit)

rFitsEstY <- rbind.fill(rFitsEstY)

# 6    Sort GOF --------------------------------------------------------------

library(fitdistrplus)
# preallocate list for gofs
rGofsY <- vector("list", fitObjsY)


for(i in 1:fitObjs){
  tempGof <- lapply(rFits[[i]], gofstat)
  temprGofs <- lapply(tempGof, function(x){
    data.frame(x[c("cvm", "ad", "ks")])})
  rGofsY[[i]] <- rbind.fill(temprGofs)
  names(rGofsY[[i]]) <- c("CVM", "AD", "KS")
  rGofsY[[i]]["Omr_stn"] <- rep(xOmr_stns[i],length(fitObjsY))
  rGofsY[[i]]["FIT"] <- c("norm","lnorm","logis","gamma","weibull","gumbel")
}
rm(tempGof,temprGofs)

# make list to data frame long
rGofsY <- rbind.fill(rGofsY)

# 7    Find best fitting distribution --------------------------------------

# find minmum in Anderson Darling test
minADY <- with(rGofsY, tapply(AD, Omr_stn, min))
ADfitsY <- rGofsY$FIT[which(rGofsY$AD %in% minADY)]
ADfitsY <- data.frame(Omr_stn=xOmr_stns, FIT=ADfitsY)

# find minmum in Kolmogorov Smirnof test
minKS <- with(rGofs, tapply(KS, Omr_stn, min))
KSfits <- rGofs$FIT[which(rGofs$KS %in% minKS)]

# find minmum in Kolmogorov Cramer von-Mises test
minCVM <- with(rGofs, tapply(CVM, Omr_stn, min))
CVMfits <- rGofs$FIT[which(rGofs$CVM %in% minCVM)]

ADfitsEstsY <- merge(rFitsEstY,ADfitsY,by=c("Omr_stn","FIT"))

# 8    Get return levels of fits yearly non GEV ---------------------------

kvantil <- c(0.98,0.99,0.995)
retLevYear <- vector("list",fitObjsY)

# loop to get return values
for(i in 1:fitObjs){
  if(ADfitsEstsY$FIT[[i]] == "norm"){
    retLevYear[[i]] <- data.frame(qnorm(kvantil, 
                                        ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  }  
  if(ADfitsEstsY$FIT[[i]] == "lnorm"){
    retLevYear[[i]] <- data.frame(qlnorm(kvantil,
                                         ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  } 
  if(ADfitsEstsY$FIT[[i]] == "logis")
  {
    retLevYear[[i]] <- data.frame(qlogis(kvantil,
                                         ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  } 
  if(ADfitsEstsY$FIT[[i]] == "gamma"){
    retLevYear[[i]] <- data.frame(qgamma(kvantil, 
                                         ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  } 
  if(ADfitsEstsY$FIT[[i]] == "weibull"){
    retLevYear[[i]] <- data.frame(qweibull(kvantil,
                                           ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  } 
  if(ADfitsEstsY$FIT[[i]] == "gumbel"){
    retLevYear[[i]] <- data.frame(qgumbel(kvantil,
                                          ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  } 
  # make data readable
  colnames(retLevYear[[i]]) <- c("ReturnLevel")
  retLevYear[[i]]["ReturnPeriod"] <- c("50","100","200")
  retLevYear[[i]]["Omr_stn"] <- ADfitsEstsY$Omr_stn[i]
  retLevYear[[i]]["FIT"] <- ADfitsEstsY$FIT[i]
}

retLevYear <- rbind.fill(retLevYear)

retLevYear <- rbind.fill(retLevYear)
retLevYear$ReturnPeriod <- factor(retLevYear$ReturnPeriod)
retLevYear$Omr_stn <- factor(retLevYear$Omr_stn)
retLevYear$FIT <- factor(retLevYear$FIT)

# 9    Get return levels of fits yearly GEV ------------------------------

retLevYearGEV <- vector("list",length(GevFitsEstListY))

# loop to get return values
for(i in 1:length(GevFitsEstListY)){
  retLevYearGEV[[i]] <- data.frame(qgev(kvantil, GevFitsEstListY[[i]]$ests[1],
                                       GevFitsEstListY[[i]]$ests[3],
                                       GevFitsEstListY[[i]]$ests[2]))
  colnames(retLevYearGEV[[i]]) <- c("ReturnLevel")
  retLevYearGEV[[i]]["ReturnPeriod"] <- c("50","100","200")
  retLevYearGEV[[i]]["Omr_stn"] <- GevFitsEstListY[[i]]$Omr_stn
  
}

retLevYearGEV <- rbind.fill(retLevYearGEV)



# Difference between GEV and regular distributions

retLevYearDiff <- retLevYear[which(retLevYear$Omr_stn %in% retLevYearGEV$Omr_stn),]
retLevYearDiff["ReturnLevelGEV"] <- retLevYearGEV$ReturnLevel

retLevYearDiff <- ddply(retLevYearDiff, .(ReturnPeriod), 
                       mutate, diff=ReturnLevel-ReturnLevelGEV)
dd = -6
retLevYearDiff[which(retLevYearDiff$diff < dd),]

retLevYearDiffss <- retLevYearDiff[which(retLevYearDiff$ReturnPeriod == 100 & retLevYearDiff$diff > dd),]
#

ggplot(retLevYearDiffss, aes(x=Omr_stn, y=diff, colour=FIT)) +
  geom_point() +
  geom_hline(yintercept=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Differens [m]")

ggplot(retLevYearDiffss, aes(x=FIT, y=diff, colour=FIT)) +
  geom_jitter() +
  #   coord_flip() +
  #   geom_hline(yintercept=0)+
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Differens FIT-GEV [m]") + xlab("Anpassning") +
  ggtitle("Årsmax")

# 10   Plot Count for each distribution ------------------------------------
retLevYearCount <- retLevYear[which(retLevYear$ReturnPeriod==100),]

ggplot(retLevYearCount, aes(x=FIT, fill=FIT))+
  geom_bar(stat="bin") +
  ylab("Antal") + xlab("Fördelning") +
  theme_minimal() +
  theme(legend.position="none") +
  coord_flip() +
  scale_x_discrete(limits=c("gamma","lnorm","gumbel","norm","logis","weibull")) +
  geom_text(stat="bin", color="black", hjust=1, size=4,
            aes(y=..count.., label=..count..))

# 11   Plot Correlation between intervall and distribution -----------------

rSumCorrY <- join(rSumTableL180, ADfitsY, by="Omr_stn",type = "right")
rSumCorrY$Period <- as.numeric(rSumCorrY$Period)



x.corrFoerdelY <- ggplot(rSumCorrY, aes(x=Period, y=FIT, colour=FIT))+
                  geom_boxplot() + 
                    geom_jitter(alpha=.4, width=10) +
                    xlab("Dagar mätperiod") + ylab("Fördelning") +
                    theme_minimal() +
                    theme(legend.position="none") 
#                     coord_flip() +
                    scale_x_discrete(limits=c("gamma","lnorm","gumbel",
                                              "norm","logis","weibull"))

ggsave(x.corrFoerdel, file="./img/maetper-foerdel.png", dpi=600)


# TEST YEARLY VS MONTHLY ------------------------------------------------------

comp <- droplevels(retLevMonCount[which(retLevMonCount$Omr_stn %in% retLevYearCount$Omr_stn),])

comp["FITY"] <- retLevYearCount$FIT
comp_ok <- comp[which(comp$FIT == comp$FITY),]
=======
# SGU Data import/analysis

## Find all options in part Series 10
# change according to data
# setwd("~/RProjects/exjobb")
xRaw <- file.path("./data/grvniva2.txt")
xMeta <- file.path("./data/grvn_stninfo_nivaaktiv.txt")
xMeta2 <- file.path("./data/all_metadata_nivastn.txt")

# 1.    Get metadata in good form ------------------------------------------------

# 1.1   Load metadata active stations --------------------------------------------
metadata <- read.delim(xMeta, encoding="ISO-8859-1")

# check if enough data available in TACKT
m <- data.frame(Omr_stn = metadata$ID,
  Omr = metadata$OMR,
                stn = metadata$STN,
                       Lnr = factor(metadata$LNR),
                       Northing = as.numeric(metadata$N),
                       Easting = as.numeric(metadata$E),
                       Top = factor(metadata$TOP),
                       Akvifertyp = factor(metadata$AKVIFER),
                       Geohylag = factor(metadata$GEOHYLAG),
                       Jordart = factor(metadata$JORDART),
                       Taeckt = factor(metadata$TACKT),
                       Bergart = factor(metadata$BERGART),
                       MagasinStl = factor(metadata$MAGSTLK))


# m<- m[with(m, order(Omr, stn)), ]
# 
# is.na(m)[NAs] <- TRUE

# TODO
# map other names for easier understanding: mapvalues(tempmeta$Akvifertyp, c())
# rm(tempmeta)
write.table(m,file="./data/SGUmeta_active.csv",sep="\t",row.names=F, quote=F)

# 1.2   Load metadata all stations -----------------------------------------------

metadata2 <- read.delim(xMeta2, encoding="ISO-8859-1")

# check if enough data available in TACKT
m2 <- data.frame(Omr_stn = metadata2$ID,
                Omr = metadata2$OMR,
                stn = metadata2$STN,
                Lnr = factor(metadata2$LNR),
                Northing = as.numeric(metadata2$N),
                Easting = as.numeric(metadata2$E),
                Top = factor(metadata2$TOP),
                Akvifertyp = factor(metadata2$AKVIFER),
                Geohylag = factor(metadata2$GEOHYLAG),
                Jordart = factor(metadata2$JORDART),
                Taeckt = factor(metadata2$TACKT),
                Bergart = factor(metadata2$BERGART),
                MagasinStl = factor(metadata2$MAGSTLK))

library("plyr")
# m2$Akvifertyp <- revalue(m2$Akvifertyp, c("J?"="JÖ", "B?"="BÖ","K?"="KÖ"))
m2$MagasinStl <- revalue(m2$MagasinStl, c("L"="Liten", "S"="Stor"))

setequal(levels(m2$Omr_stn), levels(m$Omr_stn))

m <- rbind(m,m2)
m<- m[!duplicated(m$Omr_stn), ]

# 1.3    Remaping of metadata -------------------------------------------------
library("plyr")
m$Akvifertyp <- revalue(m$Akvifertyp, c("J?"="JÖ", "B?"="BÖ","K?"="KÖ"))

m$Geohylag <- revalue(m$Geohylag, c("I"="Inströmningsområde",
                                    "M"="Intermediärt område",
                                    "U"="Utströmingsområde",
                                    "V"="Vattendelarläge"))

m$MagasinStl <- revalue(m$MagasinStl, c("L"="Liten", "S"="Stor"))

!##############################################################################

# 1.4    Compute which rows are missing ----------------------------------------

m2 <- data.frame(Omr_stn = m$Omr_stn)

r2 <- rMonthly$Omr_stn[!duplicated(rMonthly$Omr_stn)]

m2r2 <- setdiff(levels(r2), levels(m2$Omr_stn))

# Sort dataframe after columns
#m2r2 <- m2r2[with(m2r2, order(Omr, stn)), ]



# 2.     Get rawdata in good form ---------------------------------------------
library("xts")
library("plyr")


## load rawdata
rawdata <- read.table(xRaw, header=TRUE,strip.white=TRUE
                ,colClasses=c(rep("factor",3),"Date","NULL","numeric","NULL"))

# split wells to list object for aggregation purposes
r <- split(rawdata,rawdata$Omr_stn)
tempdata1 <- lapply(r, function(y) cbind(rnam=row.names(y), y))
tempdata2 <- lapply(r, function(y) cbind(rnam=row.names(y), y))
names(r) <- paste('r', names(tempdata1[]),sep="")


# Preallocate list for the new xts list
r.xts <- vector("list", length(r))
names(r.xts) <- names(r)

# Preallocate list for the new monthly aggregated xts
r.xts.m <- vector("list", length(r))
r.df.m <- vector("list", length(r))

# Preallocate list for the new monthly aggregated regular ts
r.ts.m <- vector("list", length(r))
names(r.ts.m) <- names(r)

# 2.1    Aggregate to monthly max ---------------------------------------------

for(i in 1:length(r)){ 
  # make list items to xts objects
  r.xts[[i]] <- xts(r[[i]]$m_o_h,as.Date(r[[i]]$Datum))
  colnames(r.xts[[i]]) <- 'm_o_h'
  
  # aggregated monthly values  
  r.xts.m[[i]] <- apply.monthly(r.xts[[i]],max)
  
  # transform to data.frame that can be used for further analysis and plotting
  r.df.m[[i]] <- data.frame(tid = index(r.xts.m[[i]]), 
                            m_o_h = r.xts.m[[i]]$m_o_h, roer = names(r[i]))
  
  # transform to regular ts, currently not working
  r.ts.m[[i]] <- ts(r.xts.m[[i]]$m_o_h,
                    start = start(r.xts.m[[i]]), 
                    end=end(r.xts.m[[i]]),frequency=12)
  }


# rowbind data to get into dataframe
rMonthly <- rbind.fill(r.df.m)


# get rid of unnecessary temporary variables
rm(r.df.m, tempdata1, i)


# make new rows with "Omr", "Stn" from roer column
rMonthly["Omr_stn"] <- gsub("r","",rMonthly$roer[])
rMonthly[c("Omr","stn")] = matrix(unlist(strsplit(rMonthly$Omr_stn[],
                      split='_', fixed=TRUE)), nrow=nrow(rMonthly), byrow=T)

rMonthly[,c('Omr_stn','Omr','stn')] <- lapply(rMonthly[,c('Omr_stn',
                                                        'Omr','stn')], factor)

# 2.2    Aggregate to yearly max ----------------------------------------------

# Preallocate list for the new monthly aggregated xts
rXtsY <- vector("list", length(r))
rDfY <- vector("list", length(r))
rXts <- vector("list", length(r))

for(i in 1:length(r)){ 
  # make list items to xts objects
  rXts[[i]] <- xts(r[[i]]$m_o_h,as.Date(r[[i]]$Datum))
  colnames(r.xts[[i]]) <- 'm_o_h'
  
  # aggregated yearly values  
  rXtsY[[i]] <- apply.yearly(r.xts[[i]],max)
  
  # transform to data.frame that can be used for further analysis and plotting
  rDfY[[i]] <- data.frame(tid = index(rXtsY[[i]]), 
                            m_o_h = rXtsY[[i]]$m_o_h, roer = names(r[i]))

}

rYearly <- rbind.fill(rDfY)
rm(r.xts, i,rXtsY, rDfY)

rYearly["Omr_stn"] <- gsub("r","",rYearly$roer[])
rYearly[c("Omr","stn")] = matrix(unlist(strsplit(rYearly$Omr_stn[],
                                                  split='_', fixed=TRUE)), 
                                                  nrow=nrow(rYearly), byrow=T)

rYearly[,c('Omr_stn','Omr','stn')] <- lapply(rYearly[,c('Omr_stn',
                                                      'Omr','stn')], factor)



!##############################################################################

# 2.3    Summary table ---------------------------------------------------------
library("plyr")
library("xts")

# Create table with entries: names, length, start, end
rSumTable <- cbind(index(r),names(tempdata2),ldply(r.xts.m,length),
                   ldply(r.xts.m,start),ldply(r.xts.m,end))

colnames(rSumTable) <- c('Index','Omr_stn', 'Laengd', 'Start', 'End')


# Playing to only get values with a certain length
RoerL40 <- droplevels(rSumTable[which(rSumTable$Laengd>40),]$Omr_stn)
RoerL180 <- droplevels(rSumTable[which(rSumTable$Laengd>180),]$Omr_stn)
# 
# rSumTable.roer <- rSumTable$Roer[yu]
# 
attach(rSumTable)

rSumTableL40 <- rSumTable[which(Omr_stn %in% RoerL40),]
rSumTableL180 <- rSumTable[which(Omr_stn %in% RoerL180),]

detach(rSumTable)

ggplot(rSumTableL40, aes(x=Laengd/12))+
  geom_histogram(colour="grey90", fill="black") +
  xlab("År") + ylab("Antal") 
#   theme_minimal()

ggplot(rSumTableL180, aes(x=Laengd/12))+
  geom_bar(stat="bin", colour="grey90", fill="black") +
  xlab("År") + ylab("Antal") +
  geom_text(stat="bin", color="white", hjust=1, size=3,
            aes(y=..count.., label=..count..))





# Add column with Period length
rSumTableL40 <- ddply(rSumTableL40, .(Omr_stn), mutate, Period = End-Start)
rSumTableL180 <- ddply(rSumTableL180, .(Omr_stn), mutate, Period = End-Start)

#output to csv file for print (col.names=NA if row.names=T)
write.table(rSumTableL40,file="./data/sum_month_max.csv",sep=";",row.names=F)




write.table(m2r2, file="./data/SGUmeta.csv", sep="\t",row.names=F, quote=F)


# 3.     Compute Summary statistics add join with metadata -----------------------
# Compute variance for all objects in data.frame 


# TODO build in this: psych::describe(x)


# use mutate to add column to long format data frame
rMonthlySum <- ddply(rMonthly, .(Omr_stn), summarise, Variance = var(m_o_h))

rMonthlySum[c("Omr","stn")] = matrix(unlist(strsplit(as.character(rMonthlySum$Omr_stn),
                                                 split='_', fixed=TRUE)), 
                                 nrow=nrow(rMonthlySum), byrow=T)

rMonthlySum[,c('Omr_stn','Omr','stn')] <- lapply(rMonthlySum[,c('Omr_stn',
                                                        'Omr','stn')], factor)

rMonthlySum["Median"] <- ddply(rMonthly, .(Omr_stn), summarise, 
                               Med = median(m_o_h))$Med

rMonthlySum["Std"] <- ddply(rMonthly, .(Omr_stn), summarise, 
                            Std = sd(m_o_h))$Std

rMonthlySum["Count"] <- ddply(rMonthly, .(Omr_stn), summarise, 
                              Count = length(m_o_h))$Count

rMonthlySum["Average"] <- ddply(rMonthly, .(Omr_stn), summarise,
                              Average = mean(m_o_h))$Average

rMonthlySum["Median"] <- ddply(rMonthly, .(Omr_stn), summarise,
                                Median = median(m_o_h))$Median

rMonthlySum <- ddply(rMonthlySum, .(Omr_stn), 
                     transform, ConvEV = Average+Std*2.6)


head(rMonthlySum)

m <- merge(m,rMonthlySum, by="Omr_stn")


!##############################################################################


# 4.     Summary statistics of intresting metadata ------------------------------

library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape2)

# 4.1    Barchart for each Variable -------------------------------------------
m[m==""]  <- NA ; 

m$Geohylag <- revalue(m$Geohylag, c("I"="Inströmningsområde",
                                    "M"="Intermediärt område",
                                    "U"="Utströmingsområde",
                                    "V"="Vattendelarläge"))

# Create label for plot
m = ddply(m, .(Var2), transform, lab= value)


ak <- ggplot(m, aes(x=Akvifertyp, fill=Akvifertyp))  + 
        geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
        theme(legend.position="none") +
        ylab("Antal") +
        geom_text(stat="bin", color="black", hjust=1, size=3,
            aes(y=..count.., label=..count..))


gh <- ggplot(m, aes(x=Geohylag, fill=Geohylag))  + 
  geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),legend.position="none") +
  ylab("Antal")


ja <- ggplot(m, aes(x=Jordart, fill=Jordart))  + 
  geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
  theme(legend.position="none") +
  ylab("Antal")


ta <- ggplot(m, aes(x=Taeckt, fill=Taeckt))  + 
  geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
  theme(legend.position="none") +
  ylab("Antal") 


ba <- ggplot(m, aes(x=Bergart, fill=Bergart))  + 
  geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1),legend.position="none") +
  ylab("Antal")

ms <- ggplot(m, aes(x=MagasinStl, fill=MagasinStl))  + 
  geom_bar(stat="bin") +
  #     geom_text(aes(label = lab, y = pos), size = 3) +
  theme(legend.position="none") +
  ylab("Antal")

source("./functions/multiplot.R")
multiplot(ak,gh,ja,ta,ba,ms, cols=3)

# 4.2    Stacked bar for each Variable ----------------------------------------

out <- by(data = mSumSum, INDICES = mSumSum$Var2, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(x=Var2, y=value, fill=var))  + 
    geom_bar(stat="identity") +
#     geom_text(aes(label = lab, y = pos), size = 3) +
    ylab("Antal") + xlab("") +
#     coord_polar() +
    guides(fill = guide_legend(reverse = TRUE)) 
})

do.call(grid.arrange, out)


# DEPRECATED
# mSumSum = ddply(mSumSum, .(Var2), transform, pos = cumsum(value) - 0.5*value)
# 
# mSumSum$var <- revalue(mSumSum$var, c("!NA's"="NA's"))
# 
# 
# 
# ggplot(mSumSum, aes(x=Var2, y=value)) + 
#   geom_bar(aes(fill=var)) +
# #   facet_wrap(~Var2, scales="free_x") +
#   geom_text(aes(label = lab, y = pos), size = 3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +   # Rotate tick mark labels
#   theme(legend.position="none")+
#   coord_polar()
# #   guides(fill = guide_legend(reverse = TRUE)) 


!##############################################################################


# 5.     Skewness testing, heatmap, OLS ----------------------------------------
library(e1071)
library(ggplot2)

tempSkewN <- with(rMonthly, tapply(m_o_h, Omr_stn, function(x) skewness(x, type=2)))
xSkew <- data.frame(value=as.numeric(tempSkewN))
xSkew[c("Omr","stn")] = matrix(unlist(strsplit(names(tempSkewN), split='_', 
                                               fixed=TRUE)), nrow=nrow(tempSkewN), 
                               byrow=T)

# remove failed skewtests
xSkew = xSkew[complete.cases(xSkew),]

rescal <- function(jj) if (jj>0) jj=1 else jj=0

xSkew["resca"] <- factor(with(xSkew, tapply(value, rownames(xSkew), rescal)))

# plot heatmap
p <- ggplot(xSkew, aes(stn, Omr)) + 
  geom_tile(aes(fill = resca),colour = "white") + 
  scale_fill_gradient(low = "darkgreen",high = "red")

base_size <- 9
p + theme_minimal(base_size = base_size) + labs(x = "Station",y = "Område") + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(legend.position = "none",axis.ticks = element_blank(), 
        axis.text.x = element_text(size = base_size *0.8, 
                                   angle = 60, hjust = 0, colour = "grey40"))


# Join metadata and Skewtable

SkewMetaJoin <- merge(xSkew, tempmeta, by=c("Omr","stn"))
pairs(SkewMetaJoin)

# OLS analysis, factanal(), 
xSkew$Omr = sapply(xSkew$Omr, as.factor)
xSkew$stn = sapply(xSkew$stn, as.factor)


# some data exploration with pairs plots
panel.hist <- function(x, ...) {
  # Set user coordinates of plotting region
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  par(new=TRUE)
  # Do not start new plot
  hist(x, prob=TRUE, axes=FALSE, xlab="", ylab="",
       main="", col="lightgrey")
  lines(density(x, na.rm=TRUE))
  curve( dnorm(x, mean(log(x)), sd(log(x))), col='red', add=T)
  # Add density curve
}
# Create function to compute and print R^2
panel.r2 <- function(x, y, digits=2, cex.cor, ...) {
  # Set user coordinates of plotting region
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="complete.obs")**2 # Compute R^2
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  if(missing(cex.cor)) cex.cor <- 1/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)#*r for diff size
}

pairs(~resca+Akvifertyp+Geohylag, data=SkewMetaJoin,
      lower.panel=panel.smooth, upper.panel=panel.r2,
      diag.panel=panel.hist)

xSkew.LM(lm(formula = value ~ Omr + stn, data = xSkew))

factanal(~Omr+stn+resca)




# 6      Trend testing ---------------------------------------------------------

larger3 <- which(rSumTable$Laengd>3)

rTsLar3 <- vector("list", length(larger3))
names(rTsLar3) <- names(r[larger3])
rTsLar3 <- r.ts.m[larger3]

#TODO use a for loop instead
options(warn=-1)
rTrend <- do.call(rbind,lapply(rTsLar3,function(x) unlist(SeasonalMannKendall(x))))
options(warn=0)

rTrend <- data.frame(rTrend)

rTrendConf <- subset(ty, tau > 0.0025 | tau < -0.0025)

# tauPos <- subset(rTrend, tau > -0.0025 & tau >
tauNeg <- subset(rTrend, tau < 0.0025)



# 7.     Subsetting and plotting ----------------------------------------------

library(ggplot2)
library(plyr)

# 7.1    Subsetting after Omr -------------------------------------------------

# Monthly subset
attach(rMonthly)
x <- droplevels(rMonthly[ which(Omr==85),])
# x <- rMonthly[ which(Omr==11 & stn %in% c(1,10,100,102,12,124,125,126,127,128,129)),]
detach(rMonthly)

z = x

# Yearly subset
attach(rYearly)
y <- rYearly[ which(Omr==31 & stn %in% c(2)),]
detach(rYearly)

z = x

# plot regular lineplot
x.line <- ggplot(z,aes(x = tid,y = m_o_h, colour = stn)) + 
              geom_line(size=1) +
              geom_smooth(method="loess") +
              facet_wrap(~stn, scales="free_y") +
#             geom_point(colour="black",size=3.5,shape=21,fill="grey80") +
              geom_hline(yintercept=10.92, linetype=2) +
#               geom_text(x="1992-03-15", y=67.3, label="67,59") +
              ylab("m.ö.h.") + xlab("Tid") +
              theme_minimal()

ggsave(x.line, file="./img/line.png", dpi=600)

# plot density plot

x.dens <- ggplot(z,aes(x = m_o_h, colour = Omr_stn)) + 
          geom_density(alpha=0.55) +
          facet_wrap(~stn, scales="free") +
          xlab("m.ö.h. [m]") + ylab("frekvens") +
          theme_bw()
  



# plot ecdf plot
x2 <- ddply(z,.(stn),transform, ecd = ecdf(m_o_h)(m_o_h))

x.ecdf <- ggplot(x2,aes(x = m_o_h, y = ecd, colour = stn, fill = stn)) + 
          geom_step(alpha=0.55, size=1.1) +
          facet_wrap(~stn, scales="free_x") +
          theme_minimal()




# 7.2    Subsetting after Omr_stn Monthly -------------------------------------
attach(rMonthly)
rMonthly40 <- droplevels(rMonthly[which(Omr_stn %in% RoerL40),])
detach(rMonthly)

# x <- rMonthly[ which(Omr==13 & stn %in% c(101,102,103,107,2,3,4,5)),]
OmrFunct <- c(1,10,1400,15,17,1720,1730,18,20,21,2300,24,25,2510,31,
              35,41,44,5,51,52,53,56,57,59,6,60,64,65,67,69,7,71,75,
              8,85,86,90,93,94,96)


# Monthly subset
attach(rMonthly40)
x <- droplevels(rMonthly40[ which(Omr %in% OmrFunct ),])
detach(rMonthly40)
testdistfit(x)


source('./functions/testdistfit.R')

# 10.    Some distribution fitting ---------------------------------------------

library(fitdistrplus)
library(MASS)
library(VGAM)

# 10.1   Model two parameter distribution --------------------------------------
# Distributions left to fit:  "bimodal" "Pareto" "truncuated pareto"



# All distributions with bootstrap CI
# define number of bootstrap simulations
# nrit = 1001

## NORMAL DIST
fitNorm <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'norm'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitNorm <-fitNorm[fitNorm != "NULL"]
# bootNorm <- lapply(fitNorm,function(u) bootdist(u, bootmethod='nonparam',nrit))


## LOGNORMAL DIST
fitLnorm <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'lnorm'),error=function(e) NULL)))
# trim list to get rid of empty elements, left in the dataframe
fitLnorm <-fitLnorm[fitLnorm != "NULL"]
# bootLnorm <- lapply(fitLnorm,function(u) bootdist(u, bootmethod='nonparam',nrit))


## LOGISTIC DIST
fitLogis <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'logis'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitLogis <-fitLogis[fitLogis != "NULL"]
# bootLogis <- lapply(fitLogis,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GAMMA
fitGamma <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'gamma'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitGamma <-fitGamma[fitGamma != "NULL"]
# bootGamma <- lapply(fitGamma,function(u) bootdist(u, bootmethod='nonparam',nrit))


## WEIBULL DIST
fitWeibull <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'weibull'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitWeibull <-fitWeibull[fitWeibull != "NULL"]
# bootWeibull <- lapply(fitWeibull,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GUMBEL DIST
fitGumbel <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'gumbel', start = list(location = 0, scale = 1)),
           error=function(e) NULL)))
# trim list to get rid of empty elements, left in the dataframe
fitGumbel <-fitGumbel[fitGumbel != "NULL"]
# bootGumbel <- lapply(fitGumbel,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GEV DIST
fitGev <- with(x, tapply(m_o_h, Omr_stn, function(u) tryCatch(gev(u),error=function(e) NULL ))) 
# trim list to get rid of empty elements, left in the dataframe
fitGev <-fitGev[fitGev != "NULL"]



# 10.2    Fitting GEV Monthly--------------------------------------------------

GevFitsEst <- vector("list", length(fitGev))

for(i in 1:length(fitGev)){
    GevFitsEst[[i]] <- data.frame(ests=fitGev[[i]]$par.ests)
    GevFitsEst[[i]]["Omr_stn"] <-  names(fitGev[i])
    
}
GevFitsEstList <-GevFitsEst
GevFitsEst <- rbind.fill(GevFitsEst)
GevFitsEst["par"] <- rep(factor(c("shape","scale","location")),length(fitGev))

PlotGevFit <- ggplot(GevFitsEst[which(GevFitsEst$par=="shape"),], 
                     aes(x=ests, fill=par))+
                geom_bar(stat="bin", fill="black") +
                geom_vline(xintercept = 0, colour= "red", size=2,linetype = 2) +
                coord_flip() +
                xlab(expression(xi)) + ylab("antal") +
                geom_text(stat="bin", color="white", hjust=1, size=4,
                aes(y=..count.., label=..count..))




# 10.1.1 Sort Fits -------------------------------------------------------------

# preallocate list for fits
rFits <- vector("list", length(fitNorm))
# rBoots <- vector("list", length(fitNorm))

for(i in 1:length(fitNorm)){
  rFits[[i]] <- list(fitNorm[[i]],
#              fitCauchy[[i]],
             fitLnorm[[i]],
             fitLogis[[i]],
             fitGamma[[i]],
             fitWeibull[[i]],
             fitGumbel[[i]])
#   rBoots[[i]]<- list(bootNorm[[i]]$CI,
#                      bootCauchy[[i]]$CI,
#                      bootLnorm[[i]]$CI,
#                      bootLogis[[i]]$CI,
#                      bootGamma[[i]]$CI,
#                      bootWeibull[[i]]$CI,
#                      bootGumbel[[i]]$CI)
}

fitObjs <- length(fitGumbel)

rm(fitNorm,fitLnorm,fitLogis,fitGamma,fitWeibull,i,fitGumbel)
# rm(bootNorm, bootCauchy,bootLnorm, bootLogis,bootGamma,bootWeibull,bootGumbel)
# denscomp(rFits[[2]])

                  

# 10.1.2 Extract Estimates -----------------------------------------------------

library(plyr)

# preallocate list
rFitsEst <- vector("list", length(rFits))
xOmr_stns <- as.character(x$Omr_stn[!duplicated(x$Omr_stn)])

# get estimates in good form
for(i in 1:length(rFits)){
  tempFit <- rFits[[i]]
  rFitsEst[[i]] <- data.frame(t(data.frame(sapply(tempFit,"[[", "estimate"))))
  colnames(rFitsEst[[i]]) <- c("par1","par2")
  rFitsEst[[i]]["Omr_stn"] <- rep(xOmr_stns[i],nrow(rFitsEst[[i]]))
  rFitsEst[[i]]["FIT"] <- c("norm","lnorm","logis","gamma","weibull","gumbel")
}

rm(tempFit)

rFitsEst <- rbind.fill(rFitsEst)

# 10.1.3 Sort GOF --------------------------------------------------------------

library(fitdistrplus)
# preallocate list for gofs
rGofs <- vector("list", fitObjs)


for(i in 1:fitObjs){
    tempGof <- lapply(rFits[[i]], gofstat)
    temprGofs <- lapply(tempGof, function(x){
                        data.frame(x[c("cvm", "ad", "ks")])})
    rGofs[[i]] <- rbind.fill(temprGofs)
    names(rGofs[[i]]) <- c("CVM", "AD", "KS")
    rGofs[[i]]["Omr_stn"] <- rep(xOmr_stns[i],length(fitObjs))
    rGofs[[i]]["FIT"] <- c("norm","lnorm","logis","gamma","weibull","gumbel")
}
rm(tempGof,temprGofs)

# make list to data frame long
rGofs <- rbind.fill(rGofs)


# 10.2    Find best fitting distribution --------------------------------------

# find minmum in Anderson Darling test
minAD <- with(rGofs, tapply(AD, Omr_stn, min))
ADfits <- rGofs$FIT[which(rGofs$AD %in% minAD)]
ADfits <- data.frame(Omr_stn=xOmr_stns, FIT=ADfits)

# find minmum in Kolmogorov Smirnof test
minKS <- with(rGofs, tapply(KS, Omr_stn, min))
KSfits <- rGofs$FIT[which(rGofs$KS %in% minKS)]

# find minmum in Kolmogorov Cramer von-Mises test
minCVM <- with(rGofs, tapply(CVM, Omr_stn, min))
CVMfits <- rGofs$FIT[which(rGofs$CVM %in% minCVM)]

ADfitsEsts <- merge(rFitsEst,ADfits,by=c("Omr_stn","FIT"))

# 10.2.1 Plot cdf against ecdf -----------------------------------------------



ADfitsDist <- ADfitsEsts[which(ADfitsEsts$FIT=="norm"),]

Distfits <- droplevels(merge(ADfitsDist, rMonthly40, by="Omr_stn"))

source("./functions/cdf_comp.R")
DistCDfs <- with(Distfits, tapply(m_o_h, Omr_stn, function(u) cdf_comp(u,'weibull'))) 

lapply(names(DistCDfs), function(x)ggsave(filename=paste("./img/norm_CDF_",x,".png",sep=""), 
                                            plot=DistCDfs[[x]], dpi=300))

source("./functions/hist_with_density.R")
multiplot(CauchyCDfs, cols=4)



# 10.3    Get return levels of best fits monthly ------------------------------

# 10.3.1  Get return levels of fits monthly non GEV ---------------------------

kvantil <- c(0.98,0.99,0.995)
retLevMon <- vector("list",fitObjs)

# loop to get return values
for(i in 1:fitObjs){
  if(ADfitsEsts$FIT[[i]] == "norm"){
    retLevMon[[i]] <- data.frame(qnorm(kvantil, 
                                ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
#   if(ADfitsEsts$FIT[[i]] == "cauchy"){
#     retLevMon[[i]] <- data.frame(qcauchy(kvantil,
#                                 ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
#   } 
  if(ADfitsEsts$FIT[[i]] == "lnorm"){
    retLevMon[[i]] <- data.frame(qlnorm(kvantil,
                                ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "logis")
  {
    retLevMon[[i]] <- data.frame(qlogis(kvantil,
                                ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "gamma"){
    retLevMon[[i]] <- data.frame(qgamma(kvantil, 
                                ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "weibull"){
    retLevMon[[i]] <- data.frame(qweibull(kvantil,
                      ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "gumbel"){
    retLevMon[[i]] <- data.frame(qgumbel(kvantil,
                                          ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  # make data readable
  colnames(retLevMon[[i]]) <- c("ReturnLevel")
  retLevMon[[i]]["ReturnPeriod"] <- c("50","100","200")
  retLevMon[[i]]["Omr_stn"] <- ADfitsEsts$Omr_stn[i]
  retLevMon[[i]]["FIT"] <- ADfitsEsts$FIT[i]
}

retLevMon <- rbind.fill(retLevMon)
retLevMon$ReturnPeriod <- factor(retLevMon$ReturnPeriod)
retLevMon$Omr_stn <- factor(retLevMon$Omr_stn)
retLevMon$FIT <- factor(retLevMon$FIT)

# 10.3.2  Get return levels of fits monthly GEV ------------------------------

retLevMonGEV <- vector("list",length(GevFitsEstList))

# loop to get return values
for(i in 1:length(GevFitsEstList)){
    retLevMonGEV[[i]] <- data.frame(qgev(kvantil, GevFitsEstList[[i]]$ests[1],
                                         GevFitsEstList[[i]]$ests[3],
                                         GevFitsEstList[[i]]$ests[2]))
    colnames(retLevMonGEV[[i]]) <- c("ReturnLevel")
    retLevMonGEV[[i]]["ReturnPeriod"] <- c("50","100","200")
    retLevMonGEV[[i]]["Omr_stn"] <- GevFitsEstList[[i]]$Omr_stn

}

retLevMonGEV <- rbind.fill(retLevMonGEV)



# Difference between GEV and regular distributions

retLevMonDiff <- retLevMon[which(retLevMon$Omr_stn %in% retLevMonGEV$Omr_stn),]
retLevMonDiff["ReturnLevelGEV"] <- retLevMonGEV$ReturnLevel

retLevMonDiff <- ddply(retLevMonDiff, .(ReturnPeriod), 
                       mutate, diff=ReturnLevel-ReturnLevelGEV)
dd = -3
retLevMonDiff[which(retLevMonDiff$diff < dd),]

retLevMonDiffss <- retLevMonDiff[which(retLevMonDiff$ReturnPeriod == 100 & retLevMonDiff$diff > dd),]

ggplot(retLevMonDiffss, aes(x=Omr_stn, y=diff, colour=FIT)) +
  geom_point() +
  geom_hline(yintercept=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Differens [m]")

diffFitM <- ggplot(retLevMonDiffss, aes(x=FIT, y=diff, colour=FIT)) +
  geom_jitter() +
#   coord_flip() +
#   geom_hline(yintercept=0)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Differens FIT-GEV [m]") + xlab("Anpassning") +
  ggtitle("Månadsmax")



# 10.4    Get return levels of best fits monthly/12 ------------------------------

kvantil <- c(0.998333,0.999167,0.9995455)

retLevMon2 <- vector("list",fitObjs)

# loop to get return values
for(i in 1:fitObjs){
  if(ADfitsEsts$FIT[[i]] == "norm"){
    retLevMon2[[i]] <- data.frame(qnorm(kvantil, 
                                       ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  #   if(ADfitsEsts$FIT[[i]] == "cauchy"){
  #     retLevMon[[i]] <- data.frame(qcauchy(kvantil,
  #                                 ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  #   } 
  if(ADfitsEsts$FIT[[i]] == "lnorm"){
    retLevMon2[[i]] <- data.frame(qlnorm(kvantil,
                                        ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "logis")
  {
    retLevMon2[[i]] <- data.frame(qlogis(kvantil,
                                        ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "gamma"){
    retLevMon2[[i]] <- data.frame(qgamma(kvantil, 
                                        ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "weibull"){
    retLevMon2[[i]] <- data.frame(qweibull(kvantil,
                                          ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  if(ADfitsEsts$FIT[[i]] == "gumbel"){
    retLevMon2[[i]] <- data.frame(qgumbel(kvantil,
                                         ADfitsEsts$par1[[i]], ADfitsEsts$par2[[i]]))
  } 
  # make data readable
  colnames(retLevMon2[[i]]) <- c("ReturnLevel")
  retLevMon2[[i]]["ReturnPeriod"] <- c("50","100","200")
  retLevMon2[[i]]["Omr_stn"] <- ADfitsEsts$Omr_stn[i]
  retLevMon2[[i]]["FIT"] <- ADfitsEsts$FIT[i]
}

retLevMon2 <- rbind.fill(retLevMon2)
retLevMon2$ReturnPeriod <- factor(retLevMon2$ReturnPeriod)
retLevMon2$Omr_stn <- factor(retLevMon2$Omr_stn)
retLevMon2$FIT <- factor(retLevMon2$FIT)



# 11.   Regression Analysis Monthly -------------------------------------------

library(ggplot2)
library(plyr)

attach(retLevMon)
names(retLevMon)<- c("ReturnLevel","ReturnPeriod","Omr_stn","FIT")
detach(retLevMon)

attach(rMonthlySum)
rMonthlySum40 <- droplevels(rMonthlySum[which(Omr_stn %in% RoerL40),])
detach(rMonthlySum)
attach(rMonthlySum40)
regMonthlySum <- droplevels(rMonthlySum40[ which(Omr %in% OmrFunct),])
detach(rMonthlySum40)


# 11.1    Statistics on fitted data -------------------------------------------


# 11.1.1  Plot Count for each distribution ------------------------------------
retLevMonCount <- droplevels(retLevMon[which(retLevMon$ReturnPeriod==100),])

ggplot(retLevMonCount, aes(x=FIT, fill=FIT))+
  geom_bar(stat="bin") +
  ylab("Antal") + xlab("Fördelning") +
  theme_minimal() +
  theme(legend.position="none") +
  coord_flip() +
  scale_x_discrete(limits=c("gamma","lnorm","gumbel","norm","logis","weibull")) +
  geom_text(stat="bin", color="black", hjust=1, size=4,
            aes(y=..count.., label=..count..))


# 11.1.2  Plot Correlation between intervall and distribution -----------------

rSumCorr <- join(rSumTableL40, ADfits, by="Omr_stn",type = "right")
rSumCorr$Period <- as.numeric(rSumCorr$Period)



x.corrFoerdel <- ggplot(rSumCorr, aes(x=Period, y=FIT, colour=FIT))+
                    geom_boxplot() + 
                    geom_jitter(alpha=.4, width=10) +
                    xlab("Dagar mätperiod") + ylab("Fördelning") +
                    theme_minimal() +
                    theme(legend.position="none") +
#                     coord_flip() +
                    scale_y_discrete(limits=c("gamma","lnorm","gumbel",
                                              "norm","logis","weibull"))

ggsave(x.corrFoerdel, file="./img/maetper-foerdel.png", dpi=600)




# 11.2  Compute Kest ----------------------------------------------------------





regMonth <- data.frame(Med=rep(regMonthlySum$Median,each=3),
                       Avg=rep(regMonthlySum$Average,each=3),
                       Std=rep(regMonthlySum$Std, each=3), 
                       Återkomstid=retLevMon$ReturnPeriod,
                       Åter = retLevMon$ReturnLevel,
                       Omr_stn = retLevMon$Omr_stn,
                       FIT = retLevMon$FIT)



# attach a column with k est
regMonth <- ddply(regMonth, .(Återkomstid), mutate, kest_med=(Åter-Med)/Std)
regMonth <- ddply(regMonth, .(Återkomstid), mutate, kest_avg=(Åter-Avg)/Std)


# Compute summary of kest_median
kestSum <- ddply(regMonth, .(Återkomstid), summarise, Std_med=sd(kest_med))
kestSum["Mean_med"] <- ddply(regMonth, .(Återkomstid), 
                                 summarise, Mean_med=mean(kest_med))$Mean_med
kestSum["Median_med"] <- ddply(regMonth, .(Återkomstid), 
                                 summarise, Median_med=median(kest_med))$Median_med

# Compute summary of kest_avg
kestSum["Std_avg"] <- ddply(regMonth, .(Återkomstid), 
                                 summarise, Std_avg=mean(kest_avg))$Std_avg
kestSum["Mean_avg"] <- ddply(regMonth, .(Återkomstid), 
                                 summarise, Mean_avg=mean(kest_avg))$Mean_avg
kestSum["Median_avg"] <- ddply(regMonth, .(Återkomstid), 
                                 summarise, Median_avg=median(kest_avg))$Median_avg



kestL4 <- (regMonthMed[which(regMonthMed$kest>6),])

# k_est plotted(median) ~ return period and distribution
ggplot(regMonth, aes(x=Återkomstid, y=kest_med)) +
  geom_jitter(alpha=.55,aes(fill=FIT,colour=FIT)) + 
  geom_boxplot(alpha=.55, aes(fill=FIT)) +
  ylab(expression(k[est])) +
  scale_x_discrete(limits=c("50","100","200"))


# k_est plotted(average) ~ return period and distribution
ggplot(regMonth, aes(x=Återkomstid, y=kest_avg)) +
  geom_jitter(alpha=.55,aes(fill=FIT,colour=FIT)) + 
  geom_boxplot(alpha=.55, aes(fill=FIT)) +
  ylab(expression(k[est])) +
  scale_x_discrete(limits=c("50","100","200"))


ggplot(regMonth, aes(x=Återkomstid, y=kest)) +
  geom_jitter(alpha=.55,aes(colour=FIT)) + 
  geom_boxplot(alpha=.55, aes(fill=Återkomstid)) +
  ylab(expression(k[est])) +
  scale_x_discrete(limits=c("50","100","200"))

# TODO: variance, average vs median


# 11.3  Combine Regression in plot ---------------------------------------------

attach(rMonthlySum)
rMonthlySum40 <- rMonthlySum[which(Omr_stn %in% RoerL40),]
regMonth <- rMonthlySum40[ which(Omr==13 & stn %in% c(101,102,103,2,3,4,5)),]
detach(rMonthlySum)

regMonth <- data.frame(Avg=rep(regMonth$Average,each=3),
                       Std=regMonth$Std, Återkomstid=retLevMon$ReturnPeriod,
                       Åter = retLevMon$ReturnLevel,
                       Omr_stn=retLevMon$Omr_stn)


# attach a column with k est
regMonth <- ddply(regMonth, .(Återkomstid), mutate, kest=(Åter-Avg)/Std)


ggplot(regMonth, aes(x=Återkomstid, y=kest, colour=Återkomstid)) +
        geom_jitter() + geom_boxplot(alpha=.7) +
  scale_x_discrete(limits=c("50","100","200"))
  

ggplot(regMonth, aes(x=kest)) +
  geom_point() 










regPlot <- ggplot(regMonth, aes(x=indVar, y=depVar, colour=Återkomstid)) + 
              geom_point(size=4) + 
  #           stat_smooth(method=lm, level=0.95) +
              stat_smooth(method=lm, se=F, fullrange=F) +
              xlab("Std(data)") + ylab("BestFit(data) - Mean(data)")


# 5.     Multiple correspondence Analysis ---------------------------------------

library(FactoMineR)
# Create factor table ready 
mSum <- m[, c(1,8:16)]

mSum <- merge(mSum, retLevMonCount, by="Omr_stn")
drops <- c("Omr_stn","ReturnLevel","ReturnPeriod")
mSum <- droplevels(mSum[,!names(mSum) %in% drops])

droplevels(mSum[which(mSum$FIT==cauchy),])

mSumCA <- CA(mSum)
mSumMCA <- MCA(mSum)
par(bty='n')
plot(mSumMCA, cex=0.7, selectMod="contrib 20", unselect="grey60", select="contrib 10")
plot(mSumMCA, choix="var")
HCPC(mSumMCA)

plot(mca(mSum, nf=2, abbrev=T),rows=F)

mSum1 <- mSum[complete.cases(mSum),]

catdes

# 12     Comparison etc. ------------------------------------------------------

# Select only metadata of stations that has been fitted
compDistMeta <- droplevels(m[which(m$Omr_stn %in% retLevMonCount$Omr_stn),])
# Select only fitted data that has metadata
compDistMeta <- droplevels(m[which(retLevMonCount$Omr_stn %in% m$Omr_stn),])
retLevMonCountX <-droplevels(retLevMonCount[which(retLevMonCount$Omr_stn %in% m$Omr_stn),])

compDistMeta["FIT"] <- retLevMonCountX$FIT
# comp_ok <- comp[which(comp$FIT == comp$FITY),]

comp.ak<- ggplot(compDistMeta, aes(x=FIT, fill=Akvifertyp))+
              geom_bar(stat="bin") +
              coord_polar()

comp.gh<- ggplot(compDistMeta, aes(x=FIT, fill=Geohylag))+
  geom_bar(stat="bin") +
  coord_polar()


comp.ja<- ggplot(compDistMeta, aes(x=FIT, fill=Jordart))+
  geom_bar(stat="bin") +
  coord_polar()


comp.ta<- ggplot(compDistMeta, aes(x=FIT, fill=Taeckt))+
  geom_bar(stat="bin") +
  coord_polar()


comp.ba<- ggplot(compDistMeta, aes(x=FIT, fill=Bergart))+
  geom_bar(stat="bin") +
  coord_polar()


comp.ms<- ggplot(compDistMeta, aes(x=FIT, fill=MagasinStl))+
  geom_bar(stat="bin") +
  coord_polar()

library(gridExtra)
source("./functions/multiplot.R")
multiplot(comp.ak,comp.gh,comp.ja,comp.ta,comp.ba,comp.ms, cols=3)

# YEARLY OPERATIONS -----------------------------------------------------------


# 1    Subsetting after Omr_stn Yearly --------------------------------------
attach(rYearly)
rYearly180 <- droplevels(rYearly[which(Omr_stn %in% RoerL180),])
detach(rYearly)

# x <- rMonthly[ which(Omr==13 & stn %in% c(101,102,103,107,2,3,4,5)),]
OmrFunct <- c(1,10,1400,15,17,1720,1730,18,20,21,2300,24,25,2510,31,
              35,41,44,5,51,52,53,56,57,59,6,60,64,65,67,69,7,71,75,
              8,85,86,90,93,94,96)


# Monthly subset
attach(rYearly180)
x <- droplevels(rYearly180[ which(Omr %in% OmrFunct ),])
detach(rYearly180)

# 2    Model two parameter distribution --------------------------------------
# Distributions left to fit:  "bimodal" "Pareto" "truncuated pareto"

x <- droplevels(x[which(x$Omr_stn %in% names(fitLnorm)),])


## NORMAL DIST
fitNorm <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'norm'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitNorm <-fitNorm[fitNorm != "NULL"]
# bootNorm <- lapply(fitNorm,function(u) bootdist(u, bootmethod='nonparam',nrit))


## LOGNORMAL DIST
fitLnorm <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'lnorm'),error=function(e) NULL)))
# trim list to get rid of empty elements, left in the dataframe
fitLnorm <-fitLnorm[fitLnorm != "NULL"]
# bootLnorm <- lapply(fitLnorm,function(u) bootdist(u, bootmethod='nonparam',nrit))


## LOGISTIC DIST
fitLogis <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'logis'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitLogis <-fitLogis[fitLogis != "NULL"]
# bootLogis <- lapply(fitLogis,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GAMMA
fitGamma <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'gamma'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitGamma <-fitGamma[fitGamma != "NULL"]
# bootGamma <- lapply(fitGamma,function(u) bootdist(u, bootmethod='nonparam',nrit))


## WEIBULL DIST
fitWeibull <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'weibull'),error=function(e) NULL))) 
# trim list to get rid of empty elements, left in the dataframe
fitWeibull <-fitWeibull[fitWeibull != "NULL"]
# bootWeibull <- lapply(fitWeibull,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GUMBEL DIST
fitGumbel <- with(x, tapply(m_o_h, Omr_stn, function(u) 
  tryCatch(fitdist(u,'gumbel', start = list(location = 0, scale = 1)),
           error=function(e) NULL)))
# trim list to get rid of empty elements, left in the dataframe
fitGumbel <-fitGumbel[fitGumbel != "NULL"]
# bootGumbel <- lapply(fitGumbel,function(u) bootdist(u, bootmethod='nonparam',nrit))


## GEV DIST
fitGev <- with(x, tapply(m_o_h, Omr_stn, function(u) tryCatch(gev(u),error=function(e) NULL ))) 
# trim list to get rid of empty elements, left in the dataframe
fitGev <-fitGev[fitGev != "NULL"]
# All distributions with bootstrap CI
# define number of bootstrap simulations
# nrit = 1001


# 3    Fitting GEV Yearly --------------------------------------------------

GevFitsEstY <- vector("list", length(fitGev))

for(i in 1:length(fitGev)){
  GevFitsEstY[[i]] <- data.frame(ests=fitGev[[i]]$par.ests)
  GevFitsEstY[[i]]["Omr_stn"] <-  names(fitGev[i])
  
}
GevFitsEstListY <-GevFitsEstY
GevFitsEstY <- rbind.fill(GevFitsEstY)
GevFitsEstY["par"] <- rep(factor(c("shape","scale","location")),length(fitGev))

PlotGevFitY <- ggplot(GevFitsEstY[which(GevFitsEstY$par=="shape"),], 
                      aes(x=ests, fill=par)) +
  geom_bar(stat="bin", fill="black") +
  geom_vline(xintercept = 0, colour= "red", size=2,linetype = 2) +
  coord_flip() +
  xlab(expression(xi)) + ylab("antal") +
  geom_text(stat="bin", color="white", hjust=1, size=2,
            aes(y=..count.., label=..count..))

# 4    Sort Fits -------------------------------------------------------------

# preallocate list for fits
rFitsY <- vector("list", length(fitNorm))
# rBootsY <- vector("list", length(fitNorm))

for(i in 1:length(fitNorm)){
  rFitsY[[i]] <- list(fitNorm[[i]],
                     #              fitCauchy[[i]],
                     fitLnorm[[i]],
                     fitLogis[[i]],
                     fitGamma[[i]],
                     fitWeibull[[i]],
                     fitGumbel[[i]])
  #   rBoots[[i]]<- list(bootNorm[[i]]$CI,
  #                      bootCauchy[[i]]$CI,
  #                      bootLnorm[[i]]$CI,
  #                      bootLogis[[i]]$CI,
  #                      bootGamma[[i]]$CI,
  #                      bootWeibull[[i]]$CI,
  #                      bootGumbel[[i]]$CI)
}

fitObjsY <- length(fitNorm)

rm(fitNorm,fitLnorm,fitLogis,fitGamma,fitWeibull,i,fitGumbel)
# rm(bootNorm, bootCauchy,bootLnorm, bootLogis,bootGamma,bootWeibull,bootGumbel)
# denscomp(rFits[[2]])

# 5    Extract Estimates -----------------------------------------------------

library(plyr)

# preallocate list
rFitsEstY <- vector("list", length(rFitsY))
xOmr_stns <- as.character(x$Omr_stn[!duplicated(x$Omr_stn)])

# get estimates in good form
for(i in 1:length(rFitsY)){
  tempFit <- rFitsY[[i]]
  rFitsEstY[[i]] <- data.frame(t(data.frame(sapply(tempFit,"[[", "estimate"))))
  colnames(rFitsEstY[[i]]) <- c("par1","par2")
  rFitsEstY[[i]]["Omr_stn"] <- rep(xOmr_stns[i],nrow(rFitsEstY[[i]]))
  rFitsEstY[[i]]["FIT"] <- c("norm","lnorm","logis","gamma","weibull","gumbel")
}

rm(tempFit)

rFitsEstY <- rbind.fill(rFitsEstY)

# 6    Sort GOF --------------------------------------------------------------

library(fitdistrplus)
# preallocate list for gofs
rGofsY <- vector("list", fitObjsY)


for(i in 1:fitObjs){
  tempGof <- lapply(rFits[[i]], gofstat)
  temprGofs <- lapply(tempGof, function(x){
    data.frame(x[c("cvm", "ad", "ks")])})
  rGofsY[[i]] <- rbind.fill(temprGofs)
  names(rGofsY[[i]]) <- c("CVM", "AD", "KS")
  rGofsY[[i]]["Omr_stn"] <- rep(xOmr_stns[i],length(fitObjsY))
  rGofsY[[i]]["FIT"] <- c("norm","lnorm","logis","gamma","weibull","gumbel")
}
rm(tempGof,temprGofs)

# make list to data frame long
rGofsY <- rbind.fill(rGofsY)

# 7    Find best fitting distribution --------------------------------------

# find minmum in Anderson Darling test
minADY <- with(rGofsY, tapply(AD, Omr_stn, min))
ADfitsY <- rGofsY$FIT[which(rGofsY$AD %in% minADY)]
ADfitsY <- data.frame(Omr_stn=xOmr_stns, FIT=ADfitsY)

# find minmum in Kolmogorov Smirnof test
minKS <- with(rGofs, tapply(KS, Omr_stn, min))
KSfits <- rGofs$FIT[which(rGofs$KS %in% minKS)]

# find minmum in Kolmogorov Cramer von-Mises test
minCVM <- with(rGofs, tapply(CVM, Omr_stn, min))
CVMfits <- rGofs$FIT[which(rGofs$CVM %in% minCVM)]

ADfitsEstsY <- merge(rFitsEstY,ADfitsY,by=c("Omr_stn","FIT"))

# 8    Get return levels of fits yearly non GEV ---------------------------

kvantil <- c(0.98,0.99,0.995)
retLevYear <- vector("list",fitObjsY)

# loop to get return values
for(i in 1:fitObjs){
  if(ADfitsEstsY$FIT[[i]] == "norm"){
    retLevYear[[i]] <- data.frame(qnorm(kvantil, 
                                        ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  }  
  if(ADfitsEstsY$FIT[[i]] == "lnorm"){
    retLevYear[[i]] <- data.frame(qlnorm(kvantil,
                                         ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  } 
  if(ADfitsEstsY$FIT[[i]] == "logis")
  {
    retLevYear[[i]] <- data.frame(qlogis(kvantil,
                                         ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  } 
  if(ADfitsEstsY$FIT[[i]] == "gamma"){
    retLevYear[[i]] <- data.frame(qgamma(kvantil, 
                                         ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  } 
  if(ADfitsEstsY$FIT[[i]] == "weibull"){
    retLevYear[[i]] <- data.frame(qweibull(kvantil,
                                           ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  } 
  if(ADfitsEstsY$FIT[[i]] == "gumbel"){
    retLevYear[[i]] <- data.frame(qgumbel(kvantil,
                                          ADfitsEstsY$par1[[i]], ADfitsEstsY$par2[[i]]))
  } 
  # make data readable
  colnames(retLevYear[[i]]) <- c("ReturnLevel")
  retLevYear[[i]]["ReturnPeriod"] <- c("50","100","200")
  retLevYear[[i]]["Omr_stn"] <- ADfitsEstsY$Omr_stn[i]
  retLevYear[[i]]["FIT"] <- ADfitsEstsY$FIT[i]
}

retLevYear <- rbind.fill(retLevYear)

retLevYear <- rbind.fill(retLevYear)
retLevYear$ReturnPeriod <- factor(retLevYear$ReturnPeriod)
retLevYear$Omr_stn <- factor(retLevYear$Omr_stn)
retLevYear$FIT <- factor(retLevYear$FIT)

# 9    Get return levels of fits yearly GEV ------------------------------

retLevYearGEV <- vector("list",length(GevFitsEstListY))

# loop to get return values
for(i in 1:length(GevFitsEstListY)){
  retLevYearGEV[[i]] <- data.frame(qgev(kvantil, GevFitsEstListY[[i]]$ests[1],
                                       GevFitsEstListY[[i]]$ests[3],
                                       GevFitsEstListY[[i]]$ests[2]))
  colnames(retLevYearGEV[[i]]) <- c("ReturnLevel")
  retLevYearGEV[[i]]["ReturnPeriod"] <- c("50","100","200")
  retLevYearGEV[[i]]["Omr_stn"] <- GevFitsEstListY[[i]]$Omr_stn
  
}

retLevYearGEV <- rbind.fill(retLevYearGEV)



# Difference between GEV and regular distributions

retLevYearDiff <- retLevYear[which(retLevYear$Omr_stn %in% retLevYearGEV$Omr_stn),]
retLevYearDiff["ReturnLevelGEV"] <- retLevYearGEV$ReturnLevel

retLevYearDiff <- ddply(retLevYearDiff, .(ReturnPeriod), 
                       mutate, diff=ReturnLevel-ReturnLevelGEV)
dd = -6
retLevYearDiff[which(retLevYearDiff$diff < dd),]

retLevYearDiffss <- retLevYearDiff[which(retLevYearDiff$ReturnPeriod == 100 & retLevYearDiff$diff > dd),]
#

ggplot(retLevYearDiffss, aes(x=Omr_stn, y=diff, colour=FIT)) +
  geom_point() +
  geom_hline(yintercept=0)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Differens [m]")

ggplot(retLevYearDiffss, aes(x=FIT, y=diff, colour=FIT)) +
  geom_jitter() +
  #   coord_flip() +
  #   geom_hline(yintercept=0)+
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Differens FIT-GEV [m]") + xlab("Anpassning") +
  ggtitle("Årsmax")

# 10   Plot Count for each distribution ------------------------------------
retLevYearCount <- retLevYear[which(retLevYear$ReturnPeriod==100),]

ggplot(retLevYearCount, aes(x=FIT, fill=FIT))+
  geom_bar(stat="bin") +
  ylab("Antal") + xlab("Fördelning") +
  theme_minimal() +
  theme(legend.position="none") +
  coord_flip() +
  scale_x_discrete(limits=c("gamma","lnorm","gumbel","norm","logis","weibull")) +
  geom_text(stat="bin", color="black", hjust=1, size=4,
            aes(y=..count.., label=..count..))

# 11   Plot Correlation between intervall and distribution -----------------

rSumCorrY <- join(rSumTableL180, ADfitsY, by="Omr_stn",type = "right")
rSumCorrY$Period <- as.numeric(rSumCorrY$Period)



x.corrFoerdelY <- ggplot(rSumCorrY, aes(x=Period, y=FIT, colour=FIT))+
                  geom_boxplot() + 
                    geom_jitter(alpha=.4, width=10) +
                    xlab("Dagar mätperiod") + ylab("Fördelning") +
                    theme_minimal() +
                    theme(legend.position="none") 
#                     coord_flip() +
                    scale_x_discrete(limits=c("gamma","lnorm","gumbel",
                                              "norm","logis","weibull"))

ggsave(x.corrFoerdel, file="./img/maetper-foerdel.png", dpi=600)


# TEST YEARLY VS MONTHLY ------------------------------------------------------

comp <- droplevels(retLevMonCount[which(retLevMonCount$Omr_stn %in% retLevYearCount$Omr_stn),])

comp["FITY"] <- retLevYearCount$FIT
comp_ok <- comp[which(comp$FIT == comp$FITY),]
>>>>>>> 4183920323dda0ea229f23646bd0cfe57ffabce3
