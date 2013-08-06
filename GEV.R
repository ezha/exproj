# Skript for convergence check: When do return level stabilize?

library(xts)
library(plyr)
library(ggplot2)

# Read in data ----------------------------------------------------------------

xRaw <- file.path("./data/grvniva3.txt")

rawdata <- read.table(xRaw, header=TRUE,strip.white=TRUE
                      ,colClasses=c(rep("factor",3),"Date","NULL","numeric","NULL"))

# split wells to list object for aggregation purposes
r <- split(rawdata,rawdata$Omr_stn)


# Transform data to xts and monthly, yearly max -------------------------------

rXtsM <- vector("list", length(r))
names(rXtsM) <- names(r)

rXtsY <- vector("list", length(r))
names(rXtsY) <- names(r)

rXtsYo <- vector("list", length(r))
names(rXtsYo) <- names(r)

rXtsYDate <- vector("list", length(r))
names(rXtsYDate) <- names(r)

rXts <- vector("list", length(r))
names(rXts) <- names(r)



for(i in 1:length(r)){ 
  # make list items to xts objects
    rXts[[i]] <- xts(r[[i]]$m_o_h,as.Date(r[[i]]$Datum))
    colnames(rXts[[i]]) <- 'm_o_h'

  # aggregated monthly values  
    rXtsM[[i]] <- apply.monthly(rXts[[i]],max)
    
      
  # get rid of all measurements shorter than 40 years == 480 months  
    if(length(rXtsM[[i]]) >= 480){
      # get original date, which can be replaced after shifting year
      rXtsYDate[[i]] <- index(rXtsM[[i]])
      
      # without lag
      rXtsYo[[i]] <- apply.yearly(rXtsM[[i]],max)
      # get rid of NA
      rXtsYo[[i]] <- rXtsYo[[i]][!is.na(rXtsYo[[i]])]   
      
      
      # aggregated yearly values with 3 months lag to
      # get hydrologic year
      rXtsY[[i]] <- apply.yearly(lag(rXtsM[[i]],3),max)
      # get rid of NA
      rXtsY[[i]] <- rXtsY[[i]][!is.na(rXtsY[[i]])] 
      ###
      ###   TODO!!! shift the date 
      ###
    } 
    

}

# get rid of empty lists
rXtsY <- rXtsY[rXtsY != "NULL"]
rXtsYo <- rXtsYo[rXtsYo != "NULL"]
rXtsYDate <- rXtsYDate[rXtsYDate != "NULL"]


# Check out difference hydrologic year ----------------------------------------

# preallocate list for difference
rXtsYo_ <- vector("list",length(rXtsY))
rYdiff <- vector("list",length(rXtsY))
names(rYdiff) <- names(rXtsY)

for(i in 1:length(rXtsY)){
  
  # get rid of
  if(length(rXtsYo[[i]]) == (length(rXtsY[[i]])+1)){
    rXtsYo_[[i]] <- rXtsYo[[i]][1:(length(rXtsYo[[i]])-1),] 
  } else if(length(rXtsYo[[i]]) == (length(rXtsY[[i]])+2)){
    rXtsYo_[[i]] <- rXtsYo[[i]][2:(length(rXtsYo[[i]])-1),]
  }
#   print(paste("lagged",names(rXtsY[i]),length(rXtsY[[i]])))
#   print(paste(names(rXtsYo[i]),length(rXtsYo[[i]])))
  # calculate difference
  rYdiff[[i]] <- as.numeric(rXtsY[[i]]) - as.numeric(rXtsYo_[[i]])
  
}

rm(rXtsYo_)

# How strong is influence of hydrologic-year-principle?
rYdiffS <- ldply(rYdiff, sum)
# Higher accumulated peaks with hydrologic year shift?
dim(rYdiffS[which(rYdiffS$V1 >= 0),])[1]
# --> 61 out of 143 ~ 43% are positive that means less than half is larger when shifted
# How about variance?
var(rYdiffS[which(rYdiffS$V1 >= 0),2])
# Variance for larger when shifted: 0.431 m^2
# Variance for smaller when shifted: 1.203 m^2


# export stations that are larger with a shift
zz <- rYdiffS[which(rYdiffS$V1 >= 0),]
colnames(zz) <- c("Omr_stn","diff")
rYLargWShift <- join(m, zz, by="Omr_stn", type="inner")
write.table(rYLargWShift,file="./data/rYLargWShift.csv",sep="\t",row.names=F, quote=F)


# export stations that are smaller with a shift
yy <- rYdiffS[which(rYdiffS$V1 <= 0),]
colnames(yy) <- c("Omr_stn","diff")
rYSmallWShift <- join(m, yy, by="Omr_stn", type="inner")
write.table(rYSmallWShift,file="./data/rYSmallWShift.csv",sep="\t",row.names=F, quote=F)


# Reformat xts to data frame --------------------------------------------------


rDfM <- vector("list", length(rXtsM))
names(rDfM) <- names(rXtsM)
# turn monthly xts to data.frame plottable to month-year boxplot
for(i in 1:length(rXtsM)){
  rDfM_ <- as.data.frame(aggregate(rXtsM[[i]],as.yearmon, mean,na.pass=TRUE))
  rDfM[[i]] <- data.frame(Omr_stn = rep(names(rXtsM[i]),
                                           length(dim(rDfM_)[1])),
                             Month = factor(format
                                            (as.yearmon(rownames(rDfM_),
                                                        "%b %Y"), "%b"),
                                            levels=c("jan","feb","mar",
                                                     "apr","maj","jun",
                                                     "jul","aug","sep",
                                                     "okt","nov","dec")),
                             Year = format(as.yearmon(rownames(rDfM_), 
                                                      "%b %Y"), "%Y"),
                             m_o_h = rDfM_[,1])
}
rm(rDfM_)

K=4
# plot boxplot of aggregated month
ggplot(rDfM[[K]], aes(x=Month, y=m_o_h)) + 
  geom_boxplot() + 
#   geom_jitter() +
  ggtitle(paste("station", names(rDfM[K]))) +
  xlab("månad") + ylab("m.ö.h.") 



# turn yearly xts to data.frame for further manipulation
rDfY <- lapply(rXtsY,as.data.frame)


# rename and create columns for better access and readability
for(i in 1:length(rDfY)){
  rDfY[[i]] <- data.frame(m_o_h = rDfY[[i]]$m_o_h, 
                          tid = as.Date(rownames(rDfY[[i]])), 
                          Omr_stn = names(rDfY[i]))
}

# Save yearly values longer than 40 years also as list with data.frame
rLDfY <- rDfY
# turn list of data frames to data frame
rDfY <- rbind.fill(rDfY)


# Compute 40 year return level ------------------------------------------------
library(lmom)
library(ggplot2)

points <- vector("list", length(dfRibb))
ribbons <- vector("list", length(rLDfY))
plots <- vector("list", length(rLDfY))
time <- vector("list", length(rLDfY))

proc.time()
for(l in 36:length(rLDfY)){


  zz = 1
  iter = 10
  rYPermMaxx <- vector("list", iter)
  rYPermMinn <- vector("list", iter)
  
  while(zz <= iter){

  
    K <- c(5,10,12,15,20,30)
    rYPerm <- vector("list", length(K))
    names(rYPerm) <- as.character(K)
    kvant <- seq(0.1, 0.995, 0.005)
    permut <- 100000
    
    
    # Create list with samples for K
    for (i in 1:length(K)){
      rYPerm[[i]] <- suppressWarnings(matrix(sample(rLDfY[[l]]$m_o_h), permut, K[i]))
#       rYPerm[[i]] <-  data.frame(apply(temp_,1,sort))
    #    rYPerm[[i]] <- temp_[!duplicated(temp_),]
    }
    #rm(temp_)
    
    # Compute L-moments
    rYPermLm <- lapply(rYPerm, function(x) tryCatch(apply(x,1,samlmu),
                                                    error=function(e) NULL))
    
    # Compute GEVparameters to l-moments 
    rYPermGEVPL <- lapply(rYPermLm, function(x) tryCatch(apply(x,2,pelgev),
                                                      error=function(e) NULL))
    # Compute GEVquantiles
    rYPermGEVLQ <- lapply(rYPermGEVPL, 
                       function(x) tryCatch(apply(x,2,function(y) quagev(kvant,y)),
                                                                  error=function(e) NULL))
    
    
    # prepare ribbon for plotting etc.
    rYPermMax <- vector("list", length(K))
    names(rYPermMax) <- as.character(K)
    rYPermMin <- vector("list", length(K))
    names(rYPermMin) <- as.character(K)
    
    
    #error workaround:
#     rYPermLQ <- rYPermLQ[rYPermPL != "NULL"]
    
    # get max/min value from all random permuts
    for(i in 1:length(K)){
        rYPermMax[[i]] <- tryCatch(apply(rYPermGEVLQ[[i]],1,max), 
                                   error=function(e) NULL)
        rYPermMin[[i]] <- tryCatch(apply(rYPermGEVLQ[[i]],1,min),
                                   error=function(e) NULL)
    }
    proc.time()
    
    rYPermMaxx[[zz]] <- rYPermMax
    rYPermMinn[[zz]] <- rYPermMin
    
    print(paste(zz,"th's iteration in ", l, "th well"))
    
    
    zz = zz + 1
    
  }

  # get max and min from rYPermMaxx etc.
  tempa_ <- vector("list", length(iter))
  tempi_ <- vector("list", length(iter))
  
  rYPermMa <- vector("list", length(K))
  names(rYPermMa) <- names(rYPerm)
  rYPermMi <- vector("list", length(K))
  names(rYPermMi) <- names(rYPerm)
  
    
  for(j in 1:length(K)){
    for(i in 1:iter){
      # save all max/min of all iters and one measuring length in one list
      tempa_[[i]] <- as.data.frame(rYPermMaxx[[i]][j])
      tempi_[[i]] <- as.data.frame(rYPermMinn[[i]][j])
      }
    
    # get max/min of all iters, by combining all and getting max from every row
    # for every K
    
    rYPermMa[[j]] <- tryCatch(apply(do.call(cbind,tempa_),1,max), 
                              error=function(e) NULL)
    rYPermMi[[j]] <- tryCatch(apply(do.call(cbind,tempi_),1,min), 
                              error=function(e) NULL)
  }


  # Preparing for plotting

  observ <- sort(rLDfY[[l]]$m_o_h)
  n <- length(observ)

  
  # Data points are plotted at the Gringorten plotting position, 
  # i.e. the i'th smallest of n data points is plotted at the horizontal 
  # position corresponding to nonexceedance probability
  
  points[[l]] <- data.frame(obs = observ, 
                      kvantil = ((1:n)-0.44)/(n+0.12),
                      redkv = -log(-log(((1:n)-0.44)/(n+0.12))))

  
  

  
    ribbons[[l]] <- data.frame(kvantil = kvant,
               redkv = -log(-log(kvant)),
               GEV = quagev(kvant, pelgev(samlmu(rLDfY[[l]]$m_o_h))),
               LP3 = exp(quape3(kvant, pelpe3(samlmu(log(rLDfY[[l]]$m_o_h))))),
               max5 = ifelse(rep(is.null(rYPermMa[[1]]),180),NA,rYPermMa[[1]]), 
               min5 = ifelse(rep(is.null(rYPermMi[[1]]),180),NA,rYPermMi[[1]]),
               max10 = ifelse(rep(is.null(rYPermMa[[2]]),180),NA,rYPermMa[[2]]), 
               min10 = ifelse(rep(is.null(rYPermMi[[2]]),180),NA,rYPermMi[[2]]),
               max12 = ifelse(rep(is.null(rYPermMa[[3]]),180),NA,rYPermMa[[3]]),
               min12 = ifelse(rep(is.null(rYPermMi[[3]]),180),NA,rYPermMi[[3]]),
               max15 = ifelse(rep(is.null(rYPermMa[[4]]),180),NA,rYPermMa[[4]]), 
               min15 = ifelse(rep(is.null(rYPermMi[[4]]),180),NA,rYPermMi[[4]]),
               max20 = ifelse(rep(is.null(rYPermMa[[5]]),180),NA,rYPermMa[[5]]), 
               min20 = ifelse(rep(is.null(rYPermMi[[5]]),180),NA,rYPermMi[[5]]),
               max30 = ifelse(rep(is.null(rYPermMa[[6]]),180),NA,rYPermMa[[6]]),
               min30 = ifelse(rep(is.null(rYPermMi[[6]]),180),NA,rYPermMi[[6]]))
  
  
  

#   library(ggplot2)
    plots[[l]] <- ggplot(ribbons[[l]], aes(x = redkv, y = GEV)) +
                    geom_ribbon(aes(ymin = min5, ymax = max5),
                                alpha = 0.8, fill = "#FDE0DD") +
                    geom_ribbon(aes(ymin = min10, ymax = max10),
                                alpha = 0.8, fill = "#FCC5C0") +
                    geom_ribbon(aes(ymin = min12, ymax = max12),
                                alpha = 0.8, fill = "#FA9FB5") +
                    geom_ribbon(aes(ymin = min15, ymax = max15),
                                alpha = 0.8, fill = "#F768A1") +
                    geom_ribbon(aes(ymin = min20, ymax = max20),
                                alpha = 0.8, fill = "#DD3497") +
                    geom_ribbon(aes(ymin = min30, ymax = max30),
                                alpha = 0.8, fill = "#AE017E") +
    geom_line(aes(x = redkv, y = GEV), colour = "#7A0177", size = 1.2) +
    geom_line(aes(x = redkv, y = LP3), colour = "#000000", size = 1.2) +
    geom_point(data = points[[l]], aes(x = redkv, y = obs), colour = "#FFF7F3") +
    scale_x_continuous(breaks = -log(-log(c(.5, .8, .9, .95, .98, .99, .995))),
                        labels = c(2, 5, 10, 20, 50, 100, 200), 
                       limits=c(-log(-log(.1)),-log(-log(.9951)))) +
    xlab("återkomsttid") + 
    ylab("återkomstnivå") +
    theme_bw() +
    scale_fill_manual(values=c("#FDE0DD","#FCC5C0", "#FA9FB5", "#F768A1","DD3497","AE017E"), 
                      name="Mättid",
                      breaks=c("ymin5","ymin10", "ymin12", "ymin15", "ymin20", "ymin30"),
                      labels=c("5 år","10 år", "12 år", "15 år", "20 år", "30 år")) 
 


  
  if(l != 1){
    proc.time() - time[[1]]
  }
  time[[l]] <- proc.time()
}




z = 84
kvRibb <- c(177,179,180)
dfRibb_ <- vector("list",3)
dfRibb <- vector("list",z)

for(i in 1:z){
  for(j in 1:3){
    # Compute percentage
    ma5 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],4])*-1
      #       / ribbons[[i]][kvRibb[j],3]*-100
    mi5 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],5])
      #       ribbons[[i]][kvRibb[j],3]*100
    ma10 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],6])*-1
    mi10 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],7])
    ma12 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],8])*-1
    mi12 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],9])
    ma15 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],10])*-1
    mi15 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],11])
    ma20 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],12])*-1
    mi20 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],13])
    ma30 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],14])*-1
    mi30 <- (ribbons[[i]][kvRibb[j],3] - ribbons[[i]][kvRibb[j],15])
    if(j==1) RL=50
    if(j==2) RL=100 
    if(j==3) RL=200
    dfRibb_[j] <- data.frame(c(ma5,mi5,ma10,mi10,ma12,mi12,
                               ma15,mi15,ma20,mi20,ma30,mi30,RL))
  }
  dfRibb[[i]] <- do.call(rbind, dfRibb_)
  colnames(dfRibb[[i]]) = c("max5","min5","max10","min10","max12","min12",
                            "max15","min15","max20","min20","max30","min30", "Återkomsttid")
  
}

library(plyr)
library(reshape2)
dfRibb <- lapply(dfRibb,as.data.frame)
dfRibb <- rbind.fill(dfRibb)

# melt dataframe to get into decent format for plotting
dfRibbM <- melt(dfRibb, id.var="Återkomsttid")

# subset to get only underestimation of values
dfRibbMin <- dfRibbM[which(dfRibbM$variable %in% c("min5","min10",
                                      "min12","min15","min20","min30")),]
dfRibbMin$Återkomsttid <- factor(dfRibbMin$Återkomsttid)

# attach name of well
dfRibbMin$rör <- factor(rep(names(rLDfY[1:z]),each=3))


# remap the measurement length
dfRibbMin$variable <- revalue(dfRibbMin$variable, c("min5"="5 år",
                                                    "min10"="10 år",
                                                    "min12"="12 år",
                                                    "min15"="15 år",
                                                    "min20"="20 år",
                                                    "min30"="30 år"))


# remove outliers
sub_dfRibbMin <- dfRibbMin[which(!dfRibbMin$rör %in% c("19_3","24_1","37_34","39_1",
                                                       "1_7","1_8","14_6","17_1",
                                                       "19_5","3_4","33_4")),]



# BOXPLOT of possible underestimation
ggplot(data=sub_dfRibbMin, aes(x=variable, y=value, colour=Återkomsttid)) +
  geom_boxplot() +
  facet_wrap(~Återkomsttid, scales="fixed") +
  xlab("mättid") + ylab("underskattning [m]") +
  scale_colour_manual(breaks = c(50, 100, 200),
                      labels = c("50 år", "100 år", "200 år"),
                                 values = c("#000000", "#000000", "#000000"))
 

dfRibbMin[which(dfRibbMin$value >1.5 & dfRibbMin$variable=="5 år"),]

# Underestimation of values by year and Return Period
undersk50 <- data.frame()


# alle rowbinden, dann jedes dritte meanen bzw
lapply(dfRibb, function(x) apply(x, 2, ))
