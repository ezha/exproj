

# POT with all values

# TODO
# 1. Create POT from 143 wells with lowest of five maximas as 
# lowest threshold + declustering
library(POT)
library(xts)

r.xts <- vector("list", length(r))
names(r.xts) <- names(r)

# Preallocate list for the new monthly aggregated xts
r.xts.m <- vector("list", length(r))
rDfM <- vector("list", length(r))

# Preallocate list for the new monthly aggregated regular ts
r.ts.m <- vector("list", length(r))
names(r.ts.m) <- names(r)

# Produce monthly data from
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
}

plot(rDfM[[2]]$m_o_h)
x <- rDfM[[6]]$m_o_h
xdf <- data.frame(time=rDfM[[6]]$tid,
                  obs=rDfM[[6]]$m_o_h)

tcplot(x, u.range = c(22.8, 23.2))

u = 23.05

mrlplot(x, u.range = c(22.6, 23.3))

u = 23.05

events <- clust(xdf, u = 23.05, tim.cond = 6, clust.max = TRUE, plot=TRUE)
diplot(events, u.range = c(22, 23.5))

npy = length(events[,"obs"]) / (diff(range(as.numeric(xdf$time), na.rm = TRUE))/356.25)

plot(fitgpd(events[,"obs"], 23.05, "mle"), npy=npy)

fitr6 <- fitgpd(events[,"obs"], 23.05, "mle")

clust(data, 4, 12, clust.max = TRUE)
flows <- obs
date <- time
diplot