library(xts)

# Skräp ----------------------
bl<- coredata(rXtsM[[33]])
index((rXtsM[[33]]))
#-----------------------------

rLDfYmin <- ldply(rLDfY, summarise, min= min(m_o_h))

rXts10Y <- vector("list", length(rXtsM))
names(rXts10Y) <- names(rXts)
rDf10Y <- vector("list", length(rXtsM))

for(i in 1:length(rXtsM)){
  if(length(rXtsM[[i]]) >= 120){
    rXts10Y[[i]] <- apply.yearly(lag(rXtsM[[i]],3),max)
    #rXts10Y[[i]] <- rXts10Y[[i]][!is.na(rXts10Y[[i]])] 
    rDf10Y[[i]] <- data.frame(tid = index(rXts10Y[[i]]), 
                            m_o_h = rXts10Y[[i]]$m_o_h, 
                            Omr_stn = names(rXts[i]))
    # get rid of NA values
    
    
  }
}

rXts10Y <- rXts10Y[rXts10Y != "NULL"]
rDf10Y <- rDf10Y[rDf10Y != "NULL"]

# turn yearly xts to data.frame for further manipulation
rDf10Y <- lapply(rDf10Y, function(x) 
  ddply(x, .(Omr_stn), transform, 
        norm = (m_o_h - mean(m_o_h, na.rm=T))/sd(m_o_h, na.rm=T)))

# TODO
# 1. Create POT from 143 wells with lowest of five maximas as 
# lowest threshold + declustering

