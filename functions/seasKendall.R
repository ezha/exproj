larger3 <- which(rSumTable$Laengd>3)

rTsLar3 <- vector("list", length(larger3))
names(rTsLar3) <- names(r[larger3])


rTrend <- ldply
rTrend <- tapply(rTsLar3,SeasonalMannKendall)


<- with(x, tapply(m_o_h, stn, SeasonalMannKendall) 