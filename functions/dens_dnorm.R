# produce pairs plot to show

x<-as.numeric(rdlist.GW1.TS.mo)
m=mean(x)
s=sd(x)
y=seq(1,length(x),1)

plot(density(x),  main="6010" ,xlim=c(77,84))
curve( dnorm(x, m, s), col='red', add=T)