
# Filter and plot fyllnadsgrad --------------------------------------------
# 'x' is data as TS!!, 'int' is the intervall choosen to extract every int-nth value
fyllg<-function(x,int){
  b <- x[seq(1, length(x), int)]
  # scale data and turn it into percent
  b.norm <- (sort(b, decreasing=F) - min(b,na.rm=T))/(max(b,na.rm=T) - min(b,na.rm=T))*100 
  # range(b.norm)
  plot(x=seq(0,100,100/(length(b)-1)), y=b.norm, type="s", asp=1, xlab="Andel nivådata [%]",ylab="Fyllnadsgrad [%]")
}