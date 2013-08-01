# FUNCTION TO DRAW HISTOGRAM OF DATA WITH EMPIRICAL AND FITTED DENSITITES
# data  = values to be fitted
# func  = name of function to fit (e.g., 'norm', 'gumbel' etc.)
# start = named list of parameters to pass to fitting function 
hist_with_density = function(data, func, start = NULL){
  # load libraries
  library(VGAM); library(fitdistrplus); library(ggplot2)
  
  # fit density to data
  fit   = fitdist(data, func, start = start)
  args  = as.list(fit$estimate)
  dfunc = match.fun(paste('d', func, sep = ''))
  
  # plot histogram, empirical and fitted densities
  p0 = qplot(data, geom = 'blank') +
    geom_line(aes(y = ..density..,colour = 'Empirical'),stat = 'density') +
    stat_function(fun = dfunc, args = args, aes(colour = func))  +
    geom_histogram(aes(y = ..density..), alpha = 0.8) +
    scale_colour_manual(name = '', values = c('red', 'blue')) + 
    theme(legend.position = 'top', legend.direction = 'horizontal')
#     theme_minimal()
  return(p0)  
}
# 
# data1 = sample(10:50,1000,rep=TRUE)
# (hist_with_density(x$m_o_h, 'weibull'))
# 
# data2 = rnorm(1000, 2, 1)
# (hist_with_density(data2, 'norm'))