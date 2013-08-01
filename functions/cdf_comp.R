# FUNCTION TO DRAW CDF OF DATA WITH EMPIRICAL AND FITTED DENSITITES
# data  = values to be fitted
# func  = name of function to fit (e.g., 'norm', 'gumbel' etc.)
# start = named list of parameters to pass to fitting function 
cdf_comp = function(data, func, start = NULL){
  # load libraries
  library(VGAM); library(fitdistrplus); library(ggplot2)
  
  # fit density to data
  fit   = fitdist(data, func, start = start)
  args  = as.list(fit$estimate)
  pfunc = match.fun(paste('p', func, sep = ''))
  
  # plot histogram, empirical and fitted densities
  p1 <- qplot(data, geom = 'blank') +
    stat_ecdf(aes(colour = 'Empirical'), geom = "step") +
    stat_function(fun = pfunc, args = args, aes(colour = func)) +
    scale_colour_manual(name = '', values = c('red', 'blue')) + 
    theme(legend.position = 'top', legend.direction = 'horizontal')
  #     theme_minimal()
  return(p1)  
}

# data1 = sample(10:50,1000,rep=TRUE)
# (hist_with_density(data$m_o_h, 'weibull'))