# Function for calculating a multifractal spectrum
# Input:
#   v - vector containing numbers by species
#   Qmin - initial value of q (optional)
#   Qmax - final value of q (optional)
#   step - step of q (optional)
# Return: list consisting of 2 dataframes
# vars - contains the following variables:
#  q - orders of moments
#  M - moments of distribution of individuals by species
#  t - scale indicators
#  D - generalized Renyi dimensions
# mfs - contains the following variables:
#  a - singularity indexes
#  f - the spectrum of singularities
MFS <- function(v, Qmin = -7, Qmax = 5, step = 0.01){ 
  v <- v[v != 0] # remove zero values
  p <- v/sum(v) # relative frequencies
  q <- seq(Qmin, Qmax, step) # vector of orders of moments of distribution of individuals by species
  
  M <- sapply(q, FUN = function(i) sum(p^i)) # moments
  t <- log(M)/log(sum(v)) # scale indicators
  D <- t/(1 - q) # generalized Renyi dimensions
  
  a <- (t[1:(length(t) - 1)] - t[2:length(t)])/step #  discrete derivative of the mass indices (singularity indices)
  f <- q[-1]*a + t[-1] # spectrum of singularities
  
  return(list(vars = data.frame(q = q, M = M, t = t, D = D), mfs = data.frame(a = a, f = f))) 
}

# calculation sample
mf <- MFS(c(1,1,1,10,10,100,100))

# plotting a multifractal spectrum using the ggplot2 library
library(ggplot2)
ggplot(mf$mfs, aes(x = a, y = f)) +
  geom_line() + # line
  # geom_point() + # dots
  labs(x = "\u03B1", y = "f(\u03B1)") + # axis labels
  theme_bw() # theme
ggsave('plot_mfa.jpeg', width = 10, height = 8) # save image

# plotting a multifractal spectrum using R
plot(f ~ a, data = mf$mfs, type = 'l')
