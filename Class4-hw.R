# A child plays a simple gambling game where on each trial they receive a reward with some unknown probability. After playing for some number of trials (each trial is either a win or a loss), the child verbally reports to you their estimate of the probability of winning. The below vectors give the number of observed wins, losses, and estimates for several iterations of this game, played by the same child on different weeks. Assume each week is independent, except for using the same priors.
# 
W=c(5,5,9,8,1)
L=c(3,2,4,7,1)
E=c(0.6,0.63,0.67,0.61,0.5)

lambda <- 0.1

our.unnormalized.posterior <- function(a,b) {
  sum(dbeta(E,a+W,b+L,log=T)) - lambda*a - lambda*b
}

# If (1) the child was doing Bayesian inference to estimate the proportion of winning, and (2) their estimate is a random sample from their posterior, what should we believe about the child’s priors a and b? 
# 
# Yes, you should do Bayesian inference from this data to discover the priors the child used (e.g. the values of a and b are the “hypotheses” for your inference – the child’s inference is cognitive; yours is a data-analysis inference about their inference.) You may pick reasonable priors for your own inference of a and b. 
# 
# 1) Plot the posterior distribution jointly over a and b in a 2D plot (I used expand.grid and geom_raster in ggplot). Describe the main patterns in your plot and explain them. 

library(purrr)
library(ggplot2)

z <- expand.grid(a=seq(0,10,length.out=100)
                ,b=seq(0,10,length.out=100))
z$unnormalized.posterior <- unlist(pmap(z,our.unnormalized.posterior))

ggplot(z, aes(x=a,y=b,fill=exp(unnormalized.posterior))) + 
    geom_raster()

# 2) Come up with a synthetic dataset which would lead us to believe the child’s prior had b much larger than a, explain why, and test it on your code. 
