logsumexp <- function(x) {
  m <- max(x)
  return(log(sum(exp(x-m)))+m)
}

#Imagine that a bug encounters colors in the world (e.g. nanometers of wavelength) from a 
#Gamma(14, 0.03) distribution. If you don’t know what a Gamma distribution is, please read 
#about it and plot a few examples via dgamma in R to get familiar. Assume that the bug’s 
#perceptual system is Bayesian, using a normal likelihood with a standard deviation of 50nm. 
#Assume that the bug’s perceptual system uses a prior that equals the real world distribution. 

x <- 1:1000
plot(x, dgamma(x,14,0.03), type="l")

LIKELIHOOD.NOISE <- 5


#It is fine to treat the color space as discrete (e.g. 1,2,3,...1000nm) for everything here.

# 1) Write a function to compute the posterior mean of P(color|d) for any d observed data (wavelength) d.
# Note this is a function of d but not h.

log.prior <- function(h) { dgamma(h,14,0.03,log=T)  }

log.likelihood <- function(data, h) { dnorm(data,h,LIKELIHOOD.NOISE,log=T) }

log.posterior <- function(h,d) { # NOTE: This assumes h is a list of all hypotheses
  v <- log.prior(h) + log.likelihood(d,h)
  return(v - logsumexp(v))
}

posterior.mean <- function(data) {
  H <- 0:1000
  return(sum(H*exp(log.posterior(H,data))))
}

print(posterior.mean(250))

# 2) Plot the absolute difference between the function in (1) and the data d. Explain what you see – what
# predictions does this kind of perception make in terms of how perception relates to the real world
# stimulus? How could you test these predictions?

Q <- NULL
for(d in 1:1000) {
  Q <- rbind(Q, data.frame(d=d, posterior.mean=posterior.mean(d)))
}
plot(Q$d, abs(Q$d-Q$posterior.mean), type="l")

# 3) What is the average distance between the percept and the stimulus? Remember to take into account
# how often the world generates each color, and the fact that a real-world stimulus gets corrupted by
# Normal(0,50) noise, as assumed in the likelihood. You can answer this probably most easily with a
# simulation (e.g. by simulating the world generating a color, then that color being corrupted according to
# the noise in the likelihood, and then perceived via Bayes).

Q <- NULL
for(s in 1:10000) { # simulate this number of perceptual encounters
  
  # draw a real world color
  r <- rgamma(1, 14, 0.03)
  
  # corrupt via a normal
  x <- rnorm(1,r,LIKELIHOOD.NOISE)

  # the bug does inference:
  #estimate <- posterior.mean(x)
  
  # for maximum likelihoods:
  estimate <- x # maximum of normal(x,LIKELIHOOD.NOISE)
  
  Q <- rbind(Q, data.frame(real=r,x=x,estimate=estimate))
}

print(mean(abs(Q$estimate-Q$real)))

# 4) What is the average distance between percept and stimulus for a bug that used maximum likelihood
# estimation?

print(mean(abs(Q$x-Q$real)))