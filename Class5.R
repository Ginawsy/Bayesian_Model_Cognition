
## Metropolis algorithm

x <- seq(0,20, 0.01)

# f is a function that is proportional to a distribution 
# (e.g. you don't know the normalizing constant / denominator)
# or f is like P(H) * P(D|H)
f <- function(z) { exp(-(z-8)**2 / 7) + exp( -(3-z)**4 ) }

x11()
plot(x, f(x), type="l")

# Let's sample from the distribution that f is proportional to
samples <- NULL
current <- 10 # this holds the "current" value of the sample
while(length(samples) < 1000) {
  
  # first, we propose a change to current
  proposed <- current + rnorm(1,0,1)
  
  # if f(proposal) > f(current), always take it
  # otherwise take it with probability f(proposal)/f(current)
  if(runif(1) < f(proposed) / f(current)) {
    current <- proposed
  }
  # else, keep current as it is
  # WARNING: You *should* always keep current
  
  samples <- append(samples, current)
}
#x11()
#plot(samples, 1:length(samples), type="l", ylim=c(1,1000))
#hist(samples, freq=FALSE)

# Plot two runs here
plot(samples1, 1:length(samples), type="l", ylim=c(1,1000)) # started from 1
lines(samples, 1:length(samples), col=2)                    # started from 10

# This kind of sampler equates *time* with *probability*

# Usually we do the following:
# - throw out the first samples (maybe 50%) as "burn in" since those depend on starting location
# - run multiple chains and check that they give the same answer
#   (e.g. compute that they have the same means, variances)

# A little bit on why this kind of thing works
# "detailed balance" - a property of an algorithm/process which says that:
#  P(A) M(A->B) = P(B) M(B->A)
# P(A) / P(B) = M(B->A)/M(A->B)
# P is the probability of being in each region
# M is the probability (according to the algorithm) of *moving* between regions

# Why does the Metropolis algorithm satisfy detailed balance?

# Assume first that f(proposed) > f(current):
#  P(current) M(current->proposed) = P(proposed) M(proposed->current)
#  P(current) * 1                  = P(proposed) P(current) / P(proposed)
#  P(current)                      = P(current)

# A few other notes:
# - This version has symmetric proposal Metropolis-Hastings (allows asymmetric proposals)
# - Variants that deal with "temperature"
# - Also variants that use derivative information
# - Also variants that use "momentum" or "energy"

x11()
x <- seq(0,25, 0.01)
g <- function(z) { exp(-(z-18)**2 / 7) + exp( -(3-z)**4 ) }
plot(x, g(x), type="l")
x11()
plot(x, g(x)**(1/100), type="l")

