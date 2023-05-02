logsumexp <- function(x) {
  m <- max(x)
  return(log(sum(exp(x-m)))+m)
}

## NOTE: This is not a great way to do it because it assumes that 
## we have passed in all hypotheses (for the logsumexp to work)
log.prior <- function(x) { 
  #v <- dnorm(x,2,0.5,log=T)
  v <- rep(0,length(x))
  return(v - logsumexp(v)) # The prior is Normal(2,1)
}

log.likelihood <- function(data,x) { # e.g. log.likelihood(c(3.2, 4.1), 1.5)
  # take a vector of hypotheses x and return a vector of log likelihoods for data
  
  l <- rep(0, length(x))
  for(d in data) {
    l <- l + dnorm(d, x, 1, log=T) # likelihood is Normal(0,1) for each data point
  }
  
  return(l)
}

log.posterior <- function(x,d) {
  v <- log.prior(x) + log.likelihood(d,x)
  return(v - logsumexp(v))
}

# define a hypothesis space
STEP <- 0.1
h <- seq(-10,10,STEP)

data <- c(7.3, 7.2)
plot(h, exp(log.prior(h)), type="l", ylim=c(0,.2))
lines(h, exp(log.likelihood(data,h)), type="l", col=4)
lines(h, exp(log.posterior(h,data)), type="l", col=2)


