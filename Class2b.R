
# German tank problem 
# We see some samples (let's assume uniform) from 1, 2, 3, .... M
# We'd like to know, what is M?

# Q: What's the maximum likelihood estimate? 
# A: max(sample)

# Another way: assume sample mean is true mean
# mean(sample) = mean(1,2,3,...M) = M/2
# M = 2 * mean(sample)

log.prior <- function(x) {
  return(dexp(x, rate=0.001, log=T)) 
}

log.likelihood <- function(data, x) {
  # the likelihood is 1/#x for each item in d, but only if 
  # x > d
  # log(1/M) = -log(M)
  return(ifelse(x>max(data), -log(x)*length(data), -Inf))
}

logsumexp <- function(x) {
  m <- max(x)
  return(log(sum(exp(x-m)))+m)
}

log.posterior <- function(x,d) {
  v <- log.prior(x) + log.likelihood(d,x)
  return(v - logsumexp(v))
}

h <- seq(0,4000)
sample <- c(101, 344, 1200) # likelihood: (1/M)**length(sample)
plot(h, exp(log.posterior(h,sample)), type="l")

# let's compute a posterior mean
print(sum(h*exp(log.posterior(h,sample))))

