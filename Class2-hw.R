# Consider a version of the coin-flipping model we used in class with the following hypotheses:
# H1 – Fair coin
# H2 – Coin that is weighted 75% heads
# H3 – Coin that is weighted 75% tails
# H4 – Coin that (somehow) always alternates between heads and fair flips
# (e.g. heads, fair flip, heads, fair flip, etc)
# Suppose the model is given data HTHTHT.... (alternating heads and tails of length n).
# 1) Choose priors on H1-H4 that seem reasonable and explain why.
 
#         fair    75h      75t    alternate
prior <- c(0.99, 0.01/3, 0.01/3, 0.01/3)
#           0.65, 0.15,  0.15,   0.05

# def likelihood(n):
# H
likelihood <- function(n) {
  return(c(0.5**n
           ,0.75**ceiling(n/2) * 0.25**floor(n/2)
           ,0.25**ceiling(n/2) * 0.75**floor(n/2)
           ,0.5**floor(n/2)
           ))
}


# P(H|D) = P(D|H) P(H) / P(D)

posterior <- function(n) {
  v <- prior * likelihood(n)
  return(v/sum(v))
}

# Plot:
D <- NULL
for(n in 1:25) {
  D <- rbind(D, posterior(n))
}

# 2) Plots the posterior probability of H1-H4 as a function of the length of the data n.
# 3) Compare to a plot where H1-H4 are given equal (uniform) priors of 1⁄4 each. Explain what
# differences you see.



# Log versions:
log.prior <- log(c(0.99, 0.01/3, 0.01/3, 0.01/3))

# def likelihood(n):
log.likelihood <- function(n) {
  return(c(log(0.5)*n
           ,log(0.75)*ceiling(n/2) + log(0.25)*floor(n/2)
           ,log(0.25)*ceiling(n/2) + log(0.75)*floor(n/2)
           ,log(0.5)*floor(n/2)
  ))
}

# logsumexp(c(a,b)) is the log of the sum of exp(a)+exp(b)
# think of a,b as log probablities, and logsumexp(c(a,b)) is 
# the log of their summed probability which is NOT the sum
# of their log probabilities
logsumexp <- function(x) {
  m <- max(x)
  return(log(sum(exp(x-m)))+m)
}

#log(sum(exp(x-m)))+m
#log(exp(m)*sum(exp(x-m)))
#log(sum(exp(m)*exp(x-m)))
#log(sum(exp(x-m+m)))
#log(sum(exp(x)))

# log(P(H|D)) = log(P(D|H)) + log(P(H)) - log(P(D))
log.posterior <- function(n) {
  v <- log.prior + log.likelihood(n)
  return(v-logsumexp(v)) # use logsumexp
}

# Plot:
D <- NULL
for(n in 1:25) {
  D <- rbind(D, exp(log.posterior(n)))
}

