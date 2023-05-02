library(tidyr)
# Leftover demo from last time:
#library(gtools) # for rdirichlet
#rdirichlet(1, c(1,1,1,1))

logsumexp <- function(x) {
  m <- max(x)
  return(log(sum(exp(x-m)))+m)
}

# A simple version of the number game:
# Ingredients:
# - A list of hypotheses (kinds of numbers people know)
# - A prior
# - A "size principle" likelihood

N <- 100 # upper range for numbers
# Here is our list of hypotheses:
H <- list( seq(0,N,1)
           ,seq(1,N,2)
           ,seq(0,N,2)
           ,seq(0,N,5)
           ,seq(0,N,10)
           ,seq(0,N,3)
           ,seq(0,N,7)
           ,seq(0,N,11)
           ,c(1,4,9,16,25,36,49,64,81)
           ,c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97)
           ,c(2,4,8,16,32,64)
      )

data <- c(16)

log.prior <- rep(0, length(H))

# Implement a "size principle" likelihood P(data|H[[i]])
# HINT: what log probability should we use when the data is not in H?
log.likelihood <- rep(NA, length(H))
for(i in 1:length(H)) {
  # first check if H[[i]] cover all of the data
  if(all(is.element(data, H[[i]]))) {
    log.likelihood[i] <- - length(data) * log(length(H[[i]]))
  }
  else {
    log.likelihood[i] <- -Inf
  }
}

log.posterior <- log.prior + log.likelihood
log.posterior <- log.posterior - logsumexp(log.posterior) # normalized

# Let's make a posterior predictive plot

# predictive[n] is the probability that n is in the target concept
predictive <- rep(0,N) 
for(n in 1:N) {
  # predictive[n] is the model's prediction about n (which is a weighted
  # average over hypotheses)
  
  for(i in 1:length(H)) { # sum over hypotheses
    predictive[n] <- predictive[n] + ifelse(is.element(n,H[[i]]), exp(log.posterior[i]), 0)  
  }
  
}
barplot(predictive, names.arg=1:N)
