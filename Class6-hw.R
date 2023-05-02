
# 1. Using the “number game” domain and hypotheses from Class 4, implement a
# Metropolis sampler. Be sure to keep everything as log probabilities. You can
# use a proposal distribution that (unlike in class) does not depend on the
# current hypothesis, but proposes uniformly between all hypotheses. Implement
# this with “burn in” (one chain is fine).

# (a) Plot a histogram of the estimated posterior probability for each
# hypothesis for the dataset D=10,20.

logsumexp <- function(x) {
  m <- max(x)
  return(log(sum(exp(x-m)))+m)
}

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

data <- c(10,20)

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

current <- 1 # my current hypothesis (index into H)
samples <- NULL 
while(length(samples) < 1000) {
  proposed <- sample.int(length(H), size=1)
  
  if(runif(1) < exp(log.posterior[proposed] - log.posterior[current])) {
    current <- proposed
  }
  
  samples <- append(samples, current)
  
}
#
# (b) If people were mentally running one Metropolis step every 200ms, how often
# would they typically change their current guess?
#
# 2. A reviewer for a paper comments that Bayesian models are implausible
# because they must actively represent / hold in mind all hypotheses at the same
# time in order to select between them via Bayes rule. Write a 5-10 sentence
# response to them explaining why they’re wrong.
