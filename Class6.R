
library(rstan)

# simple beta-binomial model:
#fit <- stan("Class6a.stan", data=list(h=10, t=5), iter=5000, chain=2)

# Assignment 4 (in Class6b.stan):

W=c(5,5,9,8,1)
L=c(3,2,4,7,1)
E=c(0.6,0.63,0.67,0.61,0.5)

stan.data <- list(N=length(W), W=W, L=L, E=E)
fit <- stan("Class6b.stan", data=stan.data, iter=5000, chain=2)