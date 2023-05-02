# We’ll be working on sampling algorithms next class. For homework, implement the following algorithm to draw a sample from a beta(a,b) distribution (e.g. you will be implementing rbeta yourself, but you can use dbeta and runif to do it). 
# 
# To do this, we’ll use an old method which can be understood by looking at the plot of the density dbeta(a,b). Find a bound M such that M > dbeta(x,a,b) for all x. You can do this a few ways, but the fastest might be to take the height of dbeta at the mode (we can assume a,b>1 for simplicity). 

# The algorithm is: sample an x-location u uniformly in (0,1), and a y-location v uniformly in (0,M). Keep u as a sample of beta(a,b) if v < dbeta(u,a,b); otherwise toss both u and v, and start again. 
# 
# 1) Draw a picture and think about why this works to draw samples from beta(a,b). Explain in a few sentences why this works. 
# 
# 2) Implement the algorthim (it should only be a few lines) and use it to draw samples from beta(a,b) for a few different shapes of beta distributions. Make sure that histograms of your sampler line up with what is expected from dbeta.

a <- 1
b <- 10

x <- seq(0,1,0.01)
plot(x, dbeta(x,a,b), type="l")

M <- dbeta((a-1)/(a+b-2),a,b)

samples <- NULL
#while(length(samples) < 10000) {
for(s in 1:10000) {
  
  u <- runif(1,min=0,max=1)
  v <- runif(1,min=0,max=M)
  if(v < dbeta(u,a,b)) {
    samples <- append(samples, u)
  }
}

hist(samples, freq=FALSE)
lines(x, dbeta(x,a,b))

# 3) What happens if you make M larger than necessary? Try it and find out. 
