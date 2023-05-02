

p <- seq(0,1,0.01)

#               a b
plot(p, dbeta(p,0.1, 0.1), type="l")

# A few properties:
# mean is a/(a+b)
# mode is (a-1)/(a+b-2)
# var  is a*b/((a+b)^2(a+b+1))


# Let's do a simple example: primate gambling experiment
# There are two choices

aL <- 1000 # priors for L
bL <- 1000
WL <- 0.2 # true probability of reward on the left

aR <- 1000 # priors for R
bR <- 1000
WR <- 0.8 # true probability of reward on the right

choices <- NULL
for(t in 1:1000) {
  # Let's compute the belief in probability of reward for each choice
  rewardL <- aL / (aL + bL) # mean of left beta
  rewardR <- aR / (aR + bR)
  
  # choice rule says how to choose, given our beliefs
  # For now -- choose proportional to expected reward
  #p.choose.L <- rewardL / (rewardL + rewardR) # "luce choice rule"
  #which <- rbinom(1,1,p.choose.L) # which did I pick on this trial
  
  # Choose according to whether a *sample* from the L belief is 
  # higher than a sample from the R belief = "Thompson sampling"
  which <- 1*(rbeta(1,aL,bL) > rbeta(1,aR,bR))
  
  choices <- append(choices, which) # keep track of what I chose
  
  # Now simulate the outcome
  if(which == 1) { # choose left
    outcome <- rbinom(1,1,WL)
    if(outcome == 1) { aL <- aL + 1} # update belief with the observed outcome
    else             { bL <- bL + 1}
  }
  else { 
    outcome <- rbinom(1,1,WR)
    if(outcome == 1) { aR <- aR + 1} # update belief with the observed outcome
    else             { bR <- bR + 1}
    
  }
}


