
// Simple introduction to stan modeling

data { // tell me how to receive data from R 
  int h; // get some number of heads
  int t; // some number of tails
}

parameters { // variables that stan will infer from the data
  real<lower=0,upper=1> w;
}

model { // how the variables (h,t,w) are related (likelihood) and what priors they have
  
  // the prior:
  w ~ beta(5,5);  // read as: "w is distributed as a beta(5,5)"
  
  // the likelihood
  h ~ binomial(h+t, w);
}

