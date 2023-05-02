
// Simple introduction to stan modeling

data { 
  int N; // how many data points we have
  int W[N]; // W is an array of ints of length N
  int L[N];
  real E[N];
}

parameters { 
  real<lower=0> a;
  real<lower=0> b;
}

model { 
  
  // prior a,b (which determine the kid's priors on the reward probability)
  a ~ exponential(0.1);
  b ~ exponential(0.1);
  
  // likelihood
  for(i in 1:N) {
    E[i] ~ beta(W[i]+a, L[i]+b);
  }
  
}

