
// Simple linear regression in stan 
//   P(Reaction | b0,b1,Days) ~ Normal(b0+b1*Days,noise)

data { 
  int N; // how many data points we have
  real x[N];
  real y[N];  
}

parameters { 
  real b0;
  real b1;
  
  real<lower=0> noise;
}

model { 
  // P(b0,b1,noise | Days, Reaction)
  
  b0 ~ normal(0, 1);
  b1 ~ normal(0, 1);
  
  noise ~ exponential(1);
  
  // likelihood  P(Reaction | b0,b1,Days, noise) ~ Normal(b0+b1*Days,noise)
  for(i in 1:N) {
    y[i] ~ normal(b0+b1*x[i], noise);
  }
}

