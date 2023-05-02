
data { 
  int N; // total number of rows
  int NITEMS; // how many different items
  int accuracy[N];
  int which_item[N];
  real rt[N];
}

parameters {
  real difficulty[NITEMS];
  real b0;
  real b1;
  real<lower=0> noise; 
}


model {
  
  difficulty ~ normal(0,3);
  
  b0 ~ normal(0,10);
  b1 ~ normal(0,10);
  noise ~ exponential(0.1); // NOTE: the prior on noise should depend on our measurement scale
  
  for(i in 1:N) {
      real p = inv_logit(-difficulty[which_item[i]]); 
      accuracy[i] ~ bernoulli(p);

      rt[i] ~ normal(b0 + b1*difficulty[which_item[i]], noise);
  }
  
}