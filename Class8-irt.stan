// Item response theory model 
// Main idea: each item has a difficulty and each subject has an "ability"

data { 
  int N; // total number of rows
  int NITEMS; // how many different items
  int NSUBJ;
  int accuracy[N];
  int which_item[N];
  int which_subject[N];
}

parameters {
  real<lower=0> difficulty[NITEMS];
  real subject_effect[NSUBJ];
  real<lower=0> subject_sd;
  
  real<lower=0> b;
}


model {
  
  difficulty ~ normal(0,1);
  subject_effect ~ normal(0,1);
  subject_sd ~ exponential(1);
  
  b ~ normal(0,3);
  
  for(i in 1:N) {
      real p = inv_logit(b*(-difficulty[which_item[i]] + subject_sd*subject_effect[which_subject[i]])); 
      accuracy[i] ~ bernoulli(p);
  }
  
}
