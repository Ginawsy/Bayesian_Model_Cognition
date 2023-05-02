
// Regression with subject effects 
// "partial pooling"
// full pooling: no subject effects (e.g. lm)
// no pooling:  independent regressions for each subject 

data { 
  int N; // how many data points we have
  int NSUBJ; // how many subjects
  real x[N];
  real y[N];  
  int subject[N]; // which subject gave the i'th answer
}

parameters { 
  
  real b0; // group level parameters
  real b1;
  
  real subj_b0[NSUBJ]; // the i'th subject's adjustment to b0
  real subj_b1[NSUBJ]; // the i'th subject's adjustment to b1
  
  real<lower=0> scale_b0;
  real<lower=0> scale_b1;
  
  real<lower=0> noise;
}

model { 
  // P(b0,b1,noise | Days, Reaction)
  
  b0 ~ normal(0, 1);
  b1 ~ normal(0, 1);
  
  scale_b1 ~ exponential(1);
  scale_b0 ~ exponential(1);
  noise ~ exponential(1);
  
  subj_b0 ~ normal(0,1);
  subj_b1 ~ normal(0,1);

  // likelihood 
  for(i in 1:N) {
    //use slope and intercept specific to subject[i] for y[i]:
    
    // want this subject's intercept to be b0+subj_b0[subject[i]]
    //                     slope     to be b1+subj_b1[subject[i]]
    y[i] ~ normal( (b0+subj_b0[subject[i]]*scale_b0) + 
                   (b1+subj_b1[subject[i]]*scale_b1)*x[i], noise);
  }
}

// a version that doesn't work as well in implementation:
//  subj_b0 ~ normal(b0,1);
//  subj_b1 ~ normal(b1,1);
//  y[i] ~ normal( subj_b0[subject[i]] + subj_b1[subject[i]]*x[i], noise); 

