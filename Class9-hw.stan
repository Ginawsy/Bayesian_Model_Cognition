data { 
  int N;
  int n1[N];
  int n2[N];
  int correct[N];
}

parameters {
  real<lower=0> W;
}

model {
  
  W ~ exponential(1);
  
  for(i in 1:N) {
    
    // compute the probability of answering correctly (blue stuff on board)
    // (i.e. your belief that n1 < n2)
    real p_n1Ln2 = exp(normal_lcdf(0 | n1[i]-n2[i], W*sqrt(n1[i]^2+n2[i]^2)));
    
    if(n1[i] < n2[i]) {
      correct[i] ~ bernoulli(p_n1Ln2);
    }
    else {
      correct[i] ~ bernoulli(1-p_n1Ln2);
    }
  }
  
}