data {
  int n;
  int y[n];
}

parameters {
  real<lower=0,upper=1> theta;
}

model {
 y ~ bernoulli(theta); 
 theta ~ beta(1.0, 1.0);
}
