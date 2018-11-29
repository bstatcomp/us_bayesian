data {
  int n;
  int y[n];
}

parameters {
  real<lower=0> lambda;
}

model {
 y ~ poisson(lambda); 
 lambda ~ gamma(2.0, 1.0);
}
