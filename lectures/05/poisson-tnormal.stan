data {
  int n;
  int y[n];
  real mu0;
  real s0;
}

parameters {
  real<lower=0> lambda;
}

model {
 y ~ poisson(lambda); 
 lambda ~ normal(mu0, s0)T[0,];
}
