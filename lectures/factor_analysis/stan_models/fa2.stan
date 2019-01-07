data {
  int<lower=0> n;
  int<lower=0> m;
  int<lower=0> p;
  matrix[m,n] X;
}

parameters {
  matrix[m,p] Lambda;
  matrix[p,n] Factor;
  real<lower=0> fac_var;
  cov_matrix[m] Psi_full;
}

transformed parameters{
  cov_matrix[m] Psi;
  cholesky_factor_cov[m,p] L_tri; // lower triangular factor loadings Matrix 
  {
    for (i in 1:m) {
      for (j in 1:p) {
        L_tri[i,j] = Lambda[i,j]; // fill with Lambda
      }
    }
    for(i in 1:m){
      for(j in (i+1):p){
        L_tri[i,j] = 0; // constrain the upper triangular elements to zero 
      }
    }
    for (j in 1:p) {
      L_tri[j,j] = 1; // constrain the diagonal
    }
  }
  Psi = Psi_full;
  for (i in 1:m) {
    for (j in 1:m) {
      if (i != j) {
        Psi[i,j] = 0;
      }
    }
  }
}

model {
  fac_var  ~ gamma(0.5, 1);
  Psi_full ~ inv_wishart(m*m, diag_matrix(rep_vector(1,m)));
  for (i in 1:p)
    Lambda[ ,i] ~ multi_normal(rep_vector(0,m), Psi);
  for (i in 1:n) {
    for (j in 1:p) {
      Factor[j,i] ~ normal(0, fac_var);
    }
  }
  for (i in 1:n) 
    X[ ,i] ~ multi_normal(L_tri * Factor[ ,i], Psi);
}
