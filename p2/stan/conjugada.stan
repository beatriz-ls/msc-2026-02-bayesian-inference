
data {
  int<lower=1> N;
  int<lower=1> P;
  matrix[N, P] X;
  vector[N] y;

  vector[P] b0;
  vector<lower=0>[P] escala_beta;
  real<lower=0> a0;
  real<lower=0> d0;
}

parameters {
  vector[P] beta;
  real<lower=0> sigma2;
}

model {
  // Distribuição a priori da variância
  sigma2 ~ inv_gamma(a0, d0);

  // Distribuição a priori conjugada dos coeficientes
  beta ~ normal(b0, sqrt(sigma2) * escala_beta);

  // Verossimilhança normal
  y ~ normal(X * beta, sqrt(sigma2));
}
