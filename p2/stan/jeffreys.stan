
data {
  int<lower=1> N;
  int<lower=1> P;
  matrix[N, P] X;
  vector[N] y;
}

parameters {
  vector[P] beta;
  real<lower=0> sigma2;
}

model {
  // Priori de Jeffreys independente
  // pi(beta, sigma2) proporcional a 1/sigma2
  target += -log(sigma2);

  // Verossimilhança normal
  y ~ normal(X * beta, sqrt(sigma2));
}
