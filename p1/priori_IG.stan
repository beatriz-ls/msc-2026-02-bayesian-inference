data {
  int<lower=1> n;
  vector[n] x;

  real<lower=0> alpha;
  real<lower=0> beta;
}

parameters {
  real<lower=0> sigma2;
}

model {
  // Priori
  sigma2 ~ inv_gamma(alpha, beta);

  // Verossimilhanca
  x ~ normal(0, sqrt(sigma2));
}

