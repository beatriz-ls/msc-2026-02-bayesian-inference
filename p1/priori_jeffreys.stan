data {
  int<lower=1> n;
  vector[n] x;
}

parameters {
  real<lower=0> sigma2;
}

model {
  // Priori de Jeffreys:
  // pi(sigma2) proporcional a 1 / sigma2
  target += -log(sigma2);

  // Verossimilhanca
  x ~ normal(0, sqrt(sigma2));
}

