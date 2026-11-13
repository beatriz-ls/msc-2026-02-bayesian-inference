functions {
  real sbeta2_lpdf(real x, real p, real q, real b) {
    return lgamma(p + q)
           - lgamma(p)
           - lgamma(q)
           - log(b)
           + (p - 1) * log(x / b)
           - (p + q) * log1p(x / b);
  }
}

data {
  int<lower=1> n;
  vector[n] x;

  real<lower=0> p;
  real<lower=0> q;
  real<lower=0> b;
}

parameters {
  real<lower=0> sigma2;
}

model {
  // Priori SBeta2
  sigma2 ~ sbeta2(p, q, b);

  // Verossimilhanca
  x ~ normal(0, sqrt(sigma2));
}

