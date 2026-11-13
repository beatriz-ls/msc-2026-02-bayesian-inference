
data {
  int<lower=1> N;
  int<lower=2> P;
  matrix[N, P] X;
  vector[N] y;

  real<lower=0> a0;
  real<lower=0> d0;

  real<lower=0> tau_spike;
  real<lower=0> tau_slab;
  real<lower=0, upper=1> prob_inclusao;
}

parameters {
  vector[P] beta;
  real<lower=0> sigma2;
}

model {
  // Priori da variância residual
  sigma2 ~ inv_gamma(a0, d0);

  // Intercepto sempre incluído
  beta[1] ~ normal(0, 100);

  // Priori spike-slab das cinco covariáveis
  for (j in 2:P) {
    target += log_mix(
      prob_inclusao,
      normal_lpdf(beta[j] | 0, tau_slab),
      normal_lpdf(beta[j] | 0, tau_spike)
    );
  }

  // Verossimilhança normal
  y ~ normal(X * beta, sqrt(sigma2));
}

generated quantities {
  vector[P-1] p_inclusao;
  array[P-1] int z;

  // Recuperar os indicadores de inclusão
  for (j in 2:P) {
    real log_slab;
    real log_spike;

    log_slab = log(prob_inclusao)
      + normal_lpdf(beta[j] | 0, tau_slab);

    log_spike = log1m(prob_inclusao)
      + normal_lpdf(beta[j] | 0, tau_spike);

    // Probabilidade condicional de inclusão
    p_inclusao[j-1] = inv_logit(log_slab - log_spike);

    // Gerar indicador de inclusão
    z[j-1] = bernoulli_rng(p_inclusao[j-1]);
  }
}
