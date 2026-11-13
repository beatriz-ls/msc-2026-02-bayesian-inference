
data {
  int<lower=1> N;
  int<lower=2> P;
  matrix[N, P] X;
  vector[N] y;

  // Hiperparâmetros da variância residual
  real<lower=0> a0;
  real<lower=0> d0;

  // Escalas das componentes spike e slab
  real<lower=0> tau_spike;
  real<lower=0> tau_slab;
}

parameters {
  // Coeficientes de regressão
  vector[P] beta;

  // Variância residual
  real<lower=0> sigma2;

  // Probabilidade de inclusão desconhecida
  real<lower=0, upper=1> prob_inclusao;
}

model {
  // Priori para a variância residual
  sigma2 ~ inv_gamma(a0, d0);

  // Intercepto sempre incluído no modelo
  beta[1] ~ normal(0, 100);

  // Priori Uniforme(0,1), equivalente a Beta(1,1)
  prob_inclusao ~ beta(1, 1);

  // Priori spike-slab marginalizada
  // Aplicada somente às cinco covariáveis
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
  // Probabilidades condicionais de inclusão
  vector[P-1] p_inclusao;

  // Indicadores de inclusão recuperados posteriormente
  array[P-1] int z;

  for (j in 2:P) {
    real log_slab;
    real log_spike;

    // Log da densidade ponderada do componente slab
    log_slab = log(prob_inclusao)
      + normal_lpdf(beta[j] | 0, tau_slab);

    // Log da densidade ponderada do componente spike
    log_spike = log1m(prob_inclusao)
      + normal_lpdf(beta[j] | 0, tau_spike);

    // Probabilidade condicional P(z_j = 1 | beta_j, p)
    p_inclusao[j-1] = inv_logit(log_slab - log_spike);

    // Gerar o indicador z_j a partir de sua distribuição condicional
    z[j-1] = bernoulli_rng(p_inclusao[j-1]);
  }
}
