library(rstan)      ## rodar MCMC
library(posterior)  ## funções para posteriori

options(mc.cores = parallel::detectCores())

rstan_options(auto_write = TRUE)


# Compilando modelos -----------------------------------------------------------

modelo_IG <- stan_model(
  file = "priori_IG.stan"
)

modelo_jeffreys <- stan_model(
  file = "priori_jeffreys.stan"
)

modelo_sbeta2 <- stan_model(
  file = "priori_sbeta2.stan"
)


# Parâmetros da priori Inversa-Gama -------------------------------------------

alpha <- 2
beta <- 2


# ==============================================================================
# n = 20
# ==============================================================================


# ------------------------------------------------------------------------------
# n = 20 e sigma_R^2 = 0.25
# ------------------------------------------------------------------------------

set.seed(123)

n <- 20
sigma2_real <- 0.25

x_n20_s025 <- rnorm(
  n = n,
  mean = 0,
  sd = sqrt(sigma2_real)
)


# Inversa-Gama -----------------------------------------------------------------

dados_IG_n20_s025 <- list(
  n = n,
  x = x_n20_s025,
  alpha = alpha,
  beta = beta
)

ajuste_IG_n20_s025 <- sampling(
  object = modelo_IG,
  data = dados_IG_n20_s025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)


# Jeffreys ---------------------------------------------------------------------

dados_jeffreys_n20_s025 <- list(
  n = n,
  x = x_n20_s025
)

ajuste_jeffreys_n20_s025 <- sampling(
  object = modelo_jeffreys,
  data = dados_jeffreys_n20_s025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)


# SBeta2(1, 1, 1) --------------------------------------------------------------

dados_sbeta2_111_n20_s025 <- list(
  n = n,
  x = x_n20_s025,
  p = 1,
  q = 1,
  b = 1
)

ajuste_sbeta2_111_n20_s025 <- sampling(
  object = modelo_sbeta2,
  data = dados_sbeta2_111_n20_s025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)


# SBeta2(0.5, 0.5, 1) ----------------------------------------------------------

dados_sbeta2_0505_1_n20_s025 <- list(
  n = n,
  x = x_n20_s025,
  p = 0.5,
  q = 0.5,
  b = 1
)

ajuste_sbeta2_0505_1_n20_s025 <- sampling(
  object = modelo_sbeta2,
  data = dados_sbeta2_0505_1_n20_s025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)


# SBeta2(1, 0.5, 1) ------------------------------------------------------------

dados_sbeta2_105_1_n20_s025 <- list(
  n = n,
  x = x_n20_s025,
  p = 1,
  q = 0.5,
  b = 1
)

ajuste_sbeta2_105_1_n20_s025 <- sampling(
  object = modelo_sbeta2,
  data = dados_sbeta2_105_1_n20_s025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

# ------------------------------------------------------------------------------
# n = 20 e sigma_R^2 = 1
# ------------------------------------------------------------------------------

set.seed(124)

n <- 20
sigma2_real <- 1

x_n20_s1 <- rnorm(
  n = n,
  mean = 0,
  sd = sqrt(sigma2_real)
)

dados_IG_n20_s1 <- list(
  n = n,
  x = x_n20_s1,
  alpha = alpha,
  beta = beta
)

ajuste_IG_n20_s1 <- sampling(
  object = modelo_IG,
  data = dados_IG_n20_s1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_jeffreys_n20_s1 <- list(
  n = n,
  x = x_n20_s1
)

ajuste_jeffreys_n20_s1 <- sampling(
  object = modelo_jeffreys,
  data = dados_jeffreys_n20_s1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n20_s1 <- list(
  n = n,
  x = x_n20_s1,
  p = 1,
  q = 1,
  b = 1
)

ajuste_sbeta2_111_n20_s1 <- sampling(
  object = modelo_sbeta2,
  data = dados_sbeta2_111_n20_s1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n20_s1 <- list(
  n = n,
  x = x_n20_s1,
  p = 0.5,
  q = 0.5,
  b = 1
)

ajuste_sbeta2_0505_1_n20_s1 <- sampling(
  object = modelo_sbeta2,
  data = dados_sbeta2_0505_1_n20_s1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n20_s1 <- list(
  n = n,
  x = x_n20_s1,
  p = 1,
  q = 0.5,
  b = 1
)

ajuste_sbeta2_105_1_n20_s1 <- sampling(
  object = modelo_sbeta2,
  data = dados_sbeta2_105_1_n20_s1,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

# ------------------------------------------------------------------------------
# n = 20 e sigma_R^2 = 4
# ------------------------------------------------------------------------------

set.seed(125)

n <- 20
sigma2_real <- 4

x_n20_s4 <- rnorm(n, 0, sqrt(sigma2_real))

dados_IG_n20_s4 <- list(
  n = n,
  x = x_n20_s4,
  alpha = alpha,
  beta = beta
)

ajuste_IG_n20_s4 <- sampling(
  modelo_IG,
  data = dados_IG_n20_s4,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_jeffreys_n20_s4 <- list(
  n = n,
  x = x_n20_s4
)

ajuste_jeffreys_n20_s4 <- sampling(
  modelo_jeffreys,
  data = dados_jeffreys_n20_s4,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n20_s4 <- list(
  n = n, x = x_n20_s4,
  p = 1, q = 1, b = 1
)

ajuste_sbeta2_111_n20_s4 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_n20_s4,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n20_s4 <- list(
  n = n, x = x_n20_s4,
  p = 0.5, q = 0.5, b = 1
)

ajuste_sbeta2_0505_1_n20_s4 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_0505_1_n20_s4,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n20_s4 <- list(
  n = n, x = x_n20_s4,
  p = 1, q = 0.5, b = 1
)

ajuste_sbeta2_105_1_n20_s4 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_105_1_n20_s4,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

# ------------------------------------------------------------------------------
# n = 20 e sigma_R^2 = 16
# ------------------------------------------------------------------------------

set.seed(126)

n <- 20
sigma2_real <- 16

x_n20_s16 <- rnorm(n, 0, sqrt(sigma2_real))

dados_IG_n20_s16 <- list(
  n = n, x = x_n20_s16,
  alpha = alpha, beta = beta
)

ajuste_IG_n20_s16 <- sampling(
  modelo_IG,
  data = dados_IG_n20_s16,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_jeffreys_n20_s16 <- list(
  n = n,
  x = x_n20_s16
)

ajuste_jeffreys_n20_s16 <- sampling(
  modelo_jeffreys,
  data = dados_jeffreys_n20_s16,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n20_s16 <- list(
  n = n, x = x_n20_s16,
  p = 1, q = 1, b = 1
)

ajuste_sbeta2_111_n20_s16 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_n20_s16,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n20_s16 <- list(
  n = n, x = x_n20_s16,
  p = 0.5, q = 0.5, b = 1
)

ajuste_sbeta2_0505_1_n20_s16 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_0505_1_n20_s16,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n20_s16 <- list(
  n = n, x = x_n20_s16,
  p = 1, q = 0.5, b = 1
)

ajuste_sbeta2_105_1_n20_s16 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_105_1_n20_s16,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

# ==============================================================================
# n = 100
# ==============================================================================


# ------------------------------------------------------------------------------
# n = 100 e sigma_R^2 = 0.25
# ------------------------------------------------------------------------------

set.seed(127)

n <- 100
sigma2_real <- 0.25

x_n100_s025 <- rnorm(n, 0, sqrt(sigma2_real))

dados_IG_n100_s025 <- list(
  n = n, x = x_n100_s025,
  alpha = alpha, beta = beta
)

ajuste_IG_n100_s025 <- sampling(
  modelo_IG,
  data = dados_IG_n100_s025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_jeffreys_n100_s025 <- list(
  n = n,
  x = x_n100_s025
)

ajuste_jeffreys_n100_s025 <- sampling(
  modelo_jeffreys,
  data = dados_jeffreys_n100_s025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n100_s025 <- list(
  n = n, x = x_n100_s025,
  p = 1, q = 1, b = 1
)

ajuste_sbeta2_111_n100_s025 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_n100_s025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n100_s025 <- list(
  n = n, x = x_n100_s025,
  p = 0.5, q = 0.5, b = 1
)

ajuste_sbeta2_0505_1_n100_s025 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_0505_1_n100_s025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n100_s025 <- list(
  n = n, x = x_n100_s025,
  p = 1, q = 0.5, b = 1
)

ajuste_sbeta2_105_1_n100_s025 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_105_1_n100_s025,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  seed = 123
)


# ------------------------------------------------------------------------------
# n = 100 e sigma_R^2 = 1
# ------------------------------------------------------------------------------

set.seed(128)

n <- 100
sigma2_real <- 1

x_n100_s1 <- rnorm(n, 0, sqrt(sigma2_real))

dados_IG_n100_s1 <- list(n = n, x = x_n100_s1, alpha = alpha, beta = beta)

ajuste_IG_n100_s1 <- sampling(
  modelo_IG,
  data = dados_IG_n100_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_jeffreys_n100_s1 <- list(n = n, x = x_n100_s1)

ajuste_jeffreys_n100_s1 <- sampling(
  modelo_jeffreys,
  data = dados_jeffreys_n100_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n100_s1 <- list(
  n = n, x = x_n100_s1,
  p = 1, q = 1, b = 1
)

ajuste_sbeta2_111_n100_s1 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_n100_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n100_s1 <- list(
  n = n, x = x_n100_s1,
  p = 0.5, q = 0.5, b = 1
)

ajuste_sbeta2_0505_1_n100_s1 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_0505_1_n100_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n100_s1 <- list(
  n = n, x = x_n100_s1,
  p = 1, q = 0.5, b = 1
)

ajuste_sbeta2_105_1_n100_s1 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_105_1_n100_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_111_05_n100_s1 <- list(
  n = n, x = x_n100_s1,
  p = 1, q = 1, b = 0.5
)

ajuste_sbeta2_111_05_n100_s1 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_05_n100_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_111_2_n100_s1 <- list(
  n = n, x = x_n100_s1,
  p = 1, q = 1, b = 2
)

ajuste_sbeta2_111_2_n100_s1 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_2_n100_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)


# ------------------------------------------------------------------------------
# n = 100 e sigma_R^2 = 4
# ------------------------------------------------------------------------------

set.seed(129)

n <- 100
sigma2_real <- 4

x_n100_s4 <- rnorm(n, 0, sqrt(sigma2_real))

dados_IG_n100_s4 <- list(n = n, x = x_n100_s4, alpha = alpha, beta = beta)

ajuste_IG_n100_s4 <- sampling(
  modelo_IG,
  data = dados_IG_n100_s4,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_jeffreys_n100_s4 <- list(n = n, x = x_n100_s4)

ajuste_jeffreys_n100_s4 <- sampling(
  modelo_jeffreys,
  data = dados_jeffreys_n100_s4,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n100_s4 <- list(
  n = n, x = x_n100_s4,
  p = 1, q = 1, b = 1
)

ajuste_sbeta2_111_n100_s4 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_n100_s4,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n100_s4 <- list(
  n = n, x = x_n100_s4,
  p = 0.5, q = 0.5, b = 1
)

ajuste_sbeta2_0505_1_n100_s4 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_0505_1_n100_s4,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n100_s4 <- list(
  n = n, x = x_n100_s4,
  p = 1, q = 0.5, b = 1
)

ajuste_sbeta2_105_1_n100_s4 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_105_1_n100_s4,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

# ------------------------------------------------------------------------------
# n = 100 e sigma_R^2 = 16
# ------------------------------------------------------------------------------

set.seed(130)

n <- 100
sigma2_real <- 16

x_n100_s16 <- rnorm(n, 0, sqrt(sigma2_real))

dados_IG_n100_s16 <- list(n = n, x = x_n100_s16, alpha = alpha, beta = beta)

ajuste_IG_n100_s16 <- sampling(
  modelo_IG,
  data = dados_IG_n100_s16,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_jeffreys_n100_s16 <- list(n = n, x = x_n100_s16)

ajuste_jeffreys_n100_s16 <- sampling(
  modelo_jeffreys,
  data = dados_jeffreys_n100_s16,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n100_s16 <- list(
  n = n, x = x_n100_s16,
  p = 1, q = 1, b = 1
)

ajuste_sbeta2_111_n100_s16 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_n100_s16,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n100_s16 <- list(
  n = n, x = x_n100_s16,
  p = 0.5, q = 0.5, b = 1
)

ajuste_sbeta2_0505_1_n100_s16 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_0505_1_n100_s16,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n100_s16 <- list(
  n = n, x = x_n100_s16,
  p = 1, q = 0.5, b = 1
)

ajuste_sbeta2_105_1_n100_s16 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_105_1_n100_s16,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

# ==============================================================================
# n = 500
# ==============================================================================


# ------------------------------------------------------------------------------
# n = 500 e sigma_R^2 = 0.25
# ------------------------------------------------------------------------------

set.seed(131)

n <- 500
sigma2_real <- 0.25

x_n500_s025 <- rnorm(n, 0, sqrt(sigma2_real))

dados_IG_n500_s025 <- list(n = n, x = x_n500_s025, alpha = alpha, beta = beta)

ajuste_IG_n500_s025 <- sampling(
  modelo_IG,
  data = dados_IG_n500_s025,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_jeffreys_n500_s025 <- list(n = n, x = x_n500_s025)

ajuste_jeffreys_n500_s025 <- sampling(
  modelo_jeffreys,
  data = dados_jeffreys_n500_s025,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n500_s025 <- list(
  n = n, x = x_n500_s025,
  p = 1, q = 1, b = 1
)

ajuste_sbeta2_111_n500_s025 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_n500_s025,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n500_s025 <- list(
  n = n, x = x_n500_s025,
  p = 0.5, q = 0.5, b = 1
)

ajuste_sbeta2_0505_1_n500_s025 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_0505_1_n500_s025,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n500_s025 <- list(
  n = n, x = x_n500_s025,
  p = 1, q = 0.5, b = 1
)

ajuste_sbeta2_105_1_n500_s025 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_105_1_n500_s025,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

# ------------------------------------------------------------------------------
# n = 500 e sigma_R^2 = 1
# ------------------------------------------------------------------------------

set.seed(132)

n <- 500
sigma2_real <- 1

x_n500_s1 <- rnorm(n, 0, sqrt(sigma2_real))

dados_IG_n500_s1 <- list(n = n, x = x_n500_s1, alpha = alpha, beta = beta)

ajuste_IG_n500_s1 <- sampling(
  modelo_IG,
  data = dados_IG_n500_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_jeffreys_n500_s1 <- list(n = n, x = x_n500_s1)

ajuste_jeffreys_n500_s1 <- sampling(
  modelo_jeffreys,
  data = dados_jeffreys_n500_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n500_s1 <- list(
  n = n, x = x_n500_s1,
  p = 1, q = 1, b = 1
)

ajuste_sbeta2_111_n500_s1 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_n500_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n500_s1 <- list(
  n = n, x = x_n500_s1,
  p = 0.5, q = 0.5, b = 1
)

ajuste_sbeta2_0505_1_n500_s1 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_0505_1_n500_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n500_s1 <- list(
  n = n, x = x_n500_s1,
  p = 1, q = 0.5, b = 1
)

ajuste_sbeta2_105_1_n500_s1 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_105_1_n500_s1,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

# ------------------------------------------------------------------------------
# n = 500 e sigma_R^2 = 4
# ------------------------------------------------------------------------------

set.seed(133)

n <- 500
sigma2_real <- 4

x_n500_s4 <- rnorm(n, 0, sqrt(sigma2_real))

dados_IG_n500_s4 <- list(n = n, x = x_n500_s4, alpha = alpha, beta = beta)

ajuste_IG_n500_s4 <- sampling(
  modelo_IG,
  data = dados_IG_n500_s4,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_jeffreys_n500_s4 <- list(n = n, x = x_n500_s4)

ajuste_jeffreys_n500_s4 <- sampling(
  modelo_jeffreys,
  data = dados_jeffreys_n500_s4,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n500_s4 <- list(
  n = n, x = x_n500_s4,
  p = 1, q = 1, b = 1
)

ajuste_sbeta2_111_n500_s4 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_n500_s4,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n500_s4 <- list(
  n = n, x = x_n500_s4,
  p = 0.5, q = 0.5, b = 1
)

ajuste_sbeta2_0505_1_n500_s4 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_0505_1_n500_s4,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n500_s4 <- list(
  n = n, x = x_n500_s4,
  p = 1, q = 0.5, b = 1
)

ajuste_sbeta2_105_1_n500_s4 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_105_1_n500_s4,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

# ------------------------------------------------------------------------------
# n = 500 e sigma_R^2 = 16
# ------------------------------------------------------------------------------

set.seed(134)

n <- 500
sigma2_real <- 16

x_n500_s16 <- rnorm(n, 0, sqrt(sigma2_real))

dados_IG_n500_s16 <- list(n = n, x = x_n500_s16, alpha = alpha, beta = beta)

ajuste_IG_n500_s16 <- sampling(
  modelo_IG,
  data = dados_IG_n500_s16,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_jeffreys_n500_s16 <- list(n = n, x = x_n500_s16)

ajuste_jeffreys_n500_s16 <- sampling(
  modelo_jeffreys,
  data = dados_jeffreys_n500_s16,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_111_n500_s16 <- list(
  n = n, x = x_n500_s16,
  p = 1, q = 1, b = 1
)

ajuste_sbeta2_111_n500_s16 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_111_n500_s16,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_0505_1_n500_s16 <- list(
  n = n, x = x_n500_s16,
  p = 0.5, q = 0.5, b = 1
)

ajuste_sbeta2_0505_1_n500_s16 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_0505_1_n500_s16,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

dados_sbeta2_105_1_n500_s16 <- list(
  n = n, x = x_n500_s16,
  p = 1, q = 0.5, b = 1
)

ajuste_sbeta2_105_1_n500_s16 <- sampling(
  modelo_sbeta2,
  data = dados_sbeta2_105_1_n500_s16,
  chains = 4, iter = 4000, warmup = 1000,
  seed = 123
)

# Resultados gerais ajustes ----------------------------------------------------

# ==============================================================================
# n = 20
# ==============================================================================

# sigma2 = 0.25 ---------------------------------------------------------------

print(
  ajuste_IG_n20_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n20_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n20_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n20_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n20_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# sigma2 = 1 ------------------------------------------------------------------

print(
  ajuste_IG_n20_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n20_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n20_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n20_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n20_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# sigma2 = 4 ------------------------------------------------------------------

print(
  ajuste_IG_n20_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n20_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n20_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n20_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n20_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# sigma2 = 16 -----------------------------------------------------------------

print(
  ajuste_IG_n20_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n20_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n20_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n20_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n20_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# ==============================================================================
# n = 100
# ==============================================================================

# sigma2 = 0.25 ---------------------------------------------------------------

print(
  ajuste_IG_n100_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n100_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n100_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n100_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n100_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# sigma2 = 1 ------------------------------------------------------------------

print(
  ajuste_IG_n100_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n100_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n100_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n100_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n100_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# sigma2 = 4 ------------------------------------------------------------------

print(
  ajuste_IG_n100_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n100_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n100_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n100_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n100_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# sigma2 = 16 -----------------------------------------------------------------

print(
  ajuste_IG_n100_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n100_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n100_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n100_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n100_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# ==============================================================================
# n = 500
# ==============================================================================

# sigma2 = 0.25 ---------------------------------------------------------------

print(
  ajuste_IG_n500_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n500_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n500_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n500_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n500_s025,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# sigma2 = 1 ------------------------------------------------------------------

print(
  ajuste_IG_n500_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n500_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n500_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n500_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n500_s1,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# sigma2 = 4 ------------------------------------------------------------------

print(
  ajuste_IG_n500_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n500_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n500_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n500_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n500_s4,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)


# sigma2 = 16 -----------------------------------------------------------------

print(
  ajuste_IG_n500_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_jeffreys_n500_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_111_n500_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_0505_1_n500_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)

print(
  ajuste_sbeta2_105_1_n500_s16,
  pars = "sigma2",
  probs = c(0.025, 0.5, 0.975)
)