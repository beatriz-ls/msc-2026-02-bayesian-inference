# ==============================================================================
# ANÁLISE DOS RESULTADOS
# ==============================================================================

library(rstan)
library(posterior)


# ==============================================================================
# n = 20
# ==============================================================================


# ------------------------------------------------------------------------------
# n = 20 e sigma_R^2 = 0.25
# ------------------------------------------------------------------------------

n <- 20
sigma2_real <- 0.25
x <- x_n20_s025


# Inversa-Gama: resultado analítico --------------------------------------------

alpha_post_IG_n20_s025 <- alpha + n / 2
beta_post_IG_n20_s025 <- beta + sum(x^2) / 2

media_analitica_IG_n20_s025 <-
  beta_post_IG_n20_s025 / (alpha_post_IG_n20_s025 - 1)

moda_analitica_IG_n20_s025 <-
  beta_post_IG_n20_s025 / (alpha_post_IG_n20_s025 + 1)

mediana_analitica_IG_n20_s025 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_IG_n20_s025,
    rate = beta_post_IG_n20_s025
  )


# Inversa-Gama: Stan -----------------------------------------------------------

post_IG_n20_s025 <-
  rstan::extract(ajuste_IG_n20_s025)$sigma2

media_stan_IG_n20_s025 <-
  mean(post_IG_n20_s025)

mediana_stan_IG_n20_s025 <-
  median(post_IG_n20_s025)

dens_IG_n20_s025 <-
  density(post_IG_n20_s025)

moda_stan_IG_n20_s025 <-
  dens_IG_n20_s025$x[
    which.max(dens_IG_n20_s025$y)
  ]


# Jeffreys: resultado analítico ------------------------------------------------

alpha_post_J_n20_s025 <- n / 2
beta_post_J_n20_s025 <- sum(x^2) / 2

media_analitica_J_n20_s025 <-
  beta_post_J_n20_s025 / (alpha_post_J_n20_s025 - 1)

moda_analitica_J_n20_s025 <-
  beta_post_J_n20_s025 / (alpha_post_J_n20_s025 + 1)

mediana_analitica_J_n20_s025 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_J_n20_s025,
    rate = beta_post_J_n20_s025
  )


# Jeffreys: Stan ---------------------------------------------------------------

post_J_n20_s025 <-
  rstan::extract(ajuste_jeffreys_n20_s025)$sigma2

media_stan_J_n20_s025 <-
  mean(post_J_n20_s025)

mediana_stan_J_n20_s025 <-
  median(post_J_n20_s025)

dens_J_n20_s025 <-
  density(post_J_n20_s025)

moda_stan_J_n20_s025 <-
  dens_J_n20_s025$x[
    which.max(dens_J_n20_s025$y)
  ]


# SBeta2(1, 1, 1) --------------------------------------------------------------

post_SB111_n20_s025 <-
  rstan::extract(ajuste_sbeta2_111_n20_s025)$sigma2

media_SB111_n20_s025 <-
  mean(post_SB111_n20_s025)

mediana_SB111_n20_s025 <-
  median(post_SB111_n20_s025)

dens_SB111_n20_s025 <-
  density(post_SB111_n20_s025)

moda_SB111_n20_s025 <-
  dens_SB111_n20_s025$x[
    which.max(dens_SB111_n20_s025$y)
  ]


# SBeta2(0.5, 0.5, 1) ----------------------------------------------------------

post_SB0505_n20_s025 <-
  rstan::extract(ajuste_sbeta2_0505_1_n20_s025)$sigma2

media_SB0505_n20_s025 <-
  mean(post_SB0505_n20_s025)

mediana_SB0505_n20_s025 <-
  median(post_SB0505_n20_s025)

dens_SB0505_n20_s025 <-
  density(post_SB0505_n20_s025)

moda_SB0505_n20_s025 <-
  dens_SB0505_n20_s025$x[
    which.max(dens_SB0505_n20_s025$y)
  ]


# SBeta2(1, 0.5, 1) ------------------------------------------------------------

post_SB105_n20_s025 <-
  rstan::extract(ajuste_sbeta2_105_1_n20_s025)$sigma2

media_SB105_n20_s025 <-
  mean(post_SB105_n20_s025)

mediana_SB105_n20_s025 <-
  median(post_SB105_n20_s025)

dens_SB105_n20_s025 <-
  density(post_SB105_n20_s025)

moda_SB105_n20_s025 <-
  dens_SB105_n20_s025$x[
    which.max(dens_SB105_n20_s025$y)
  ]


# Tabela -----------------------------------------------------------------------

resultados_n20_s025 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0(
      "IG(", round(alpha_post_IG_n20_s025, 3), ", ",
      round(beta_post_IG_n20_s025, 3), ")"
    ),
    paste0(
      "IG(", round(alpha_post_J_n20_s025, 3), ", ",
      round(beta_post_J_n20_s025, 3), ")"
    ),
    NA,
    NA,
    NA
  ),
  
  media_analitica = c(
    media_analitica_IG_n20_s025,
    media_analitica_J_n20_s025,
    NA, NA, NA
  ),
  
  media_stan = c(
    media_stan_IG_n20_s025,
    media_stan_J_n20_s025,
    media_SB111_n20_s025,
    media_SB0505_n20_s025,
    media_SB105_n20_s025
  ),
  
  moda_analitica = c(
    moda_analitica_IG_n20_s025,
    moda_analitica_J_n20_s025,
    NA, NA, NA
  ),
  
  moda_stan = c(
    moda_stan_IG_n20_s025,
    moda_stan_J_n20_s025,
    moda_SB111_n20_s025,
    moda_SB0505_n20_s025,
    moda_SB105_n20_s025
  ),
  
  mediana_analitica = c(
    mediana_analitica_IG_n20_s025,
    mediana_analitica_J_n20_s025,
    NA, NA, NA
  ),
  
  mediana_stan = c(
    mediana_stan_IG_n20_s025,
    mediana_stan_J_n20_s025,
    mediana_SB111_n20_s025,
    mediana_SB0505_n20_s025,
    mediana_SB105_n20_s025
  )
)

resultados_n20_s025

# ------------------------------------------------------------------------------
# n = 20 e sigma_R^2 = 1
# ------------------------------------------------------------------------------

n <- 20
sigma2_real <- 1
x <- x_n20_s1


# Inversa-Gama -----------------------------------------------------------------

alpha_post_IG_n20_s1 <- alpha + n / 2
beta_post_IG_n20_s1 <- beta + sum(x^2) / 2

media_analitica_IG_n20_s1 <-
  beta_post_IG_n20_s1 / (alpha_post_IG_n20_s1 - 1)

moda_analitica_IG_n20_s1 <-
  beta_post_IG_n20_s1 / (alpha_post_IG_n20_s1 + 1)

mediana_analitica_IG_n20_s1 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_IG_n20_s1,
    rate = beta_post_IG_n20_s1
  )

post_IG_n20_s1 <- rstan::extract(ajuste_IG_n20_s1)$sigma2

media_stan_IG_n20_s1 <- mean(post_IG_n20_s1)
mediana_stan_IG_n20_s1 <- median(post_IG_n20_s1)

dens_IG_n20_s1 <- density(post_IG_n20_s1)

moda_stan_IG_n20_s1 <-
  dens_IG_n20_s1$x[which.max(dens_IG_n20_s1$y)]


# Jeffreys ---------------------------------------------------------------------

alpha_post_J_n20_s1 <- n / 2
beta_post_J_n20_s1 <- sum(x^2) / 2

media_analitica_J_n20_s1 <-
  beta_post_J_n20_s1 / (alpha_post_J_n20_s1 - 1)

moda_analitica_J_n20_s1 <-
  beta_post_J_n20_s1 / (alpha_post_J_n20_s1 + 1)

mediana_analitica_J_n20_s1 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_J_n20_s1,
    rate = beta_post_J_n20_s1
  )

post_J_n20_s1 <- rstan::extract(ajuste_jeffreys_n20_s1)$sigma2

media_stan_J_n20_s1 <- mean(post_J_n20_s1)
mediana_stan_J_n20_s1 <- median(post_J_n20_s1)

dens_J_n20_s1 <- density(post_J_n20_s1)

moda_stan_J_n20_s1 <-
  dens_J_n20_s1$x[which.max(dens_J_n20_s1$y)]


# SBeta2 -----------------------------------------------------------------------

post_SB111_n20_s1 <-
  rstan::extract(ajuste_sbeta2_111_n20_s1)$sigma2

media_SB111_n20_s1 <- mean(post_SB111_n20_s1)
mediana_SB111_n20_s1 <- median(post_SB111_n20_s1)
dens_SB111_n20_s1 <- density(post_SB111_n20_s1)

moda_SB111_n20_s1 <-
  dens_SB111_n20_s1$x[which.max(dens_SB111_n20_s1$y)]


post_SB0505_n20_s1 <-
  rstan::extract(ajuste_sbeta2_0505_1_n20_s1)$sigma2

media_SB0505_n20_s1 <- mean(post_SB0505_n20_s1)
mediana_SB0505_n20_s1 <- median(post_SB0505_n20_s1)
dens_SB0505_n20_s1 <- density(post_SB0505_n20_s1)

moda_SB0505_n20_s1 <-
  dens_SB0505_n20_s1$x[which.max(dens_SB0505_n20_s1$y)]


post_SB105_n20_s1 <-
  rstan::extract(ajuste_sbeta2_105_1_n20_s1)$sigma2

media_SB105_n20_s1 <- mean(post_SB105_n20_s1)
mediana_SB105_n20_s1 <- median(post_SB105_n20_s1)
dens_SB105_n20_s1 <- density(post_SB105_n20_s1)

moda_SB105_n20_s1 <-
  dens_SB105_n20_s1$x[which.max(dens_SB105_n20_s1$y)]


resultados_n20_s1 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  media_analitica = c(
    media_analitica_IG_n20_s1,
    media_analitica_J_n20_s1,
    NA, NA, NA
  ),
  media_stan = c(
    media_stan_IG_n20_s1,
    media_stan_J_n20_s1,
    media_SB111_n20_s1,
    media_SB0505_n20_s1,
    media_SB105_n20_s1
  ),
  moda_analitica = c(
    moda_analitica_IG_n20_s1,
    moda_analitica_J_n20_s1,
    NA, NA, NA
  ),
  moda_stan = c(
    moda_stan_IG_n20_s1,
    moda_stan_J_n20_s1,
    moda_SB111_n20_s1,
    moda_SB0505_n20_s1,
    moda_SB105_n20_s1
  ),
  mediana_analitica = c(
    mediana_analitica_IG_n20_s1,
    mediana_analitica_J_n20_s1,
    NA, NA, NA
  ),
  mediana_stan = c(
    mediana_stan_IG_n20_s1,
    mediana_stan_J_n20_s1,
    mediana_SB111_n20_s1,
    mediana_SB0505_n20_s1,
    mediana_SB105_n20_s1
  )
)

# ==============================================================================
# n = 20, sigma2 = 4
# ==============================================================================

n <- 20
x <- x_n20_s4

alpha_IG_n20_s4 <- alpha + n / 2
beta_IG_n20_s4 <- beta + sum(x^2) / 2

media_A_IG_n20_s4 <- beta_IG_n20_s4 / (alpha_IG_n20_s4 - 1)
moda_A_IG_n20_s4 <- beta_IG_n20_s4 / (alpha_IG_n20_s4 + 1)
mediana_A_IG_n20_s4 <- 1 / qgamma(0.5, alpha_IG_n20_s4,
                                  rate = beta_IG_n20_s4)

post_IG_n20_s4 <- extract(ajuste_IG_n20_s4)$sigma2
media_M_IG_n20_s4 <- mean(post_IG_n20_s4)
mediana_M_IG_n20_s4 <- median(post_IG_n20_s4)
d <- density(post_IG_n20_s4)
moda_M_IG_n20_s4 <- d$x[which.max(d$y)]


alpha_J_n20_s4 <- n / 2
beta_J_n20_s4 <- sum(x^2) / 2

media_A_J_n20_s4 <- beta_J_n20_s4 / (alpha_J_n20_s4 - 1)
moda_A_J_n20_s4 <- beta_J_n20_s4 / (alpha_J_n20_s4 + 1)
mediana_A_J_n20_s4 <- 1 / qgamma(0.5, alpha_J_n20_s4,
                                 rate = beta_J_n20_s4)

post_J_n20_s4 <- extract(ajuste_jeffreys_n20_s4)$sigma2
media_M_J_n20_s4 <- mean(post_J_n20_s4)
mediana_M_J_n20_s4 <- median(post_J_n20_s4)
d <- density(post_J_n20_s4)
moda_M_J_n20_s4 <- d$x[which.max(d$y)]


post_SB111_n20_s4 <- extract(ajuste_sbeta2_111_n20_s4)$sigma2
media_SB111_n20_s4 <- mean(post_SB111_n20_s4)
mediana_SB111_n20_s4 <- median(post_SB111_n20_s4)
d <- density(post_SB111_n20_s4)
moda_SB111_n20_s4 <- d$x[which.max(d$y)]

post_SB0505_n20_s4 <- extract(ajuste_sbeta2_0505_1_n20_s4)$sigma2
media_SB0505_n20_s4 <- mean(post_SB0505_n20_s4)
mediana_SB0505_n20_s4 <- median(post_SB0505_n20_s4)
d <- density(post_SB0505_n20_s4)
moda_SB0505_n20_s4 <- d$x[which.max(d$y)]

post_SB105_n20_s4 <- extract(ajuste_sbeta2_105_1_n20_s4)$sigma2
media_SB105_n20_s4 <- mean(post_SB105_n20_s4)
mediana_SB105_n20_s4 <- median(post_SB105_n20_s4)
d <- density(post_SB105_n20_s4)
moda_SB105_n20_s4 <- d$x[which.max(d$y)]

resultados_n20_s4 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0(
      "IG(", round(alpha_IG_n20_s4, 3), ", ",
      round(beta_IG_n20_s4, 3), ")"
    ),
    paste0(
      "IG(", round(alpha_J_n20_s4, 3), ", ",
      round(beta_J_n20_s4, 3), ")"
    ),
    NA,
    NA,
    NA
  ),
  
  media_analitica = c(
    media_A_IG_n20_s4,
    media_A_J_n20_s4,
    NA,
    NA,
    NA
  ),
  
  media_stan = c(
    media_M_IG_n20_s4,
    media_M_J_n20_s4,
    media_SB111_n20_s4,
    media_SB0505_n20_s4,
    media_SB105_n20_s4
  ),
  
  moda_analitica = c(
    moda_A_IG_n20_s4,
    moda_A_J_n20_s4,
    NA,
    NA,
    NA
  ),
  
  moda_stan = c(
    moda_M_IG_n20_s4,
    moda_M_J_n20_s4,
    moda_SB111_n20_s4,
    moda_SB0505_n20_s4,
    moda_SB105_n20_s4
  ),
  
  mediana_analitica = c(
    mediana_A_IG_n20_s4,
    mediana_A_J_n20_s4,
    NA,
    NA,
    NA
  ),
  
  mediana_stan = c(
    mediana_M_IG_n20_s4,
    mediana_M_J_n20_s4,
    mediana_SB111_n20_s4,
    mediana_SB0505_n20_s4,
    mediana_SB105_n20_s4
  )
)

resultados_n20_s4

# ==============================================================================
# n = 20, sigma2 = 16
# ==============================================================================

n <- 20
x <- x_n20_s16

alpha_IG_n20_s16 <- alpha + n / 2
beta_IG_n20_s16 <- beta + sum(x^2) / 2

media_A_IG_n20_s16 <- beta_IG_n20_s16 / (alpha_IG_n20_s16 - 1)
moda_A_IG_n20_s16 <- beta_IG_n20_s16 / (alpha_IG_n20_s16 + 1)
mediana_A_IG_n20_s16 <- 1 / qgamma(0.5, alpha_IG_n20_s16,
                                   rate = beta_IG_n20_s16)

post_IG_n20_s16 <- extract(ajuste_IG_n20_s16)$sigma2
media_M_IG_n20_s16 <- mean(post_IG_n20_s16)
mediana_M_IG_n20_s16 <- median(post_IG_n20_s16)
d <- density(post_IG_n20_s16)
moda_M_IG_n20_s16 <- d$x[which.max(d$y)]


alpha_J_n20_s16 <- n / 2
beta_J_n20_s16 <- sum(x^2) / 2

media_A_J_n20_s16 <- beta_J_n20_s16 / (alpha_J_n20_s16 - 1)
moda_A_J_n20_s16 <- beta_J_n20_s16 / (alpha_J_n20_s16 + 1)
mediana_A_J_n20_s16 <- 1 / qgamma(0.5, alpha_J_n20_s16,
                                  rate = beta_J_n20_s16)

post_J_n20_s16 <- extract(ajuste_jeffreys_n20_s16)$sigma2
media_M_J_n20_s16 <- mean(post_J_n20_s16)
mediana_M_J_n20_s16 <- median(post_J_n20_s16)
d <- density(post_J_n20_s16)
moda_M_J_n20_s16 <- d$x[which.max(d$y)]


post_SB111_n20_s16 <- extract(ajuste_sbeta2_111_n20_s16)$sigma2
media_SB111_n20_s16 <- mean(post_SB111_n20_s16)
mediana_SB111_n20_s16 <- median(post_SB111_n20_s16)
d <- density(post_SB111_n20_s16)
moda_SB111_n20_s16 <- d$x[which.max(d$y)]

post_SB0505_n20_s16 <- extract(ajuste_sbeta2_0505_1_n20_s16)$sigma2
media_SB0505_n20_s16 <- mean(post_SB0505_n20_s16)
mediana_SB0505_n20_s16 <- median(post_SB0505_n20_s16)
d <- density(post_SB0505_n20_s16)
moda_SB0505_n20_s16 <- d$x[which.max(d$y)]

post_SB105_n20_s16 <- extract(ajuste_sbeta2_105_1_n20_s16)$sigma2
media_SB105_n20_s16 <- mean(post_SB105_n20_s16)
mediana_SB105_n20_s16 <- median(post_SB105_n20_s16)
d <- density(post_SB105_n20_s16)
moda_SB105_n20_s16 <- d$x[which.max(d$y)]

resultados_n20_s16 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0(
      "IG(", round(alpha_IG_n20_s16, 3), ", ",
      round(beta_IG_n20_s16, 3), ")"
    ),
    paste0(
      "IG(", round(alpha_J_n20_s16, 3), ", ",
      round(beta_J_n20_s16, 3), ")"
    ),
    NA,
    NA,
    NA
  ),
  
  media_analitica = c(
    media_A_IG_n20_s16,
    media_A_J_n20_s16,
    NA,
    NA,
    NA
  ),
  
  media_stan = c(
    media_M_IG_n20_s16,
    media_M_J_n20_s16,
    media_SB111_n20_s16,
    media_SB0505_n20_s16,
    media_SB105_n20_s16
  ),
  
  moda_analitica = c(
    moda_A_IG_n20_s16,
    moda_A_J_n20_s16,
    NA,
    NA,
    NA
  ),
  
  moda_stan = c(
    moda_M_IG_n20_s16,
    moda_M_J_n20_s16,
    moda_SB111_n20_s16,
    moda_SB0505_n20_s16,
    moda_SB105_n20_s16
  ),
  
  mediana_analitica = c(
    mediana_A_IG_n20_s16,
    mediana_A_J_n20_s16,
    NA,
    NA,
    NA
  ),
  
  mediana_stan = c(
    mediana_M_IG_n20_s16,
    mediana_M_J_n20_s16,
    mediana_SB111_n20_s16,
    mediana_SB0505_n20_s16,
    mediana_SB105_n20_s16
  )
)

resultados_n20_s16

# ==============================================================================
# n = 100
# ==============================================================================

# ------------------------------------------------------------------------------
# n = 100 e sigma_R^2 = 0.25
# ------------------------------------------------------------------------------

n <- 100
sigma2_real <- 0.25
x <- x_n100_s025


# Inversa-Gama: analítico ------------------------------------------------------

alpha_post_IG_n100_s025 <- alpha + n / 2
beta_post_IG_n100_s025 <- beta + sum(x^2) / 2

media_analitica_IG_n100_s025 <-
  beta_post_IG_n100_s025 / (alpha_post_IG_n100_s025 - 1)

moda_analitica_IG_n100_s025 <-
  beta_post_IG_n100_s025 / (alpha_post_IG_n100_s025 + 1)

mediana_analitica_IG_n100_s025 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_IG_n100_s025,
    rate = beta_post_IG_n100_s025
  )


# Inversa-Gama: Stan -----------------------------------------------------------

post_IG_n100_s025 <-
  rstan::extract(ajuste_IG_n100_s025)$sigma2

media_stan_IG_n100_s025 <- mean(post_IG_n100_s025)
mediana_stan_IG_n100_s025 <- median(post_IG_n100_s025)

dens_IG_n100_s025 <- density(post_IG_n100_s025)

moda_stan_IG_n100_s025 <-
  dens_IG_n100_s025$x[
    which.max(dens_IG_n100_s025$y)
  ]


# Jeffreys: analítico ----------------------------------------------------------

alpha_post_J_n100_s025 <- n / 2
beta_post_J_n100_s025 <- sum(x^2) / 2

media_analitica_J_n100_s025 <-
  beta_post_J_n100_s025 / (alpha_post_J_n100_s025 - 1)

moda_analitica_J_n100_s025 <-
  beta_post_J_n100_s025 / (alpha_post_J_n100_s025 + 1)

mediana_analitica_J_n100_s025 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_J_n100_s025,
    rate = beta_post_J_n100_s025
  )


# Jeffreys: Stan ---------------------------------------------------------------

post_J_n100_s025 <-
  rstan::extract(ajuste_jeffreys_n100_s025)$sigma2

media_stan_J_n100_s025 <- mean(post_J_n100_s025)
mediana_stan_J_n100_s025 <- median(post_J_n100_s025)

dens_J_n100_s025 <- density(post_J_n100_s025)

moda_stan_J_n100_s025 <-
  dens_J_n100_s025$x[
    which.max(dens_J_n100_s025$y)
  ]


# SBeta2(1,1,1) ---------------------------------------------------------------

post_SB111_n100_s025 <-
  rstan::extract(ajuste_sbeta2_111_n100_s025)$sigma2

media_SB111_n100_s025 <- mean(post_SB111_n100_s025)
mediana_SB111_n100_s025 <- median(post_SB111_n100_s025)

dens_SB111_n100_s025 <- density(post_SB111_n100_s025)

moda_SB111_n100_s025 <-
  dens_SB111_n100_s025$x[
    which.max(dens_SB111_n100_s025$y)
  ]


# SBeta2(0.5,0.5,1) -----------------------------------------------------------

post_SB0505_n100_s025 <-
  rstan::extract(ajuste_sbeta2_0505_1_n100_s025)$sigma2

media_SB0505_n100_s025 <- mean(post_SB0505_n100_s025)
mediana_SB0505_n100_s025 <- median(post_SB0505_n100_s025)

dens_SB0505_n100_s025 <- density(post_SB0505_n100_s025)

moda_SB0505_n100_s025 <-
  dens_SB0505_n100_s025$x[
    which.max(dens_SB0505_n100_s025$y)
  ]


# SBeta2(1,0.5,1) -------------------------------------------------------------

post_SB105_n100_s025 <-
  rstan::extract(ajuste_sbeta2_105_1_n100_s025)$sigma2

media_SB105_n100_s025 <- mean(post_SB105_n100_s025)
mediana_SB105_n100_s025 <- median(post_SB105_n100_s025)

dens_SB105_n100_s025 <- density(post_SB105_n100_s025)

moda_SB105_n100_s025 <-
  dens_SB105_n100_s025$x[
    which.max(dens_SB105_n100_s025$y)
  ]


# Tabela -----------------------------------------------------------------------

resultados_n100_s025 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0(
      "IG(", round(alpha_post_IG_n100_s025, 3), ", ",
      round(beta_post_IG_n100_s025, 3), ")"
    ),
    paste0(
      "IG(", round(alpha_post_J_n100_s025, 3), ", ",
      round(beta_post_J_n100_s025, 3), ")"
    ),
    NA, NA, NA
  ),
  
  media_analitica = c(
    media_analitica_IG_n100_s025,
    media_analitica_J_n100_s025,
    NA, NA, NA
  ),
  
  media_stan = c(
    media_stan_IG_n100_s025,
    media_stan_J_n100_s025,
    media_SB111_n100_s025,
    media_SB0505_n100_s025,
    media_SB105_n100_s025
  ),
  
  moda_analitica = c(
    moda_analitica_IG_n100_s025,
    moda_analitica_J_n100_s025,
    NA, NA, NA
  ),
  
  moda_stan = c(
    moda_stan_IG_n100_s025,
    moda_stan_J_n100_s025,
    moda_SB111_n100_s025,
    moda_SB0505_n100_s025,
    moda_SB105_n100_s025
  ),
  
  mediana_analitica = c(
    mediana_analitica_IG_n100_s025,
    mediana_analitica_J_n100_s025,
    NA, NA, NA
  ),
  
  mediana_stan = c(
    mediana_stan_IG_n100_s025,
    mediana_stan_J_n100_s025,
    mediana_SB111_n100_s025,
    mediana_SB0505_n100_s025,
    mediana_SB105_n100_s025
  )
)

resultados_n100_s025

# ------------------------------------------------------------------------------
# n = 100 e sigma_R^2 = 1
# ------------------------------------------------------------------------------

n <- 100
sigma2_real <- 1
x <- x_n100_s1


# Inversa-Gama -----------------------------------------------------------------

alpha_post_IG_n100_s1 <- alpha + n / 2
beta_post_IG_n100_s1 <- beta + sum(x^2) / 2

media_analitica_IG_n100_s1 <-
  beta_post_IG_n100_s1 / (alpha_post_IG_n100_s1 - 1)

moda_analitica_IG_n100_s1 <-
  beta_post_IG_n100_s1 / (alpha_post_IG_n100_s1 + 1)

mediana_analitica_IG_n100_s1 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_IG_n100_s1,
    rate = beta_post_IG_n100_s1
  )

post_IG_n100_s1 <-
  rstan::extract(ajuste_IG_n100_s1)$sigma2

media_stan_IG_n100_s1 <- mean(post_IG_n100_s1)
mediana_stan_IG_n100_s1 <- median(post_IG_n100_s1)

dens_IG_n100_s1 <- density(post_IG_n100_s1)

moda_stan_IG_n100_s1 <-
  dens_IG_n100_s1$x[
    which.max(dens_IG_n100_s1$y)
  ]


# Jeffreys ---------------------------------------------------------------------

alpha_post_J_n100_s1 <- n / 2
beta_post_J_n100_s1 <- sum(x^2) / 2

media_analitica_J_n100_s1 <-
  beta_post_J_n100_s1 / (alpha_post_J_n100_s1 - 1)

moda_analitica_J_n100_s1 <-
  beta_post_J_n100_s1 / (alpha_post_J_n100_s1 + 1)

mediana_analitica_J_n100_s1 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_J_n100_s1,
    rate = beta_post_J_n100_s1
  )

post_J_n100_s1 <-
  rstan::extract(ajuste_jeffreys_n100_s1)$sigma2

media_stan_J_n100_s1 <- mean(post_J_n100_s1)
mediana_stan_J_n100_s1 <- median(post_J_n100_s1)

dens_J_n100_s1 <- density(post_J_n100_s1)

moda_stan_J_n100_s1 <-
  dens_J_n100_s1$x[
    which.max(dens_J_n100_s1$y)
  ]


# SBeta2 -----------------------------------------------------------------------

post_SB111_n100_s1 <-
  rstan::extract(ajuste_sbeta2_111_n100_s1)$sigma2

media_SB111_n100_s1 <- mean(post_SB111_n100_s1)
mediana_SB111_n100_s1 <- median(post_SB111_n100_s1)

dens_SB111_n100_s1 <- density(post_SB111_n100_s1)

moda_SB111_n100_s1 <-
  dens_SB111_n100_s1$x[
    which.max(dens_SB111_n100_s1$y)
  ]


post_SB0505_n100_s1 <-
  rstan::extract(ajuste_sbeta2_0505_1_n100_s1)$sigma2

media_SB0505_n100_s1 <- mean(post_SB0505_n100_s1)
mediana_SB0505_n100_s1 <- median(post_SB0505_n100_s1)

dens_SB0505_n100_s1 <- density(post_SB0505_n100_s1)

moda_SB0505_n100_s1 <-
  dens_SB0505_n100_s1$x[
    which.max(dens_SB0505_n100_s1$y)
  ]


post_SB105_n100_s1 <-
  rstan::extract(ajuste_sbeta2_105_1_n100_s1)$sigma2

media_SB105_n100_s1 <- mean(post_SB105_n100_s1)
mediana_SB105_n100_s1 <- median(post_SB105_n100_s1)

dens_SB105_n100_s1 <- density(post_SB105_n100_s1)

moda_SB105_n100_s1 <-
  dens_SB105_n100_s1$x[
    which.max(dens_SB105_n100_s1$y)
  ]


# Tabela -----------------------------------------------------------------------

resultados_n100_s1 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0(
      "IG(", round(alpha_post_IG_n100_s1, 3), ", ",
      round(beta_post_IG_n100_s1, 3), ")"
    ),
    paste0(
      "IG(", round(alpha_post_J_n100_s1, 3), ", ",
      round(beta_post_J_n100_s1, 3), ")"
    ),
    NA, NA, NA
  ),
  
  media_analitica = c(
    media_analitica_IG_n100_s1,
    media_analitica_J_n100_s1,
    NA, NA, NA
  ),
  
  media_stan = c(
    media_stan_IG_n100_s1,
    media_stan_J_n100_s1,
    media_SB111_n100_s1,
    media_SB0505_n100_s1,
    media_SB105_n100_s1
  ),
  
  moda_analitica = c(
    moda_analitica_IG_n100_s1,
    moda_analitica_J_n100_s1,
    NA, NA, NA
  ),
  
  moda_stan = c(
    moda_stan_IG_n100_s1,
    moda_stan_J_n100_s1,
    moda_SB111_n100_s1,
    moda_SB0505_n100_s1,
    moda_SB105_n100_s1
  ),
  
  mediana_analitica = c(
    mediana_analitica_IG_n100_s1,
    mediana_analitica_J_n100_s1,
    NA, NA, NA
  ),
  
  mediana_stan = c(
    mediana_stan_IG_n100_s1,
    mediana_stan_J_n100_s1,
    mediana_SB111_n100_s1,
    mediana_SB0505_n100_s1,
    mediana_SB105_n100_s1
  )
)

resultados_n100_s1

# ------------------------------------------------------------------------------
# n = 100 e sigma_R^2 = 4
# ------------------------------------------------------------------------------

n <- 100
sigma2_real <- 4
x <- x_n100_s4


alpha_post_IG_n100_s4 <- alpha + n / 2
beta_post_IG_n100_s4 <- beta + sum(x^2) / 2

media_analitica_IG_n100_s4 <-
  beta_post_IG_n100_s4 / (alpha_post_IG_n100_s4 - 1)

moda_analitica_IG_n100_s4 <-
  beta_post_IG_n100_s4 / (alpha_post_IG_n100_s4 + 1)

mediana_analitica_IG_n100_s4 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_IG_n100_s4,
    rate = beta_post_IG_n100_s4
  )

post_IG_n100_s4 <- rstan::extract(ajuste_IG_n100_s4)$sigma2
media_stan_IG_n100_s4 <- mean(post_IG_n100_s4)
mediana_stan_IG_n100_s4 <- median(post_IG_n100_s4)

dens_IG_n100_s4 <- density(post_IG_n100_s4)

moda_stan_IG_n100_s4 <-
  dens_IG_n100_s4$x[which.max(dens_IG_n100_s4$y)]


alpha_post_J_n100_s4 <- n / 2
beta_post_J_n100_s4 <- sum(x^2) / 2

media_analitica_J_n100_s4 <-
  beta_post_J_n100_s4 / (alpha_post_J_n100_s4 - 1)

moda_analitica_J_n100_s4 <-
  beta_post_J_n100_s4 / (alpha_post_J_n100_s4 + 1)

mediana_analitica_J_n100_s4 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_J_n100_s4,
    rate = beta_post_J_n100_s4
  )

post_J_n100_s4 <- rstan::extract(ajuste_jeffreys_n100_s4)$sigma2
media_stan_J_n100_s4 <- mean(post_J_n100_s4)
mediana_stan_J_n100_s4 <- median(post_J_n100_s4)

dens_J_n100_s4 <- density(post_J_n100_s4)

moda_stan_J_n100_s4 <-
  dens_J_n100_s4$x[which.max(dens_J_n100_s4$y)]


post_SB111_n100_s4 <-
  rstan::extract(ajuste_sbeta2_111_n100_s4)$sigma2

media_SB111_n100_s4 <- mean(post_SB111_n100_s4)
mediana_SB111_n100_s4 <- median(post_SB111_n100_s4)

dens_SB111_n100_s4 <- density(post_SB111_n100_s4)

moda_SB111_n100_s4 <-
  dens_SB111_n100_s4$x[which.max(dens_SB111_n100_s4$y)]


post_SB0505_n100_s4 <-
  rstan::extract(ajuste_sbeta2_0505_1_n100_s4)$sigma2

media_SB0505_n100_s4 <- mean(post_SB0505_n100_s4)
mediana_SB0505_n100_s4 <- median(post_SB0505_n100_s4)

dens_SB0505_n100_s4 <- density(post_SB0505_n100_s4)

moda_SB0505_n100_s4 <-
  dens_SB0505_n100_s4$x[which.max(dens_SB0505_n100_s4$y)]


post_SB105_n100_s4 <-
  rstan::extract(ajuste_sbeta2_105_1_n100_s4)$sigma2

media_SB105_n100_s4 <- mean(post_SB105_n100_s4)
mediana_SB105_n100_s4 <- median(post_SB105_n100_s4)

dens_SB105_n100_s4 <- density(post_SB105_n100_s4)

moda_SB105_n100_s4 <-
  dens_SB105_n100_s4$x[which.max(dens_SB105_n100_s4$y)]


resultados_n100_s4 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0(
      "IG(", round(alpha_post_IG_n100_s4, 3), ", ",
      round(beta_post_IG_n100_s4, 3), ")"
    ),
    paste0(
      "IG(", round(alpha_post_J_n100_s4, 3), ", ",
      round(beta_post_J_n100_s4, 3), ")"
    ),
    NA, NA, NA
  ),
  
  media_analitica = c(
    media_analitica_IG_n100_s4,
    media_analitica_J_n100_s4,
    NA, NA, NA
  ),
  
  media_stan = c(
    media_stan_IG_n100_s4,
    media_stan_J_n100_s4,
    media_SB111_n100_s4,
    media_SB0505_n100_s4,
    media_SB105_n100_s4
  ),
  
  moda_analitica = c(
    moda_analitica_IG_n100_s4,
    moda_analitica_J_n100_s4,
    NA, NA, NA
  ),
  
  moda_stan = c(
    moda_stan_IG_n100_s4,
    moda_stan_J_n100_s4,
    moda_SB111_n100_s4,
    moda_SB0505_n100_s4,
    moda_SB105_n100_s4
  ),
  
  mediana_analitica = c(
    mediana_analitica_IG_n100_s4,
    mediana_analitica_J_n100_s4,
    NA, NA, NA
  ),
  
  mediana_stan = c(
    mediana_stan_IG_n100_s4,
    mediana_stan_J_n100_s4,
    mediana_SB111_n100_s4,
    mediana_SB0505_n100_s4,
    mediana_SB105_n100_s4
  )
)

resultados_n100_s4

# ------------------------------------------------------------------------------
# n = 100 e sigma_R^2 = 16
# ------------------------------------------------------------------------------

n <- 100
sigma2_real <- 16
x <- x_n100_s16


alpha_post_IG_n100_s16 <- alpha + n / 2
beta_post_IG_n100_s16 <- beta + sum(x^2) / 2

media_analitica_IG_n100_s16 <-
  beta_post_IG_n100_s16 / (alpha_post_IG_n100_s16 - 1)

moda_analitica_IG_n100_s16 <-
  beta_post_IG_n100_s16 / (alpha_post_IG_n100_s16 + 1)

mediana_analitica_IG_n100_s16 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_IG_n100_s16,
    rate = beta_post_IG_n100_s16
  )

post_IG_n100_s16 <- rstan::extract(ajuste_IG_n100_s16)$sigma2

media_stan_IG_n100_s16 <- mean(post_IG_n100_s16)
mediana_stan_IG_n100_s16 <- median(post_IG_n100_s16)

dens_IG_n100_s16 <- density(post_IG_n100_s16)

moda_stan_IG_n100_s16 <-
  dens_IG_n100_s16$x[which.max(dens_IG_n100_s16$y)]


alpha_post_J_n100_s16 <- n / 2
beta_post_J_n100_s16 <- sum(x^2) / 2

media_analitica_J_n100_s16 <-
  beta_post_J_n100_s16 / (alpha_post_J_n100_s16 - 1)

moda_analitica_J_n100_s16 <-
  beta_post_J_n100_s16 / (alpha_post_J_n100_s16 + 1)

mediana_analitica_J_n100_s16 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_J_n100_s16,
    rate = beta_post_J_n100_s16
  )

post_J_n100_s16 <- rstan::extract(ajuste_jeffreys_n100_s16)$sigma2

media_stan_J_n100_s16 <- mean(post_J_n100_s16)
mediana_stan_J_n100_s16 <- median(post_J_n100_s16)

dens_J_n100_s16 <- density(post_J_n100_s16)

moda_stan_J_n100_s16 <-
  dens_J_n100_s16$x[which.max(dens_J_n100_s16$y)]


post_SB111_n100_s16 <-
  rstan::extract(ajuste_sbeta2_111_n100_s16)$sigma2

media_SB111_n100_s16 <- mean(post_SB111_n100_s16)
mediana_SB111_n100_s16 <- median(post_SB111_n100_s16)

dens_SB111_n100_s16 <- density(post_SB111_n100_s16)

moda_SB111_n100_s16 <-
  dens_SB111_n100_s16$x[which.max(dens_SB111_n100_s16$y)]


post_SB0505_n100_s16 <-
  rstan::extract(ajuste_sbeta2_0505_1_n100_s16)$sigma2

media_SB0505_n100_s16 <- mean(post_SB0505_n100_s16)
mediana_SB0505_n100_s16 <- median(post_SB0505_n100_s16)

dens_SB0505_n100_s16 <- density(post_SB0505_n100_s16)

moda_SB0505_n100_s16 <-
  dens_SB0505_n100_s16$x[which.max(dens_SB0505_n100_s16$y)]


post_SB105_n100_s16 <-
  rstan::extract(ajuste_sbeta2_105_1_n100_s16)$sigma2

media_SB105_n100_s16 <- mean(post_SB105_n100_s16)
mediana_SB105_n100_s16 <- median(post_SB105_n100_s16)

dens_SB105_n100_s16 <- density(post_SB105_n100_s16)

moda_SB105_n100_s16 <-
  dens_SB105_n100_s16$x[which.max(dens_SB105_n100_s16$y)]


resultados_n100_s16 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0(
      "IG(", round(alpha_post_IG_n100_s16, 3), ", ",
      round(beta_post_IG_n100_s16, 3), ")"
    ),
    paste0(
      "IG(", round(alpha_post_J_n100_s16, 3), ", ",
      round(beta_post_J_n100_s16, 3), ")"
    ),
    NA, NA, NA
  ),
  
  media_analitica = c(
    media_analitica_IG_n100_s16,
    media_analitica_J_n100_s16,
    NA, NA, NA
  ),
  
  media_stan = c(
    media_stan_IG_n100_s16,
    media_stan_J_n100_s16,
    media_SB111_n100_s16,
    media_SB0505_n100_s16,
    media_SB105_n100_s16
  ),
  
  moda_analitica = c(
    moda_analitica_IG_n100_s16,
    moda_analitica_J_n100_s16,
    NA, NA, NA
  ),
  
  moda_stan = c(
    moda_stan_IG_n100_s16,
    moda_stan_J_n100_s16,
    moda_SB111_n100_s16,
    moda_SB0505_n100_s16,
    moda_SB105_n100_s16
  ),
  
  mediana_analitica = c(
    mediana_analitica_IG_n100_s16,
    mediana_analitica_J_n100_s16,
    NA, NA, NA
  ),
  
  mediana_stan = c(
    mediana_stan_IG_n100_s16,
    mediana_stan_J_n100_s16,
    mediana_SB111_n100_s16,
    mediana_SB0505_n100_s16,
    mediana_SB105_n100_s16
  )
)

resultados_n100_s16

# ==============================================================================
# n = 500
# ==============================================================================


# ------------------------------------------------------------------------------
# n = 500 e sigma_R^2 = 0.25
# ------------------------------------------------------------------------------

n <- 500
sigma2_real <- 0.25
x <- x_n500_s025


alpha_post_IG_n500_s025 <- alpha + n / 2
beta_post_IG_n500_s025 <- beta + sum(x^2) / 2

media_analitica_IG_n500_s025 <-
  beta_post_IG_n500_s025 / (alpha_post_IG_n500_s025 - 1)

moda_analitica_IG_n500_s025 <-
  beta_post_IG_n500_s025 / (alpha_post_IG_n500_s025 + 1)

mediana_analitica_IG_n500_s025 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_IG_n500_s025,
    rate = beta_post_IG_n500_s025
  )

post_IG_n500_s025 <- rstan::extract(ajuste_IG_n500_s025)$sigma2
media_stan_IG_n500_s025 <- mean(post_IG_n500_s025)
mediana_stan_IG_n500_s025 <- median(post_IG_n500_s025)

dens_IG_n500_s025 <- density(post_IG_n500_s025)

moda_stan_IG_n500_s025 <-
  dens_IG_n500_s025$x[which.max(dens_IG_n500_s025$y)]


alpha_post_J_n500_s025 <- n / 2
beta_post_J_n500_s025 <- sum(x^2) / 2

media_analitica_J_n500_s025 <-
  beta_post_J_n500_s025 / (alpha_post_J_n500_s025 - 1)

moda_analitica_J_n500_s025 <-
  beta_post_J_n500_s025 / (alpha_post_J_n500_s025 + 1)

mediana_analitica_J_n500_s025 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_J_n500_s025,
    rate = beta_post_J_n500_s025
  )

post_J_n500_s025 <- rstan::extract(ajuste_jeffreys_n500_s025)$sigma2
media_stan_J_n500_s025 <- mean(post_J_n500_s025)
mediana_stan_J_n500_s025 <- median(post_J_n500_s025)

dens_J_n500_s025 <- density(post_J_n500_s025)

moda_stan_J_n500_s025 <-
  dens_J_n500_s025$x[which.max(dens_J_n500_s025$y)]


post_SB111_n500_s025 <-
  rstan::extract(ajuste_sbeta2_111_n500_s025)$sigma2

media_SB111_n500_s025 <- mean(post_SB111_n500_s025)
mediana_SB111_n500_s025 <- median(post_SB111_n500_s025)

dens_SB111_n500_s025 <- density(post_SB111_n500_s025)

moda_SB111_n500_s025 <-
  dens_SB111_n500_s025$x[which.max(dens_SB111_n500_s025$y)]


post_SB0505_n500_s025 <-
  rstan::extract(ajuste_sbeta2_0505_1_n500_s025)$sigma2

media_SB0505_n500_s025 <- mean(post_SB0505_n500_s025)
mediana_SB0505_n500_s025 <- median(post_SB0505_n500_s025)

dens_SB0505_n500_s025 <- density(post_SB0505_n500_s025)

moda_SB0505_n500_s025 <-
  dens_SB0505_n500_s025$x[which.max(dens_SB0505_n500_s025$y)]


post_SB105_n500_s025 <-
  rstan::extract(ajuste_sbeta2_105_1_n500_s025)$sigma2

media_SB105_n500_s025 <- mean(post_SB105_n500_s025)
mediana_SB105_n500_s025 <- median(post_SB105_n500_s025)

dens_SB105_n500_s025 <- density(post_SB105_n500_s025)

moda_SB105_n500_s025 <-
  dens_SB105_n500_s025$x[which.max(dens_SB105_n500_s025$y)]


resultados_n500_s025 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0(
      "IG(", round(alpha_post_IG_n500_s025, 3), ", ",
      round(beta_post_IG_n500_s025, 3), ")"
    ),
    paste0(
      "IG(", round(alpha_post_J_n500_s025, 3), ", ",
      round(beta_post_J_n500_s025, 3), ")"
    ),
    NA, NA, NA
  ),
  
  media_analitica = c(
    media_analitica_IG_n500_s025,
    media_analitica_J_n500_s025,
    NA, NA, NA
  ),
  
  media_stan = c(
    media_stan_IG_n500_s025,
    media_stan_J_n500_s025,
    media_SB111_n500_s025,
    media_SB0505_n500_s025,
    media_SB105_n500_s025
  ),
  
  moda_analitica = c(
    moda_analitica_IG_n500_s025,
    moda_analitica_J_n500_s025,
    NA, NA, NA
  ),
  
  moda_stan = c(
    moda_stan_IG_n500_s025,
    moda_stan_J_n500_s025,
    moda_SB111_n500_s025,
    moda_SB0505_n500_s025,
    moda_SB105_n500_s025
  ),
  
  mediana_analitica = c(
    mediana_analitica_IG_n500_s025,
    mediana_analitica_J_n500_s025,
    NA, NA, NA
  ),
  
  mediana_stan = c(
    mediana_stan_IG_n500_s025,
    mediana_stan_J_n500_s025,
    mediana_SB111_n500_s025,
    mediana_SB0505_n500_s025,
    mediana_SB105_n500_s025
  )
)

resultados_n500_s025

# ------------------------------------------------------------------------------
# n = 500 e sigma_R^2 = 1
# ------------------------------------------------------------------------------

n <- 500
sigma2_real <- 1
x <- x_n500_s1


alpha_post_IG_n500_s1 <- alpha + n / 2
beta_post_IG_n500_s1 <- beta + sum(x^2) / 2

media_analitica_IG_n500_s1 <-
  beta_post_IG_n500_s1 / (alpha_post_IG_n500_s1 - 1)

moda_analitica_IG_n500_s1 <-
  beta_post_IG_n500_s1 / (alpha_post_IG_n500_s1 + 1)

mediana_analitica_IG_n500_s1 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_IG_n500_s1,
    rate = beta_post_IG_n500_s1
  )

post_IG_n500_s1 <- rstan::extract(ajuste_IG_n500_s1)$sigma2
media_stan_IG_n500_s1 <- mean(post_IG_n500_s1)
mediana_stan_IG_n500_s1 <- median(post_IG_n500_s1)

dens_IG_n500_s1 <- density(post_IG_n500_s1)

moda_stan_IG_n500_s1 <-
  dens_IG_n500_s1$x[which.max(dens_IG_n500_s1$y)]


alpha_post_J_n500_s1 <- n / 2
beta_post_J_n500_s1 <- sum(x^2) / 2

media_analitica_J_n500_s1 <-
  beta_post_J_n500_s1 / (alpha_post_J_n500_s1 - 1)

moda_analitica_J_n500_s1 <-
  beta_post_J_n500_s1 / (alpha_post_J_n500_s1 + 1)

mediana_analitica_J_n500_s1 <-
  1 / qgamma(
    0.5,
    shape = alpha_post_J_n500_s1,
    rate = beta_post_J_n500_s1
  )

post_J_n500_s1 <- rstan::extract(ajuste_jeffreys_n500_s1)$sigma2
media_stan_J_n500_s1 <- mean(post_J_n500_s1)
mediana_stan_J_n500_s1 <- median(post_J_n500_s1)

dens_J_n500_s1 <- density(post_J_n500_s1)

moda_stan_J_n500_s1 <-
  dens_J_n500_s1$x[which.max(dens_J_n500_s1$y)]


post_SB111_n500_s1 <- rstan::extract(ajuste_sbeta2_111_n500_s1)$sigma2

media_SB111_n500_s1 <- mean(post_SB111_n500_s1)
mediana_SB111_n500_s1 <- median(post_SB111_n500_s1)

dens_SB111_n500_s1 <- density(post_SB111_n500_s1)

moda_SB111_n500_s1 <-
  dens_SB111_n500_s1$x[which.max(dens_SB111_n500_s1$y)]


post_SB0505_n500_s1 <- rstan::extract(ajuste_sbeta2_0505_1_n500_s1)$sigma2

media_SB0505_n500_s1 <- mean(post_SB0505_n500_s1)
mediana_SB0505_n500_s1 <- median(post_SB0505_n500_s1)

dens_SB0505_n500_s1 <- density(post_SB0505_n500_s1)

moda_SB0505_n500_s1 <-
  dens_SB0505_n500_s1$x[which.max(dens_SB0505_n500_s1$y)]


post_SB105_n500_s1 <- rstan::extract(ajuste_sbeta2_105_1_n500_s1)$sigma2

media_SB105_n500_s1 <- mean(post_SB105_n500_s1)
mediana_SB105_n500_s1 <- median(post_SB105_n500_s1)

dens_SB105_n500_s1 <- density(post_SB105_n500_s1)

moda_SB105_n500_s1 <-
  dens_SB105_n500_s1$x[which.max(dens_SB105_n500_s1$y)]


resultados_n500_s1 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0("IG(", round(alpha_post_IG_n500_s1, 3), ", ",
           round(beta_post_IG_n500_s1, 3), ")"),
    paste0("IG(", round(alpha_post_J_n500_s1, 3), ", ",
           round(beta_post_J_n500_s1, 3), ")"),
    NA, NA, NA
  ),
  
  media_analitica = c(
    media_analitica_IG_n500_s1,
    media_analitica_J_n500_s1,
    NA, NA, NA
  ),
  
  media_stan = c(
    media_stan_IG_n500_s1,
    media_stan_J_n500_s1,
    media_SB111_n500_s1,
    media_SB0505_n500_s1,
    media_SB105_n500_s1
  ),
  
  moda_analitica = c(
    moda_analitica_IG_n500_s1,
    moda_analitica_J_n500_s1,
    NA, NA, NA
  ),
  
  moda_stan = c(
    moda_stan_IG_n500_s1,
    moda_stan_J_n500_s1,
    moda_SB111_n500_s1,
    moda_SB0505_n500_s1,
    moda_SB105_n500_s1
  ),
  
  mediana_analitica = c(
    mediana_analitica_IG_n500_s1,
    mediana_analitica_J_n500_s1,
    NA, NA, NA
  ),
  
  mediana_stan = c(
    mediana_stan_IG_n500_s1,
    mediana_stan_J_n500_s1,
    mediana_SB111_n500_s1,
    mediana_SB0505_n500_s1,
    mediana_SB105_n500_s1
  )
)

resultados_n500_s1

# ------------------------------------------------------------------------------
# n = 500 e sigma_R^2 = 4
# ------------------------------------------------------------------------------

n <- 500
sigma2_real <- 4
x <- x_n500_s4


alpha_post_IG_n500_s4 <- alpha + n / 2
beta_post_IG_n500_s4 <- beta + sum(x^2) / 2

media_analitica_IG_n500_s4 <-
  beta_post_IG_n500_s4 / (alpha_post_IG_n500_s4 - 1)

moda_analitica_IG_n500_s4 <-
  beta_post_IG_n500_s4 / (alpha_post_IG_n500_s4 + 1)

mediana_analitica_IG_n500_s4 <-
  1 / qgamma(0.5,
             shape = alpha_post_IG_n500_s4,
             rate = beta_post_IG_n500_s4)

post_IG_n500_s4 <- rstan::extract(ajuste_IG_n500_s4)$sigma2

media_stan_IG_n500_s4 <- mean(post_IG_n500_s4)
mediana_stan_IG_n500_s4 <- median(post_IG_n500_s4)

dens_IG_n500_s4 <- density(post_IG_n500_s4)

moda_stan_IG_n500_s4 <-
  dens_IG_n500_s4$x[which.max(dens_IG_n500_s4$y)]


alpha_post_J_n500_s4 <- n / 2
beta_post_J_n500_s4 <- sum(x^2) / 2

media_analitica_J_n500_s4 <-
  beta_post_J_n500_s4 / (alpha_post_J_n500_s4 - 1)

moda_analitica_J_n500_s4 <-
  beta_post_J_n500_s4 / (alpha_post_J_n500_s4 + 1)

mediana_analitica_J_n500_s4 <-
  1 / qgamma(0.5,
             shape = alpha_post_J_n500_s4,
             rate = beta_post_J_n500_s4)

post_J_n500_s4 <- rstan::extract(ajuste_jeffreys_n500_s4)$sigma2

media_stan_J_n500_s4 <- mean(post_J_n500_s4)
mediana_stan_J_n500_s4 <- median(post_J_n500_s4)

dens_J_n500_s4 <- density(post_J_n500_s4)

moda_stan_J_n500_s4 <-
  dens_J_n500_s4$x[which.max(dens_J_n500_s4$y)]


post_SB111_n500_s4 <- rstan::extract(ajuste_sbeta2_111_n500_s4)$sigma2
media_SB111_n500_s4 <- mean(post_SB111_n500_s4)
mediana_SB111_n500_s4 <- median(post_SB111_n500_s4)
dens_SB111_n500_s4 <- density(post_SB111_n500_s4)
moda_SB111_n500_s4 <- dens_SB111_n500_s4$x[which.max(dens_SB111_n500_s4$y)]


post_SB0505_n500_s4 <- rstan::extract(ajuste_sbeta2_0505_1_n500_s4)$sigma2
media_SB0505_n500_s4 <- mean(post_SB0505_n500_s4)
mediana_SB0505_n500_s4 <- median(post_SB0505_n500_s4)
dens_SB0505_n500_s4 <- density(post_SB0505_n500_s4)
moda_SB0505_n500_s4 <- dens_SB0505_n500_s4$x[which.max(dens_SB0505_n500_s4$y)]


post_SB105_n500_s4 <- rstan::extract(ajuste_sbeta2_105_1_n500_s4)$sigma2
media_SB105_n500_s4 <- mean(post_SB105_n500_s4)
mediana_SB105_n500_s4 <- median(post_SB105_n500_s4)
dens_SB105_n500_s4 <- density(post_SB105_n500_s4)
moda_SB105_n500_s4 <- dens_SB105_n500_s4$x[which.max(dens_SB105_n500_s4$y)]


resultados_n500_s4 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0("IG(", round(alpha_post_IG_n500_s4, 3), ", ",
           round(beta_post_IG_n500_s4, 3), ")"),
    paste0("IG(", round(alpha_post_J_n500_s4, 3), ", ",
           round(beta_post_J_n500_s4, 3), ")"),
    NA, NA, NA
  ),
  
  media_analitica = c(
    media_analitica_IG_n500_s4,
    media_analitica_J_n500_s4,
    NA, NA, NA
  ),
  
  media_stan = c(
    media_stan_IG_n500_s4,
    media_stan_J_n500_s4,
    media_SB111_n500_s4,
    media_SB0505_n500_s4,
    media_SB105_n500_s4
  ),
  
  moda_analitica = c(
    moda_analitica_IG_n500_s4,
    moda_analitica_J_n500_s4,
    NA, NA, NA
  ),
  
  moda_stan = c(
    moda_stan_IG_n500_s4,
    moda_stan_J_n500_s4,
    moda_SB111_n500_s4,
    moda_SB0505_n500_s4,
    moda_SB105_n500_s4
  ),
  
  mediana_analitica = c(
    mediana_analitica_IG_n500_s4,
    mediana_analitica_J_n500_s4,
    NA, NA, NA
  ),
  
  mediana_stan = c(
    mediana_stan_IG_n500_s4,
    mediana_stan_J_n500_s4,
    mediana_SB111_n500_s4,
    mediana_SB0505_n500_s4,
    mediana_SB105_n500_s4
  )
)

resultados_n500_s4

# ------------------------------------------------------------------------------
# n = 500 e sigma_R^2 = 16
# ------------------------------------------------------------------------------

n <- 500
sigma2_real <- 16
x <- x_n500_s16


alpha_post_IG_n500_s16 <- alpha + n / 2
beta_post_IG_n500_s16 <- beta + sum(x^2) / 2

media_analitica_IG_n500_s16 <-
  beta_post_IG_n500_s16 / (alpha_post_IG_n500_s16 - 1)

moda_analitica_IG_n500_s16 <-
  beta_post_IG_n500_s16 / (alpha_post_IG_n500_s16 + 1)

mediana_analitica_IG_n500_s16 <-
  1 / qgamma(0.5,
             shape = alpha_post_IG_n500_s16,
             rate = beta_post_IG_n500_s16)

post_IG_n500_s16 <- rstan::extract(ajuste_IG_n500_s16)$sigma2

media_stan_IG_n500_s16 <- mean(post_IG_n500_s16)
mediana_stan_IG_n500_s16 <- median(post_IG_n500_s16)

dens_IG_n500_s16 <- density(post_IG_n500_s16)

moda_stan_IG_n500_s16 <-
  dens_IG_n500_s16$x[which.max(dens_IG_n500_s16$y)]


alpha_post_J_n500_s16 <- n / 2
beta_post_J_n500_s16 <- sum(x^2) / 2

media_analitica_J_n500_s16 <-
  beta_post_J_n500_s16 / (alpha_post_J_n500_s16 - 1)

moda_analitica_J_n500_s16 <-
  beta_post_J_n500_s16 / (alpha_post_J_n500_s16 + 1)

mediana_analitica_J_n500_s16 <-
  1 / qgamma(0.5,
             shape = alpha_post_J_n500_s16,
             rate = beta_post_J_n500_s16)

post_J_n500_s16 <- rstan::extract(ajuste_jeffreys_n500_s16)$sigma2

media_stan_J_n500_s16 <- mean(post_J_n500_s16)
mediana_stan_J_n500_s16 <- median(post_J_n500_s16)

dens_J_n500_s16 <- density(post_J_n500_s16)

moda_stan_J_n500_s16 <-
  dens_J_n500_s16$x[which.max(dens_J_n500_s16$y)]


post_SB111_n500_s16 <- rstan::extract(ajuste_sbeta2_111_n500_s16)$sigma2
media_SB111_n500_s16 <- mean(post_SB111_n500_s16)
mediana_SB111_n500_s16 <- median(post_SB111_n500_s16)
dens_SB111_n500_s16 <- density(post_SB111_n500_s16)
moda_SB111_n500_s16 <- dens_SB111_n500_s16$x[which.max(dens_SB111_n500_s16$y)]


post_SB0505_n500_s16 <- rstan::extract(ajuste_sbeta2_0505_1_n500_s16)$sigma2
media_SB0505_n500_s16 <- mean(post_SB0505_n500_s16)
mediana_SB0505_n500_s16 <- median(post_SB0505_n500_s16)
dens_SB0505_n500_s16 <- density(post_SB0505_n500_s16)
moda_SB0505_n500_s16 <- dens_SB0505_n500_s16$x[which.max(dens_SB0505_n500_s16$y)]


post_SB105_n500_s16 <- rstan::extract(ajuste_sbeta2_105_1_n500_s16)$sigma2
media_SB105_n500_s16 <- mean(post_SB105_n500_s16)
mediana_SB105_n500_s16 <- median(post_SB105_n500_s16)
dens_SB105_n500_s16 <- density(post_SB105_n500_s16)
moda_SB105_n500_s16 <- dens_SB105_n500_s16$x[which.max(dens_SB105_n500_s16$y)]


resultados_n500_s16 <- data.frame(
  priori = c(
    "Inversa-Gama",
    "Jeffreys",
    "SBeta2(1,1,1)",
    "SBeta2(0.5,0.5,1)",
    "SBeta2(1,0.5,1)"
  ),
  
  distribuicao_analitica = c(
    paste0("IG(", round(alpha_post_IG_n500_s16, 3), ", ",
           round(beta_post_IG_n500_s16, 3), ")"),
    paste0("IG(", round(alpha_post_J_n500_s16, 3), ", ",
           round(beta_post_J_n500_s16, 3), ")"),
    NA, NA, NA
  ),
  
  media_analitica = c(
    media_analitica_IG_n500_s16,
    media_analitica_J_n500_s16,
    NA, NA, NA
  ),
  
  media_stan = c(
    media_stan_IG_n500_s16,
    media_stan_J_n500_s16,
    media_SB111_n500_s16,
    media_SB0505_n500_s16,
    media_SB105_n500_s16
  ),
  
  moda_analitica = c(
    moda_analitica_IG_n500_s16,
    moda_analitica_J_n500_s16,
    NA, NA, NA
  ),
  
  moda_stan = c(
    moda_stan_IG_n500_s16,
    moda_stan_J_n500_s16,
    moda_SB111_n500_s16,
    moda_SB0505_n500_s16,
    moda_SB105_n500_s16
  ),
  
  mediana_analitica = c(
    mediana_analitica_IG_n500_s16,
    mediana_analitica_J_n500_s16,
    NA, NA, NA
  ),
  
  mediana_stan = c(
    mediana_stan_IG_n500_s16,
    mediana_stan_J_n500_s16,
    mediana_SB111_n500_s16,
    mediana_SB0505_n500_s16,
    mediana_SB105_n500_s16
  )
)

resultados_n500_s16

# Resultados -------------------------------------------------------------------

resultados_n20_s025
resultados_n20_s1
resultados_n20_s4
resultados_n20_s16

resultados_n100_s025
resultados_n100_s1
resultados_n100_s4
resultados_n100_s16

resultados_n500_s025
resultados_n500_s1
resultados_n500_s4
resultados_n500_s16

# GRáficos ---------------------------------------------------------------------


library(rstan)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

extrair_posteriori <- function(ajuste, priori, n, sigmaR2) {
  tibble(
    sigma2 = rstan::extract(ajuste)$sigma2,
    priori = priori,
    n = n,
    sigmaR2 = sigmaR2
  )
}

posteriori_todas <- bind_rows(
  # n = 20
  extrair_posteriori(ajuste_IG_n20_s025, "Inversa-Gama", 20, 0.25),
  extrair_posteriori(ajuste_jeffreys_n20_s025, "Jeffreys", 20, 0.25),
  extrair_posteriori(ajuste_sbeta2_111_n20_s025, "SBeta2(1,1,1)", 20, 0.25),
  extrair_posteriori(ajuste_sbeta2_0505_1_n20_s025, "SBeta2(0.5,0.5,1)", 20, 0.25),
  extrair_posteriori(ajuste_sbeta2_105_1_n20_s025, "SBeta2(1,0.5,1)", 20, 0.25),
  
  extrair_posteriori(ajuste_IG_n20_s1, "Inversa-Gama", 20, 1),
  extrair_posteriori(ajuste_jeffreys_n20_s1, "Jeffreys", 20, 1),
  extrair_posteriori(ajuste_sbeta2_111_n20_s1, "SBeta2(1,1,1)", 20, 1),
  extrair_posteriori(ajuste_sbeta2_0505_1_n20_s1, "SBeta2(0.5,0.5,1)", 20, 1),
  extrair_posteriori(ajuste_sbeta2_105_1_n20_s1, "SBeta2(1,0.5,1)", 20, 1),
  
  extrair_posteriori(ajuste_IG_n20_s4, "Inversa-Gama", 20, 4),
  extrair_posteriori(ajuste_jeffreys_n20_s4, "Jeffreys", 20, 4),
  extrair_posteriori(ajuste_sbeta2_111_n20_s4, "SBeta2(1,1,1)", 20, 4),
  extrair_posteriori(ajuste_sbeta2_0505_1_n20_s4, "SBeta2(0.5,0.5,1)", 20, 4),
  extrair_posteriori(ajuste_sbeta2_105_1_n20_s4, "SBeta2(1,0.5,1)", 20, 4),
  
  extrair_posteriori(ajuste_IG_n20_s16, "Inversa-Gama", 20, 16),
  extrair_posteriori(ajuste_jeffreys_n20_s16, "Jeffreys", 20, 16),
  extrair_posteriori(ajuste_sbeta2_111_n20_s16, "SBeta2(1,1,1)", 20, 16),
  extrair_posteriori(ajuste_sbeta2_0505_1_n20_s16, "SBeta2(0.5,0.5,1)", 20, 16),
  extrair_posteriori(ajuste_sbeta2_105_1_n20_s16, "SBeta2(1,0.5,1)", 20, 16),
  
  # n = 100
  extrair_posteriori(ajuste_IG_n100_s025, "Inversa-Gama", 100, 0.25),
  extrair_posteriori(ajuste_jeffreys_n100_s025, "Jeffreys", 100, 0.25),
  extrair_posteriori(ajuste_sbeta2_111_n100_s025, "SBeta2(1,1,1)", 100, 0.25),
  extrair_posteriori(ajuste_sbeta2_0505_1_n100_s025, "SBeta2(0.5,0.5,1)", 100, 0.25),
  extrair_posteriori(ajuste_sbeta2_105_1_n100_s025, "SBeta2(1,0.5,1)", 100, 0.25),
  
  extrair_posteriori(ajuste_IG_n100_s1, "Inversa-Gama", 100, 1),
  extrair_posteriori(ajuste_jeffreys_n100_s1, "Jeffreys", 100, 1),
  extrair_posteriori(ajuste_sbeta2_111_n100_s1, "SBeta2(1,1,1)", 100, 1),
  extrair_posteriori(ajuste_sbeta2_0505_1_n100_s1, "SBeta2(0.5,0.5,1)", 100, 1),
  extrair_posteriori(ajuste_sbeta2_105_1_n100_s1, "SBeta2(1,0.5,1)", 100, 1),
  
  extrair_posteriori(ajuste_IG_n100_s4, "Inversa-Gama", 100, 4),
  extrair_posteriori(ajuste_jeffreys_n100_s4, "Jeffreys", 100, 4),
  extrair_posteriori(ajuste_sbeta2_111_n100_s4, "SBeta2(1,1,1)", 100, 4),
  extrair_posteriori(ajuste_sbeta2_0505_1_n100_s4, "SBeta2(0.5,0.5,1)", 100, 4),
  extrair_posteriori(ajuste_sbeta2_105_1_n100_s4, "SBeta2(1,0.5,1)", 100, 4),
  
  extrair_posteriori(ajuste_IG_n100_s16, "Inversa-Gama", 100, 16),
  extrair_posteriori(ajuste_jeffreys_n100_s16, "Jeffreys", 100, 16),
  extrair_posteriori(ajuste_sbeta2_111_n100_s16, "SBeta2(1,1,1)", 100, 16),
  extrair_posteriori(ajuste_sbeta2_0505_1_n100_s16, "SBeta2(0.5,0.5,1)", 100, 16),
  extrair_posteriori(ajuste_sbeta2_105_1_n100_s16, "SBeta2(1,0.5,1)", 100, 16),
  
  # n = 500
  extrair_posteriori(ajuste_IG_n500_s025, "Inversa-Gama", 500, 0.25),
  extrair_posteriori(ajuste_jeffreys_n500_s025, "Jeffreys", 500, 0.25),
  extrair_posteriori(ajuste_sbeta2_111_n500_s025, "SBeta2(1,1,1)", 500, 0.25),
  extrair_posteriori(ajuste_sbeta2_0505_1_n500_s025, "SBeta2(0.5,0.5,1)", 500, 0.25),
  extrair_posteriori(ajuste_sbeta2_105_1_n500_s025, "SBeta2(1,0.5,1)", 500, 0.25),
  
  extrair_posteriori(ajuste_IG_n500_s1, "Inversa-Gama", 500, 1),
  extrair_posteriori(ajuste_jeffreys_n500_s1, "Jeffreys", 500, 1),
  extrair_posteriori(ajuste_sbeta2_111_n500_s1, "SBeta2(1,1,1)", 500, 1),
  extrair_posteriori(ajuste_sbeta2_0505_1_n500_s1, "SBeta2(0.5,0.5,1)", 500, 1),
  extrair_posteriori(ajuste_sbeta2_105_1_n500_s1, "SBeta2(1,0.5,1)", 500, 1),
  
  extrair_posteriori(ajuste_IG_n500_s4, "Inversa-Gama", 500, 4),
  extrair_posteriori(ajuste_jeffreys_n500_s4, "Jeffreys", 500, 4),
  extrair_posteriori(ajuste_sbeta2_111_n500_s4, "SBeta2(1,1,1)", 500, 4),
  extrair_posteriori(ajuste_sbeta2_0505_1_n500_s4, "SBeta2(0.5,0.5,1)", 500, 4),
  extrair_posteriori(ajuste_sbeta2_105_1_n500_s4, "SBeta2(1,0.5,1)", 500, 4),
  
  extrair_posteriori(ajuste_IG_n500_s16, "Inversa-Gama", 500, 16),
  extrair_posteriori(ajuste_jeffreys_n500_s16, "Jeffreys", 500, 16),
  extrair_posteriori(ajuste_sbeta2_111_n500_s16, "SBeta2(1,1,1)", 500, 16),
  extrair_posteriori(ajuste_sbeta2_0505_1_n500_s16, "SBeta2(0.5,0.5,1)", 500, 16),
  extrair_posteriori(ajuste_sbeta2_105_1_n500_s16, "SBeta2(1,0.5,1)", 500, 16)
)

valores_verdadeiros <- tibble(
  sigmaR2 = c(0.25, 1, 4, 16),
  sigma_true = c(0.25, 1, 4, 16)
)

grafico_posteriori <- function(n_escolhido) {
  
  dados_plot <- posteriori_todas |>
    filter(n == n_escolhido) |>
    left_join(valores_verdadeiros, by = "sigmaR2")
  
  ggplot(dados_plot, aes(x = sigma2, colour = priori)) +
    geom_density(linewidth = 0.8) +
    geom_vline(
      aes(xintercept = sigma_true),
      linetype = "dashed",
      linewidth = 0.6,
      inherit.aes = FALSE
    ) +
    facet_wrap(
      ~ sigmaR2,
      scales = "free",
      ncol = 2,
      labeller = label_bquote(sigma[R]^2 == .(sigmaR2))
    ) +
    labs(
      x = expression(sigma^2),
      y = "Densidade a posteriori",
      colour = "Priori",
      title = paste("Distribuições a posteriori para n =", n_escolhido)
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
}

grafico_posteriori(20)
grafico_posteriori(100)
grafico_posteriori(500)

g20 <- grafico_posteriori(20)
g100 <- grafico_posteriori(100)
g500 <- grafico_posteriori(500)

ggsave(
  "p1/figuras/posteriori/posteriori_n20.png",
  plot = g20,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "p1/figuras/posteriori/posteriori_n100.png",
  plot = g100,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "p1/figuras/posteriori/posteriori_n500.png",
  plot = g500,
  width = 8,
  height = 6,
  dpi = 300
)