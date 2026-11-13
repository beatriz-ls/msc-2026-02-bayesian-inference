
################################################################################
# PROVA PRÁTICA II - INFERÊNCIA BAYESIANA
# Exercício 1(c) - Modelos bayesianos de regressão linear
################################################################################

library(tidyverse)
library(cmdstanr)
library(posterior)

# Instalação inicial do CmdStan, caso ainda não esteja instalado:
cmdstanr::install_cmdstan()

################################################################################
# 1. LEITURA DOS DADOS
################################################################################

# Ajuste o caminho conforme a localização do arquivo TXT
dados <- read.delim(
  "p2/dados.txt",
  header = TRUE,
  sep = "\t",
  dec = "."
) |>
  select(y, x1, x2, x3, x5, x7) |>
  rename(
    preco = y,
    impostos = x1,
    banheiros = x2,
    tamanho_lote = x3,
    vagas_garagem = x5,
    quartos = x7
  )

# Criar a pasta de resultados
dir.create("p2/stan", recursive = TRUE, showWarnings = FALSE)
dir.create("p2/plot", recursive = TRUE, showWarnings = FALSE)

################################################################################
# 2. MATRIZ DE DELINEAMENTO
################################################################################

formula_modelo <- preco ~ impostos + banheiros +
  tamanho_lote + vagas_garagem + quartos

X <- model.matrix(formula_modelo, data = dados)
Y <- dados$preco

n <- nrow(X)
p <- ncol(X)

# Nomes dos parâmetros
nomes_beta <- colnames(X)

# Ajuste clássico para comparação com as estimativas bayesianas
modelo_mqo <- lm(formula_modelo, data = dados)

beta_mqo <- coef(modelo_mqo)

# Dados compartilhados pelos três modelos Stan
dados_stan <- list(
  N = n,
  P = p,
  X = X,
  y = as.vector(Y)
)

################################################################################
# 3. HIPERPARÂMETROS
################################################################################

# Priori conjugada natural:
# beta | sigma2 ~ Normal_p(0, sigma2 * 10000 * I)
# sigma2 ~ Gama-Inversa(0.01, 0.01)

b0 <- rep(0, p)
escala_beta <- rep(100, p)

# Hiperparâmetros da gama inversa
a0 <- 0.01
d0 <- 0.01

# Spike-slab:
# Spike: beta_j ~ Normal(0, 0.1^2)
# Slab:  beta_j ~ Normal(0, 10^2)
# Probabilidade a priori de inclusão: 0.5

tau_spike <- 0.1
tau_slab <- 10
prob_inclusao <- 0.5


################################################################################
# 4. MODELO 1 - PRIORI CONJUGADA NATURAL
################################################################################

# Compilar
modelo_conjugada <- cmdstan_model("p2/stan/conjugada.stan")

# Adicionar os hiperparâmetros aos dados
dados_conjugada <- c(
  dados_stan,
  list(
    b0 = b0,
    escala_beta = escala_beta,
    a0 = a0,
    d0 = d0
  )
)

# Amostragem MCMC
ajuste_conjugada <- modelo_conjugada$sample(
  data = dados_conjugada,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 4000,
  adapt_delta = 0.95,
  refresh = 500
)

# Resumo inicial
ajuste_conjugada$summary()


################################################################################
# 5. MODELO 2 - JEFFREYS INDEPENDENTE
################################################################################

# Compilar
modelo_jeffreys <- cmdstan_model("p2/stan/jeffreys.stan")

# Amostragem MCMC
ajuste_jeffreys <- modelo_jeffreys$sample(
  data = dados_stan,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 4000,
  adapt_delta = 0.95,
  refresh = 500
)

ajuste_jeffreys$summary()


################################################################################
# 6. MODELO 3 - SPIKE-SLAB
################################################################################

# Compilar
modelo_spike_slab <- cmdstan_model("p2/stan/spike-slab.stan")

# Dados do modelo
dados_spike_slab <- c(
  dados_stan,
  list(
    a0 = a0,
    d0 = d0,
    tau_spike = tau_spike,
    tau_slab = tau_slab,
    prob_inclusao = prob_inclusao
  )
)

# Amostragem MCMC
ajuste_spike_slab <- modelo_spike_slab$sample(
  data = dados_spike_slab,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 4000,
  adapt_delta = 0.99,
  refresh = 500
)

ajuste_spike_slab$summary()

################################################################################
# 7. MODELO 4 - SPIKE-SLAB COM PROBABILIDADE DE INCLUSÃO UNIFORME
################################################################################

# Compilar o modelo
modelo_spike_uniforme <- cmdstan_model(
  "p2/stan/spike_slab_uniforme.stan"
)

# Dados e hiperparâmetros
dados_spike_uniforme <- c(
  dados_stan,
  list(
    a0 = a0,
    d0 = d0,
    tau_spike = tau_spike,
    tau_slab = tau_slab
  )
)

# Amostragem MCMC
ajuste_spike_uniforme <- modelo_spike_uniforme$sample(
  data = dados_spike_uniforme,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 4000,
  adapt_delta = 0.99,
  refresh = 500
)

# Resumo dos parâmetros
ajuste_spike_uniforme$summary()

# Diagnósticos de convergência
ajuste_spike_uniforme$diagnostic_summary()

# Salvar ajuste
ajuste_spike_uniforme$save_object(
  file = "p2/stan/ajuste_spike_uniforme.rds"
)


################################################################################
# 8. RESUMOS DAS DISTRIBUIÇÕES A POSTERIORI
################################################################################

# Função para calcular intervalo HPD empírico de 95%
hpd_95 <- function(x) {
  x <- sort(x[is.finite(x)])
  n <- length(x)
  k <- ceiling(0.95 * n)
  
  larguras <- x[(k + 1):n] - x[1:(n - k)]
  inicio <- which.min(larguras)
  
  c(
    inferior = x[inicio],
    superior = x[inicio + k]
  )
}

# Função para estimar a moda marginal a posteriori
moda_posteriori <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

# Função para extrair as estatísticas de cada modelo
resume_posteriori <- function(ajuste, nome_modelo) {
  
  amostras <- as.matrix(
    ajuste$draws(
      variables = c("beta", "sigma2"),
      format = "draws_matrix"
    )
  )
  
  nomes_parametros <- c(
    paste0("beta[", seq_len(p), "]"),
    "sigma2"
  )
  
  map_dfr(nomes_parametros, function(parametro) {
    
    x <- amostras[, parametro]
    hpd <- hpd_95(x)
    
    tibble(
      modelo = nome_modelo,
      parametro = parametro,
      media = mean(x),
      mediana = median(x),
      moda = moda_posteriori(x),
      hpd_inferior = hpd["inferior"],
      hpd_superior = hpd["superior"],
      prob_positivo = mean(x > 0)
    )
  })
}

# Resumir os quatro ajustes
resultados_bayesianos <- bind_rows(
  resume_posteriori(ajuste_conjugada, "Conjugada"),
  resume_posteriori(ajuste_jeffreys, "Jeffreys"),
  resume_posteriori(ajuste_spike_slab, "Spike-slab (p=0,5)"),
  resume_posteriori(ajuste_spike_uniforme, "Spike-slab (p~U)")
) |>
  mutate(
    nome = recode(
      parametro,
      !!!setNames(
        c(nomes_beta, "Variância residual"),
        c(paste0("beta[", seq_len(p), "]"), "sigma2")
      )
    )
  )

# Visualizar
resultados_bayesianos |>
  select(modelo, nome, media, mediana, moda) |>
  mutate(across(where(is.numeric), ~round(.x, 4))) |>
  print(n = Inf)

# Salvar
write.csv(
  resultados_bayesianos,
  "p2/stan/resultados_posteriori.csv",
  row.names = FALSE
)
# Visualizar os resultados
resultados_bayesianos |>
  select(modelo, nome, media, mediana, moda) |>
  mutate(across(where(is.numeric), ~round(.x, 4))) |>
  print(n = Inf)

# Salvar resultados
write.csv(
  resultados_bayesianos,
  "p2/stan/resultados_posteriori.csv",
  row.names = FALSE
)


################################################################################
# 9. COMPARAÇÃO COM MÍNIMOS QUADRADOS
################################################################################

comparacao_estimadores <- resultados_bayesianos |>
  filter(parametro != "sigma2") |>
  mutate(
    mqo = as.numeric(beta_mqo)[
      match(nome, nomes_beta)
    ]
  ) |>
  select(
    modelo, nome, mqo, media, mediana, moda
  ) |>
  mutate(
    across(where(is.numeric), ~round(.x, 4))
  )

print(comparacao_estimadores, n = Inf)

write.csv(
  comparacao_estimadores,
  "p2/stan/comparacao_estimadores.csv",
  row.names = FALSE
)


################################################################################
# 10. INTERVALOS HPD E PROBABILIDADES POSTERIORES
################################################################################

tabela_inferencia <- resultados_bayesianos |>
  filter(parametro != "sigma2") |>
  mutate(
    hpd_contem_zero =
      hpd_inferior <= 0 & hpd_superior >= 0
  ) |>
  select(
    modelo,
    nome,
    hpd_inferior,
    hpd_superior,
    hpd_contem_zero,
    prob_positivo
  )

print(
  tabela_inferencia |>
    mutate(across(where(is.numeric), ~round(.x, 4))),
  n = Inf
)

write.csv(
  tabela_inferencia,
  "p2/stan/inferencia_coeficientes.csv",
  row.names = FALSE
)


################################################################################
# 11. PROBABILIDADES POSTERIORES DE INCLUSÃO - ITENS (c5) E (d)
################################################################################

# Indicadores do spike-slab com p = 0,5
amostras_z_fixo <- as.matrix(
  ajuste_spike_slab$draws(
    variables = "z",
    format = "draws_matrix"
  )
)

# Probabilidades condicionais do spike-slab com p = 0,5
amostras_p_fixo <- as.matrix(
  ajuste_spike_slab$draws(
    variables = "p_inclusao",
    format = "draws_matrix"
  )
)

# Indicadores do spike-slab com p ~ Uniforme(0,1)
amostras_z_uniforme <- as.matrix(
  ajuste_spike_uniforme$draws(
    variables = "z",
    format = "draws_matrix"
  )
)

# Probabilidades condicionais do spike-slab com p ~ Uniforme(0,1)
amostras_p_uniforme <- as.matrix(
  ajuste_spike_uniforme$draws(
    variables = "p_inclusao",
    format = "draws_matrix"
  )
)

# Comparar os dois modelos
tabela_inclusao <- tibble(
  variavel = nomes_beta[-1],
  
  # Modelo com probabilidade fixa
  percentual_z_1_fixo = 100 * colMeans(amostras_z_fixo),
  prob_inclusao_fixo = colMeans(amostras_p_fixo),
  
  # Modelo com probabilidade uniforme
  percentual_z_1_uniforme = 100 * colMeans(amostras_z_uniforme),
  prob_inclusao_uniforme = colMeans(amostras_p_uniforme)
) |>
  mutate(
    diferenca = prob_inclusao_uniforme - prob_inclusao_fixo
  )

print(tabela_inclusao)

# Salvar tabela
write.csv(
  tabela_inclusao,
  "p2/stan/comparacao_probabilidades_inclusao.csv",
  row.names = FALSE
)