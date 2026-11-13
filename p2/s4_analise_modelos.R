################################################################################
# PROVA PRATICA II - ITEM 1(c)
# Avaliacao dos modelos bayesianos ja ajustados no Stan
# Nao recompila modelos e nao executa MCMC novamente.
# Saidas: p2/tabelas/ e p2/plot/avaliacao/
################################################################################

library(tidyverse)
library(posterior)
library(bayesplot)
library(viridis)

set.seed(123)
dir.create("p2/tabelas", recursive = TRUE, showWarnings = FALSE)
dir.create("p2/plot/avaliacao", recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1. RECUPERAR AJUSTES E DADOS
################################################################################

# Se estiver na mesma sessao do R que executou o Stan, usa os objetos existentes.
# Caso contrario, carrega os ajustes salvos como RDS.
carrega_ajuste <- function(nome_objeto, arquivo) {
  if (exists(nome_objeto, envir = .GlobalEnv)) {
    return(get(nome_objeto, envir = .GlobalEnv))
  }
  if (file.exists(arquivo)) return(readRDS(arquivo))
  stop(paste0(
    "Ajuste nao encontrado: ", nome_objeto, ". Salve o ajuste com ",
    nome_objeto, "$save_object(file = ", arquivo, ") ou rode este script ",
    "na mesma sessao onde os ajustes existem."
  ))
}

modelos <- list(
  Conjugada = carrega_ajuste("ajuste_conjugada", "p2/stan/ajuste_conjugada.rds"),
  Jeffreys = carrega_ajuste("ajuste_jeffreys", "p2/stan/ajuste_jeffreys.rds"),
  `Spike-slab` = carrega_ajuste("ajuste_spike_slab", "p2/stan/ajuste_spike_slab.rds")
)

if (!exists("dados", envir = .GlobalEnv)) {
  candidatos <- c("p2/dados_imoveis.txt", "dados_imoveis.txt")
  caminho <- candidatos[file.exists(candidatos)][1]
  if (is.na(caminho)) stop("Nao encontrei dados_imoveis.txt. Ajuste o caminho no script.")
  dados <- read.delim(caminho, header = TRUE, sep = "\t", dec = ".") |>
    select(y, x1, x2, x3, x5, x7) |>
    rename(preco = y, impostos = x1, banheiros = x2,
           tamanho_lote = x3, vagas_garagem = x5, quartos = x7)
}

formula_modelo <- preco ~ impostos + banheiros + tamanho_lote + vagas_garagem + quartos
X <- model.matrix(formula_modelo, data = dados)
Y <- dados$preco
P <- ncol(X)
N <- nrow(X)
nomes_beta <- colnames(X)
nomes_exibicao <- c("Intercepto", "Impostos", "Banheiros", "Tamanho do lote",
                    "Vagas de garagem", "Quartos")
chaves_beta <- paste0("beta[", seq_len(P), "]")
dicionario <- setNames(nomes_exibicao, chaves_beta)

modelo_mqo <- lm(formula_modelo, data = dados)
beta_mqo <- coef(modelo_mqo)

cores_modelos <- setNames(viridis(3, option = "D", begin = 0.1, end = 0.85), names(modelos))
cores_variaveis <- setNames(viridis(P, option = "D", begin = 0.08, end = 0.92), nomes_exibicao)

tema <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"))

salvar_plot <- function(grafico, nome, largura = 11, altura = 7) {
  ggsave(file.path("p2/plot/avaliacao", nome), plot = grafico,
         width = largura, height = altura, dpi = 300, bg = "white")
}

################################################################################
# 2. CONVERGENCIA MCMC
################################################################################

# Resumimos apenas beta e sigma2; os indicadores z no generated quantities
# nao entram no criterio principal de convergencia dos parametros continuos.
diagnosticos <- imap_dfr(modelos, ~.x$summary(variables = c("beta", "sigma2")) |>
                           mutate(modelo = .y)) |>
  select(modelo, variable, mean, sd, rhat, ess_bulk, ess_tail) |>
  mutate(
    variavel = recode(variable, !!!dicionario, sigma2 = "Variancia residual"),
    alerta = is.na(rhat) | rhat >= 1.01 | ess_bulk < 400 | ess_tail < 400
  ) |>
  relocate(modelo, variavel, variable)

write_csv(diagnosticos, "p2/tabelas/01_diagnosticos_mcmc.csv")
print(diagnosticos, n = Inf)

# Divergencias informadas pelos diagnostics de amostragem.
divergencias <- imap_dfr(modelos, function(ajuste, nome) {
  sd <- as.matrix(ajuste$sampler_diagnostics(format = "draws_matrix"))
  tibble(
    modelo = nome,
    divergencias = if ("divergent__" %in% colnames(sd)) sum(sd[, "divergent__"]) else NA_integer_,
    total_iteracoes = nrow(sd)
  )
})
write_csv(divergencias, "p2/tabelas/02_divergencias.csv")
print(divergencias)

# Traceplot dos coeficientes de cada modelo
imap(modelos, function(ajuste, nome) {
  g <- bayesplot::mcmc_trace(
    ajuste$draws(variables = "beta", format = "draws_array")
  ) + ggtitle(paste("Traceplots -", nome))
  salvar_plot(g, paste0("01_trace_", gsub("[^[:alnum:]]", "_", nome), ".png"), 12, 10)
  invisible(g)
})

################################################################################
# 3. AMOSTRAS E ESTATISTICAS POSTERIORES (ITENS c1-c4)
################################################################################

# Intervalo empirico mais curto contendo ao menos 95% das amostras.
# Aproximacao HPD: cuidado se a posteriori for multimodal, pois uma regiao
# HPD verdadeira pode ser desconexa.
intervalo_mais_curto <- function(x, prob = 0.95) {
  x <- sort(x[is.finite(x)])
  n <- length(x)
  k <- min(n - 1L, ceiling(prob * n))
  if (n < 3L) return(c(inferior = NA_real_, superior = NA_real_))
  inicios <- seq_len(n - k)
  larguras <- x[inicios + k] - x[inicios]
  i <- inicios[which.min(larguras)]
  c(inferior = x[i], superior = x[i + k])
}

moda_marginal <- function(x) {
  d <- density(x, n = 2048)
  d$x[which.max(d$y)]
}

matrizes <- map(modelos, ~as.matrix(.x$draws(
  variables = c("beta", "sigma2"), format = "draws_matrix")))

resumo_posteriori <- imap_dfr(matrizes, function(matriz, nome) {
  map_dfr(colnames(matriz), function(param) {
    valores <- matriz[, param]
    intervalo <- intervalo_mais_curto(valores)
    tibble(
      modelo = nome,
      parametro = param,
      variavel = recode(param, !!!dicionario, sigma2 = "Variancia residual"),
      media = mean(valores),
      mediana = median(valores),
      moda_marginal = moda_marginal(valores),
      hpd_inferior = unname(intervalo["inferior"]),
      hpd_superior = unname(intervalo["superior"]),
      prob_positivo = mean(valores > 0)
    )
  })
}) |>
  mutate(hpd_contem_zero = hpd_inferior <= 0 & hpd_superior >= 0)

write_csv(resumo_posteriori, "p2/tabelas/03_resumo_posteriori.csv")

comparacao <- resumo_posteriori |>
  filter(parametro %in% chaves_beta) |>
  mutate(mqo = as.numeric(beta_mqo[match(parametro, chaves_beta)])) |>
  select(modelo, variavel, mqo, media, mediana, moda_marginal,
         hpd_inferior, hpd_superior, hpd_contem_zero, prob_positivo)
write_csv(comparacao, "p2/tabelas/04_comparacao_mqo_bayes.csv")
print(comparacao, n = Inf)

# Densidades marginais por coeficiente, com MQO tracejado
amostras_beta <- imap_dfr(matrizes, function(matriz, nome) {
  as_tibble(matriz[, chaves_beta, drop = FALSE], .name_repair = "minimal") |>
    mutate(modelo = nome)
}) |>
  pivot_longer(cols = all_of(chaves_beta),
               names_to = "parametro", values_to = "valor") |>
  mutate(variavel = recode(parametro, !!!dicionario))

linhas_mqo <- tibble(variavel = nomes_exibicao, mqo = unname(beta_mqo))

g_densidades <- ggplot(amostras_beta, aes(x = valor, color = modelo)) +
  geom_density(linewidth = 0.9) +
  geom_vline(data = linhas_mqo, aes(xintercept = mqo),
             inherit.aes = FALSE, linetype = "dashed", color = "grey40") +
  facet_wrap(~variavel, scales = "free", ncol = 3) +
  scale_color_manual(values = cores_modelos) +
  labs(title = "Distribuicoes marginais a posteriori",
       subtitle = "Linha tracejada: estimativa MQO",
       x = "Coeficiente", y = "Densidade", color = "Modelo") + tema
salvar_plot(g_densidades, "02_densidades_coeficientes.png", 12, 8)

# Intervalos de mais alta densidade aproximados e medias posteriores
# Legenda: cada ponto representa a media posterior.
g_intervalos <- comparacao |>
  ggplot(aes(x = media, y = variavel, color = modelo)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  geom_errorbarh(aes(xmin = hpd_inferior, xmax = hpd_superior),
                 height = 0.2, position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6), size = 2.4) +
  facet_wrap(~variavel, scales = "free_x", ncol = 2) +
  scale_color_manual(values = cores_modelos) +
  labs(title = "Medias posteriores e intervalos HPD de 95%",
       x = "Valor do coeficiente", y = NULL, color = "Modelo") + tema +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
salvar_plot(g_intervalos, "03_intervalos_hpd.png", 12, 8)

# Probabilidade de efeito positivo (nao equivale a causalidade)
g_sinal <- comparacao |>
  filter(variavel != "Intercepto") |>
  ggplot(aes(x = variavel, y = prob_positivo, fill = modelo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = cores_modelos) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(title = "Probabilidade posterior de efeito positivo",
       x = NULL, y = "P(beta > 0 | dados)", fill = "Modelo") + tema
salvar_plot(g_sinal, "04_probabilidade_positiva.png", 10, 6)

################################################################################
# 4. SPIKE-SLAB: PROBABILIDADE DE INCLUSAO (ITEM c5)
################################################################################

ajuste_spike <- modelos[["Spike-slab"]]
p_inclusao <- as.matrix(ajuste_spike$draws(
  variables = "p_inclusao", format = "draws_matrix"))
z_draws <- as.matrix(ajuste_spike$draws(
  variables = "z", format = "draws_matrix"))

tabela_inclusao <- tibble(
  variavel = nomes_exibicao[-1],
  percentual_z_1 = 100 * colMeans(z_draws),
  prob_inclusao = colMeans(p_inclusao)
)
write_csv(tabela_inclusao, "p2/tabelas/05_inclusao_spike_slab.csv")
print(tabela_inclusao)

g_inclusao <- ggplot(tabela_inclusao,
                    aes(x = reorder(variavel, prob_inclusao),
                        y = prob_inclusao, fill = variavel)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = scales::percent(prob_inclusao, accuracy = 0.1)),
            hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = cores_variaveis) +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2)) +
  labs(title = "Spike-slab: probabilidade posterior de inclusao",
       x = NULL, y = "P(z = 1 | dados)") + tema +
  theme(legend.position = "none")
salvar_plot(g_inclusao, "05_inclusao_spike_slab.png", 9, 5)

################################################################################
# 5. AJUSTE AOS DADOS E VERIFICACAO PREDITIVA A POSTERIORI
################################################################################

# Gera uma amostra de replicacoes de Y para cada modelo.
# Mu = X beta e Y_rep ~ Normal(mu, sigma2).
replicar_y <- function(matriz, X, n_rep = 400L) {
  ids <- sample(seq_len(nrow(matriz)), size = n_rep,
                replace = n_rep > nrow(matriz))
  B <- matriz[ids, chaves_beta, drop = FALSE]
  mu <- B %*% t(X)  # n_rep x n_observacoes
  sigmas <- sqrt(matriz[ids, "sigma2"])
  ruido <- matrix(rnorm(length(mu)), nrow = nrow(mu), ncol = ncol(mu))
  yrep <- mu + sweep(ruido, 1, sigmas, `*`)
  list(mu = mu, yrep = yrep)
}

replicacoes <- map(matrizes, replicar_y, X = X)

# PPC: distribuicoes de precos observados e replicados
imap(replicacoes, function(obj, nome) {
  g <- bayesplot::ppc_dens_overlay(Y, obj$yrep[1:100, , drop = FALSE]) +
    ggtitle(paste("Distribuicao preditiva a posteriori -", nome))
  salvar_plot(g, paste0("06_ppc_", gsub("[^[:alnum:]]", "_", nome), ".png"), 9, 5)
  invisible(g)
})

# Precos esperados a posteriori e intervalos preditivos (incluem ruido residual)
precos_ajustados <- imap_dfr(replicacoes, function(obj, nome) {
  tibble(
    modelo = nome,
    imovel = seq_len(N),
    observado = Y,
    estimado = colMeans(obj$mu),
    pred_inf = apply(obj$yrep, 2, quantile, probs = 0.025),
    pred_sup = apply(obj$yrep, 2, quantile, probs = 0.975),
    residuo = observado - estimado
  )
})
write_csv(precos_ajustados, "p2/tabelas/06_precos_ajustados.csv")

g_obs_ajust <- ggplot(precos_ajustados,
                     aes(x = estimado, y = observado, color = modelo)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 2.3, alpha = 0.85) +
  facet_wrap(~modelo, ncol = 3) +
  scale_color_manual(values = cores_modelos) +
  coord_equal() +
  labs(title = "Preco observado versus media posterior ajustada",
       x = "Preco medio ajustado", y = "Preco observado") + tema +
  theme(legend.position = "none")
salvar_plot(g_obs_ajust, "07_observado_ajustado.png", 12, 5)

# Indicadores de ajuste nos dados de treinamento (NAO sao validacao fora da amostra)
tabela_ajuste <- precos_ajustados |>
  group_by(modelo) |>
  summarise(
    RMSE_treino = sqrt(mean((observado - estimado)^2)),
    MAE_treino = mean(abs(observado - estimado)),
    cobertura_pred_95 = mean(observado >= pred_inf & observado <= pred_sup),
    .groups = "drop"
  )
write_csv(tabela_ajuste, "p2/tabelas/07_ajuste_treinamento.csv")
print(tabela_ajuste)

################################################################################
# 6. RESUMO FINAL NO CONSOLE
################################################################################

cat("\n===== ALERTAS DE CONVERGENCIA =====\n")
print(diagnosticos |> filter(alerta), n = Inf)
cat("\n===== DIVERGENCIAS =====\n")
print(divergencias)
cat("\n===== COMPARACAO DE COEFICIENTES =====\n")
print(comparacao, n = Inf)
cat("\n===== INCLUSAO SPIKE-SLAB =====\n")
print(tabela_inclusao)
cat("\n===== AJUSTE EM TREINO =====\n")
print(tabela_ajuste)
cat("\nArquivos salvos em p2/tabelas/ e p2/plot/avaliacao/\n")

# Notas:
# - A moda estimada e MARGINAL (por densidade kernel), nao a moda conjunta.
# - O HPD aqui e uma aproximacao por intervalo curto, nao uma regiao desconexa.
# - Probabilidade de inclusao depende da escolha dos hiperparametros spike/slab.
# - RMSE/MAE aqui sao dos proprios dados ajustados, nao previsao externa.
# - Para comparacao preditiva fora da amostra, implementar LOO ou validacao cruzada.
