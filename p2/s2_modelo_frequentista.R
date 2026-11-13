################################################################################
# Exercício 1(b) - Regressão linear múltipla
################################################################################

library(tidyverse)
library(broom)

# Ajustar o modelo por mínimos quadrados ordinários
modelo_mqo <- lm(
  preco ~ impostos + banheiros + tamanho_lote + vagas_garagem + quartos,
  data = dados
)

# Resumo do modelo
summary(modelo_mqo)

# Estimativas dos coeficientes
coef(modelo_mqo)

# Intervalos de confiança clássicos de 95%
confint(modelo_mqo)

# Tabela com estimativas, erros-padrão, estatísticas t e valores-p
tabela_mqo <- tidy(modelo_mqo, conf.int = TRUE) |>
  mutate(across(where(is.numeric), ~round(.x, 4)))

print(tabela_mqo)

# Obter valores observados e ajustados
dados_ajuste <- dados |>
  mutate(
    preco_ajustado = fitted(modelo_mqo),
    residuos = resid(modelo_mqo)
  )

# Gráfico de valores observados versus ajustados
grafico_ajuste <- ggplot(
  dados_ajuste,
  aes(x = preco_ajustado, y = preco)
) +
  geom_point(
    color = cores_variaveis["Preço de venda"],
    size = 2.8,
    alpha = 0.8
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.8
  ) +
  coord_equal() +
  labs(
    title = "Valores observados versus ajustados",
    x = "Preço ajustado (milhares)",
    y = "Preço observado (milhares)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

# Visualizar
grafico_ajuste

# Salvar
ggsave(
  "p2/plot/modelo_mqo_observados_ajustados.png",
  plot = grafico_ajuste,
  width = 6.5,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Coeficientes, erros-padrão, estatísticas t e valores-p
summary(modelo_mqo)

# Medidas de qualidade do ajuste
broom::glance(modelo_mqo) |>
  select(
    r.squared,
    adj.r.squared,
    sigma,
    statistic,
    p.value
  )


# Verificação da fórmula matricial

# Matriz de delineamento, incluindo o intercepto
X <- model.matrix(modelo_mqo)

# Vetor resposta
Y <- dados$preco

# Estimador de mínimos quadrados
beta_mqo <- solve(crossprod(X), crossprod(X, Y))

print(beta_mqo)

# Verificar se coincide com os coeficientes de lm()
all.equal(
  as.numeric(beta_mqo),
  as.numeric(coef(modelo_mqo))
)

# Estimativa não viesada da variância residual
n <- nrow(X)
p <- ncol(X)

sigma2_mqo <- sum(residuals(modelo_mqo)^2) / (n - p)

sigma2_mqo

##Avaliação do ajuste e dos resíduos--------------------------------------------

# Medidas gerais de ajuste
glance(modelo_mqo)

# Dados ajustados e resíduos
diagnostico <- augment(modelo_mqo)

# Resíduos versus valores ajustados
grafico_residuos <- ggplot(
  diagnostico,
  aes(x = .fitted, y = .resid)
) +
  geom_point(
    color = cores_variaveis["Preço de venda"],
    size = 2.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "grey40"
  ) +
  labs(
    title = "Resíduos versus valores ajustados",
    x = "Valores ajustados",
    y = "Resíduos"
  ) +
  theme_minimal(base_size = 12)

grafico_residuos

ggsave(
  "p2/plot/07_residuos_mqo.png",
  grafico_residuos,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Gráfico Q-Q para avaliar a normalidade dos resíduos
grafico_qq <- ggplot(
  diagnostico,
  aes(sample = .std.resid)
) +
  stat_qq(
    color = cores_variaveis["Preço de venda"],
    size = 2.5
  ) +
  stat_qq_line(color = "grey40") +
  labs(
    title = "Gráfico Q-Q dos resíduos padronizados",
    x = "Quantis teóricos",
    y = "Quantis amostrais"
  ) +
  theme_minimal(base_size = 12)

grafico_qq

ggsave(
  "p2/plot/08_qqplot_mqo.png",
  grafico_qq,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


