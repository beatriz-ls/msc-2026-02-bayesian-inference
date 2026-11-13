library(tidyverse)
library(viridis)
library(corrplot)
library(ggplot2)

# data--------------------------------------------------------------------------
dados <- read.table(
  "p2/dados.txt",
  header = TRUE,
  sep = "\t",
  dec = "."
)

#Tratamento---------------------------------------------------------------------

## Selecionar as variáveis de interesse
dados <- dados |>
  select(y, x1, x2, x3, x5, x7)|>
  rename(
    preco = y,
    impostos = x1,
    banheiros = x2,
    tamanho_lote = x3,
    vagas_garagem = x5,
    quartos = x7
  )

## Análise descritiva-----------------------------------------------------------

# Nomes para apresentação nos gráficos
nomes_variaveis <- c(
  preco = "Preço de venda",
  impostos = "Impostos",
  banheiros = "Banheiros",
  tamanho_lote = "Tamanho do lote",
  vagas_garagem = "Vagas de garagem",
  quartos = "Quartos"
)

# Definir uma cor fixa para cada variável
cores_variaveis <- setNames(
  viridis(6, option = "D", begin = 0.08, end = 0.92),
  nomes_variaveis
)

tabela_descritiva <- dados |>
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "valor"
  ) |>
  group_by(variavel) |>
  summarise(
    n = sum(!is.na(valor)),
    media = mean(valor, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    dp = sd(valor, na.rm = TRUE),
    minimo = min(valor, na.rm = TRUE),
    q1 = quantile(valor, 0.25, na.rm = TRUE),
    q3 = quantile(valor, 0.75, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    variavel = recode(variavel, !!!nomes_variaveis),
    across(where(is.numeric), ~round(.x, 2))
  )

print(tabela_descritiva)

# Organizar os dados das variáveis contínuas
dados_continuos <- dados |>
  select(preco, impostos, tamanho_lote) |>
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "valor"
  ) |>
  mutate(
    variavel = recode(variavel, !!!nomes_variaveis)
  )

# Organizar os dados das variáveis discretas
dados_discretos <- dados |>
  select(banheiros, vagas_garagem, quartos) |>
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "valor"
  ) |>
  mutate(
    variavel = recode(variavel, !!!nomes_variaveis),
    valor = factor(valor)
  )

###### Analise descritiva cruzada variavel resposta preco #######

# Relação entre o preço e as covariáveis contínuas
grafico_continuas <- ggplot(
  dados_continuos,
  aes(x = valor, y = preco, color = variavel)
) +
  geom_point(
    size = 2.8,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    linewidth = 0.9,
    alpha = 0.15
  ) +
  facet_wrap(~variavel, scales = "free_x", ncol = 2) +
  scale_color_manual(values = cores_variaveis) +
  labs(
    title = "Preço de venda segundo as características contínuas",
    x = NULL,
    y = "Preço de venda (milhares)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

grafico_continuas

ggsave(
  "p2/plot/01_preco_covariaveis_continuas.png",
  grafico_continuas,
  width = 11,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Comparar o preço entre categorias
grafico_discretas <- ggplot(
  dados_discretos,
  aes(
    x = categoria,
    y = preco,
    fill = variavel
  )
) +
  geom_boxplot(
    width = 0.55,
    alpha = 0.65,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.1,
    height = 0,
    size = 1.8,
    alpha = 0.7
  ) +
  facet_wrap(~variavel, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = cores_variaveis) +
  labs(
    title = "Distribuição do preço segundo as características dos imóveis",
    x = NULL,
    y = "Preço de venda (milhares)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

grafico_discretas

ggsave(
  "p2/plot/03_preco_covariaveis_discretas.png",
  grafico_discretas,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Resumo do preço por categoria
resumo_preco <- dados_discretos |>
  group_by(variavel, categoria) |>
  summarise(
    n = n(),
    preco_medio = mean(preco),
    preco_mediano = median(preco),
    .groups = "drop"
  )

# Gráfico de preço médio por categoria
grafico_medias <- ggplot(
  resumo_preco,
  aes(
    x = categoria,
    y = preco_medio,
    fill = variavel
  )
) +
  geom_col(
    width = 0.65,
    alpha = 0.85
  ) +
  geom_text(
    aes(label = paste0("n = ", n)),
    vjust = -0.5,
    size = 3.5
  ) +
  facet_wrap(~variavel, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = cores_variaveis) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Preço médio de venda por característica do imóvel",
    x = NULL,
    y = "Preço médio (milhares)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

grafico_medias

ggsave(
  "p2/plot/04_preco_medio_categorias.png",
  grafico_medias,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Calcular a matriz de correlação
matriz_cor <- dados |>
  cor(
    use = "complete.obs",
    method = "pearson"
  )

# Renomear linhas e colunas
rownames(matriz_cor) <- nomes_variaveis[rownames(matriz_cor)]
colnames(matriz_cor) <- nomes_variaveis[colnames(matriz_cor)]

# Salvar matriz de correlação
png(
  "p2/plot/05_matriz_correlacao.png",
  width = 2600,
  height = 2200,
  res = 300
)

corrplot(
  matriz_cor,
  method = "color",
  type = "lower",
  col = viridis(200, option = "D"),
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE
)

dev.off()