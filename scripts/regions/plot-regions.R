library(tidyverse)
library(corrplot)

files <- list.files("data", pattern = "regions_.*\\.csv$", full.names = TRUE)

regions <- files |>
  map_df(read.csv2)

# Prepare data to plot
regions_long <- regions |>
  pivot_longer(
    cols = c(
      MEDIA_NOTA_CN,
      MEDIA_NOTA_CH,
      MEDIA_NOTA_LC,
      MEDIA_NOTA_MT,
      MEDIA_NOTA_REDACAO,
      MEDIA_MEDIA_SIMPLES
    ),
    names_to = "DISCIPLINA",
    values_to = "NOTA"
  )

regions_long$DISCIPLINA <- recode(
  regions_long$DISCIPLINA,
  "MEDIA_NOTA_CN" = "Ciências da Natureza",
  "MEDIA_NOTA_CH" = "Ciências Humanas",
  "MEDIA_NOTA_LC" = "Linguagens",
  "MEDIA_NOTA_MT" = "Matemática",
  "MEDIA_NOTA_REDACAO" = "Redação",
  "MEDIA_MEDIA_SIMPLES" = "Média Simples"
)

regions_long$DISCIPLINA <- factor(
  regions_long$DISCIPLINA,
  levels = c(
    "Ciências da Natureza",
    "Ciências Humanas",
    "Linguagens",
    "Matemática",
    "Redação",
    "Média Simples"
  )
)

# Scatter - Renda x Nota
ggplot(regions_long, aes(x = MEDIA_RENDA, y = NOTA, color = REGIAO)) +
  geom_point(size = 3) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = alpha("blue", 0.7),
    size = 1,
    alpha = 0.1
  ) +
  facet_wrap(~ DISCIPLINA, scales = "free_y", ncol = 3) +
  labs(
    title = "Relação entre renda per capita média dos participantes e notas médias do ENEM por região (2023-2024)",
    x = "Renda per capita média (salários mínimos)",
    y = "Nota média",
    color = "Região",
    caption = "Fonte: INEP – Microdados ENEM"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(size = 10, hjust = 0)
  )

# Heatmap - correlação entre renda e nota
df <- regions |> select(MEDIA_RENDA, starts_with("MEDIA_NOTA"))
corr <- cor(df, use = "pairwise.complete.obs")
col_pal <- colorRampPalette(c("#B2182B", "#FFFFFF", "#2166AC"))

par(mar = c(5, 5, 5, 6)) 

corrplot(
  corr,
  method = "color",
  col = col_pal(200),
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.9,
  tl.cex = 0.9,
  cl.cex = 0.8,
  diag = FALSE,
  mar = c(2,0,0,0)
)

title(
  main = "Correlação entre renda per capita média dos participantes e notas médias do ENEM por região",
  cex.main = 1.4,
  font.main = 2,
  line = 1.5
)

mtext(
  "Fonte: INEP – Microdados ENEM",
  side = 1,
  line = 3,
  cex = 0.9,
)

# Linha temporal
ggplot(regions, aes(ANO, MEDIA_MEDIA_SIMPLES, color = REGIAO)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(regions$ANO)) +
  labs(
    title = "Evolução das notas médias do ENEM por região",
    x = "Ano",
    y = "Nota média",
    color = "Região",
    caption = "Fonte: INEP – Microdados ENEM",
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(size = 10, hjust = 0)
  )

ggplot(regions, aes(ANO, MEDIA_RENDA, color = REGIAO)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(regions$ANO)) +
  labs(
    title = "Evolução da renda per capita média dos participantes do ENEM por região",
    x = "Ano",
    y = "Renda per capita média (salários mínimos)",
    color = "Região",
    caption = "Fonte: INEP – Microdados ENEM",
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(size = 10, hjust = 0)
  )
