library(tidyverse)

files <- list.files("data/regions", pattern = "regions_.*\\.csv$", full.names = TRUE)

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
graph_scatter <- ggplot(regions_long, aes(x = MEDIA_RENDA, y = NOTA, color = REGIAO)) +
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
    title = "Relação entre renda per capita média dos participantes e notas médias do ENEM",
    subtitle = "Por região e ano (2018-2024)",
    x = "Renda per capita média (salários mínimos)",
    y = "Nota média",
    color = "Região",
    caption = "Fonte: INEP – Microdados ENEM"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(size = 10, hjust = 0)
  )

# Correlação entre renda e nota
calc_corr_renda_notas <- function(df) {
  df |>
    summarise(
      CN  = cor(MEDIA_RENDA, MEDIA_NOTA_CN,  use = "pairwise.complete.obs"),
      CH  = cor(MEDIA_RENDA, MEDIA_NOTA_CH,  use = "pairwise.complete.obs"),
      LC  = cor(MEDIA_RENDA, MEDIA_NOTA_LC,  use = "pairwise.complete.obs"),
      MT  = cor(MEDIA_RENDA, MEDIA_NOTA_MT,  use = "pairwise.complete.obs"),
      RED = cor(MEDIA_RENDA, MEDIA_NOTA_REDACAO, use = "pairwise.complete.obs")
    ) |>
    pivot_longer(everything(),
                 names_to = "DISCIPLINA",
                 values_to = "CORRELACAO"
    )
}

corr_ano <- regions |>
  group_by(ANO) |>
  group_modify(~ calc_corr_renda_notas(.x)) |>
  ungroup()

corr_total <- calc_corr_renda_notas(regions) |>
  mutate(ANO = "2018–2024")

corr_all <- bind_rows(
  corr_ano |> mutate(ANO = as.character(ANO)),
  corr_total
) |>
  mutate(
    ANO = factor(
      ANO,
      levels = c(as.character(2018:2024), "2018–2024")
    )
  )

graph_corr <- ggplot(corr_all, aes(x = DISCIPLINA, y = CORRELACAO)) +
  geom_col(fill = "#2166AC") +
  geom_text(aes(label = round(CORRELACAO, 2)), vjust = 1.5, color = "white") +
  facet_wrap(~ ANO, ncol = 3) +
  labs(
    title = "Correlação entre renda per capita média e notas médias do ENEM ",
    subtitle = "Por disciplina e ano (2018-2024)",
    x = "Disciplina",
    y = "Correlação",
    caption = "Fonte: INEP – Microdados ENEM"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.caption = element_text(size = 10, hjust = 0)
  )

# Linha temporal
graph_notas_tempo <- ggplot(regions, aes(ANO, MEDIA_MEDIA_SIMPLES, color = REGIAO)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(regions$ANO)) +
  labs(
    title = "Evolução das notas médias do ENEM",
    subtitle = "Por região e ano (2018-2024)",
    x = "Ano",
    y = "Nota média",
    color = "Região",
    caption = "Fonte: INEP – Microdados ENEM",
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(size = 10, hjust = 0)
  )

graph_renda_tempo <- ggplot(regions, aes(ANO, MEDIA_RENDA, color = REGIAO)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = unique(regions$ANO)) +
  labs(
    title = "Evolução da renda per capita média dos participantes do ENEM",
    subtitle = "Por região e ano (2018-2024)",
    x = "Ano",
    y = "Renda per capita média (salários mínimos)",
    color = "Região",
    caption = "Fonte: INEP – Microdados ENEM",
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(size = 10, hjust = 0)
  )

ggsave("plots/regions/scatter_renda_notas.png", graph_scatter, width = 10, height = 6, dpi = 320)
ggsave("plots/regions/correlacao_por_ano.png", graph_corr, width = 10, height = 6, dpi = 320)
ggsave("plots/regions/notas_tempo.png", graph_notas_tempo, width = 10, height = 6, dpi = 320)
ggsave("plots/regions/renda_tempo.png", graph_renda_tempo, width = 10, height = 6, dpi = 320)
