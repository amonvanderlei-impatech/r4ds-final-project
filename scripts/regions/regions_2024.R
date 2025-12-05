library(tidyverse)

# Aux functions
codigo_para_regiao <- function(x) {
  case_when(
    x == "1" ~ "Norte",
    x == "2" ~ "Nordeste", 
    x == "3" ~ "Sudeste",
    x == "4" ~ "Sul",
    x == "5" ~ "Centro-Oeste",
    TRUE ~ NA_character_
  )
}

codigo_para_renda <- function(x) {
  case_when(
    x == "A" ~ 0,
    x == "B" ~ 0.5,
    x == "C" ~ 1.25,
    x == "D" ~ 1.75,
    x == "E" ~ 2.25,
    x == "F" ~ 2.75,
    x == "G" ~ 3.5,
    x == "H" ~ 4.5,
    x == "I" ~ 5.5,
    x == "J" ~ 6.5,
    x == "K" ~ 7.5,
    x == "L" ~ 8.5,
    x == "M" ~ 9.5,
    x == "N" ~ 11,
    x == "O" ~ 13.5,
    x == "P" ~ 17.5,
    x == "Q" ~ 23,
    TRUE ~ NA_real_,
  )
}

# Import raw data
participantes <- read.csv2("data/raw/PARTICIPANTES_2024.csv")
resultados <- read.csv2("data/raw/RESULTADOS_2024.csv")

# Clean data
# Participantes
participantes_cols <- participantes |> select(NU_ANO, CO_MUNICIPIO_PROVA, Q005, Q007)
participantes_clean <- participantes_cols |>
  mutate(
    REGIAO = substr(CO_MUNICIPIO_PROVA, 1, 1),
    REGIAO = codigo_para_regiao(REGIAO),
    RENDA_PER_CAPITA = codigo_para_renda(Q007) / Q005
  ) |>
  select(-CO_MUNICIPIO_PROVA, -Q005, -Q007) |>
  group_by(REGIAO, NU_ANO) |>
  summarise(
    MEDIA_RENDA = mean(RENDA_PER_CAPITA, na.rm = TRUE),
    MEDIANA_RENDA = median(RENDA_PER_CAPITA, na.rm = TRUE),
    DESVIO_RENDA = sd(RENDA_PER_CAPITA, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(ANO = NU_ANO)

# Resultados
resultados_cols <- resultados |> select(NU_ANO, CO_MUNICIPIO_PROVA, NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO)

resultados_clean <- resultados_cols |>
  mutate(
    REGIAO = substr(CO_MUNICIPIO_PROVA, 1, 1),
    REGIAO = codigo_para_regiao(REGIAO),
    NU_NOTA_CN = as.numeric(NU_NOTA_CN),
    NU_NOTA_CH = as.numeric(NU_NOTA_CH),
    NU_NOTA_LC = as.numeric(NU_NOTA_LC),
    NU_NOTA_MT = as.numeric(NU_NOTA_MT),
    NU_NOTA_REDACAO = as.numeric(NU_NOTA_REDACAO),
    MEDIA_SIMPLES = (NU_NOTA_CN + NU_NOTA_CH + NU_NOTA_LC + NU_NOTA_MT + NU_NOTA_REDACAO) / 5,
  ) |>
  select(-CO_MUNICIPIO_PROVA) |>
  group_by(REGIAO, NU_ANO) |>
  summarise(
    MEDIA_NOTA_CN = mean(NU_NOTA_CN, na.rm = TRUE),
    MEDIANA_NOTA_CN = median(NU_NOTA_CN, na.rm = TRUE),
    DESVIO_NOTA_CN = sd(NU_NOTA_CN, na.rm = TRUE),
    
    MEDIA_NOTA_CH = mean(NU_NOTA_CH, na.rm = TRUE),
    MEDIANA_NOTA_CH = median(NU_NOTA_CH, na.rm = TRUE),
    DESVIO_NOTA_CH = sd(NU_NOTA_CH, na.rm = TRUE),
    
    MEDIA_NOTA_LC = mean(NU_NOTA_LC, na.rm = TRUE),
    MEDIANA_NOTA_LC = median(NU_NOTA_LC, na.rm = TRUE),
    DESVIO_NOTA_LC = sd(NU_NOTA_LC, na.rm = TRUE),
    
    MEDIA_NOTA_MT = mean(NU_NOTA_MT, na.rm = TRUE),
    MEDIANA_NOTA_MT = median(NU_NOTA_MT, na.rm = TRUE),
    DESVIO_NOTA_MT = sd(NU_NOTA_MT, na.rm = TRUE),
    
    MEDIA_NOTA_REDACAO = mean(NU_NOTA_REDACAO, na.rm = TRUE),
    MEDIANA_NOTA_REDACAO = median(NU_NOTA_REDACAO, na.rm = TRUE),
    DESVIO_NOTA_REDACAO = sd(NU_NOTA_REDACAO, na.rm = TRUE),
    
    MEDIA_MEDIA_SIMPLES = mean(MEDIA_SIMPLES, na.rm = TRUE),
    MEDIANA_MEDIA_SIMPLES = median(MEDIA_SIMPLES, na.rm = TRUE),
    DESVIO_MEDIA_SIMPLES = sd(MEDIA_SIMPLES, na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  rename(ANO = NU_ANO)

# Join data
regions <- participantes_clean |>
  inner_join(resultados_clean, by = c("REGIAO", "ANO"))

# Write data in a csv
write.csv2(regions, "data/regions_2024.csv", row.names = FALSE)
