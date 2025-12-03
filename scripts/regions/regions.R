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
    x == "B" ~ 1412,
    x == "C" ~ 1765,
    x == "D" ~ 2471,
    x == "E" ~ 3177,
    x == "F" ~ 3883,
    x == "G" ~ 4942,
    x == "H" ~ 6354,
    x == "I" ~ 7766,
    x == "J" ~ 9178,
    x == "K" ~ 10590,
    x == "L" ~ 12002,
    x == "M" ~ 13414,
    x == "N" ~ 15532,
    x == "O" ~ 19062,
    x == "P" ~ 24710,
    x == "Q" ~ 28240,
    TRUE ~ NA_real_,
  )
}

# Import raw data
participantes <- read.csv2("data/raw/PARTICIPANTES_2024.csv")
resultados <- read.csv2("data/raw/RESULTADOS_2024.csv")

# Clean data
# Participantes
participantes <- participantes |> select(NU_ANO, CO_MUNICIPIO_PROVA, Q005, Q007)
participantes <- participantes |>
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
resultados <- resultados |> select(NU_ANO, CO_MUNICIPIO_PROVA, NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO)

resultados <- resultados |>
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
regions <- participantes |>
  inner_join(resultados, by = c("REGIAO", "ANO"))

# Write data in a csv
write.csv2(regions, "data/regions.csv", row.names = FALSE)
