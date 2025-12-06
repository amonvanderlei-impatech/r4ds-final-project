library(dplyr)
library(tidyr)

# --------------------------
# 1. CARREGAMENTO E TRANSFORMAÇÃO BASE
# --------------------------

participantes <- read.csv2("PARTICIPANTES_2024.csv")
participantes_filtrados <- participantes %>%
  select("CO_MUNICIPIO_PROVA", "SG_UF_PROVA", "Q005", "Q007", "TP_SEXO", "TP_COR_RACA")
write.csv2(participantes_filtrados, "Participantes_filtrados.csv", row.names = FALSE)
rm(participantes)

# Define as faixas de renda e calcula a Renda_Media per capita
faixas_renda <- c(
  A = 0,            # Nenhuma Renda
  B = 0.5,          # Até R$ 1.412,00
  C = 1.25,         # De R$ 1.412,01 até R$ 2.118,00
  D = 1.75,         # De R$ 2.118,01 até R$ 2.824,00
  E = 2.25,         # De R$ 2.824,01 até R$ 3.530,00
  F = 2.75,         # De R$ 3.530,01 até R$ 4.236,00
  G = 3.5,         # De R$ 4.236,01 até R$ 5.648,00
  H = 4.5,         # De R$ 5.648,01 até R$ 7.060,00
  I = 5.5,         # De R$ 7.060,01 até R$ 8.472,00
  J = 6.5,         # De R$ 8.472,01 até R$ 9.884,00
  K = 7.5,        # De R$ 9.884,01 até R$ 11.296,00
  L = 8.5,        # De R$ 11.296,01 até R$ 12.708,00
  M = 9.5,        # De R$ 12.708,01 até R$ 14.120,00
  N = 11,        # De R$ 14.120,01 até R$ 16.944,00
  O = 13.5,        # De R$ 16.944,01 até R$ 21.180,00
  P = 17.5,        # De R$ 21.180,01 até R$ 28.240,00
  Q = 22.5         # Acima de R$ 28.240,00
)

# Transforma dados base: Renda per capita e contagens para ambos os níveis
dados_base <- participantes_filtrados %>%
  # 1. Calcula a Renda Per Capita
  mutate(Renda_Media = faixas_renda[Q007] / Q005) %>%
  select(-Q005, -Q007) # Remove as colunas originais

# Contagem e Pivô por Município
contagens_municipio <- dados_base %>%
  group_by(CO_MUNICIPIO_PROVA, SG_UF_PROVA) %>%
  summarise(
    RENDA_MEDIA = mean(Renda_Media, na.rm = TRUE),
    .groups = "drop"
  )

contagens_sexo_municipio <- dados_base %>%
  group_by(CO_MUNICIPIO_PROVA, SG_UF_PROVA, TP_SEXO) %>%
  summarise(NUM_PESSOAS = n(), .groups = "drop") %>%
  pivot_wider(names_from = TP_SEXO, values_from = NUM_PESSOAS,
              names_prefix = "SEXO_", values_fill = list(NUM_PESSOAS = 0))

contagens_cor_municipio <- dados_base %>%
  group_by(CO_MUNICIPIO_PROVA, SG_UF_PROVA, TP_COR_RACA) %>%
  summarise(NUM_PESSOAS = n(), .groups = "drop") %>%
  pivot_wider(names_from = TP_COR_RACA, values_from = NUM_PESSOAS,
              names_prefix = "COR_", values_fill = list(NUM_PESSOAS = 0))

# --------------------------
# 2. DADOS GERAIS POR MUNICÍPIO (Juntando Contagens e Notas)
# --------------------------

media_municipios <- read.csv2("Media_notas_municipios.csv")

dados_gerais_municipios <- contagens_municipio %>%
  merge(contagens_sexo_municipio, by = c("CO_MUNICIPIO_PROVA","SG_UF_PROVA"), all = TRUE) %>%
  merge(contagens_cor_municipio, by = c("CO_MUNICIPIO_PROVA","SG_UF_PROVA"), all = TRUE) %>%
  merge(media_municipios, by = c("CO_MUNICIPIO_PROVA","SG_UF_PROVA"), all = TRUE)

# Cálculo das Porcentagens e Seleção Final (Otimizado)
porcentagem_municipios <- dados_gerais_municipios %>%
  # Colunas que já estão agregadas e devem ser mantidas
  mutate(
    TOTAL_PES = SEXO_F + SEXO_M,
    POR_FEM = (SEXO_F / TOTAL_PES) * 100,
    POR_MAS = (SEXO_M / TOTAL_PES) * 100,
    POR_SI = (COR_0 / TOTAL_PES) * 100,
    POR_BR = (COR_1 / TOTAL_PES) * 100,
    POR_PT = (COR_2 / TOTAL_PES) * 100,
    POR_PD = (COR_3 / TOTAL_PES) * 100,
    POR_AM = (COR_4 / TOTAL_PES) * 100,
    POR_IN = (COR_5 / TOTAL_PES) * 100
  ) %>%
  select(
    CO_MUNICIPIO_PROVA, SG_UF_PROVA,
    starts_with("POR_"),
    RENDA_MEDIA,
    starts_with("MEDIA_")
  )

write.csv2(porcentagem_municipios, "Porcentagem_municipios.csv", row.names = FALSE)
rm(participantes_filtrados, media_municipios, contagens_municipio, contagens_sexo_municipio, contagens_cor_municipio, dados_gerais_municipios)

# --------------------------
# 3. DADOS GERAIS POR ESTADO (Estrutura paralela)
# --------------------------

media_estados <- read.csv2("Media_notas_estados.csv")

# Contagem e Pivô por Estado
contagens_estado <- dados_base %>%
  group_by(SG_UF_PROVA) %>%
  summarise(
    RENDA_MEDIA = mean(Renda_Media, na.rm = TRUE),
    .groups = "drop"
  )

contagens_sexo_estado <- dados_base %>%
  group_by(SG_UF_PROVA, TP_SEXO) %>%
  summarise(NUM_PESSOAS = n(), .groups = "drop") %>%
  pivot_wider(names_from = TP_SEXO, values_from = NUM_PESSOAS,
              names_prefix = "SEXO_", values_fill = list(NUM_PESSOAS = 0))

contagens_cor_estado <- dados_base %>%
  group_by(SG_UF_PROVA, TP_COR_RACA) %>%
  summarise(NUM_PESSOAS = n(), .groups = "drop") %>%
  pivot_wider(names_from = TP_COR_RACA, values_from = NUM_PESSOAS,
              names_prefix = "COR_", values_fill = list(NUM_PESSOAS = 0))

dados_gerais_estados <- contagens_estado %>%
  merge(contagens_sexo_estado, by = "SG_UF_PROVA", all = TRUE) %>%
  merge(contagens_cor_estado, by = "SG_UF_PROVA", all = TRUE) %>%
  merge(media_estados, by = "SG_UF_PROVA", all = TRUE)

# Cálculo das Porcentagens e Seleção Final (Otimizado)
porcentagem_estados <- dados_gerais_estados %>%
  mutate(
    TOTAL_PES = SEXO_F + SEXO_M,
    POR_FEM = (SEXO_F / TOTAL_PES) * 100,
    POR_MAS = (SEXO_M / TOTAL_PES) * 100,
    POR_SI = (COR_0 / TOTAL_PES) * 100,
    POR_BR = (COR_1 / TOTAL_PES) * 100,
    POR_PT = (COR_2 / TOTAL_PES) * 100,
    POR_PD = (COR_3 / TOTAL_PES) * 100,
    POR_AM = (COR_4 / TOTAL_PES) * 100,
    POR_IN = (COR_5 / TOTAL_PES) * 100
  ) %>%
  select(
    SG_UF_PROVA,
    starts_with("POR_"),
    RENDA_MEDIA,
    starts_with("MEDIA_")
  )

write.csv2(porcentagem_estados, "Porcentagem_estados.csv", row.names = FALSE)
rm(list = ls(pattern = "^contagens"))
rm(media_estados, dados_base, dados_gerais_estados, porcentagem_municipios, porcentagem_estados, faixas_renda)

porcentagem_municipios <- read.csv2("Porcentagem_municipios.csv")
porcentagem_estados <- read.csv2("Porcentagem_estados.csv")
