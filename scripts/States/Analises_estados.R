library(dplyr)
library(tidyr)

# Resultados

# --------------------------
# 1. CARREGAMENTO E TRATAMENTO
# --------------------------

Resultados <- read.csv2("RESULTADOS_ANO.csv", stringsAsFactors = FALSE)
colunas_notas <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")

# Selecionar colunas de interesse
dados_filtrados <- Resultados %>%
  select("CO_MUNICIPIO_PROVA", "SG_UF_PROVA", all_of(colunas_notas))
rm(Resultados)

# Tratamento Otimizado: Convertendo "" e 0 para NA
resultados_tratados <- dados_filtrados %>%
  
  # 1. Converter para character e substituir "" por NA
  # (Necessário caso o R não reconheça "" como NA ao converter para numérico)
  mutate(across(all_of(colunas_notas), ~na_if(as.character(.), ""))) %>%
  
  # 2. Converter para numeric (NA's de "" permanecem NA)
  mutate(across(all_of(colunas_notas), as.numeric)) %>%
  
  # 3. Substituir todos os zeros (0) por NA
  mutate(across(all_of(colunas_notas), ~na_if(., 0)))

# Salvar dados intermediários (opcional, mas bom para checagem)
write.csv2(resultados_tratados, "Resultados_filtrados_ANO.csv", row.names = FALSE)
rm(dados_filtrados)


# --------------------------
# 2. CÁLCULO DA MÉDIA POR MUNICÍPIO
# --------------------------

# Preparar dados para cálculo da média geral (média por participante)
dados_municipio <- resultados_tratados %>%
  
  # Calcular a média do participante (por linha), ignorando NAs e ZEROS (que agora são NA)
  rowwise() %>%
  mutate(
    MEDIA_PARTICIPANTE = mean(c(NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO), na.rm = TRUE)
  ) %>%
  ungroup()

# Calcular a média das notas por município e UF
media_municipios <- dados_municipio %>%
  group_by(CO_MUNICIPIO_PROVA, SG_UF_PROVA) %>%
  summarise(
    MEDIA_CN      = mean(NU_NOTA_CN, na.rm = TRUE),
    MEDIA_CH      = mean(NU_NOTA_CH, na.rm = TRUE),
    MEDIA_LC      = mean(NU_NOTA_LC, na.rm = TRUE),
    MEDIA_MT      = mean(NU_NOTA_MT, na.rm = TRUE),
    MEDIA_REDACAO = mean(NU_NOTA_REDACAO, na.rm = TRUE),
    
    # Média Geral correta: média das médias dos participantes
    MEDIA_GERAL = mean(MEDIA_PARTICIPANTE, na.rm = TRUE),
    
    .groups = "drop"
  )

# Salvar os dados com as médias por município
write.csv2(media_municipios, "Media_notas_municipios_ANO.csv", row.names = FALSE)
rm(dados_municipio)

# --------------------------
# 3. CÁLCULO DA MÉDIA POR ESTADO
# --------------------------

# Calcular a média das notas por UF (reutilizando 'resultados_tratados')
media_estados <- resultados_tratados %>%
  
  # Recalcula a Média do Participante, pois a próxima agregação é diferente (UF)
  rowwise() %>%
  mutate(
    MEDIA_PARTICIPANTE = mean(c(NU_NOTA_CN, NU_NOTA_CH, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  group_by(SG_UF_PROVA) %>%
  summarise(
    MEDIA_CN      = mean(NU_NOTA_CN, na.rm = TRUE),
    MEDIA_CH      = mean(NU_NOTA_CH, na.rm = TRUE),
    MEDIA_LC      = mean(NU_NOTA_LC, na.rm = TRUE),
    MEDIA_MT      = mean(NU_NOTA_MT, na.rm = TRUE),
    MEDIA_REDACAO = mean(NU_NOTA_REDACAO, na.rm = TRUE),
    MEDIA_GERAL = mean(MEDIA_PARTICIPANTE, na.rm = TRUE), # Média Geral correta
    .groups = "drop"
  )

# Salvar os dados com as médias por estado
write.csv2(media_estados, "Media_notas_estados_ANO.csv", row.names = FALSE)

# Limpeza final
rm(resultados_tratados, media_municipios, media_estados, colunas_notas)

# Participantes

# --------------------------
# 1. CARREGAMENTO E TRANSFORMAÇÃO BASE
# --------------------------

participantes <- read.csv2("PARTICIPANTES_ANO.csv")
participantes_filtrados <- participantes %>%
  select("CO_MUNICIPIO_PROVA", "SG_UF_PROVA", "Q005", "Q007", "TP_SEXO", "TP_COR_RACA")
write.csv2(participantes_filtrados, "Participantes_filtrados_ANO.csv", row.names = FALSE)
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

media_municipios <- read.csv2("Media_notas_municipios_ANO.csv")

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

write.csv2(porcentagem_municipios, "Porcentagem_municipios_ANO.csv", row.names = FALSE)
rm(participantes_filtrados, media_municipios, contagens_municipio, contagens_sexo_municipio, contagens_cor_municipio, dados_gerais_municipios)

# --------------------------
# 3. DADOS GERAIS POR ESTADO (Estrutura paralela)
# --------------------------

media_estados <- read.csv2("Media_notas_estados_ANO.csv")

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

write.csv2(porcentagem_estados, "Porcentagem_estados_ANO.csv", row.names = FALSE)
rm(list = ls(pattern = "^contagens"))
rm(media_estados, dados_base, dados_gerais_estados, porcentagem_municipios, porcentagem_estados, faixas_renda)