library(dplyr)
library(tidyr)

# --------------------------
# 1. CARREGAMENTO E TRATAMENTO
# --------------------------

Resultados <- read.csv2("RESULTADOS_2024.csv", stringsAsFactors = FALSE)
colunas_notas <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")

# Selecionar colunas de interesse
dados_filtrados <- Resultados %>%
  select("CO_MUNICIPIO_PROVA", "SG_UF_PROVA", all_of(colunas_notas))

# Liberar memória (bom para arquivos grandes)
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
write.csv2(resultados_tratados, "Resultados_filtrados.csv", row.names = FALSE)
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
write.csv2(media_municipios, "Media_notas_municipios.csv", row.names = FALSE)
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
write.csv2(media_estados, "Media_notas_estados.csv", row.names = FALSE)

# Limpeza final
rm(resultados_tratados, media_municipios, media_estados, colunas_notas)
