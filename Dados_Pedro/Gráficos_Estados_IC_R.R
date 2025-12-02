# Bibliotecas necessárias:

library(geobr)
library(tidyverse)
library(ggthemes)
library(sf)
library(scales)


# Dataframes neccessários:

municipios <- read.csv2("Porcentagem_municipios_corrigido.csv")
estados <- read.csv2("Porcentagem_estados_corrigido.csv")

# Gráfico - Nota média ~ Renda Rédia (Cor - Estado)

plot1 <- ggplot(
  municipios,
  aes(x=RENDA_MEDIA,y=MEDIA_GERAL)
)+
  geom_point(aes(color=SG_UF_PROVA))+
  geom_smooth(method="lm")+
  labs(
    title = "NOTA EM FUNÇÃO DA RENDA POR UNIDADE FEDERATIVA",
    subtitle = "Nota média no ENEM 2024 do município em função da renda média dos alunos do município",
    x = "Renda média dos estudantes (R$)",
    y = "Nota média",
    color = "Unidade Federativa",
    caption = "Fonte: Microdados ENEM 2024"
  )+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right")

print(plot1)
ggsave(filename = "Dados/Plots/Grafico_Nota_Renda_Cor_Estado.png", plot=plot1)
rm(plot1)

# Gráfico - Nota média ~ Renda Rédia (Estados)

plot2 <- ggplot(
  estados,
  aes(x=RENDA_MEDIA,y=MEDIA_GERAL)
)+
  geom_point()+
  geom_smooth(method="lm")+
  labs(
    title = "NOTA EM FUNÇÃO DA RENDA - UNIDADES FEDERATIVAS",
    subtitle = "Nota média no ENEM 2024 da UF em função da renda média dos alunos da UF",
    x = "Renda média dos estudantes (R$)",
    y = "Nota média",
    color = "Unidade Federativa",
    caption = "Fonte: Microdados ENEM 2024"
  )+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right")

print(plot2)
ggsave(filename = "Dados/Plots/Grafico_Nota_Renda_Estados.png", plot=plot2)
rm(plot2)

# Gráfico - Nota Média ~ Renda média (Agrupados - Estados)

plot3 <- ggplot(municipios, aes(x = RENDA_MEDIA, y = MEDIA_GERAL)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "DIVISÃO POR UNIDADE FEDERATIVA",
    subtitle = "Nota média no ENEM do município em função da renda média dos alunos do município",
    x = "Renda Média",
    y = "Nota Média",
    caption = "Fonte: Microdados ENEM 2024"
  ) +
  facet_wrap(~ SG_UF_PROVA) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right")

print(plot3)
ggsave(filename = "Dados/Plots/Grafico_Nota_Renda_Agrupados_Estados.png", plot=plot3)
rm(plot3)

# Gráficos - Nota Média ~ Renda média (Individuais - Estados) & Coeficientes

UFs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", 
         "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", 
         "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")

coeficientes <- list()

for (i in UFs) {
  dados <- municipios %>%
    filter(SG_UF_PROVA == i)
  
  grafico <- ggplot(
    dados,
    aes(x=RENDA_MEDIA, y=MEDIA_GERAL)
  ) +
    geom_point() +
    geom_smooth(method="lm") +
    labs(
      title=i, 
      subtitle="Nota média no ENEM do município em função da renda média dos alunos do município",
      x="Renda média dos estudantes (R$)",
      y="Nota média",
      caption="Fonte: Microdados ENEM 2024"
    )+
    theme_bw()+
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "right")
  
  print(grafico)
  
  ggsave(filename = paste0("Dados/Plots/Individuais_Estado/Grafico_Nota_Renda_", i, ".png"), plot = grafico)
  
  aux <- lm(MEDIA_GERAL ~ RENDA_MEDIA, data = dados)
  coeficiente <- coef(aux)["RENDA_MEDIA"]
  coeficientes[[i]] <- tibble(SG_UF_PROVA = i, COEFICIENTE = coeficiente)
}

coeficientes_estados <- bind_rows(coeficientes)

write.csv2(coeficientes_estados, "Dados/Coeficientes_Estados.csv")

for (i in 1:1) {
  rm(coeficientes)
  rm(dados)
  rm(aux)
  rm(coeficiente)
  rm(grafico)
  rm(i)
  rm(UFs)
}

# Mapa coroplético - Renda média (Estados)

brasil_sf <- read_state(code_state = "all") %>%
  rename(SG_UF_PROVA = abbrev_state) %>%
  sf::st_simplify(dTolerance = 0.01)
dados_mapa <- brasil_sf %>%
  left_join(estados, by = "SG_UF_PROVA")

plot4 <- ggplot() +
  geom_sf(data = dados_mapa, aes(fill = RENDA_MEDIA), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    na.value = "lightgray",
    labels = label_number(accuracy = 0.01,
                          prefix = "R$ ", 
                          big.mark = ".", 
                          decimal.mark = ",")
  ) +
  labs(
    title = "RENDA MÉDIA",
    subtitle = "Visualização da renda média dos estudantes por unidade federativa",
    fill = "Renda Média",
    x = NULL, y = NULL,
    caption = "Fonte: Microdados ENEM 2024"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    panel.grid.major = element_line(colour = 'transparent'),
    axis.text = element_blank()
  )

print(plot4)
ggsave(filename = "Dados/Plots/Mapa_Renda_Estados.png", plot=plot4)
rm(plot4)

# Mapa coroplético - Nota média (Estados)

plot5 <- ggplot() +
  geom_sf(data = dados_mapa, aes(fill = MEDIA_GERAL), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    na.value = "lightgray"
  ) +
  labs(
    title = "NOTA MÉDIA",
    subtitle = "Visualização da nota média no ENEM dos estudantes por unidade federativa",
    fill = "Nota Média",
    x = NULL, y = NULL,
    caption = "Fonte: Microdados ENEM 2024"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    panel.grid.major = element_line(colour = 'transparent'),
    axis.text = element_blank()
  )

print(plot5)
ggsave(filename = "Dados/Plots/Mapa_Nota_Estados.png", plot=plot5)
rm(plot5)

# Mapa coroplético - Nota média / Renda Média (Estados)

razao_nota_renda <- estados%>%
  group_by(SG_UF_PROVA)%>%
  summarise(RAZAO = (MEDIA_GERAL-251.12)/RENDA_MEDIA)
brasil_sf <- read_state(code_state = "all") %>%
  rename(SG_UF_PROVA = abbrev_state) %>%
  sf::st_simplify(dTolerance = 0.01)
dados_mapa <- brasil_sf %>%
  left_join(razao_nota_renda, by = "SG_UF_PROVA")

plot6 <- ggplot() +
  geom_sf(data = dados_mapa, aes(fill = RAZAO), color = "black", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1,
    na.value = "lightgray"
  ) +
  labs(
    title = "RAZÃO ENTRE NOTA E A RENDA",
    subtitle = "Visualização da razão da nota média no ENEM pela Renda média dos estudantes por unidade federativa",
    fill = "Razão",
    x = NULL, y = NULL,
    caption = "Fonte: Microdados ENEM 2024"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    panel.grid.major = element_line(colour = 'transparent'),
    axis.text = element_blank()
  )

print(plot6)
ggsave(filename = "Dados/Plots/Mapa_Nota_Div_Renda_Estados.png", plot=plot6)
rm(plot6)

write.csv2(razao_nota_renda, "Dados/Nota_Div_Renda_Estados.csv")

rm(brasil_sf)
rm(dados_mapa)
