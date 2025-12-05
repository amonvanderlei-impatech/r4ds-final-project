# Libraries
library(tidyverse)
library(ggrepel)
library(geobr)

# Income auxiliar table
pontos_medios <- tribble(
  ~Q007, ~ponto_medio,
  "A", 0,
  "B", 0.5,
  "C", 1.25,
  "D", 1.75,
  "E", 2.25,
  "F", 2.75,
  "G", 3.5,
  "H", 4.5,
  "I", 5.5,
  "J", 6.5,
  "K", 7.5,
  "L", 8.5,
  "M", 9.5,
  "N", 11,
  "O", 13.5,
  "P", 17.5,
  "Q", 22.5,
)

# Data
IBGE <- read_csv2("C:/Users/affur/OneDrive/Documentos/microdados_enem_2024/IBGE/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.csv") %>%
  select(Código_Município_Completo, Nome_Município)
renda <- read_csv2("C:/Users/affur/OneDrive/Documentos/microdados_enem_2024/DADOS/PARTICIPANTES_2024.csv") %>%
  left_join(pontos_medios) %>%
  mutate(RENDA = ponto_medio / Q005) %>%
  group_by(CO_MUNICIPIO_PROVA) %>%
  summarise(RENDA_MEDIA = mean(RENDA)) %>%
  left_join(IBGE, by = c("CO_MUNICIPIO_PROVA" = "Código_Município_Completo")) %>%
  rename(code_muni = CO_MUNICIPIO_PROVA)
nota <- read_csv2("C:/Users/affur/OneDrive/Documentos/microdados_enem_2024/DADOS/RESULTADOS_2024.csv") %>%
  mutate(NOTA = (NU_NOTA_CN + NU_NOTA_CH + NU_NOTA_LC + NU_NOTA_MT + NU_NOTA_REDACAO) / 25) %>%
  group_by(CO_MUNICIPIO_PROVA) %>%
  summarise(NOTA_MEDIA = mean(NOTA, na.rm = TRUE)) %>%
  left_join(IBGE, by = c("CO_MUNICIPIO_PROVA" = "Código_Município_Completo")) %>%
  rename(code_muni = CO_MUNICIPIO_PROVA)
resultado <- renda %>% left_join(nota) %>%
  mutate(RAZAO = NOTA_MEDIA / (1000 * RENDA_MEDIA))
highlights <- resultado %>%
  filter(NOTA_MEDIA < 650 | NOTA_MEDIA > 875| RENDA_MEDIA > 1.75|
         Nome_Município == "Vitória" | Nome_Município == "Maceió" | Nome_Município == "Assaré")
municipios <- read_municipality(year = 2024)
estados <- read_state(year = 2020)
mapa <- municipios %>%
  left_join(resultado, by = "code_muni") %>%
  mutate(na_lista = ifelse(is.na(RENDA_MEDIA), "Ausente", "Presente"))


# Graph 1
ggplot(data = resultado, mapping = aes(x = RENDA_MEDIA, y = NOTA_MEDIA)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() +
  geom_label_repel(data = highlights, aes(label = Nome_Município)) +
  geom_point(data = highlights, color = "red") +
  labs(title = "Médias de nota e renda por município",
       x = "Renda média (Salários mínimos)",
       y = "Nota média (Média aritimética)")

# Graph 2
ggplot(mapa) + theme_bw() +
  geom_sf(aes(fill = na_lista), color = NA) +
  scale_fill_manual(values = c("Presente" = "blue", "Ausente" = "red")) +
  labs(title = "Municípios com polos de aplicação",
       fill = "Situação") +
  geom_sf(fill = NA, color = "black", data = estados)

# Graph 3
ggplot(mapa) + theme_bw() +
  geom_sf(aes(fill = RAZAO), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Razão entre nota e renda",
       fill = "Razão") +
  geom_sf(fill = NA, color = "black", data = estados)

# Graph 4
ggplot(mapa) + theme_bw() +
  geom_sf(aes(fill = RENDA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Renda média dos alunos que realizaram a prova no municipio",
       fill = "Renda média (salários mínimos)") +
  geom_sf(fill = NA, color = "black", data = estados)

# Graph 5
ggplot(mapa) + theme_bw() +
  geom_sf(aes(fill = NOTA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Nota média dos alunos que realizaram a prova no municipio",
       fill = "Nota média") +
  geom_sf(fill = NA, color = "black", data = estados)
