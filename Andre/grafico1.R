library(tidyverse)
library(ggrepel)

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
dicionario <- tribble(
  ~CO_MUNICIPIO_PROVA, ~NO_MUNICIPIO_PROVA,
  1300201, "Atalaia do Norte / AM",
  1300607, "Benjamin Constant / AM",
  1301951, "Itamarati / AM",
  1302108, "Japurá / AM",
  1304237, "Tonantins / AM",
  2301604, "Assaré / CE",
  2704302, "Maceió / AL",
  3205309, "Vitória / ES",
  3556206, "Valinhos / SP",
  3556701, "Vinhedo/ SP"
  )

conferencia <- read_csv2("C:/Users/affur/OneDrive/Documentos/microdados_enem_2024/DADOS/PARTICIPANTES_2024.csv") %>%
  distinct(CO_MUNICIPIO_PROVA, .keep_all = TRUE)
renda <- read_csv2("C:/Users/affur/OneDrive/Documentos/microdados_enem_2024/DADOS/PARTICIPANTES_2024.csv") %>%
  left_join(pontos_medios) %>%
  mutate(RENDA = ponto_medio / Q005) %>%
  group_by(CO_MUNICIPIO_PROVA) %>%
  summarise(RENDA_MEDIA = mean(RENDA))
nota <- read_csv2("C:/Users/affur/OneDrive/Documentos/microdados_enem_2024/DADOS/RESULTADOS_2024.csv") %>%
  mutate(NOTA = (NU_NOTA_CN + NU_NOTA_CH + NU_NOTA_LC + NU_NOTA_MT + NU_NOTA_REDACAO) / 25) %>%
  group_by(CO_MUNICIPIO_PROVA) %>%
  summarise(NOTA_MEDIA = mean(NOTA, na.rm = TRUE))

resultado <- renda %>% left_join(nota)
highlights <- resultado %>%
  filter(NOTA_MEDIA < 650 | NOTA_MEDIA > 875| RENDA_MEDIA > 1.75|
          CO_MUNICIPIO_PROVA == 3205309 | CO_MUNICIPIO_PROVA == 2704302 | CO_MUNICIPIO_PROVA == 2301604)
municipios <- dicionario %>% left_join(highlights)
ggplot(data = resultado, mapping = aes(x = RENDA_MEDIA, y = NOTA_MEDIA)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() +
  geom_label_repel(data = municipios, aes(label = NO_MUNICIPIO_PROVA)) +
  geom_point(data = municipios, color = "red") +
  labs(title = "Médias de nota e renda por município",
       x = "Renda média (Salários mínimos)",
       y = "Nota média (Média aritimética)")
