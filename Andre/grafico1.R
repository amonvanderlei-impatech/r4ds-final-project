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

IBGE <- read_csv2("C:/Users/affur/OneDrive/Documentos/microdados_enem_2024/IBGE/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.csv") %>%
  select(Código_Município_Completo, Nome_Município)
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
municipios <- resultado %>% left_join(IBGE, by = c("CO_MUNICIPIO_PROVA" = "Código_Município_Completo"))

highlights <- municipios %>%
  filter(NOTA_MEDIA < 650 | NOTA_MEDIA > 875| RENDA_MEDIA > 1.75|
         Nome_Município == "Vitória" | Nome_Município == "Maceió" | Nome_Município == "Assaré")

ggplot(data = municipios, mapping = aes(x = RENDA_MEDIA, y = NOTA_MEDIA)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() +
  geom_label_repel(data = municipios, aes(label = Nome_Município)) +
  geom_point(data = municipios, color = "red") +
  labs(title = "Médias de nota e renda por município",
       x = "Renda média (Salários mínimos)",
       y = "Nota média (Média aritimética)")
