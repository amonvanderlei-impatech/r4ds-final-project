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
resultado20 <- read_csv2("data/municipalities/municipios_2020.csv")
resultado21 <- read_csv2("data/municipalities/municipios_2021.csv")
resultado22 <- read_csv2("data/municipalities/municipios_2022.csv")
resultado23 <- read_csv2("data/municipalities/municipios_2023.csv")
resultado24 <- read_csv2("data/municipalities/municipios_2024.csv")
entrou21 <- resultado21 %>% anti_join(resultado20, by = "code_muni")
entrou22 <- resultado22 %>% anti_join(resultado21, by = "code_muni")
entrou23 <- resultado23 %>% anti_join(resultado22, by = "code_muni")
entrou24 <- resultado24 %>% anti_join(resultado23, by = "code_muni")
saiu21 <- resultado20 %>% anti_join(resultado21, by = "code_muni")
saiu22 <- resultado21 %>% anti_join(resultado22, by = "code_muni")
saiu23 <- resultado22 %>% anti_join(resultado23, by = "code_muni")
saiu24 <- resultado23 %>% anti_join(resultado24, by = "code_muni")
highlight20 <- resultado20 %>% filter(Nome_Município == "Vitória" | Nome_Município == "Maceió" | Nome_Município == "Assaré" | Nome_Município == "Japurá" | Nome_Município == "Vinhedo" | Nome_Município == "Rio de Janeiro")
highlight21 <- resultado21 %>% filter(Nome_Município == "Vitória" | Nome_Município == "Maceió" | Nome_Município == "Assaré" | Nome_Município == "Japurá" | Nome_Município == "Vinhedo" | Nome_Município == "Rio de Janeiro")
highlight22 <- resultado22 %>% filter(Nome_Município == "Vitória" | Nome_Município == "Maceió" | Nome_Município == "Assaré" | Nome_Município == "Japurá" | Nome_Município == "Vinhedo" | Nome_Município == "Rio de Janeiro")
highlight23 <- resultado23 %>% filter(Nome_Município == "Vitória" | Nome_Município == "Maceió" | Nome_Município == "Assaré" | Nome_Município == "Japurá" | Nome_Município == "Vinhedo" | Nome_Município == "Rio de Janeiro")
highlight24 <- resultado24 %>% filter(Nome_Município == "Vitória" | Nome_Município == "Maceió" | Nome_Município == "Assaré" | Nome_Município == "Japurá" | Nome_Município == "Vinhedo" | Nome_Município == "Rio de Janeiro")
highlight <- bind_rows(highlight20, highlight21, highlight22, highlight23, highlight24)
municipios <- read_municipality(year = 2024)
estados <- read_state(year = 2020)
mapa <- municipios %>%
  mutate(origem = case_when(
    code_muni %in% entrou21$code_muni ~ "entrou em 2021",
    code_muni %in% entrou22$code_muni ~ "entrou em 2022",
    code_muni %in% entrou23$code_muni ~ "entrou em 2023",
    code_muni %in% entrou24$code_muni ~ "entrou em 2024",
    code_muni %in% saiu21$code_muni ~ "saiu em 2021",
    code_muni %in% saiu22$code_muni ~ "saiu em 2022",
    code_muni %in% saiu23$code_muni ~ "saiu em 2023",
    code_muni %in% saiu24$code_muni ~ "saiu em 2024",
    code_muni %in% resultado20$code_muni ~ "estava em 2020",
    TRUE ~ "não tem polo"
  ))
mapa20 <- municipios %>% left_join(resultado20)
mapa21 <- municipios %>% left_join(resultado21)
mapa22 <- municipios %>% left_join(resultado22)
mapa23 <- municipios %>% left_join(resultado23)
mapa24 <- municipios %>% left_join(resultado24)

# Médias de nota e renda por município 2024
ggplot(data = resultado24, mapping = aes(x = RENDA_MEDIA, y = NOTA_MEDIA)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() +
  geom_label_repel(data = highlight24, aes(label = Nome_Município)) +
  geom_point(data = highlight24, color = "red") +
  labs(title = "Médias de nota e renda por município (2024)",
       x = "Renda média (Salários mínimos)",
       y = "Nota média (Média aritmética)")
ggsave("plots/municipalities/medias24.png")

# Médias de nota e renda por município 2023
ggplot(data = resultado23, mapping = aes(x = RENDA_MEDIA, y = NOTA_MEDIA)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() +
  geom_label_repel(data = highlight23, aes(label = Nome_Município)) +
  geom_point(data = highlight23, color = "red") +
  labs(title = "Médias de nota e renda por município (2023)",
       x = "Renda média (Salários mínimos)",
       y = "Nota média (Média aritmética)")
ggsave("plots/municipalities/medias23.png")

# Médias de nota e renda por município 2022
ggplot(data = resultado22, mapping = aes(x = RENDA_MEDIA, y = NOTA_MEDIA)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() +
  geom_label_repel(data = highlight22, aes(label = Nome_Município)) +
  geom_point(data = highlight22, color = "red") +
  labs(title = "Médias de nota e renda por município (2022)",
       x = "Renda média (Salários mínimos)",
       y = "Nota média (Média aritmética)")
ggsave("plots/municipalities/medias22.png")

# Médias de nota e renda por município 2021
ggplot(data = resultado21, mapping = aes(x = RENDA_MEDIA, y = NOTA_MEDIA)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() +
  geom_label_repel(data = highlight21, aes(label = Nome_Município)) +
  geom_point(data = highlight21, color = "red") +
  labs(title = "Médias de nota e renda por município (2021)",
       x = "Renda média (Salários mínimos)",
       y = "Nota média (Média aritmética)")
ggsave("plots/municipalities/medias21.png")

# Médias de nota e renda por município 2020
ggplot(data = resultado20, mapping = aes(x = RENDA_MEDIA, y = NOTA_MEDIA)) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() +
  geom_label_repel(data = highlight20, aes(label = Nome_Município)) +
  geom_point(data = highlight20, color = "red") +
  labs(title = "Médias de nota e renda por município (2020)",
       x = "Renda média (Salários mínimos)",
       y = "Nota média (Média aritmética)")
ggsave("plots/municipalities/medias20.png")

#Série temporal de médias de renda e notas de municípios selecionados
ggplot(data = highlight, mapping = aes(x = RENDA_MEDIA, y = NOTA_MEDIA, color = ANO)) +
  geom_point()+ theme_bw() +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  geom_text_repel(data = highlight22, aes(label = Nome_Município), color = "grey50") +
  labs(title = "Série temporal de médias de renda e notas de municípios selecionados",
       x = "Renda média (Salários mínimos)",
       y = "Nota média (Média aritmética)")
ggsave("plots/municipalities/medias_temporais.png")

# municípios com polos de aplicação
ggplot(mapa) + theme_bw() +
  geom_sf(aes(fill = origem), color = NA) +
  scale_fill_manual(values = c(
    "entrou em 2021" = "orange",
    "entrou em 2022" = "yellow",
    "entrou em 2023" = "green",
    "entrou em 2024" = "cyan",
    "saiu em 2021" = "purple",
    "saiu em 2022" = "pink",
    "saiu em 2023" = "brown",
    "saiu em 2024" = "red",
    "estava em 2020" = "blue",
    "não tem polo" = "grey50")) +
  labs(title = "Municípios com polos de aplicação",
       fill = "Situação") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/polos.png")

# Renda média por município (2024)
ggplot(mapa24) + theme_bw() +
  geom_sf(aes(fill = RENDA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Renda média dos alunos do município (2024)",
       fill = "Renda média (salários mínimos)") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/renda24.png")

# Renda média por município (2023)
ggplot(mapa23) + theme_bw() +
  geom_sf(aes(fill = RENDA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Renda média dos alunos do município (2023)",
       fill = "Renda média (salários mínimos)") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/renda23.png")

# Renda média por município (2022)
ggplot(mapa22) + theme_bw() +
  geom_sf(aes(fill = RENDA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Renda média dos alunos do município (2022)",
       fill = "Renda média (salários mínimos)") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/renda22.png")

# Renda média por município (2021)
ggplot(mapa21) + theme_bw() +
  geom_sf(aes(fill = RENDA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Renda média dos alunos do município (2021)",
       fill = "Renda média (salários mínimos)") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/renda21.png")

# Renda média por município (2020)
ggplot(mapa20) + theme_bw() +
  geom_sf(aes(fill = RENDA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Renda média dos alunos do município (2020)",
       fill = "Renda média (salários mínimos)") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/renda20.png")

# Nota média por município (2024)
ggplot(mapa24) + theme_bw() +
  geom_sf(aes(fill = NOTA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Nota média dos alunos do município (2024)",
       fill = "Nota média") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/nota24.png")

# Nota média por município (2023)
ggplot(mapa23) + theme_bw() +
  geom_sf(aes(fill = NOTA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Nota média dos alunos do município (2023)",
       fill = "Nota média") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/nota23.png")

# Nota média por município (2022)
ggplot(mapa22) + theme_bw() +
  geom_sf(aes(fill = NOTA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Nota média dos alunos do município (2022)",
       fill = "Nota média") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/nota22.png")

# Nota média por município (2021)
ggplot(mapa21) + theme_bw() +
  geom_sf(aes(fill = NOTA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Nota média dos alunos do município (2021)",
       fill = "Nota média") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/nota21.png")

# Nota média por município (2020)
ggplot(mapa20) + theme_bw() +
  geom_sf(aes(fill = NOTA_MEDIA), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Nota média dos alunos do município (2020)",
       fill = "Nota média") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/nota20.png")

# Razão entre nota e renda por município (2024)
ggplot(mapa24) + theme_bw() +
  geom_sf(aes(fill = RAZAO), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Razão entre nota e renda do município (2024)",
       fill = "Razão") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/razao24.png")

# Razão entre nota e renda por município (2023)
ggplot(mapa23) + theme_bw() +
  geom_sf(aes(fill = RAZAO), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Razão entre nota e renda do município (2023)",
       fill = "Razão") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/razao23.png")

# Razão entre nota e renda por município (2022)
ggplot(mapa22) + theme_bw() +
  geom_sf(aes(fill = RAZAO), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Razão entre nota e renda do município (2022)",
       fill = "Razão") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/razao22.png")

# Razão entre nota e renda por município (2021)
ggplot(mapa21) + theme_bw() +
  geom_sf(aes(fill = RAZAO), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Razão entre nota e renda do município (2021)",
       fill = "Razão") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/razao21.png")

# Razão entre nota e renda por município (2020)
ggplot(mapa20) + theme_bw() +
  geom_sf(aes(fill = RAZAO), color = NA) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(title = "Razão entre nota e renda do município (2020)",
       fill = "Razão") +
  geom_sf(fill = NA, color = "black", data = estados)
ggsave("plots/municipalities/razao20.png")
