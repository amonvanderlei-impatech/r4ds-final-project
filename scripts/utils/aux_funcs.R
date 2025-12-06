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
    x == "Q" ~ 22.5,
    TRUE ~ NA_real_,
  )
}
