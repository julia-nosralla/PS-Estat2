library(dplyr)

Dados3 <- Dados1 %>% full_join(y = Dados2, by = "Unique ID")

Quadro2 <- Dados3 %>% filter(Brand != "Na") %>%
  filter(`Motivo devolução.x` != "NA") %>% 
  filter(`Motivo devolução.y` != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  group_by(Brand, `Motivo devolução.y`) %>%
  summarise(freq = n())

tabela_3 <- Dados3 %>% filter(Brand != "Na") %>%
  filter(`Motivo devolução.x` != "NA") %>% 
  filter(`Motivo devolução.y` != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  group_by(`Motivo devolução.y`) %>%
  summarise(freq = n())

Quadro2 <- rename(Quadro2, "Motivo de devolução" = "Motivo devolução.y")
