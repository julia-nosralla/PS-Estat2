library(dplyr)

Quadro2 <- Dados1 %>% filter(Brand != "Na") %>%
  filter(`Motivo devolução` != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  group_by(Brand, `Motivo devolução`) %>%
  summarise(Freq = n())

tabela_3 <-  Dados1 %>% filter(Brand != "Na") %>%
  filter(`Motivo devolução` != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  group_by(`Motivo devolução`) %>%
  summarise(Freq = n())
