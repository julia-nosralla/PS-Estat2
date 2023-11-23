library(dplyr)

Quadro4 <- Dados1 %>%
  filter(Brand != "Na") %>%
  filter(Rating != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  group_by(Brand) %>%
  summarise(Média = round(mean(Rating), digits = 2))
