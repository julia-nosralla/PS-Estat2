library(dplyr)

Quadro <- Dados1 %>% filter(Price != "Na") %>%
  filter(Rating != "NA") %>% 
  filter (! duplicated(`Product ID`))

cor(Quadro$Price, Quadro$Rating)

Quadro_resumo2 <-  Quadro %>%
  summarise(media = mean(Rating),
            desvio_p = sd(Rating),
            min = min(Rating),
            q25 = quantile(Rating, probs = .25, na.rm = TRUE),
            mediana = quantile(Rating, probs = .5, na.rm = TRUE),
            q75 = quantile(Rating, probs = .75, na.rm = TRUE), 
            max = max(Rating))
