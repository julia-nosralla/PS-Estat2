library(dplyr)

Quadro_resumo <- Dados1 %>%
  filter (! duplicated(`Product ID`)) %>%
  group_by(Brand)%>%
  summarise(media = mean(Price, na.rm = TRUE),
            desvio_p = sd(Price, na.rm = TRUE),
            min = min(Price, na.rm = TRUE),
            q25 = quantile(Price, probs = .25, na.rm = TRUE),
            mediana = quantile(Price, probs = .5, na.rm = TRUE),
            q75 = quantile(Price, probs = .75, na.rm = TRUE), 
            max = max(Price, na.rm = TRUE))
Quadro_resumo <- na.omit(Quadro_resumo)

