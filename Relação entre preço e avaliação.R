library(dplyr)

Quadro <- Dados1 %>% filter(Price != "Na") %>%
  filter(Rating != "NA") %>% 
  filter (! duplicated(`Product ID`))

cor(Quadro$Price, Quadro$Rating)

Quadro_resumo2 <- Quadro %>%
  summarize(Média = round(mean(Rating),2),
            `Desvio Padrão` = round(sd(Rating),2),
            `Variância` = round(var(Rating),2),
            `Mínimo` = round(min(Rating),2),
            `1º Quartil` = round(quantile(Rating, probs = .25),2),
            Mediana = round(quantile(Rating, probs = .5),2),
            `3º Quartil` = round(quantile(Rating, probs = .75),2),
            `Máximo` = round(max(Rating),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",","))

Quadro_resumo3 <- Quadro %>%
  summarize(Média = round(mean(Price),2),
            `Desvio Padrão` = round(sd(Price),2),
            `Variância` = round(var(Price),2),
            `Mínimo` = round(min(Price),2),
            `1º Quartil` = round(quantile(Price, probs = .25),2),
            Mediana = round(quantile(Price, probs = .5),2),
            `3º Quartil` = round(quantile(Price, probs = .75),2),
            `Máximo` = round(max(Price),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",","))

xtable::xtable(Quadro_resumo2)
xtable::xtable(Quadro_resumo3)