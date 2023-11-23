library(xtable)
library(stringr)

quadro_resumo <- Dados1 %>% 
  filter (! duplicated(`Product ID`)) %>%
  filter(Price != "NA") %>% 
  filter(Brand != "NA") %>% 
  group_by(Brand) %>% 
  summarize(Média = round(mean(Price),2),
            `Desvio Padrão` = round(sd(Price),2),
            `Variância` = round(var(Price),2),
            `Mínimo` = round(min(Price),2),
            `1º Quartil` = round(quantile(Price, probs = .25),2),
            Mediana = round(quantile(Price, probs = .5),2),
            `3º Quartil` = round(quantile(Price, probs = .75),2),
            `Máximo` = round(max(Price),2)) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = str_replace(V1,"\\.",",")) %>%
  mutate(V2 = str_replace(V2,"\\.",",")) %>%
  mutate(V3 = str_replace(V3,"\\.",",")) %>%
  mutate(V4 = str_replace(V4,"\\.",",")) %>%
  mutate(V5 = str_replace(V5,"\\.",","))
  
row.names(quadro_resumo)[1] <- "Marca"

xtable::xtable(quadro_resumo)
