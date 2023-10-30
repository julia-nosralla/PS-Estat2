library(dplyr)
library(lubridate)

tabela <- Dados1 %>% 
  filter (! duplicated(`Product ID`)) %>%
  filter(Category != "NA") %>%
  filter(Price != "NA") %>%
  filter(`Data Venda`!= "NA") %>%
  mutate(`Data Venda` = mdy(`Data Venda`))

  
tabela_2 <- tabela %>% group_by(mes = month(`Data Venda`), Category) %>%
  summarise(Total = sum(Price))
