library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)

tabela <- Dados1 %>% 
  filter (! duplicated(`Product ID`)) %>%
  filter(Category != "NA") %>%
  filter(Price != "NA") %>%
  filter(`Data Venda`!= "NA") %>%
  mutate(`Data Venda` = mdy(`Data Venda`))%>%
  mutate(Category = case_when(
  Category %>% str_detect("Category") ~ "Categoria",
  Category %>% str_detect("Kids' Fashion") ~ "Infantil",
  Category %>% str_detect("Men's Fashion") ~ "Masculino",
  Category %>% str_detect("Women's Fashion") ~ "Feminino"
))

  
tabela_2 <- tabela %>% group_by(mes = month(`Data Venda`), Category) %>%
  summarise(Total = sum(Price))

tabela_2 <- rename(tabela_2, Categoria = Category)
