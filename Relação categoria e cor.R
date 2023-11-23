
tabela_4 <- Dados1 %>% 
  filter(Category != "Na") %>%
  filter(Color != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  mutate(Category = case_when(
    Category %>% str_detect("Women's Fashion") ~ "Feminina",
    Category %>% str_detect("Men's Fashion") ~ "Masculina",
  )) %>%
  mutate(Color = case_when(
    Color %>% str_detect("Black") ~ "Preto",
    Color %>% str_detect("Blue") ~ "Azul",
    Color %>% str_detect("White") ~ "Branco",
    Color %>% str_detect("Green") ~ "Verde",
    Color %>% str_detect("Red") ~ "Vermelho",
    Color %>% str_detect("Yellow") ~ "Amarelo"
  ))
  
tabela_4 <- rename(tabela_4, Categoria = Category)