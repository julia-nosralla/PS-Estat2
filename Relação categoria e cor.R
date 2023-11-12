Masculino <- Dados1 %>% filter(Category != "Na") %>%
  filter(Color != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  filter(Category == "Men's Fashion")

Feminino <- Dados1 %>% filter(Category != "Na") %>%
  filter(Color != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  filter(Category == "Women's Fashion")
