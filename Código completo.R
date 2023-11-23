library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)
library(scales)
library(xtable)

#Código para análise do faturamento anual

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

#Gráfico de linhas do faturamento anual


cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}

ggplot(tabela_2) +
  aes(x = as_factor(mes), y = Total, group = Categoria, colour = Categoria) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Categoria", labels = c("Feminino", "Infantil", "Masculino")) +
  labs(x = "Mês", y = "Faturamento") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")

#Código para análise da variação do preço por marca

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

#Gráfico boxplot do preço por marca
Dados1 %>%
  filter(Price != 'Na') %>%
  filter(Brand != 'Na') %>%
  ggplot (aes(x = Brand, y = Price)) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = " white ") +
  labs(x = "Marca", y = "Preço") +
  theme_estat()

ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

#Código para análise da relação entre cor e categoria

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

#Gráfico de barras da relação entre cor e categoria

tabela_5 <- tabela_4 %>%
  filter(Categoria != "Na") %>%
  group_by(Color, Categoria) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = case_when(
      Color == 'Vermelho' ~ round(freq/sum(freq[Color == 'Vermelho'])*100, digits = 2),
      Color == 'Verde' ~ round(freq/sum(freq[Color == 'Verde'])*100, digits = 2),
      Color == 'Amarelo' ~ round(freq/sum(freq[Color == 'Amarelo'])*100, digits = 2),
      Color == 'Branco' ~ round(freq/sum(freq[Color == 'Branco'])*100, digits = 2),
      Color == 'Preto' ~ round(freq/sum(freq[Color == 'Preto'])*100, digits = 2),
      Color == 'Azul' ~ round(freq/sum(freq[Color == 'Azul'])*100, digits = 2))
  )


porcentagens <- str_c(tabela_5$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(tabela_5$freq, " (", porcentagens, ")"))

ggplot(tabela_5) +
  aes(
    x = fct_reorder(Color, freq, .desc = T), y = freq,
    fill = Categoria, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.5, hjust = -0.1,
    size = 3
  ) +
  labs(x = "Cor", y = "Frequência") +
  coord_flip() +
  scale_y_continuous(limits = c(0,80)) +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")

#Código para análise da relação entre avaliação e preço

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

#Gráfico de dispersão da relação entre avaliação e preço

ggplot(Quadro) +
  aes(x = Price, y = Rating) +
  geom_point(alpha = 0.3, colour = "#A11D21", size = 3) +
  labs(
    x = "Preço",
    y = "Avaliação"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")

#Código para análise do tipo de devolução por marca


Dados3 <- Dados1 %>% full_join(y = Dados2, by = "Unique ID")

Quadro2 <- Dados3 %>% filter(Brand != "Na") %>%
  filter(`Motivo devolução.x` != "NA") %>% 
  filter(`Motivo devolução.y` != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  group_by(Brand, `Motivo devolução.y`) %>%
  summarise(freq = n())

tabela_3 <- Dados3 %>% filter(Brand != "Na") %>%
  filter(`Motivo devolução.x` != "NA") %>% 
  filter(`Motivo devolução.y` != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  group_by(`Motivo devolução.y`) %>%
  summarise(freq = n())

Quadro2 <- rename(Quadro2, "Motivo de devolução" = "Motivo devolução.y")

#Gráfico de barras do tipo de devolução por marca

Quadro2 <- Quadro2 %>%
  mutate(
    freq_relativa = case_when(
      Brand == 'Nike' ~ round(freq/sum(freq[Brand == 'Nike'])*100, digits = 2),
      Brand == 'Zara' ~ round(freq/sum(freq[Brand == 'Zara'])*100, digits = 2),
      Brand == 'Gucci' ~ round(freq/sum(freq[Brand == 'Gucci'])*100, digits = 2),
      Brand == 'H&M' ~ round(freq/sum(freq[Brand == 'H&M'])*100, digits = 2),
      Brand == 'Adidas' ~ round(freq/sum(freq[Brand == 'Adidas'])*100, digits = 2))
  )

porcentagens <- str_c(Quadro2$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(Quadro2$freq, " (", porcentagens, ")"))

ggplot(Quadro2) +
  aes(
    x = fct_reorder(Brand, freq, .desc = T), y = freq,
    fill = `Motivo de devolução`, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.45, hjust = -0.05,
    size = 3
  ) +
  labs(x = "Marca", y = "Frequência") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 40)) +
  theme_estat()
ggsave("colunas-bi-freq2.pdf", width = 158, height = 93, units = "mm")

#Código para análise daavaliação média por marca

Quadro4 <- Dados1 %>%
  filter(Brand != "Na") %>%
  filter(Rating != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  group_by(Brand) %>%
  summarise(Média = round(mean(Rating), digits = 2))

#Gráfico de barras da avaliação média por marca

legendas <- str_squish(str_c(Quadro4$Média)) %>% str_replace("\\.", ",")

ggplot(Quadro4) +
  aes(x = fct_reorder(Brand, Média, .desc=T), y = Média, label = legendas) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.4, hjust =-0.1,
    size = 3
  ) + 
  labs(x = "Marca", y = "Avaliação média") +
  coord_flip() +
  scale_y_continuous(limits = c(0,3)) +
  theme_estat()
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm")

