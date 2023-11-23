library(tidyverse)
library(scales)

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
