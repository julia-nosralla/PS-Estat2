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

Quadro2 <- Dados1 %>%
  filter(Brand != "Na") %>%
  filter(`Motivo devolução` != "NA") %>% 
  filter (! duplicated(`Product ID`)) %>%
  group_by(Brand, `Motivo devolução`) %>%
  summarise(freq = n())

legendas <- str_squish(str_c(Quadro2$freq))

ggplot(Quadro2) +
  aes(
    x = fct_reorder(Brand, freq, .desc = T), y = freq,
    fill = `Motivo devolução`, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Marca", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-freq2.pdf", width = 158, height = 93, units = "mm")
