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