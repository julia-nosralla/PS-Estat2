library(scales)
library(tidyverse)


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

