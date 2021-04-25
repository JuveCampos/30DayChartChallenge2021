# Librerias ----
options(scipen = 999)
library(tidyverse)
library(ggpomological)
library(ggtext)
library(ggrepel)

# Tema ----
tema_juve = theme_pomological_fancy() +
  theme(legend.position = "bottom",
        text = element_text(family = "Poppins"),
        plot.title = element_text(family = "Poppins", face = "bold", hjust = 0.5, size = 10),
        plot.subtitle = element_text(family = "Poppins", hjust = 0.5, color = "gray50", size = 10),
        plot.caption = element_markdown(hjust = 1, size = 10, family = "Poppins"),
        axis.title = element_markdown(family = "Poppins", size = 10),
        legend.title = element_text(family = "Poppins", size = 10, face = "bold"),
        legend.text = element_text(family = "Poppins", size = 7),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        axis.text = element_text(family = "Poppins", size = 10),
        axis.ticks = element_line(color = "brown"),
        panel.border = element_rect(size = 1))

# Datos ---
cafe = readRDS("cafe_mx.rds") %>%
  arrange(Nomestado)

# Etiquetas ----
cafe$label = cafe$prod_ton
cafe$label[seq(2, nrow(cafe), by = 2)] <- " "

# Grafica ----
cafe %>%
  ggplot(aes(x = Anio, y = prod_ton)) +
  geom_line(color = "#871C28") +
  geom_point(size = 3, color = "#871C28") +
  geom_point(color = "white") +
  geom_text_repel(aes(label = prettyNum(label, big.mark = ",")),
                  size = 3, family = "Poppins") +
  tema_juve +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(breaks = seq(2005,2020, by = 2)) +
  facet_wrap(~Nomestado) +
  labs(title = "Volumen de la producción de café cereza",
       y = "Toneladas", x = "Año",
       subtitle = "Principales estados productores de café.\nProducción en Toneladas",
       caption = "<b>Fuente:</b> SIAP. Secretaría de Agricultura, 2005-2019.")

ggsave("plot_20.png",
       device = "png",
       width = 10,
       height = 6)


