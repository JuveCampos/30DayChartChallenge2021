
# Librerias ----
library(tidyverse)
library(ggpomological)
library(ggtext)

tema_juve = theme_pomological_fancy() +
  theme(legend.position = "bottom",
        text = element_text(family = "Poppins"),
        plot.title = element_text(family = "Poppins", face = "bold", hjust = 0.5, size = 15),
        plot.subtitle = element_text(family = "Poppins", hjust = 0.5, color = "gray50", size = 12),
        plot.caption = element_text(hjust = 1, size = 10, family = "Poppins"),
        axis.title = element_markdown(family = "Poppins", size = 10),
        legend.title = element_text(family = "Poppins", size = 10, face = "bold"),
        legend.text = element_text(family = "Poppins", size = 7),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        axis.text = element_text(family = "Poppins", size = 10),
        axis.ticks = element_line(color = "brown"),
        panel.border = element_rect(size = 1))

mort_mat = tibble::tribble(
  ~anio, ~tasa_mortalidad_materna,
  2007L,                     48.3,
  2008L,                     49.3,
  2009L,                     53.2,
  2010L,                     43.7,
  2011L,                     42.9,
  2012L,                     42.1,
  2013L,                     37.9,
  2014L,                     38.7,
  2015L,                       35,
  2016L,                     37.2,
  2017L,                       35,
  2018L,                     34.6,
  2019L,                     34.2,
  2020L,                     47.1
)


mort_mat %>%
  ggplot(aes(x = anio,
             y = tasa_mortalidad_materna)) +
  geom_line(size = 1) +
  geom_point(pch = 21, size = 3, fill = "red") +
  geom_hline(yintercept = pull(mort_mat[mort_mat$anio == 2020,]),
             color = "red",
             linetype = 2) +
  geom_smooth(data = mort_mat %>% filter(anio != 2020),
              aes(x = anio,
                  y = tasa_mortalidad_materna)) +
  labs(title = "Tasa de mortalidad materna",
       subtitle = "2007-2020",
       x = "", y = "Tasa de mortalidad materna",
       caption = "Fuente: Informes semanales de Mortalidad Materna.
       Fallecimientos por cada 100,000 nacidos vivos.
       Checado en la Semana Epidemiológica 11 del 2021.
       https://omm.org.mx/wp-content/uploads/2021/03/BOLETIN_MUERTE-MATERNA_11_2021.pdf
       #30DayChartChallenge - Dia 29 - Unc + Deviations" ) +
  geom_text(aes(label = str_c("Valor tasa mortalidad: ", pull(mort_mat[mort_mat$anio == 2020,])),
                x = 2018,
                y = pull(mort_mat[mort_mat$anio == 2020,]) + 0.5)) +
  tema_juve


