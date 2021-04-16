# Librerias ----
library(tidyverse)
library(rvest)
library(rebus)
library(ggpomological)
library(ggtext)
library(wesanderson)

# Datos ----
url = "https://en.wikipedia.org/wiki/List_of_highest-grossing_media_franchises"


tabs = read_html(url) %>%
  html_table() %>%
  pluck(2)

# Limpieza de los datos
datos = tabs %>%
  filter(Franchise != "" & !is.na(Franchise)) %>%
  filter(!str_detect(Franchise, pattern = "^\\$")) %>%
  mutate(ingreso = str_extract(`Total revenue (USD)`,
                               pattern = "\\$" %R% one_or_more(DGT) %R% optional(".") %R% optional(one_or_more(DGT)))) %>%
  mutate(ingreso = str_remove(ingreso, pattern = "\\$") %>% as.numeric()) %>%
  mutate(`Year of inception` = str_remove(`Year of inception`, pattern = "\\[\\w+\\]")) %>%
  mutate(antiguedad = 2021 - as.numeric(`Year of inception`)) %>%
  rename(media = `Original media`) %>%
  mutate(media = str_remove(media, pattern = "\\[\\w+\\]")) %>%
  mutate(media_2 = str_replace_all(media, c("Video game" = "Videojuegos",
                                            "Cartoon character" = "Serie o película Animada",
                                            "^Book$" = "Novela/Libro",
                                            "Animated cartoon" = "Serie o película Animada",
                                            "Film" = "Película",
                                            "Animated series" = "Serie o película Animada",
                                            "Manga" = "Anime/Manga",
                                            "^Novel$" = "Novela/Libro",
                                            "Comic book" = "Comics",
                                            "Animated film" = "Serie o película Animada",
                                            "Anime series" = "Anime/Manga",
                                            "Comic strip" = "Comics",
                                            "Television series" = "Serie de Televisión",
                                            "Cartoon" = "Serie o película Animada",
                                            "Digital Pet" = "Mascota Digital",
                                            "Musical theatre" = "Obra de teatro musical",
                                            "Digital pet" = "Mascota Digital",
                                            "Picture book" = "Serie o película Animada",
                                            "Song" = "Musica/Canción",
                                            "Greeting card" = "Tarjetas de agradecimiento",
                                            "Visual novel" = "Anime/Manga",
                                            "^Comic$" = "Comics",
                                            "Reality television" = "Reality Show",
                                            "^comics$" = "Comics"))) %>%
  mutate(media_3 = str_replace_all(media_2, pattern = or1(c("Obra de teatro musical",
                                                            "Mascota Digital",
                                                            "Tarjetas de agradecimiento",
                                                            "Musica/Canción",
                                                            "Reality Show")),
                                   replacement = "Otros"))

levels = datos %>%
  group_by(media_3) %>%
  count() %>%
  arrange(-n) %>%
  pull(media_3)

# Gráfica ----
plt = datos %>%
  mutate(media_3 = factor(media_3, levels = levels)) %>%
  ggplot(aes(x = antiguedad, y = ingreso, fill = media_3, text = str_c("Franchise: ", Franchise))) +
  # geom_point(size = 2, color = "black") +
  geom_point(size = 3, shape = 21, color = "black") +
  labs(fill = "Medio de origen",
       title = "Franquicias de entretenimiento de mayor recaudación de la historia\nRelación entre antiguedad e ingreso recabado",
       subtitle = "Highest-grossing media franchises. Relationship between total revenue and time in the market",
       caption = "Incluye datos de 157 franquicias con ingresos de más de 2 mil millones de dólares <br>
       <b>Fuente:</b> Wikipedia. _List of highest-grossing media franchises_. Consultado el 15 de abril del 2021. <br>
       @JuvenalCamposF - #30DayChartChallenge - Día 15: Bivariado.",
       x = "Antiguedad\n(años)",
       y = "Ingreso\n(miles de millones de dólares)") +
  theme_pomological_fancy() +
  scale_fill_manual(values = c("#F21A00",
                               "#FAEFAF",
                               "purple",
                               "#F2AD00",
                               "#E0BAD7",
                               "#3B9AB2",
                               "navy",
                               "gray50"
                               )) +
  theme(legend.position = "bottom",
        text = element_text(family = "Poppins"),
        plot.title = element_text(family = "Poppins", face = "bold", hjust = 0.5, size = 10),
        plot.subtitle = element_text(family = "Poppins", hjust = 0.5, color = "gray50", size = 8),
        plot.caption = element_markdown(hjust = 0.5, size = 8, family = "Poppins"),
        axis.title = element_markdown(family = "Poppins", size = 8),
        legend.title = element_text(family = "Poppins", size = 8, face = "bold"),
        legend.text = element_text(family = "Poppins", size = 7),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        axis.text = element_text(family = "Poppins", size = 8),
        axis.ticks = element_line(color = "brown"),
        panel.border = element_rect(size = 1)
        ) +
  guides(fill = guide_legend(title.position = "top",
                              title.hjust = 0.5))

plt
# Imprimimos la grafica
ggsave("grafica_15.png",
       device = "png",
       height = 6,
       width = 9
       )

plt_interactiva = plotly::ggplotly(plt, tooltip = "text")

htmlwidgets::saveWidget(plt_interactiva,
                        "15_interactiva.html")


