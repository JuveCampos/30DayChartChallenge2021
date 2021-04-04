# devtools::install_github("bradleyboehmke/harrypotter")

# Librerias ----
library(tidyverse)
library(harrypotter)
library(rebus)
library(ggpomological)
library(plotly)

# Datos ----
hp_texts <- list(
  harrypotter::philosophers_stone,
  chamber_of_secrets,
  prisoner_of_azkaban,
  goblet_of_fire,
  order_of_the_phoenix,
  half_blood_prince,
  deathly_hallows
)

# Explorando, veo que cada elemento de la lista es uno de los libros y cada elemento del vector es cada capítulo
hp_texts[[1]][1]
hp_texts[[1]][5]
hp_texts[[5]][5]

# Sacamos el numero de capítulos por libro
no_caps_por_libro = lapply(1:7, function(x){
  length(hp_texts[[x]])}) %>% unlist()

# Patrón de texto a buscar
houses = c("Gryffindor", "Slytherin", "Hufflepuff", "Ravenclaw") %>%
  str_to_lower()

# Generamos los datos ----
datos = lapply(1:7, function(y){
  # Extraccion de resultados del patrón
  casas_mencionadas_por_cap = lapply(1:no_caps_por_libro[y], function(x){
    str_extract_all(str_to_lower(hp_texts[[y]][x]), pattern = or1(houses)) %>% unlist()
  }) %>% unlist()
  # Lo metemos en una tabla
  tibble(libro = y,
         casas = casas_mencionadas_por_cap)
})

# Generamos la tabla final ----
datos = do.call(rbind.data.frame, datos)

# Generamos los datos conteo ----
datos_contados <- datos %>%
  mutate(casas = str_to_title(casas)) %>%
  group_by(casas) %>%
  count(casas)

# Generamos los datos bolita ----
datos_bolita = lapply(1:nrow(datos_contados), function(x){
  tibble(name = pull(datos_contados[x,"casas"]),
         points = runif(n = pull(datos_contados[x,"n"]),
                        min = 0,
                        max = pull(datos_contados[x,"n"])))
})

datos_bolita <- do.call(rbind, datos_bolita)

# Colores ----
color_bg = "#e1d9c4"
color_font = "#5b1012"
color_panel = "#6c523d"
font_panel = "Poppins"

# Generamos la gráfica de barras de frecuencia ----
datos_contados %>%
  ggplot(aes(x = casas, y = n, fill = casas)) +
  geom_col() +
  scale_fill_manual(values = c("#9c1203",
                               "#e3a000",
                               "#00165e",
                               "#033807")) +
  ylim(c(0, 900)) +
  theme_void() +
  theme(legend.position = "none")

# Guardamos la gráfica ----
ggsave("grafica_barras.png",
       device = "png",
       bg = "transparent")


# Gráfica de bolitas ----
plt = datos_bolita %>%
  ggplot(aes(x = name, y = points, color = name)) +
  geom_jitter(width = 0.3, size = 2.5) +
  scale_color_manual(values = c("#9c1203",
                               "#e3a000",
                               "#00165e",
                               "#033807")) +
  geom_text(data = datos_contados,
            aes(x = casas, y = n, label = n),
            hjust = 0.5,
            vjust = -1,
            family = "HarryP",
            size = 7) +
  ylim(c(0,800)) +
  theme_pomological_fancy() +
  theme(legend.position = "none",
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(family = "HarryP",
                                  size = 30,
                                  hjust = 0.5),
        plot.caption = element_text(family = "AquilineTwo",
                                    size = 9),
        axis.text.x = element_text(family = "AquilineTwo",
                                    size = 20)
      ) +
  labs(x = "", y = "", title = "Mencion a cada casa de Hogwarts en los\n7 libros de Harry Potter",
       caption = "Cada punto es una mención.
       Nota: Se incluye también las menciones al apellido del fundador de cada casa.
       Fuente: Datos de los textos de los libros provenientes de {harrypotter} de Bradley Boehmke:
       Github: bradleyboehmke/harrypotter
       @JuvenalCamposF - Week1 - Comparisons - Magical"
       )

plt

# Guardamos la gráfica ----
ggsave("grafica_bolita.png",
       device = "png",
       bg = "transparent")

# Versión interactiva ----
plotly::ggplotly(plt) %>%
  layout(plot_bgcolor='rgb(254, 247, 234)')

# Los logos se añadieron en post-producción con keynote, pero no dudo que haya una forma de hacerlo con código.

