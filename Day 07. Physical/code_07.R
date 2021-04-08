# Hacemos la gráfica ----
library(ggimage)
library(tidyverse)
library(ggExtra)

data = readxl::read_xlsx("datos_físico_pokemon-.xlsx")

# Grafica ----
plt = data[1:151,] %>%
  ggplot(aes(x = attack, y = defense)) +
  geom_point(alpha = 0.1) +
  labs(title = "Distribución de las estadísticas de ataque y defensa\nfísicas de los primeros 151 Pokemon",
       subtitle = "Distribution of physical attack and defense stats for the first 151 known Pokemon",
       x = "Ataque físico", y = "Defensa física",
       caption = "Fuente: Datos provenientes de la PokeAPI:
       https://pokeapi.co") +
  theme_minimal() +
  geom_image(aes(image = sprite)) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(family = "Futura Condensed Medium"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = rgb(248, 213, 112, maxColorValue = 255), fill = NA,  size = 2)
        # ,
        # plot.background = element_rect(fill = "#fffbdb")
        )

plt

# Show only marginal plot for x - y axis
p3 <- ggMarginal(plt, color = c("purple"),
                 fill = c("#f5d4ff"), size=4)

p3

