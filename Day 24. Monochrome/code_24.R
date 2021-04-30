# Heatmaps en R ----
library(tidyverse)
library(plotly)
library(vapoRwave)

coches = readxl::read_xlsx("coches.xlsx") %>%
  mutate(text = str_c("<b>Entidad:</b> ", ent, "<br>",
                      "<b>Año: </b>", year, "<br>",
                      "<b>Valor: </b>", prettyNum(round(valor, 2), big.mark = ",")
                      ))

# Version 1
plt_1 <- coches %>%
  filter(ent != "Nacional") %>%
  ggplot(aes(x = year,
             y = valor,
             group = ent,
             text = text)) +
  geom_line()

# Convertimos a interactiva
ggplotly(plt_1, tooltip = "text")

# Version 2
plt_2 = coches %>%
  filter(ent != "Nacional") %>%
  ggplot(aes(x = year,
             y = valor,
             group = ent,
             text = text)) +
  geom_line() +
  facet_wrap(~ent,
             ncol = 4)

# Convertimos a interactiva
ggplotly(plt_2, tooltip = "text")

# Versión 3
density(coches$valor)

# Modificacion de los datos: para que la grafica vaya de antes a despues
coches <- coches %>%
  mutate(year_factor = factor(year,
                              levels = rev(unique(coches$year))))

# Grafica ----
(  plot = coches %>%
  filter(ent != "Nacional") %>%
  ggplot(aes(x = ent,
             y = year_factor,
             fill = valor,
             text = text)) +
  geom_tile(color = "black") +
  scale_fill_gradientn(colours = c(rgb(27,67,13, maxColorValue = 255),
                                   rgb(117,251,76, maxColorValue = 255)),
                       breaks = c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800),
                       labels = c(-100, 0, 100, 200, 300, 400, 500, 600, 700, 800)
                       ) +
  labs(x = "", y = "",
       caption = "Vehículos por cada mil habitantes\nFuente: INEGI. Estadísticas de vehículos de motor registrados en circulación\n@JuvenalCamposF - #30DayChartChallenge - TS + Monochrome",
       title = str_wrap(str_c(coches$variable[1]), 50),
       subtitle = "Number of motor vehicles registered per 1,000 people by state, Mexico") +
  jwz() +
  # scale_fill_jwz() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5,
                                  color = rgb(117,251,76, maxColorValue = 255)),
        legend.position = "bottom",
        text = element_text(family = "Windows Command Prompt"),
        plot.subtitle = element_text(color = rgb(117,251,76, maxColorValue = 255)),
        plot.caption = element_text(color = rgb(117,251,76, maxColorValue = 255)),
        axis.text  = element_text(color = rgb(117,251,76, maxColorValue = 255)),
        legend.title = element_text(color = rgb(117,251,76, maxColorValue = 255)),
        legend.text = element_text(color = rgb(117,251,76, maxColorValue = 255))) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5,
                               barwidth = 15,
                               barheight = 0.5)) )

plot
# Convertimos a interactivo
plotly::ggplotly(plot, tooltip = "text")

