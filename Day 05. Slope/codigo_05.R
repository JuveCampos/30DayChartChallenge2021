# Ojo! El codigo funciona, pero no es muy elegante.
# Checar guias de como instalar ggimage (aunque al final no lo usé).

# Librerias ----
library(tidyverse)
library(leaflet)
library(ggimage)
library(ggrepel)

# Datos ----
datos <- tibble::tribble(
           ~grupo_parlamentario, ~total_diputados, ~anio,
                       "MORENA",             257L, 2018L,
                          "PAN",              78L, 2018L,
                          "PRI",              48L, 2018L,
                           "PT",              47L, 2018L,
                           "MC",              26L, 2018L,
                          "PES",              19L, 2018L,
                          "PRD",              11L, 2018L,
                         "PVEM",              11L, 2018L,
                           "SP",               3L, 2018L,
                "INDEPENDIENTE",               1L, 2015L,
                       "MORENA",              47L, 2015L,
                          "PAN",             109L, 2015L,
                          "PRI",             205L, 2015L,
                           "PT",               0L, 2015L,
                           "MC",              20L, 2015L,
                          "PES",              10L, 2015L,
                          "PRD",              54L, 2015L,
                         "PVEM",              39L, 2015L,
                           "SP",               3L, 2015L)

# Imagenes del partido ----
# Imagenes ----
img <- tibble::tribble(
  ~Partido,                                                                                                                                                                       ~SVG,
  "PAN",                                                             "https://upload.wikimedia.org/wikipedia/commons/thumb/1/14/PAN_%28Mexico%29.svg/60px-PAN_%28Mexico%29.svg.png",
  "PRI",                                                   "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b5/PRI_logo_%28Mexico%29.svg/60px-PRI_logo_%28Mexico%29.svg.png",
  "PRD",                                                   "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8f/PRD_logo_%28Mexico%29.svg/60px-PRD_logo_%28Mexico%29.svg.png",
  "PVEM",                               "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/Logo_Partido_Verde_%28México%29.svg/60px-Logo_Partido_Verde_%28México%29.svg.png",
  "MC", "https://upload.wikimedia.org/wikipedia/commons/thumb/3/34/Logo_Partido_Movimiento_Ciudadano_%28México%29.svg/60px-Logo_Partido_Movimiento_Ciudadano_%28México%29.svg.png",
  "NA",                         "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d8/Partido_Nueva_Alianza_%28México%29.svg/60px-Partido_Nueva_Alianza_%28México%29.svg.png",
  "MORENA",                                             "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ea/Morena_logo_%28Mexico%29.svg/60px-Morena_logo_%28Mexico%29.svg.png",
  "PES",                   "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Partido_Encuentro_Social_%28México%29.svg/60px-Partido_Encuentro_Social_%28México%29.svg.png",
  "IND",                                                                                 "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d6/Logo_Ind.jpg/60px-Logo_Ind.jpg",
  "PT",                         "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Worker%27s_Party_logo_%28Mexico%29.svg/44px-Worker%27s_Party_logo_%28Mexico%29.svg.png",
  "SP", "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d6/Logo_Ind.jpg/60px-Logo_Ind.jpg"
)

# Colores del partido ----
colores_partido <- tribble(
  ~Partido, ~Color,
  "PAN", "#0049d1",
  "PRI", "#de0f00",
  "PRD", "#ded300",
  "MC", "#de8d00",
  "PVEM", "#00de55",
  "MORENA", "#a30000",
  "NA", "#02ada2",
  "INDEPENDIENTE", "#b3009b",
  "PT", "#b33c00",
  "PES", "#2f0485",
  "SP", "black")

# Reescribimos tantito
colores_partido = colores_partido %>%
  mutate(Partido = factor(Partido,
                          levels = c(colores_partido$Partido)))

# Generamos paleta de colores ----
pal = colorFactor(domain = colores_partido$Partido,
            palette = colores_partido$Color)

# Juntamos con los datos de imagenes ----
datos = datos %>%
  left_join(img %>% rename(grupo_parlamentario = Partido))

# Metemos las etiquetas a la base ----
desplaz = 0.1

datos <- datos %>%
  mutate(x_label = ifelse(anio == min(anio),
                          yes = anio - desplaz,
                          no = anio + desplaz)) %>%
  mutate(h_just = ifelse(anio == min(anio),
                         yes = 1,
                         no = 0)) %>%
  mutate(label = str_c(grupo_parlamentario, ", ", total_diputados))

# Desplazamiento manual ----
coord_y = datos$total_diputados
names(coord_y) = str_c(datos$label, " - ", datos$anio)
coord_y

# Desplazamiento manual de las etiquetas
coord_y["PRD, 54 - 2015"] = 58
coord_y["PT, 47 - 2018"] = 45
coord_y["MORENA, 47 - 2015"] = 48

coord_y["PRI, 48 - 2018"] = 53
coord_y["SP, 3 - 2018"] = -5
coord_y["PVEM, 11 - 2018"] = 3
coord_y["MC, 26 - 2018"] = 28
coord_y["SP, 3 - 2015"] = 0
coord_y["INDEPENDIENTE, 1 - 2015"] = -9
coord_y["PT, 0 - 2015"] = -18

datos$coord_y = coord_y

# Gráfica ----
datos %>%
  # filter(total_diputados > 40) %>%
  ggplot(aes(x = anio,
             y = total_diputados)) +
  geom_point(color = "black", size = 3) +
  geom_point(aes(color = grupo_parlamentario)) +
  geom_line(aes(color = grupo_parlamentario)) +
  geom_text(aes(label = label, x = x_label, y = coord_y, color = grupo_parlamentario),
            hjust = datos$h_just,
            size = 3,
            alpha = 1,
            family = "Poppins") +
  scale_x_continuous(breaks = c(2015, 2018), limits = c(2014, 2019)) +
  labs(x = "",
       subtitle = "Number of Federal Deputies, by party. Mexico (2015-2018)",
       color = "Grupo Parlamentario",
       y = "Número de Diputados",
       caption = "Fuente: http://sitl.diputados.gob.mx/LXIV_leg/info_diputados.php",
       title = "Diputados Federales por legislatura,\nCámara de Diputados, México") +
  scale_color_manual(values = pal(sort(unique(datos$grupo_parlamentario)))) +
  theme(legend.position = "bottom",
        plot.title = element_text(family = "Poppins", face = "bold", hjust = 0.5),
        plot.subtitle = element_text(family = "Poppins", hjust = 0.5, color = "gray70"),
        axis.title.x = element_text(family = "Poppins", face = "bold")) +
  guides(color = guide_legend(title.position = "top",
                             title.hjust = 0.5))


# Guardamos imagen ----
ggsave("grafica_05.png",
       device = "png",
       height = 7)

