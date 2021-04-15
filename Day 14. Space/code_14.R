# Librerias
library(tidyverse)
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)
devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(magick)


# Datos ----


earth <- ne_countries(scale = "medium", returnclass = "sf")
class(earth)

mtjohn <- st_point(c(170,-43)) %>% # create an sfg object ( a vector) with the geometry type POINT from the Mt John coordinates https://en.wikipedia.org/wiki/Mount_John_University_Observatory
  st_sfc(crs=st_crs(earth)) %>%   # convert to a geospatial  sfc object ( a list) with CRS set to the same as the earth sf object
  st_sf(name="Mt John Observatory") # Convert the geometry type POINT (a dataframe) with a CRS to an sf object and the point labelled with a name



plot(lineas, max.plot = 1)


# Nota del autor original
# The planets data is not geojson
# because positions need to be calculated by date from keplerian elements



# Checamos si está todo ----
constellation_lines_sf$id %in% names$id # Parece que si

# filtramos para una

constellation_lines_sf %>%
  filter(id == constellation_lines_sf$id[89]) %>%
  plot(max.plot = 1)

constellation_lines_sf %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))%>%
  ggplot()+
  geom_sf()



# Plot the points of Virgo to visualise this new point geometry
constellation_lines_sf_p %>%
  filter(id=="Vir") %>%
  ggplot() +
  geom_sf(colour="white")



# Lineas ----
url1 <- "https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/constellations.lines.json"
lineas <- st_read(url1,stringsAsFactors = FALSE) %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))

# Atributos ----
constellations_data = jsonlite::fromJSON("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/constellations.json")
names = constellations_data$features %>%
  as_tibble()

# Puntos ----
# Create points of the constellation lines
lineas_p <-lineas %>%
  # cast the MULTILINESTRING TO MULTIPOINT
  st_cast("MULTIPOINT")

# Create a table with the X,Y coordinates
(coords <- lineas_p %>%
    filter(id=="Vir") %>%
    st_coordinates() %>%
    as.data.frame() )

# Otras estrellas ----
url3 <- "https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/stars.6.json"
stars_sf <- st_read(url3,stringsAsFactors = FALSE)

names$properties$desig


# Función para la gráfica ----
grafica_constelaciones = function(constelacion){
      # Filtramos lineas
      l = lineas %>%
        filter(id == constelacion)

      plot(l, max.plot = 1)

      # Filtramos puntos
      p = lineas_p %>%
        filter(id == constelacion)

      # Filtramos nombre
      nombre = names$properties[names$properties$desig == constelacion,]$name
      nombre

      # Filtramos estrellas cercanas
      bbox = st_bbox(l) %>%
        st_as_sfc()

      near_star = st_intersection(stars_sf, bbox)

      # max(stars_sf$mag)
      # min(stars_sf$mag)
      # plot(density(stars_sf$mag))

      # Graficamos ----
      ggplot() +
        geom_sf(data = near_star, size = 0.2*near_star$mag, color = "white") +
        geom_sf(data = l, color = "white", size = 1) +
        geom_sf(data = p, size = 4, color = "black") +
        geom_sf(data = p, size = 3, color = "white") +
        labs(subtitle = nombre,
             title = "Constelaciones: relaciones entre estrellas",
             caption = "#30DayChartChallenge - Día 14 - Relationships + Space
             Inspirado en el artículo de blog de @Kim_Fitter
             https://kimnewzealand.github.io/2019/02/21/celestial-maps/
             Datos obtenidos del repo de Github: ofrohn/d3-celestial"
             ) +
        theme_minimal() +
        theme(
          # panel.background = element_rect(color = "black"),
              plot.background = element_rect(fill = "#051457"),
              panel.background = element_rect(fill = "#051457", color = "#051457"),
              plot.title = element_text(family = "AquilineTwo", color = "white", hjust = 0.5, size = 25),
              plot.subtitle = element_text(family = "AquilineTwo", color = "white", hjust = 0.5, size = 18),
              plot.caption = element_text(family = "Times New Roman", color = "white", hjust = 1, size = 10),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.line = element_blank(),
              panel.grid = element_line(color = "gray20"))

      ggsave(str_c("constelaciones/plot_", nombre, ".png"),
             device = "png",
             width =8,
             height = 8)
}

# Lapplicamos la funcion
lapply(names$properties$desig, grafica_constelaciones)

# Creamos el gif ----
str_c("constelaciones/", list.files("constelaciones/")) %>%
  map(image_read) %>% # Lee rutas de los archivos.
  image_join() %>% # Junta imágenes
  image_animate(fps=1) %>% # Anima las imagenes, con 1 segundo entre imágenes.
  image_write("const.gif") # Escribe el gif en el directorio.

