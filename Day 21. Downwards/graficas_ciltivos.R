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

root = "02_Datos/datos_agricolas/"
files = str_c(root, list.files(root))
fila = files[1]

# Para café cereza ----

datos <- lapply(files, function(fila){
  bd = read_csv(fila,
           locale = locale(encoding = "WINDOWS-1252")) %>%
    filter(Nomestado == "Chiapas") %>%
    filter(Nomcultivo == "Café cereza") %>%
    select(Anio, Nomestado,Nommunicipio, Volumenproduccion)
  return(bd)
})

cafe_chis = do.call(rbind, datos)

vol = cafe_chis %>%
  group_by(Anio) %>%
  summarise(vol = sum(Volumenproduccion))

rgb(135, 28, 40, maxColorValue = 255)

vol %>%
  ggplot(aes(x = Anio, y = vol)) +
  geom_line(color = "#871C28") +
  geom_point(size = 3, color = "#871C28") +
  geom_point(color = "white") +
  geom_text_repel(aes(label = prettyNum(vol, big.mark = ",")),
                  size = 3) +
  tema_juve +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(breaks = 2005:2020) +
  labs(title = "Volumen de la producción de café cereza",
       y = "Toneladas", x = "Año",
       subtitle = "Estado de Chiapas.\nProducción en Toneladas",
       caption = "<b>Fuente:</b> SIAP. Secretaría de Agricultura, 2005-2019.")

ggsave(str_c("02_Datos/Datos segob ordenados/graficas/1/grafica_",
             "Produccion_cafe_cerezo_chiapas",
             ".png"),
       width = 10, height = 6, device = "png")


# Para Cacao ----

datos <- lapply(files, function(fila){
  bd = read_csv(fila,
                locale = locale(encoding = "WINDOWS-1252")) %>%
    filter(Nomestado %in% c("Chiapas", "Tabasco"))  %>%
    filter(Nomcultivo == "Cac") %>%
    select(Anio, Nomestado,Nommunicipio, Volumenproduccion)
  return(bd)
})

cacao = do.call(rbind, datos)

vol = cacao %>%
  group_by(Anio, Nomestado) %>%
  summarise(vol = sum(Volumenproduccion))

vol %>%
  ggplot(aes(x = Anio, y = vol)) +
  geom_line(color = "#871C28") +
  geom_point(size = 3, color = "#871C28") +
  geom_point(color = "white") +
  geom_label_repel(aes(label = prettyNum(vol, big.mark = ",")),
                  size = 3,
                  color = "#871C28") +
  tema_juve +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(breaks = 2005:2020) +
  labs(title = "Volumen de la producción de Cacao",
       y = "Toneladas", x = "Año",
       subtitle = "Estados de Chiapas y Tabasco.\nProducción en Toneladas",
       caption = "<b>Fuente:</b> SIAP. Secretaría de Agricultura, 2005-2019.") +
  facet_wrap(~Nomestado)

ggsave(str_c("02_Datos/Datos segob ordenados/graficas/1/grafica_",
             "Produccion_cacao_chiapas_tabasco",
             ".png"),
       width = 10, height = 6, device = "png")

