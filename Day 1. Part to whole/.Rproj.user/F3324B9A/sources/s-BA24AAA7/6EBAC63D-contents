# Informacion estatal ----
library(tidyverse)
library(sf)
library(leaflet)

# Datos ----
# http://infosiap.siap.gob.mx/gobmx/datosAbiertos/ProduccionAgricola/Cierre_agricola_mun_2019.csv

# Descarga de la información
curl::curl_download("http://infosiap.siap.gob.mx/gobmx/datosAbiertos/ProduccionAgricola/Cierre_agricola_mun_2019.csv",
                    "d_agg_2019.csv")

# Lectura de la información
datos = readr::read_csv("d_agg_2019.csv",
                        locale = locale(encoding = "WINDOWS-1252"))

# Filtramos para quedarnos con el aguacate
avocados_from_mexico = datos %>%
  filter(Nomcultivo == "Aguacate")

# Generamos los datos por estado
edo = avocados_from_mexico %>%
  group_by(Nomestado) %>%
  summarise(total_prod = sum(Volumenproduccion, na.rm = T)) %>%
  mutate(pctje = 100*(total_prod/sum(total_prod))) %>%
  arrange(-total_prod) %>%
  mutate(rank = rank(-total_prod)) %>%
  ungroup() %>%
  mutate(menos_de_uno = ifelse(pctje < 1, "Otros", Nomestado)) %>%
  group_by(menos_de_uno) %>%
  summarise(pctje_2 = sum(pctje)) %>%
  arrange(-pctje_2) %>%
  ungroup() %>%
  mutate(edo = factor(menos_de_uno, levels = c("Michoacán",
                                                  "Jalisco",
                                                  "México",
                                                  "Nayarit",
                                                  "Morelos",
                                                  "Otros"))) %>%
  mutate(alpha = str_replace_all(edo, c("Michoacán" = "1",
                                        "Jalisco" = "1",
                                        "México" = "1",
                                        "Nayarit" = "1",
                                        "Morelos" = "1",
                                        "Otros" = "1")) %>% as.numeric())

# Hacemos la gráfica de dona
plt = edo %>%
  ggplot(aes(x = 1,
             y = pctje_2,
             fill = edo
             )) +
  geom_col(alpha = edo$alpha) +
  coord_polar(theta = "y", start = 120/57.2958) +
  xlim(c(-1, 1.5)) +
  theme_void() +
  scale_fill_manual(values = c("#99621E",
                               "#1C7C54",
                               "#73E2A7",
                               "#FEFFBE",
                               "#1B512D",
                               "#B1CF5F")) +
  labs(fill = "Estado") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top",
                               title.hjust = 0.5))
plt

# Guardamos con fondo transparente
ggsave("avocado_plot.png",
       device = "png",
       bg = "transparent"
       )

# Mapas de las regiones
mpios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson", quiet = T) %>%
  mutate(CVE_ENT = as.numeric(CVE_ENT),
         CVE_MUN = as.numeric(CVE_MUN))

# Marcamos los municipios
muni = avocados_from_mexico %>%
  select(Idestado, Nomestado, Idmunicipio, Nommunicipio) %>%
  rename(CVE_ENT = Idestado,
         CVE_MUN = Idmunicipio) %>%
  mutate(avocado = 1)

# Juntamos con las geometrías
r = left_join(mpios, muni) %>%
  mutate(avocado = as.character(ifelse(is.na(avocado), 0, 1)))

# vemos como se llaman los estados
unique(r$NOM_ENT)

# Hacemos un vector con los principales estados productores
edos_chidos = c("Michoacán de Ocampo", "Morelos", "México" , "Jalisco" , "Nayarit")

# Hacemos los mapas en loop. (SI, YA SE QUE UN LAPPLY ERA MÁS EFICIENTE)
for(edo in edos_chidos){
r %>%
  filter(NOM_ENT == edo) %>%
  ggplot() +
  aes(fill = avocado) +
  geom_sf() +
  scale_fill_manual(values = c("transparent", "#B1CF5F")) +
  theme_void() +
  theme(legend.position = "none")

ggsave(str_glue("{edo}.png"),
       device = "png",
       bg = "transparent",
       width = 2,
       height = 2)

}

# Y lo demás lo arreglamos en posproducción en Keynote :9
