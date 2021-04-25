# Librerias ----
library(tidyverse)

# Datos ----
# Consultado en https://datos.covid-19.conacyt.mx/#DownZCSV
fecha = "20210423"
bd = readr::read_csv(str_glue("https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Municipio_Defunciones_{fecha}.csv"))

datos = bd %>%
  pivot_longer(cols = 4:ncol(bd)) %>%
  mutate(fecha = as.Date(name, "%d-%m-%Y"),
         cve_estado = str_extract(cve_ent, "\\d\\d")) %>%
  select(-name)

datos_morelos <- datos %>%
  filter(cve_estado == "17") %>%
  group_by(nombre) %>%
  mutate(accum = cumsum(value))


# Grafica ----
datos_morelos %>%
  # filter(nombre == "Cuernavaca") %>%
  ggplot(aes(x = fecha, y = accum, group = nombre)) +
  geom_line() +
  geom_point()










