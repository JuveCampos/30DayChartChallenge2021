# Librerias
library(jsonlite)
library(httr)
library(png)
library(magick)
library(tidyverse)

# Llamada a la API
no = 2

gen_data_pokemon <- function(no){

call1 <- paste0("https://pokeapi.co/api/v2/",
                "pokemon/", no, "/")

# Realizamos la llamada (Obtenemos la respuesta)
llamada <- GET(call1)
llamada # Status 200: Todo ok. 
class(llamada) # Tipo Response

# De la llamada, obtenemos la respuesta en formato JSON
# Formato JSON: https://en.wikipedia.org/wiki/JSON
get_data <- content(llamada, "text")
get_data
class(get_data)

# Del JSON, convertimos este texto en un objeto lista
get_data_JSON <- fromJSON(get_data, flatten = TRUE)
class(get_data_JSON)

# Renombramos el objeto, por comodidad
a <- get_data_JSON
a

tabla = a$stats %>% 
  as_tibble() %>% 
  filter(stat.name %in% c("attack", "defense")) %>% 
  select(base_stat, stat.name) %>% 
  mutate(no = no) %>% 
  pivot_wider(id_cols = "no", 
              names_from = stat.name, 
              values_from = base_stat) %>% 
  mutate(sprite = a$sprites$front_default,
         name = a$name,
         main.type = a$types$type.name[1])

return(tabla)
}

# Obtenemos los datos de los 890 pokemones registrados ----
datos = lapply(1:890, gen_data_pokemon)

datos_tibble = do.call(rbind, datos)

datos_tibble %>% 
  openxlsx::write.xlsx("datos_físico_pokemon-.xlsx")

# Grafica ----
datos_tibble %>% 
  ggplot(aes(x = attack, y = defense)) + 
  geom_point(alpha = 0.1)







