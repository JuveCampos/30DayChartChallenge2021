library(rvest)
library(tidyverse)
library(rebus)

url = "https://pokemonprices.com/browse_sets"
code <- read_html(url)

enlaces = tibble(set = code %>%
         html_nodes("a") %>%
         html_attr("href")) %>%
  filter(str_detect(set, pattern = "^\\/\\w")) %>%
  mutate(enlaces = str_c("https://pokemonprices.com", set)) %>%
  filter(!(set %in% c("/browse_sets",
                      "/browse_cards",
                      "/watchlist",
                      "/contact",
                      "/set/Guardians+Rising",
                      "/set/Colosseum",
                      "/set/Evolution",
                      "/set/Laboratory")))

# Funcion ----
juntar_datos_pokemon <- function(url){
  # url = "https://pokemonprices.com/set/Boundaries+Crossed"
  print(url)

  code_1 <- read_html(url)
  # Tablas de datos
  tabla = code_1 %>%
    html_table(header = NA)
  # Generamos un indice para quedarnos con la tabla del mayor número de renglones
  index = which(lapply(tabla, ncol) %>%
                  unlist() == max(lapply(tabla, ncol) %>%
                                    unlist()))
  tabla = tabla %>%
    pluck(index)
  # Imagenes miniatura
  imgs = code_1 %>%
    html_nodes(".card_thumb") %>%
    html_attr("src")
  tabla$img = imgs

  return(tabla)
}

# Probamos la función
aaa = juntar_datos_pokemon(url = enlaces$enlaces[1])

# Hacemos el lapply ----
datos = lapply(enlaces$enlaces, juntar_datos_pokemon)
enlaces$set

# Le pegamos el nombre del set
for(i in 1:116){
  print(i)
  datos[[i]]$set = enlaces$set[i]
}


# Numero de columnas
lapply(datos, ncol)

# nos quedamos con los datos de 9 columnas
datos_2 = datos[2:116]
datos_2.df = do.call(rbind, datos_2)

# # Esto no es necesario; estaríamos contando doble
# datos_top100 = datos[1] %>%
#   pluck(1) %>%
#   mutate(rarity = "top-100 most expensive") %>%
#   rename(pokemon = X2,
#          Average.price = X3) %>%
#   mutate(num.price = as.numeric(str_remove(Average.price, pattern = "\\$"))) %>%
#   select(pokemon,
#          Average.price,
#          rarity,
#          set,
#          img,
#          num.price)

datos_grafica = datos_2.df %>%
  rename(Average.price = X7,
         pokemon = X2,
         rarity = X4) %>%
  select(pokemon, Average.price, rarity, set, img) %>%
  mutate(num.price = as.numeric(str_remove(Average.price, pattern = "\\$"))) %>%
  filter(!is.na(num.price))

# Generamos gráfica ----
cuantiles = quantile(datos_grafica$num.price, seq(0, 1, by = 0.01))
class(cuantiles)

cuantiles_tibble = tibble(cuantil = cuantiles,
       no_cuantil = 0:100)

cuantiles_tibble %>%
  ggplot(aes(x = no_cuantil, y =cuantil )) +
  geom_point()
