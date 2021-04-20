options(scipen = 999)

library(rvest)
library(tidyverse)
library(wesanderson)
library(rpart)
library(rpart.plot)

# Urls de donde podríamos sacar los datos ----
# https://autos.mercadolibre.com.mx/_Desde_49
# https://autos.mercadolibre.com.mx/_Desde_97
# https://autos.mercadolibre.com.mx/_Desde_145

# Extraemos las fichas técnicas ----
fichas_tecnicas = function(url_mini){
  tryCatch({pag_mini = url_mini %>%
    read_html() %>%
    html_table() %>%
    pluck(1)
  tf = as.tibble(t(pag_mini[,2]))
  names(tf) = pull(pag_mini[,1])
  # print(url_mini)
  },
  error = function(e){
    print(str_c("Error! no se pudo sacar la info de: ", url_mini))
  }
  )
  return(tf)
}

desdes = 1 + 48*0:15
# Construimos las urls ----
urls = str_c("https://autos.mercadolibre.com.mx/_Desde_",
             desdes)

# url = "https://auto.mercadolibre.com.mx/MLM-870367162-bmw-x3-2012-35ia-blindada-nivel-3-plus-blindaje-blindados-_JM#position=12&type=item&tracking_id=e85dbd72-fc3a-4397-8bfc-106997890838"

get_data = function(url){

tryCatch({
  precios = read_html(url) %>%
    html_nodes(".andes-card") %>%
    html_nodes(".ui-search-result__content-wrapper") %>%
    html_nodes(".ui-search-price") %>%
    html_text() %>%
    str_remove_all(pattern = "\\$|\\,") %>%
    as.numeric()

  atributos <- tibble(attr = read_html(url) %>%
                        html_nodes(".andes-card") %>%
                        html_nodes(".ui-search-result__content-wrapper") %>%
                        html_nodes(".ui-search-card-attributes") %>%
                        html_text()) %>%
    mutate(anio = as.numeric(str_extract(attr, pattern = "^\\d\\d\\d\\d")),
           kilometros = as.numeric(str_remove_all(attr, pattern = "^\\d\\d\\d\\d|\\,|\\sKm")),
           antiguedad = 2021 - anio)


  carro = tibble(carro = read_html(url) %>%
                   html_nodes(".andes-card") %>%
                   html_nodes(".ui-search-result__content-wrapper") %>%
                   html_nodes("h2") %>%
                   html_text()) %>%
    mutate(marca = str_extract(carro, pattern = "\\w+"))

  dirs <- read_html(url) %>%
    html_nodes(".andes-card") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    unique()

  datos = cbind(carro, atributos, precios, dirs) %>%
    as_tibble() %>%
    select(carro, marca, anio, kilometros, antiguedad, precios, dirs)

  # Datos ficha tecnica
  ft_data = do.call(plyr::rbind.fill, lapply(datos$dirs, fichas_tecnicas)) %>%
    as.tibble()

  # Juntamos con los datos anteriores
  datos_2 = cbind(datos, ft_data) %>%
    as_tibble()

  print(str_c("Listo la url ", url))

  return(datos_2)
}, error = function(e){
  print(str_c("Error en la pagina: ", url))
})

}

# Juntamos los datos ----
Datos_grafica = lapply(urls, get_data)
Datos_grafica[[12]] = get_data(urls[12])
bd = do.call(plyr::rbind.fill, Datos_grafica) %>%
  as_tibble() %>%
  mutate(tipo_marca = case_when(marca %in% c("Mercedes",
                                             "Bmw",
                                             "Buick",
                                             "Audi",
                                             "Volvo",
                                             "Lincoln",
                                             "Cadillac",
                                             "Hummer",
                                             "Porsche",
                                             "Infiniti",
                                             "Jaguar") ~ "Lujo",
                                TRUE ~ "General")) %>%
  mutate(Puertas = as.numeric(Puertas),
         Motor = as.numeric(Motor)) %>%
  mutate(Transmisión = str_replace_all(Transmisión, pattern = "Automática secuencial", replacement = "Automática"))

glimpse(bd)

formula = precios ~ antiguedad + kilometros + tipo_marca  +  Motor # + `Tipo de carrocería` + Puertas + Transmisión
modelo = lm(bd, formula = formula)

modelo_2 = rpart(data = bd, formula = formula)


plt = rpart.plot(modelo_2,
                 type = 0,
           leaf.round = 0,
           box.palette = "BuGn",
           shadow = "gray",
           fallen.leaves = FALSE,
           branch = 0.3,
           digits = 2,
           under = TRUE,
           cex = 0.7,
           under.col = 'grey40',
           family = 'Poppins',
           main = 'Arbol de Decisión:\nValor de vehículo predicho dadas ciertas variables de interés.')

# plt$labs = str_replace_all(plt$labs, c("e\\+3" = ",000",
#                             "1.3e\\+6" = "1'300,000",
#                             "1e\\+6" = "1'000,000"
#                             ))

# Nunca encontré como modificar los numeros, asi que lo edité en Keynote -__-

