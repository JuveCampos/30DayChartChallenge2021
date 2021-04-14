options(scipen = 999)

library(rvest)
library(tidyverse)
library(wesanderson)

# Urls de donde podríamos sacar los datos ----
# https://autos.mercadolibre.com.mx/_Desde_49
# https://autos.mercadolibre.com.mx/_Desde_97
# https://autos.mercadolibre.com.mx/_Desde_145

desdes = 1 + 48*0:20
# Construimos las urls ----
urls = str_c("https://autos.mercadolibre.com.mx/_Desde_",
             desdes)

url = urls[1]

get_data = function(url){
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

    print(str_c("Listo la url ", url))

    return(datos)
}

# Probamos la función ----
get_data(url = url)

# Juntamos los datos ----
Datos_grafica = lapply(urls, get_data)

# Convertimos a df
Datos_grafica_2 = do.call(rbind, Datos_grafica)

# Marcas de coche
unique(Datos_grafica_2$marca) %>% sort()
table(Datos_grafica_2$marca)

# Marcas mas populares ----
mmp = Datos_grafica_2 %>%
  group_by(marca) %>%
  count() %>%
  arrange(-n) %>%
  head(n = 6) %>%
  pull(marca)

# regresion simple ----
modelo = lm(data = Datos_grafica_2,
   formula = precios ~ antiguedad + kilometros + marca)

summary(modelo)

Datos_grafica_2 %>%
  filter(marca == "Toyota") %>%
  ggplot(aes(x = antiguedad, y = precios)) +
  geom_point() +
  geom_smooth(method = "lm")

vochos = Datos_grafica_2 %>%
  filter(marca == "Volkswagen")

vochos %>%
  ggplot(aes(x = kilometros, y = precios)) +
  geom_point() +
  geom_smooth(method = "lm")


# La chida ----
cor()

datos = Datos_grafica_2 %>%
  filter(marca %in% c("Audi", "Nissan", "Toyota", "Volkswagen")) %>%
  filter(precios < 1e6)

corrs = datos %>%
  group_by(marca) %>%
  summarise(corr = str_c("Correlación: \n",
                         round(cor(precios, kilometros), 3)))

datos %>%
  ggplot() +
  geom_point(aes(x = kilometros, y = precios, color = marca)) +
  geom_smooth(aes(x = kilometros, y = precios, color = marca), method = "lm") +
  geom_text(data = corrs,
            aes(x = 150000, y = 900000, label = corr)) +
  facet_wrap(~marca) +
  scale_y_continuous(labels = scales::dollar_format(),
                     limits = c(0,1e6)) +
  scale_x_continuous(labels = scales::dollar_format(prefix = "")) +
  labs(title = "Correlación entre precios y kilometraje\nde 4 marcas de autos en venta en MX",
       subtitle = "Correlation between prices and mileage of 4 brands of cars for sale in MX",
    caption = "Fuente: Datos provenientes de mercadoLibre.com México
    @JuvenalCamposF - #30DayMapChallenge - Relationships; Correlations",
    x = "Kilómetros", y = "Precios") +
  theme_minimal() +
  scale_color_manual(values = wes_palettes$GrandBudapest1
                     ) +
  theme(legend.position = "none",
        strip.text.x = element_text(
          size = 12, face = "bold"
        ),
        strip.background = element_rect(color="black",
                                        fill="#fff59e",
                                        size=1.5,
                                        linetype="solid"),
        text = element_text(family = "Poppins"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))


ggsave("coches.png",
       device = "png",
       width = 11,
       height = 8)




