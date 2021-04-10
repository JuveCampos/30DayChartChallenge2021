# Scrap de Econométrica
library(rvest)
library(tidyverse)

# Escrapeamos ----
url = "https://onlinelibrary.wiley.com/toc/14680262/2015/83/1"
root = "https://onlinelibrary.wiley.com"

# Datos de entrada ----
tomos = 1:6
inputs = tibble(anios = 2010:2021,
                numeros = 78:89)

# Funcion de escrapeo a econometrica

escrapeo_econometrica = function(anio, numero, tomo){

  # anio = 2020
  # numero = 88
  # tomo = 4


  tryCatch({

  url_1 = str_glue("https://onlinelibrary.wiley.com/toc/14680262/{anio}/{numero}/{tomo}")
  url_1

  # Sacamos las roots de cada artículo ----
  aa = read_html(url_1) %>%
    html_nodes(".issue-items-container")

  panel_con_original_articles = which(aa %>%
                as.character() %>%
                str_detect("Original Articles"))

  aa = aa[panel_con_original_articles]
  # Enlaces
  enlaces = str_c(root, aa %>%
                    html_nodes(".issue-item") %>%
                    html_nodes(".issue-item__title") %>%
                    html_attr("href"))

  # Titulos ----
  titulos = aa %>%
    html_nodes(".issue-item") %>%
    html_nodes(".issue-item__title") %>%
    html_nodes("h2") %>%
    as.character() %>%
    str_remove_all(pattern = "<h2>|</h2>")


  datos_abstract = lapply(1:length(titulos), function(i){
# i = 1
    url_art = enlaces[i]

    # Abstract ----
    abstract <- read_html(url_art) %>%
      html_nodes(".article__body") %>%
      # html_nodes(".abstract-group") %>%
      html_nodes(".article-section") %>%
      html_nodes(".article-section__content") %>%
      html_text() %>%
      str_remove_all(pattern = "\n|\\s\\s+")

    if(length(abstract) > 1){
      abstract <- abstract[1]
    } else if(length(abstract) == 0){
      abstract <- ""
    } else {
      abstract = abstract
    }

    autores = read_html(url_art) %>%
      html_nodes("#sb-1 > div") %>%
      html_nodes("span") %>%
      html_text() %>%
      str_c(collapse = "; ")

    print(str_c("l_autores = ", print(length(autores)),
                " l_abstract = ", print(length(abstract)),
                " no_paper = ", i,
                "ENLACE = ", url_art))

    datos_abstract = cbind(abstract, autores)

    return(datos_abstract)

  })

  datos = do.call(rbind, datos_abstract) %>%
    as_tibble() %>%
    cbind(titulos) %>%
    cbind(enlaces) %>%
    mutate(year = anio,
           volume = numero,
           issue = tomo) %>%
    as_tibble()

  },

  error = function(e){
    print(str_c("Error en ", "año: ", anio,
                " numero: ", numero,
                " tomo: ", tomo, " ",
                str_glue("https://onlinelibrary.wiley.com/toc/14680262/{anio}/{numero}/{tomo}")))
  }
  )
}

# Hacemos el escrapeo ----
datos_finales = list()
j = 0
for(i in 1:nrow(inputs)){
  anio = inputs$anios[i]
  numero = inputs$numeros[i]
  for(tomo in tomos){
    dato_i = escrapeo_econometrica(anio = anio,
                                   numero = numero,
                                   tomo = tomo)
    j = j + 1
    datos_finales[[j]] = dato_i
    print(str_c("Año: ", anio,
                "Numero: ", numero,
                "Tomo: ", tomo))
  }
}

# Guardamos un respaldo ----
saveRDS(datos_finales,
        "abstracts.rds")

datos_finales = readRDS("abstracts.rds")

ncols = lapply(datos_finales, ncol) %>%
  unlist() %>%
  cbind(1:63)

lapply(datos_finales, class)

# Correcciones ----
datos_finales[[64]] <- escrapeo_econometrica(2020, 88, 4)
datos_finales[[66]] <- escrapeo_econometrica(2020, 88, 6)

# Datos finales finales
datos_finales_2 <- do.call(rbind, datos_finales[1:67])
saveRDS(datos_finales_2, "datos_econometrica.rds")
# Releemos los datos ----
datos_finales_2 = readRDS("datos_econometrica.rds")


# Función del wordcloud ----
create_wordcloud <- function(data, stop_words = c(), num_words = 100, background = "white",  mask = NULL, size = 2) {
  # Checar si esta instalado Pacman
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(wordcloud2, tm, stringr)

  # Pre-Función para eliminar simbolos raros
  quitar_signos <- function(x)  stringr::str_remove_all(x, pattern = rebus::char_class("¿¡"))

  # If text is provided, convert it to a dataframe of word frequencies
  # Si se provee el texto, convertirlo a un dataframe de frecuencia de palabras
  if (is.character(data)) {
    # Convertimos a Corpus
    corpus <- Corpus(VectorSource(data))
    # Convertimos el texto dentro del Corpus a Minusculas
    corpus <- tm_map(corpus, tolower)
    # Removemos la puntuacion (.,-!?)
    corpus <- tm_map(corpus, removePunctuation)
    # Removemos los numeros
    corpus <- tm_map(corpus, removeNumbers)
    # Removemos los signos de admiracion e interrogacion al reves
    corpus <- tm_map(corpus, quitar_signos)
    # Removemos las stopwords (palabras muy muy comunes que se usan para dar coherencia
    # a las oraciones. Para saber cuales, escribir: stopwords("spanish))
    corpus <- tm_map(corpus, removeWords, c(stopwords("english"), stop_words))
    # Generamos una matriz para hacer el conteo
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    # Obtenemos el numero de la frecuencia de cada palabra
    data <- sort(rowSums(tdm), decreasing = TRUE)
    # Generamos una tabla con la palabra en una columna y su frecuencia de uso en otra
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }


  freq_palabras <<- data

  # Make sure a proper num_words is provided
  # Nos aseguramos que un numero adecuado de palabras `num_provider` es generado`
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }

  # Grab the top n most common words
  # Recortamos la base de datos de palabras a un numero `n` especificado
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  wordcloud2(data, backgroundColor = background, color = "random-dark", fontFamily = "Times New Roman", size = size)
}

# Creamos el wordcloud ----

create_wordcloud(datos_finales_2$abstract,
                 num_words = 300,
                 size = 0.5,
                 stop_words = c("can", "also"))

create_wordcloud(datos_finales_2$titulos,
                 num_words = 300,
                 size = 0.5,
                 stop_words = c("can", "also"))

# Longitud de cada abstract ----
datos_finales_2 %>%
  mutate(lenth = str_length(abstract)) %>%
  ggplot(aes(x = lenth)) +
  geom_density(fill = "#87d68d", color = "#bcebcb") +
  scale_x_continuous(label = scales::dollar_format(prefix = "")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x =  element_text(angle = 90, size = 15),
        axis.text.y = element_blank(),
        panel.grid = element_blank()) +
  labs(y = "", x = "Medida de longitud (caracteres usados)")

ggsave("grafica_dist_abstract.png",
       device = "png",
       height = 2,
       bg = "transparent",
       width = 6)

datos_finales_2 %>%
  mutate(lenth = str_length(titulos)) %>%
  ggplot(aes(x = lenth)) +
  geom_density(fill = "#e2c391", color = "#f9db6d") +
  scale_x_continuous(label = scales::dollar_format(prefix = "")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x =  element_text(angle = 90, size = 15),
        axis.text.y = element_blank(),
        panel.grid = element_blank()) +
  labs(y = "", x = "Medida de longitud (caracteres usados)")

ggsave("grafica_dist_titulos.png",
       device = "png",
       height = 2,
       bg = "transparent",
       width = 6)

corto_largo = datos_finales_2 %>%
  mutate(lenth = str_length(titulos)) %>%
  filter(lenth == max(lenth) | lenth == min(lenth))

corto_largo$titulos
corto_largo$autores

# Términos económicos ----

terminos = freq_palabras %>%
  as_tibble() %>%
  mutate(word = str_remove(str_remove(word, pattern = "s$|‐‐|^‐|^–|^—"), "^\\s")) %>%
  group_by(word) %>%
  summarise(freq = sum(freq)) %>%
  arrange(-freq)

openxlsx::write.xlsx(terminos,
           "freq_palabras.xlsx")

# Manejo manual de los datos
# COnsistió en marcar con un uno los términos económicos
terminos = readxl::read_xlsx("freq_palabras.xlsx")

plt = terminos %>% filter(term == 1) %>%
  head(15) %>%
  ggplot(aes(x = word, y = freq)) +
  geom_col() +
  geom_text(aes(label= freq), angle = 90, hjust = -0.2, family = "Times New Roman") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
    axis.text.x =  element_text(angle = 90, size = 15),
        panel.grid = element_blank()) +

  labs(y = "", x = "") +
  ylim(c(0, 900))

ggsave("grafica_terminos.png",
       device = "png",
       height = 4,
       bg = "transparent",
       width = 12)

