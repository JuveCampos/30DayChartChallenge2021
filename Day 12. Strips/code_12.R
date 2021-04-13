##################################################
# EXTRACCIÓN DE TWEETS DE GOBERNADORES ESTATALES #
##################################################
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# Librerias ----
library(pacman)
p_load(rtweet, rebus, tidyverse, plyr)
library(emojifont)
library(scales)
library(extrafont)

## Base de datos
bd <- readxl::read_xlsx("TwGobernadores_1.xlsx")

# Generamos la query (busqueda) ----

# Para sacar los tweets de los ultimos 5 dias
query <- paste0(
  # "(",
                "from:",
       str_remove(bd$`Cuenta de Twitter`, "@")
       # ,
       # ") until:",
       # as.character(Sys.Date()+1),
       # " since:",
       # as.character(Sys.Date()-5)
       )

# Para sacar los tweets
query <- paste0("(from:",
                str_remove(bd$`Cuenta de Twitter`, "@"),
                ")")

# Obtencion de Tweets de gobernadores ----
twTodosGobernadores <- lapply(c(1:32), function(x){
  print(x)
  tryCatch({
    search_tweets(query[x],
                  n = 10000,
                  include_rts = FALSE) %>%
      select(screen_name, created_at, text, status_url)
  }, error = function(e){
    print(paste0("No se pudo con ", query[x], ""))
  })
})


# Analizamos Datos ----
lapply(twTodosGobernadores, nrow)


# Quitamos los vacios
twTodosGobernadores2 <- twTodosGobernadores[-c(which(unlist(lapply(twTodosGobernadores, length)) != 4))]

# Base global de Tweets ----
tweets <- twTodosGobernadores2 %>%
  plyr::ldply() %>%
  as_tibble() %>%
  mutate(text = tolower(text)) %>%
  mutate(economia = str_detect(text, pattern = or1(c("econom", "ingreso"))))

bd <- bd %>%
  mutate(`Cuenta de Twitter` = str_remove(`Cuenta de Twitter`,
                                          pattern = "@"))

names(bd)[5] <- "screen_name"

# Base de datos final ----
tweets <- bd %>%
  right_join(tweets)


min(tweets$created_at)
max(tweets$created_at)

plt = tweets %>%
  mutate(label = fontawesome(c('fa-twitter'))) %>%
  ggplot() +
  aes(x = str_c("@", screen_name, "\n (", Abreviatura, ")"),
      y = created_at,
      color = Partido) +
  # geom_point() +
  geom_text(aes(label = label), family='fontawesome-webfont', size=6) +
  labs(title = "Tweets por cada gobernador en los últimos 7 días, México",
       subtitle = "(Tweets by mexican governor in the past 7 days (Apr-13, 2021))",
       x = "", y = "",
       caption = "Fecha de referencia: 13 de Abril, 2020.
       Fuente: API de Twitter, accesada mediante la librería {rtweet} de R.
       No se incluyó información de los gobernadores de Nayarit o Baja California, al no encontrarse sus usuarios de Twitter.
       Asimismo, el gobernador de Tabasco no ha usado su cuenta durante los últimos días.
       @JuvenalCamposF - #30DayChartChallenge - Día 12; Strips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   family = fuente),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5,
                                  family = fuente,
                                  face =  "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     color = "gray50",
                                     family = fuente),
        plot.caption = element_text(hjust = 1,
                                    family = fuente)) +
  guides(color = guide_legend(title.position = "top",
                             title.hjust = 0.5,
                             nrow = 1)) +
  scale_color_manual(values = c("pink",
                               "orange",
                               "brown",
                               "blue",
                               "purple",
                               "yellow",
                               "red",
                               "green"))

plt

ggsave("grafica_tiras.png",
       device = "png",
       height = 8,
       width = 10)

