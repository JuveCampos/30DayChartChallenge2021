# Librerias ----
library(tidyverse)
library(gganimate)
library(janitor)
library(lubridate)

# Bases de datos ----
bd = read_csv("Datos/multiTimeline.csv",
              skip = 1)

# Nombres de las columnas
names(bd) = str_remove(names(bd),
           pattern = "\\: \\(México\\)")

bd_long = bd %>%
  pivot_longer(cols = 2:6,
               names_to = "Clubes",
               values_to = "indice_google") %>%
  mutate(dia = day(Semana),
         mes = month(Semana),
         anio = year(Semana)) %>%
  mutate(mes = str_replace_all(as.character(mes),
                               c("1" = "enero",
                                 "2" = "febrero",
                                 "3" = "marzo",
                                 "4" = "abril",
                                 "5" = "mayo",
                                 "6" = "junio",
                                 "7" = "julio",
                                 "8" = "agosto",
                                 "9" = "septiembre",
                                 "10" = "octubre",
                                 "11" = "noviembre",
                                 "12" = "diciembre")))



bd_graf <- bd_long
# %>%
#   filter(Semana == as.Date("2020-04-26"))

plt = bd_graf %>%
  ggplot(aes(x = indice_google,
             y = reorder(Clubes, indice_google))) +
  geom_tile() +
  # gganimate code to transition by year:
  transition_time(Semana) +
  ease_aes('cubic-in-out')
  # geom_text(aes(label = str_c("Popularidad: ", indice_google)),
  #           vjust = 0.5,
  #           hjust = -0.1
  #           ) +
  # scale_x_continuous(breaks = 1:100,
  #                    expand = expansion(c(0,0.5), 0))
# +
#   labs(x = "", y = "",
#        title = "Título",
#        subtitle = str_c("Semana que inició el día ", bd_graf$dia[1], " de ",bd_graf$mes[1], " del ",bd_graf$anio[1]),
#        caption = "Fuente: Datos de Google Trends consultados el 22 de Abril del 2020.")

animate(plt, nframes = 750, fps = 25, end_pause = 50, width = 1200, height = 900)

# plt %>%
#   transition_states(
#     states = Semana,
#     transition_length = 1,
#     state_length = 2
#   ) +
#   enter_fade() +
#   exit_fade() +
#   ease_aes('cubic-in-out')
#


