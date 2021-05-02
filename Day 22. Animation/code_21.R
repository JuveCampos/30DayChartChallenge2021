# Librerias ----
library(tidyverse)
library(gganimate)
library(janitor)
library(lubridate)
library(ggExtra)
library(ggimage)

# Bases de datos ----
bd = read_csv("Datos/multiTimeline.csv",
              skip = 1)

# Nombres de las columnas
names(bd) = str_remove(names(bd),
           pattern = "\\: \\(México\\)")

imgs = list.files("Datos/logos/")
unique(bd_long$Clubes) %>% writeLines()

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
                                 "12" = "diciembre"))) %>%
  mutate(sprite = case_when(Clubes == "Club América" ~ "Datos/logos/logo-aguilas.png",
                            Clubes == "Club Deportivo Guadalajara" ~ "Datos/logos/logo-chivas.png",
                            Clubes == "Club Tigres" ~ "Datos/logos/logo-tigres.png",
                            Clubes == "Club Universidad Nacional" ~ "Datos/logos/logo-pumas.png",
                            Clubes == "Club Deportivo Cruz Azul" ~ "Datos/logos/logo-cruzazul.png")) %>%
  mutate(colores = case_when(Clubes == "Club América" ~ "yellow",
                            Clubes == "Club Deportivo Guadalajara" ~ "red",
                            Clubes == "Club Tigres" ~ "gold",
                            Clubes == "Club Universidad Nacional" ~ "navyblue",
                            Clubes == "Club Deportivo Cruz Azul" ~ "blue"))

# Filtracion ----
bd_graf <- bd_long
# %>%
#   filter(Semana %in% seq.Date(from = as.Date("2020-04-26"),
#                             to = as.Date("2020-08-30"),
#                             by = 1))


p = bd_graf %>%
  ggplot(aes(x = indice_google,
             y = reorder(str_wrap(Clubes, 15),
                         indice_google))) +
  geom_col(color = "white",
           fill = bd_graf$colores,
           width = 0.8,
           alpha = 0.7) +
  geom_image(aes(image = sprite)
             ,
             size = 0.11,
             by = "width"
             ) +
  labs(title = "Popularidad de Equipos de Futbol.\nPuntuación de popularidad de Google Trends",
       subtitle = "Semana iniciada el día: {closest_state}",
       caption = "Fuente: Google Trends. Consultado a finales de Abril del 2020.\n@JuvenalCamposF - #30DayChartChallenge - Día 22, TS + Animations",
       y = "", x = "Índice Popularidad de Google Trends") +
  scale_x_continuous(expand = expansion(c(0,0.1), 0.1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(family = "Poppins"),
        axis.title.x = element_text(size = 8))



p <- p +
  transition_states(Semana,
                    transition_length = 1,
                    state_length = 2) +
  enter_fade() +
  exit_fade() +
  ease_aes("cubic-in-out")

animate(
  p,
  fps = 20,
  duration = 15,
  width = 666,
  height = 666,
  end_pause = 30,
  renderer = gifski_renderer("day21.gif")
)


# +
#   # gganimate code to transition by year:
#   transition_time(Semana) +
#   ease_aes('cubic-in-out')
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

# animate(plt, nframes = 750, fps = 25, end_pause = 50, width = 1200, height = 900)

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


