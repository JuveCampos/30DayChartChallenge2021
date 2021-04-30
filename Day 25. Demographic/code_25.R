###Script #30DayChartChallenge
#Día 25
#Demographic

##Borrar datos del entorno
rm(list=ls())


if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont,
               lubridate, scales, gganimate, gifski, ggtext)

#Directorio de trabajo
##Descargar datos de proyecciones de población de CONAPO
pob<-read.csv("http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/pob_mit_proyecciones.csv",
              encoding="latin1",
              header=TRUE,
              check.names=FALSE)

pob = pob %>%
  #Nombres a minúsculas
  janitor::clean_names()%>%
  select(!renglon)%>%
  #Omitir Nacional
  filter(entidad!="República Mexicana")%>%
  ##Valores negativos para hombres
  mutate(poblacion=ifelse(sexo=="Hombres",
                          poblacion*-1,
                          poblacion))%>%
  #Transofrmar a miles
  mutate(poblacion=poblacion/1000)%>%
  ###Se crean grupos de edad
  mutate(agegroup= case_when (
    edad >=0 & edad <5 ~  "00-04",
    edad >4 & edad <10 ~ "05-09",
    edad >9 & edad <15 ~  "10-14",
    edad >14 & edad <20 ~  "15-19",
    edad >19 & edad <25 ~  "20-24",
    edad >24 & edad <30 ~  "25-29",
    edad >29 & edad <35 ~  "30-34",
    edad >34 & edad <40 ~  "35-39",
    edad >39 & edad <45 ~  "40-44",
    edad >44 & edad <50 ~  "45-49",
    edad >49 & edad <55 ~  "50-54",
    edad >54 & edad <60 ~  "55-59",
    edad >59 & edad <65 ~  "60-64",
    edad >64 & edad <70 ~  "65-69",
    edad >69 & edad <75 ~  "70-74",
    edad >74 & edad <80 ~  "75-79",
    edad >79 & edad <85 ~  "80-84",
    edad >=85 ~ "85+"))

##Se crea el gráfico estático
(p = pob %>%
  # filter(entidad == "Aguascalientes") %>%
  ggplot(aes(
    x = agegroup,
    y = poblacion,
    fill = sexo)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("#3772ff", "#FFB5C2")) +
  ##Tema del gráfico
  theme(
    plot.background=element_rect(fill = "#35312D"),
    axis.title = element_blank(),
    axis.text.y=element_text(size=6, color = "white"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    strip.text.x = element_text(size=10,
                                face="bold",
                                color = "white"),
    strip.background = element_rect(
      fill="#95B2B8",
      color = "white",
      size=1.5, linetype="solid"),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "#35312D"),
    panel.grid.major = element_blank(),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(
      color = "white",
      size = 16,
      face = "bold"),
    plot.title = element_markdown(
      size = 16,
      hjust = 0.5,
      face = "bold"),
    plot.subtitle = element_markdown(
      size = 20,
      hjust = 0.5,
      color = "white",
      face = "bold"),
    legend.title = element_text(face = "bold",
                                size = 16),
    plot.caption = element_text(
      size = 16,
      color = "white",
      hjust = 0),
    text=element_text("Poppins", color = "white")
    )+
  labs(
    title = "Day 25 | Demographics",
    subtitle = "México. Población 1970 - 2050<br>Datos por entidad federativa y grupos de edad<br>{closest_state}",
    y = "Población",
    fill = "Sexo",
    caption = "Inspirado en la publicación de @claudiodanielpc.
    Datos: Información de CONAPO.
    Proyecciones de la Población de México y de las Entidades Federativas.
    @JuvenalCamposF - #30DayChartChallenge - Día 25: Incertidumbre + Demografía")+
  facet_wrap(~entidad, ncol=8, scale="free_x") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                               barwidth = 15,
                               barheight = 0.5))   )

##Salvar el gráfico
# ggsave("day25.png",height = 24,width = 30,
#       units="in",dpi=300)

##Animación
p <- p +
  transition_states(ano,
                    transition_length = 1,
                    state_length = 2) +
  enter_fade() +
  exit_fade() +
  ease_aes("cubic-in-out")


animate(
  p,
  fps = 20,
  duration = 15,
  width = 1075,
  height = 667,
  end_pause = 30,
  renderer = gifski_renderer("day25.gif")
)
