library(tidyverse)
library(lubridate)
library(ggpomological)
# Importar fuente Homemade Apple en https://fonts.google.com/specimen/Homemade+Apple
extrafont::font_import()


# Descarga n = 1
bd <- read_table2("https://smn.conagua.gob.mx/tools/RESOURCES/Diarios/9048.txt",
                  col_names = FALSE, col_types = cols(X6 = col_character()),
                  skip = 19) %>%
  rename(Fecha = X1,
         PRECIP = X2,
         EVAP = X3,
         TMIN = X4,
         TMAX = X5) %>%
  select(-X6)

# Exploramos los valores
unique(bd$EVAP)
unique(bd$TMAX)
unique(bd$TMIN)
unique(bd$PRECIP)

# Transformamos a numeros ----
bd[,2:5] = lapply(bd[,2:5], as.numeric)

# Convertimos a fechas la primera columna ----
bd$Fecha = as.Date(bd$Fecha, format = "%d/%m/%Y")

# Checamos los datos
min(bd$Fecha, na.rm = T)
max(bd$Fecha, na.rm = T)

# Gráfica de disponibilidad de informacion
bd %>%
  select(Fecha) %>%
  mutate(punto = 1) %>%
  ggplot(aes(x = Fecha, y = punto)) +
  geom_point()

plt = bd %>%
  mutate(anio = year(Fecha)) %>%
  group_by(anio) %>%
  count() %>%
  ggplot(aes(x = anio, y = n)) +
  geom_col()

plotly::ggplotly(plt)
# Falto de registro: @1913 (350 dias) y 2005 (340 dias)

# Hacemos una gráfica de datos diarios ----
bd %>%
  ggplot(aes(x = Fecha, y = PRECIP)) +
  geom_col()

# Hacemos una de datos mensuales, por año ----
bd %>%
  mutate(anio = year(Fecha),
         mes = month(Fecha)) %>%
  group_by(anio, mes) %>%
  summarise(Lluvia_total = sum(PRECIP)) %>%
  mutate(anio.mes = str_c(anio, "-", mes)) %>%
  ggplot(aes(x = anio.mes, y = Lluvia_total)) +
  geom_col()

# Hacemos una de datos anuales ----
bd %>%
  mutate(anio = year(Fecha)) %>%
  group_by(anio) %>%
  summarise(Lluvia_total = sum(PRECIP)) %>%
  ggplot(aes(x = anio, y = Lluvia_total)) +
  geom_col()

# Dias con lluvias más extremas ----
bd %>%
  arrange(-PRECIP) %>%
  head(n = 10)

# Mapa de calor ----
# meses = c(1:12) %>% as.character()
meses = c("Enero",
                 "Febrero" ,
                 "Marzo",
                 "Abril",
                 "Mayo",
                 "Junio",
                 "Julio",
                 "Agosto",
                 "Septiembre",
                 "Octubre",
                 "Noviembre",
                 "Diciembre")

hm = bd %>%
  mutate(anio = year(Fecha),
         mes = month(Fecha),
         dia = day(Fecha)) %>%
  mutate(dia.mes = str_c(dia, "-", mes)) %>%
  group_by(anio, mes) %>%
  summarise(Lluvia_total = sum(PRECIP)) %>%
  filter(!is.na(mes)) %>%
  mutate(mes = str_replace_all(mes, c("^1$" = "Enero",
                                      "^2$" = "Febrero" ,
                                      "^3$" = "Marzo",
                                      "^4$" = "Abril",
                                      "^5$" = "Mayo",
                                      "^6$" = "Junio",
                                      "^7$" = "Julio",
                                      "^8$" = "Agosto",
                                      "^9$" = "Septiembre",
                                      "^10$" = "Octubre",
                                      "^11$" = "Noviembre",
                                      "^12$" = "Diciembre"))) %>%
  mutate(mes = factor(mes, levels = c("Enero",
                                         "Febrero" ,
                                         "Marzo",
                                         "Abril",
                                         "Mayo",
                                         "Junio",
                                         "Julio",
                                         "Agosto",
                                         "Septiembre",
                                         "Octubre",
                                         "Noviembre",
                                         "Diciembre"))) %>%
  ggplot(aes(x = mes, y = anio, fill = Lluvia_total)) +
  geom_tile() +
  labs(x = "", y = "Año",
         caption = "Milímetros de Precipitación registrados por la estación 9048 (Tacubaya Observatorio)
       Desde el 6 de marzo de 1877 al 31 de mayo del 2018.
       Fuente: CONAGUA, SMN. Registros diarios de variables meteorológicas.",
         title = "Luvias por mes desde 1877, Ciudad de México.",
       fill = "Lluvia por mes\n(mm)",
         subtitle = "Estación 9048 - Tacubaya (OBS)") +
  scale_y_continuous(breaks = seq(1880, 2010, by = 10),
                     labels = seq(1880, 2010, by = 10)) +
  theme_pomological_fancy() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.ticks.y = element_line(color = "black"))

hm

ggsave("lluvia_tacubaya.png",
       device = "png")

