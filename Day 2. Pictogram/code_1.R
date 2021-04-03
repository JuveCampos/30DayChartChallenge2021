# Obtención de los datos
# devtools::install_github("moldach/vapoRwave")
# DOC: https://github.com/moldach/vapoRwave
# Ahi vienen las fuentes que se ocupan

# Librerias
library(waffle)
library(hrbrthemes)
library(extrafont)
library(tidyverse)
library(vapoRwave)
library(ggtext) # Esta creo que no la usé


# DATOS PROVENIENTES DE INEGI - Viviendas
# https://www.inegi.org.mx/contenidos/programas/ccpv/2020/tabulados/cpv2020_b_eum_16_vivienda.xlsx

# Descargamos archivo
curl::curl_download("https://www.inegi.org.mx/contenidos/programas/ccpv/2020/tabulados/cpv2020_b_eum_16_vivienda.xlsx",
                    "vivienda_nac.xlsx")

# Leemos el archivo
viv = readxl::read_xlsx("vivienda_nac.xlsx", sheet = "18", skip = 7) %>%
  filter(!is.na(`...1`)) %>%
  rename(aparatos = `...2`,
         viviendas_disponen = "Disponen...4",
         viviendas_no_disponen = "No disponen...5",
         no_especificado =  "No especificado...6",
         ent = "...1") %>%
  filter(aparatos == "Consola de videojuegos") %>%
  mutate(pctje_edo = 100*viviendas_disponen/(viviendas_disponen + viviendas_no_disponen + no_especificado)) %>%
  select(ent, pctje_edo) %>%
  arrange(-pctje_edo) %>%
  mutate(ent = str_replace(ent, pattern = "\\s", replacement = "@")) %>%
  separate(col = ent, into = c("CVE_EDO", "ENT"), sep = "@")

# Esto creo que no lo use -_-
extrafont::loadfonts(quiet = TRUE)
extrafont::fonttable() %>%
  as_tibble() %>%
  filter(grepl("Awesom", FamilyName)) %>%
  select(afmfile, FullName, FamilyName, FontName)

# Para la Ciudad de México ----
viv_video_edo = viv %>%
  filter(ENT == viv$ENT[1]) %>%
  mutate(no_videojuego = 100 - pctje_edo) %>%
  pivot_longer(cols = c("pctje_edo", "no_videojuego")) %>%
  mutate(value = round(value,0)) %>%
  mutate(name = str_replace_all(name, c("pctje_edo" = "Hogares con videojuegos",
                                        "no_videojuego" = "Sin Videojuegos"
                                        ))) %>%
  mutate(name = factor(name))

# Gráfica
viv_video_edo %>%
  ggplot(aes(label = name,
             values = value,
             color = name)) +
  geom_pictogram(n_rows = 10,
                 make_proportional = TRUE) +
  scale_label_pictogram(
    name = NULL,
    values = c(`Hogares con videojuegos` = "gamepad",
               `Sin Videojuegos` = "home"
               )) +
  scale_color_manual(
    name = NULL,
    values = c(`Hogares con videojuegos` = "#F8B660",
               `Sin Videojuegos` = "#4A354F")) +
  coord_equal() +
  new_retro() +
  theme_enhance_waffle() +
  theme(panel.background = element_rect(fill = "transparent",
                                        color = "yellow"),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(color = "#FF6B58",
                                        linetype = 2,
                                        size = 0.1),
        panel.grid.minor = element_line(color = "#FF6B58",
                                        linetype = 2,
                                        size = 0.1)

        ) +
  theme(legend.key.height = unit(2.25, "line")) +
  theme(legend.text = element_text(size = 10, hjust = 0, vjust = 0.75)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  labs(caption = "Datos del Censo 2020. Viviendas. INEGI, 2020.
       De cada 100 viviendas, el 21% reportó contar con al menos una consola de videojuegos
       siendo la entidad con el porcentaje más alto. En contraste, Chiapas solo reportó un 2%,
       siendo el porcentaje más bajo. @JuvenalCamposF, #30DayChartChallenge",
       title = "Viviendas con consolas de videojuegos",
       subtitle = "Ciudad de Mexico. Mexico. 2020") +
  theme(plot.caption = element_text(hjust = 1,
                                    family = "Windows Command Prompt",
                                    color = "#F8B660"))

# Guardamos la gráfica ----
ggsave("grafica_2_transparente.png",
       device = "png",
       height = 9,
       width = 7,
       bg = "transparent")


