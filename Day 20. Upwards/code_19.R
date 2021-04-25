options(scipen = 999)

# Librerias ----
library(tidyverse)
library(ggshadow)
library(wesanderson)
library(ggtext)
library(ggrepel)

# Datos ----
casos_sida = tibble::tribble(
  ~anio, ~casos_notificados, ~casos_diagnosticados,
  "1983",                  6,                    67,
  "1984",                  6,                   195,
  "1985",                 27,                   369,
  "1986",                242,                   714,
  "1987",                509,                  1609,
  "1988",                897,                  2228,
  "1989",               1584,                  2930,
  "1990",               2564,                  3799,
  "1991",               3114,                  4047,
  "1992",               3157,                  4567,
  "1993",               4980,                  4794,
  "1994",               3991,                  5343,
  "1995",               4083,                  6061,
  "1996",               4112,                  6221,
  "1997",               3579,                  6558,
  "1998",               4644,                  7898,
  "1999",               4258,                  9450,
  "2000",               4665,                  9719,
  "2001",               5621,                 10321,
  "2002",              13374,                 10272,
  "2003",               7017,                 10137,
  "2004",              26030,                 10665,
  "2005",              11531,                 11011,
  "2006",              15093,                 11765,
  "2007",              10343,                 10802,
  "2008",              15603,                 10737,
  "2009",              12260,                 10676,
  "2010",              12011,                 11340,
  "2011",              13527,                 11089,
  "2012",              12455,                 12162,
  "2013",              11911,                 11396,
  "2014",              10646,                 11014,
  "2015",              12283,                 13072,
  "2016",              15994,                 13968,
  "2017",              15097,                 14993,
  "2018",              19814,                 17371,
  "2019",              19009,                 17182,
  "2020",               9140,                  8635,
  "TOTAL",             315177,                315177
)


casos_sida = casos_sida %>%
  pivot_longer(cols = 2:3) %>%
  arrange(name) %>%
  mutate(name = str_replace_all(name, c("casos_diagnosticados" = "Casos Diagnosticados",
                                        "casos_notificados" = "Casos Notificados")))

etiquetas = casos_sida %>%
  group_by(name) %>%
  filter(anio != "TOTAL") %>%
  filter(value == min(value) |
           value == max(value) |
           anio == max(anio) |
           (anio %in% 2002 & name == "Casos Notificados") |
           (anio %in% (2018) & name == "Casos Notificados")) %>%
  ungroup()

# Corrección
etiquetas <- etiquetas[-5,]



casos_sida %>%
  filter(anio != "TOTAL") %>%
  mutate(anio = as.numeric(anio)) %>%
  ggplot(aes(x = anio, y = value, color = name)) +
  geom_line(size = 1.5) +
  geom_point(pch = 21, fill = "white", size = 3) +
  geom_label_repel(data = etiquetas,
            aes(label = prettyNum(value, big.mark = ","),
                x = as.numeric(anio), y = value, color = name),
            family = "Arial", size = 4, show.legend  = F) +
  # geom_text(aes(label = str_c("Casos diagnosticados: ", prettyNum(pull(casos_sida[nrow(casos_sida)-1,3]), big.mark = ",")),
  #               x = 1990,
  #               y = pull(casos_sida[nrow(casos_sida)-1,3]) + 500),
  #           color = "red") +
  # geom_hline(yintercept = pull(casos_sida[nrow(casos_sida)-1,3]),
  #            linetype = 2, color = "red") +
  labs(title = "Casos de SIDA a nivel nacional, México",
       caption = "Casos diagnosticados y notificados por año, México<br>
       <b>Fuente:</b> Sistema de vigilancia epidemiológica de VIH<br>
       al cuarto trimestre del 2020.<br>
       @JuvenalCamposF - #30DayChartChallenge - Day19: TimeSeries + Upwards",
       subtitle = "AIDS Cases diagnosed and reported by year, Mexico",
       x = "Año", y = "", color = "Casos") +
  scale_y_continuous(labels = scales::comma_format(suffix = "\ncasos"),
                     limits = c(0, 30000)) +
  scale_x_continuous(breaks = seq(1970,2020, by = 5)) +
  theme(plot.title = element_text(family = "Times New Roman", size = 15, face = "bold"),
        plot.subtitle = element_text(family = "Arial"),
        plot.caption = element_markdown(family = "Arial"),
        legend.title = element_text(family = "Arial", face = "bold"),
        legend.position = "top",
        axis.text = element_text(family = "Helvetica Neue", size = 10, color = "gray50"),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        axis.ticks = element_line(color = "gray70", size = 1.5),
        panel.grid.major.y = element_line(linetype = 2, color = "gray70"),
        axis.line.x = element_line(linetype = 1, color = "gray70", size = 1.1)) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  scale_color_manual(values = c(wes_palettes$BottleRocket1))

ggsave("plot_19.png",
       device = "png",
       width = 8,
       height = 5)


