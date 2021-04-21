# Librerías ----
library(tidyverse)
library(ggrepel)

# Datos ----
bd = read_csv("gender-ratios-for-mean-years-of-schooling.csv") %>%
  janitor::clean_names()

bd$entity = factor(bd$entity,
                   levels = unique(bd$entity)[c(1,3,4,5,2,6)])

labelas = bd %>%
  group_by(entity) %>%
  summarise(y = max(regional_female_to_male_years_schooling_lee_lee_2016)) %>%
  mutate(x = 2045)

# Gráfica ----
bd %>%
  ggplot(aes(x = year,
             y = regional_female_to_male_years_schooling_lee_lee_2016,
             color = entity)
         ) +
  geom_line() +
  geom_point() +
  geom_text_repel(data = labelas,
                  aes(label = entity,
                      x = x,
                      y = y),
                  direction = "y",
                  size = 3,
                  hjust = -1,
                  fontface = "bold",
                  segment.color = 'transparent'
            ) +
  labs(x = "", y = "",
       title = "Gender ratios for mean years of schooling, 1870 to 2010",
subtitle = "Female-to-male ratio of average years of schooling, expressed in percents. All education levels\nfor population aged 15-64. Regional estimates are population-weighted averages.",
caption = "Source: Lee and Lee (2016)\nOurWorldInData.org/global-rise-of-education * CC BY\n#30DayChartChallenge - Time Series + Global Change") +
  scale_x_continuous(breaks = c(1870, 1900, 1920, 1940, 1960, 1980, 2010)) +
  scale_y_continuous(breaks = c(0,20,40,60,80,100,120),
                     labels = scales::comma_format(suffix = "%")) +
  theme_minimal() +
  scale_color_manual(values = c(rgb(64, 76, 100, maxColorValue = 255),
                                rgb(58, 130, 125, maxColorValue = 255),
                                rgb(102, 64, 140, maxColorValue = 255),
                                rgb(191, 42, 102, maxColorValue = 255),
                                rgb(163, 63, 29, maxColorValue = 255),
                                rgb(127, 54, 59, maxColorValue = 255))) +
  theme(plot.title = element_text(family = "Times New Roman", size = 15),
        plot.subtitle = element_text(family = "Arial"),
        plot.caption = element_text(family = "Arial"),
        legend.position = "none",
        axis.text = element_text(family = "Helvetica Neue", size = 13, color = "gray50"),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        axis.ticks = element_line(color = "gray70", size = 1.5),
        panel.grid.major.y = element_line(linetype = 2, color = "gray70"),
        axis.line.x = element_line(linetype = 1, color = "gray70", size = 1.1))










