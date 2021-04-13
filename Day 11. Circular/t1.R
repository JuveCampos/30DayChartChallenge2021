
library(imager)
library(tidyverse)
# library(ggvoronoi)
library(kableExtra)
library(here)
library(extrafont)
library(ggtext)
library(magick)

# font: https://www.theleagueofmoveabletype.com/goudy-bookletter-1911
# (requires installing and importing through the extrafont::import_font() function)

img <- load.image("/Users/juvenalcampos/Desktop/acnh.png")

img_df <- as.data.frame(img)

img_df %>%
  arrange(x, y, cc) %>%
  filter(row_number() < 10) %>%
  kable("html") %>%
  kable_styling(full_width = F)

img_df <- img_df %>%
  mutate(channel = case_when(
    cc == 1 ~ "Red",
    cc == 2 ~ "Green",
    cc == 3 ~ "Blue"
  ))

img_wide <- img_df %>%
  select(x, y, channel, value) %>%
  spread(key = channel, value = value) %>%
  mutate(
    color = rgb(Red, Green, Blue)
  )

sample_size <- 10000
img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]
img_sample$size <- runif(sample_size)

sort(img_sample$color)

img_1 = img_sample %>%
  group_by(color) %>%
  count()
# %>%
#   arrange(-n) %>%
#   head(n = 20)


img_1 %>%
  ggplot(aes(x = 1, y = n)) +
  geom_col(color = img_1$color) +
  coord_polar()

ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color, size = size)) +
  guides(size = FALSE) +
  labs(caption = "The Gladstone Pottery Museum (Stoke-on-Trent, UK)<br><br><br><span style='font-size:12pt;'><span style='color:#9DABAC;'>#30DayChartChallenge | @CSHoggard</span></span>") +
  scale_color_identity() +
  scale_y_reverse() +
  theme_void() +
  theme(plot.caption = element_markdown(family = "Poppins", hjust = 0.5, size = 24, margin = margin(20,0,10,0)),
        plot.margin = margin(20,20,20,20),
        plot.background = element_rect(fill = NA, colour = 'grey40', size = 2))

ggsave("abstract.png", plot = last_plot(), height = 264, width = 268, units = "mm", dpi = 600)
