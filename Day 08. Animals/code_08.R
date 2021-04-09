# Librerias ----
library(rvest)
library(tidyverse)
library(ggtext)

# Scrapping ----
url = "https://animalcrossing.fandom.com/wiki/Villager_list_(New_Horizons)"

tabla = read_html(url) %>%
  html_table()

tabla = tabla[[3]]

# Dos primeros vecinos
imgs_1 = read_html(url) %>%
  html_nodes(".image") %>%
  html_nodes("img") %>%
  html_attr("src") %>%
  as.character()

imgs_1 = imgs_1[2:3]

# Imagene de agnes en adelante
imgs = read_html(url) %>%
  html_nodes(".image") %>%
  html_nodes("img") %>%
  html_attr("data-src") %>%
  as.character()

# imgs =
imgs = imgs[4:length(imgs)]

imagenes = append(imgs_1, imgs)


# Juntamos con la tabla
tabla$Image = imagenes


datos = tabla %>%
  janitor::clean_names() %>%
  separate(personality_1, into = c("sex", "personality"), sep = "\\s") %>%
  mutate(species_1 = str_replace_all(species_1, c("Alligator" = "Cocodrilo",
                                                  "Anteater" = "Oso hormiguero",
                                                  "Bear" = "Oso",
                                                  "Bird" = "Pájaro",
                                                  "Bull" = "Toro",
                                                  "Cat" = "Gato",
                                                  "Chicken" = "Pollo",
                                                  "Cow" = "Vaca",
                                                  "Cub" = "Cachorro de oso",
                                                  "Deer" = "Venado",
                                                  "Dog" = "Perro",
                                                  "Duck" = "Pato",
                                                  "Eagle" = "Águila",
                                                  "Elephant" = "Elefante",
                                                  "Frog" = "Rana",
                                                  "Goat" = "Cabra",
                                                  "Gorilla" = "Gorila",
                                                  "Hamster" = "Hamster",
                                                  "Hippo" = "Hipopótamo",
                                                  "Horse" = "Caballo",
                                                  "Kangaroo" = "Canguro",
                                                  "Koala" = "Koala",
                                                  "Lion" = "León",
                                                  "Monkey" = "Mono",
                                                  "Mouse" = "Ratón",
                                                  "Octopus" = "Pulpo",
                                                  "Ostrich" = "Avestruz",
                                                  "Penguin" = "Pingüino",
                                                  "Pig" = "Cerdo",
                                                  "Rabbit" = "Conejo",
                                                  "Rhino" = "Rinoceronte",
                                                  "Sheep" = "Oveja",
                                                  "Squirrel" = "Ardilla",
                                                  "Tiger" = "Tigre",
                                                  "Wolf" = "Lobo")))

unique(datos$species_1)
table(datos$species_1)

datos_plt = datos %>%
  group_by(species_1) %>%
  mutate(image = first(image)) %>%
  group_by(species_1, sex, image) %>%
  summarise(n = n()) %>%
  ungroup()


label_data = datos %>%
  group_by(species_1) %>%
  count()

label_data$id = 1:nrow(label_data)

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label

# calculate the ANGLE of the labels

number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id - 0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #

plt = datos_plt %>%
  ggplot() +
  geom_col(aes(x = species_1,
               y = n,
               fill = sex)) +
  geom_text(data = label_data,
            aes(x=id,
                y=n+2,
                label=n,
                hjust=hjust),
            vjust = -0.1,
            hjust = 0.5,
            family = "nintendoP_Humming-E_002pr"
            ) +
  ylim(-20,25) +
  coord_polar(start = 0) +
  theme_minimal() +
  labs(x = "", y = "", fill = "Sexo",
       title = "Vecinos de Animal Crossing por Especie y Sexo",
       subtitle = "Animal Crossing Villagers by gender and species",
       caption = "@JuvenalCamposF \n#30DayMapChallenge \nDía 8: Distribuciones - Animal ") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#81F1F7", "#c48d3f")) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  theme(
        # panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "gray10", size = 7),
        text = element_text(family = "nintendoP_Humming-E_002pr"),
        plot.caption = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50")
        )

plt

# Guardamos con fondo transparente
ggsave("grafica_animales.png",
       device = "png",
       bg = "transparent",
       width = 12,
       height = 10
)

tabla$Name
mis_vecinos = c("Kid Cat",
  "Carmen", "Ankha",
  "Lionel", "Paolo", "Portia", "Patty",
  "Eugene", "Aurora", "Julia")


mis_vecinos_df = tabla %>%
  filter(Name %in% mis_vecinos)

for(i in 1:10){
  curl::curl_download(mis_vecinos_df$Image[i],
                      destfile = str_c(mis_vecinos_df$Name[i], ".png"))

}

