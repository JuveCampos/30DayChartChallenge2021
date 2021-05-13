# Removemos objetos previos
rm(list = ls(all = TRUE)) #clear workspace

# Librerias
library(HistData)

# Leemos los datos.
data("GaltonFamilies")

# Almacenamos la base en un objeto de nombre _d_.
d <- GaltonFamilies

# Generamos una var. dummy para cuando el individuo
# es masculino o femenino
d$H <- 1 # Caso Hombres
d$H[d$gender == "female"] <- 0 # Caso Mujeres

# GENERAMOS UNA FUNCION PARA HACER BOOTSTRAP #
my_boots <- function(){
  # Sampleo
  samp <- sample(1:nrow(d), size = nrow(d), replace = TRUE)
  # Estimamos los parametros del modelo...
  m <- glm(childHeight ~ H, data = d[samp,])
  # Quedandonos con los coeficientes de m, y el sigma
  c(coef(m), sigma = sigma(m))
}

# Guardamos las replicaciones de bootstrap de los parametros cada vez que hago
# una replicación de bootstrap
boots <- replicate(50000, my_boots())
boots <- t(boots)
head(boots)

# Distribución boostrap de alpha
plot(density(boots[,1], adjust = 4))

# Distribución bootstrap de beta
plot(density(boots[,2], adjust = 4))

plot(boots[,1:2])

# GRAFICA EN 3D DE LINEAS.
library(hexbin)

# Otro plot de esas variables, pero con otra funcion
bin <- hexbin(boots[,1:2], xbins = 50)
plot(bin)

library(MASS)
den3d <- kde2d(boots[,1], boots[,2])
persp(den3d, box = FALSE, theta = 40, phi = 30, r = sqrt(3))
png(filename="faithful.png")
persp(den3d, box = FALSE, theta = 70, phi = 30, r = sqrt(3), border = "pink", col = "black")
dev.off()
a

# Libreria para hacer renders en 3D
options(rgl.useNULL = FALSE)
library(rgl)
library(rayshader)

# Libreria para hacer graficas y usar el simbolo %>%
library(vapoRwave)
library(webshot2)

# Elaboramos la gráfica de hexagonos.
ggbins <- ggplot(boots %>% as.data.frame()) +
  geom_hex(aes(x = boots[,1], y = boots[,2])) +
  new_retro() +
  scale_fill_gradientn(colors = rev(vapoRwave:::newRetro_palette)) +
  theme(axis.title.y = element_text(angle = 90, size = 9, family = "Windows Command Prompt"),
        axis.title.x = element_text(angle = 0, size = 9, family = "Windows Command Prompt"),
        axis.text = element_text(angle = 0, size = 9, family = "Windows Command Prompt"),
        plot.caption = element_text(size = 7, hjust = 1),
        legend.title = element_text(angle = 0, size = 9, family = "Windows Command Prompt"),
        legend.text =  element_text(angle = 0, size = 9, family = "Windows Command Prompt")
        ) +
  labs(title = "Distribucion Bootstrap\nde Coeficientes de regresion\n",
         x = "Distribucion Bootstrap\ndel coeficiente Beta_1",
       fill = "Puntos\nen cada\nhexagono",
         y = "Distribucion Bootstrap\ndel intercepto",
         caption = "\nDatos provienientes del paquete {histdata}
         Regresión elaborada con la base de datos GaltonFamilies,
         en la cual Y = childheight y X = variable binaria para el sexo del niño.
         50,000 muestras Bootstrap.
         @JuvenalCamposF - #30DayChartChallenge - Día 09; Estadística - Distribuciones")

ggbins

# Convertimos una grafica ggplot en 2D a una en 3D
plot_gg(ggbins,            # Objeto ggplot
        width = 5,         # Ancho del ggplot
        height = 5,        # Alto del ggplot
        multicore = TRUE,  # Arg. para utilizar todos los nucleos de la compu en el procesamiento
        scale = 250,       # Multiplicador para escala vertical del 3D
        zoom = 0.7,        # Zoom de la ventana
        theta = 10,        # Angulo horizontal de presentacion
        phi = 30,          # Angulo vertical de presentacion
        windowsize = c(800, 800))

