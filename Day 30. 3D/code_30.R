# library(tidyverse)
# library(label)
#
# bd = readRDS("Latinobarometro_2018_Esp_R_v20190303.rds")
#
# bd


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
hexbin::hexcoords(bin)

bin
plot(bin)

library(MASS)
den3d <- kde2d(boots[,1], boots[,2])
persp(den3d, box = FALSE, theta = 40, phi = 30, r = sqrt(3))
png(filename="faithful.png")
persp(den3d, box = FALSE, theta = 70, phi = 30, r = sqrt(3), border = "pink", col = "black")
dev.off()
a

