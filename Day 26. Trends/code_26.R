
# Librerias ----
library(TSstudio) # Visualizaciones y trabajo preliminar de series
library(tseries) # Visualizaciones y funciones para estadistica descriptiva
library(seasonal) # Capturar y corregir elementos de estacionalidad
library(pastecs) # Herramientas para realizar análisis de estadística descriptiva
library(forecast) # Pronósticos y simulaciones
library(tidyverse) # Para manipular datos
library(vapoRwave)

# Leemos los datos ----
data<-read.csv("PIB Ej.csv", header=TRUE)

# Los convertimos a Serie de tiempo (objeto ts)
PIB_O <- data$Producto.interno.bruto.trimestral..base.2013
PIB_O <- ts(PIB_O, frequency = 4, start = 1993)

S2<-decompose(PIB_O,type = "multiplicative")
plot(S2)
S2$trend
class(S2$trend)

(plt = autoplot(S2,
         ts.colour = 'red',
         ts.linetype = 'dashed') +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "PIB México. Análisis de Series de Tiempo\n",
       x = "Año",
       caption = "\n\nFuente: INEGI. Solo considerando hasta 2019.\n@JuvenalCamposF - #30DayChartChallenge - Unc + Trend") +
  floral_shoppe(main.text.color = "black",
                font = "OCR A Extended",
                legend.position = "bottom") +
    theme(panel.background = element_rect(fill = "gray"),
          plot.title = element_text(color = "#fff59e", size = 15),
          plot.caption = element_text(color = "#fff59e", family = "OCR A Extended", size = 8),
          strip.text = element_text(
            size = 10, face = "bold", hjust = 0.5, color = "#FF0076"),
          strip.background = element_rect(color="#fff59e",
                                          fill="#fff59e",
                                          size=1.5,
                                          linetype="solid"))
  )

