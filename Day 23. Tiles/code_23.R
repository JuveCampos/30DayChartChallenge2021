# Librerias ----
library(tidyverse)
library(lubridate)
library(vapoRwave)

# library(XML)

# #load apple health export.xml file
# root = "exportar.xml"
# xml <- xmlParse(root)
# 
# #transform xml file to data frame - select the Record rows from the xml file
# df <- XML:::xmlAttrsToDataFrame(xml["//Record"])
# str(df)
# saveRDS(df, "salud.rds")
# df = read_rds("salud.rds")
# 
# #make value variable numeric
# df$value <- as.numeric(as.character(df$value))
# str(df)
# 
# #make endDate in a date time variable POSIXct using lubridate with eastern time zone
# df$endDate <-ymd_hms(df$endDate,tz="America/Mexico_City")
# str(df)
# 
# ##add in year month date dayofweek hour columns
# df$month<-format(df$endDate,"%m")
# df$year<-format(df$endDate,"%Y")
# df$date<-format(df$endDate,"%Y-%m-%d")
# df$dayofweek <-wday(df$endDate, label=TRUE, abbr=FALSE)
# df$hour <-format(df$endDate,"%H")
# str(df)
# 
# saveRDS(df, "salud.rds")
# rm(list = ls())

# df = read_rds("salud.rds") %>% 
#   mutate(type = str_remove(type, pattern = "HKQuantityTypeIdentifier|HKDataType|HKCategoryTypeIdentifier"))

# Datos de caminata ---- 
# walk = df %>% 
#   as_tibble() %>% 
#   filter(type == "StepCount") %>% 
#   group_by(date) %>% 
#   summarise(value = sum(as.numeric(value)), 
#             dayOfWeek = first(dayofweek), 
#             year = first(year)) %>% 
#   ungroup() %>% 
#   mutate(week = week(date)) %>% 
#   mutate(date = as.Date(date, format = "%Y-%m-%d")) 
  
# saveRDS(walk, "walk.rds")  
walk = readRDS("walk.rds")  %>% 
  mutate(value2 = ifelse(value >= 10000, 
                         10000, value), 
         mes = month(date)) %>% 
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
  mutate(dia_semana = str_replace_all(dayOfWeek, c("Sunday" = "Domingo",
                                                   "Monday" = "Lunes",
                                                   "Tuesday" = "Martes",
                                                   "Wednesday" = "Miércoles",
                                                   "Thursday" = "Jueves",
                                                   "Friday" = "Viernes",
                                                   "Saturday" = "Sábado"))) %>% 
  mutate(dia_semana = factor(dia_semana, levels = rev(c("Domingo", 
                                                        "Lunes", 
                                                        "Martes", 
                                                        "Miércoles", 
                                                        "Jueves", 
                                                        "Viernes", 
                                                        "Sábado"))))

(plt = walk %>%
  # filter(year == 2016) %>% 
  ggplot(aes(x = week, 
             y = dia_semana, 
             fill = value2, 
             text = date)) + 
  geom_tile(color = "black") + 
  facet_wrap(~year, ncol = 1) + 
  scale_fill_gradientn(colors = viridis::magma(n = 10)) +
  scale_x_continuous(breaks = seq(5,60, by = 5)) +  new_retro() +
  theme(legend.position = "bottom", 
        strip.text.x = element_text(
        size = 10, face = "bold", hjust = 0.5, color = "#FF0076"),
        strip.background = element_rect(color="#fff59e",
                                        fill="#fff59e",
                                        size=1.5,
                                        linetype="solid"),
        text = element_text(family = "Poppins"), 
        axis.text = element_text(family = "Poppins", face = "bold"), 
        plot.title = element_text(family = "Poppins"), 
        plot.subtitle = element_text(family = "Poppins"), 
        legend.title = element_text(family = "Poppins")) + 
  labs(fill = "Pasos (truncado a 10,000)", x = "Semana", y = "",
       title = "Pasos diarios dados por día registrados por mi Apple Watch, 2016-2021", 
       subtitle = "Daily steps registered by my Apple Watch, 2016-2021") + 
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, 
                               barwidth = 15,
                               barheight = 0.5)) )

