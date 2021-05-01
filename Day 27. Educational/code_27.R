# Librerias ----
library(tidyverse)
library(readxl)
library(janitor)
library(vapoRwave)

# Datos ----
bd = readxl::read_xls("06_educacion.xls",
                      sheet = "11",
                      skip = 6) %>%
  clean_names() %>%
  filter(!is.na(entidad_federativa))

# Nos quedamos con los totales ----
unique(bd_2$estimador)
bd_2 = bd %>%
  filter(sexo == "Total") %>%
  filter(grupos_quinquenales_de_edad == "Total") %>%
  filter(estimador %in% c("Valor",
                          "Límite inferior de confianza",
                          "Límite superior de confianza")) %>%
  select(entidad_federativa,
         estimador,
         grado_promedio_de_escolaridad) %>%
  pivot_wider(id_cols = c(entidad_federativa),
              values_from = grado_promedio_de_escolaridad,
              names_from = estimador) %>%
  clean_names() %>%
  mutate(entidad_federativa = str_remove(entidad_federativa, pattern = "\\d+\\s"))

names(bd_2)
bd_2 %>%
  ggplot(aes(x = reorder(entidad_federativa, valor), y = valor)) +
  geom_col() +
  geom_errorbar(aes(ymin=limite_inferior_de_confianza,
                    ymax=limite_superior_de_confianza)) +
  coord_polar()


# Municipios ----

# Conseguimos los demas estados:
# https://www.inegi.org.mx/contenidos/programas/intercensal/2015/tabulados/06_educacion_oax.xls
edos <- c("ags", "bc", "bcs", "cam","coah", "col","chis", "chih", "cdmx",
          "dgo", "gto", "gro", "hgo", "jal", "mex", "mich","mor", "nay", "nl",
          "oax", "pue", "qro", "qroo","slp", "sin", "son","tab","tamps","tlax","ver","yuc","zac")

# Planteamos el loop de descarga:
for(estado in edos){
  # estado = "ags"
  print(estado)
  curl::curl_download(url = str_c("https://www.inegi.org.mx/contenidos/programas/intercensal/2015/tabulados/06_educacion_", estado, ".xls"),
                      destfile =
                        str_c("datos_intercensal_2015/Educ_", estado, ".xls"))
}

# Datos locales
root = "datos_intercensal_2015/"
files = str_c(root, list.files(root))

bds = lapply(files, function(files){
  # files = "datos_intercensal_2015/Educ_cdmx.xls"

  if(files == "datos_intercensal_2015/Educ_cdmx.xls"){
    bd_loc = read_xls(files,
                      sheet = "04", skip = 6) %>%
      clean_names() %>%
      rename(municipio = delegacion)

  } else {
    bd_loc = read_xls(files,
                      sheet = "04", skip = 6) %>%
      clean_names()
  }

  bd_loc = bd_loc %>%
    filter(!is.na(entidad_federativa)) %>%
    filter(sexo == "Total") %>%
    filter(estimador %in% c("Valor",
                            "Límite inferior de confianza",
                            "Límite superior de confianza")) %>%
    select(entidad_federativa,
           estimador,
           municipio,
           grado_promedio_de_escolaridad) %>%
    filter(estimador %in% c("Valor",
                            "Límite inferior de confianza",
                            "Límite superior de confianza")) %>%
    pivot_wider(id_cols = c(entidad_federativa, municipio),
                values_from = grado_promedio_de_escolaridad,
                names_from = estimador) %>%
    clean_names() %>%
    mutate(entidad_federativa = str_remove(entidad_federativa, pattern = "\\d+\\s"),
           municipio = str_remove_all(municipio, pattern = "\\d+\\s|\\s\\*"))
  print(files)
  return(bd_loc)
})

bd_mpios = do.call(rbind, bds) %>%
  filter(municipio != "Total") %>%
  arrange(-valor) %>%
  filter(!is.na(valor))

head(bd_mpios)
tail(bd_mpios)

unique(bd_grafica$entidad_federativa)
bd_grafica = rbind(head(bd_mpios) %>%
                     mutate(class = "top_5"),
                   tail(bd_mpios) %>%
                     mutate(class = "bottom_5")) %>%
  mutate(abr = str_replace_all(entidad_federativa,
                               c("Ciudad de México" = "CDMX",
                                 "Nuevo León" = "NL",
                                 "Oaxaca" = "OAX",
                                 "Veracruz de Ignacio de la Llave" = "VER",
                                 "Guerrero" = "GRO"))) %>%
  mutate(municipio = str_c(municipio, ", ", abr))

bd_grafica$municipio = factor(bd_grafica$municipio,
                              levels = bd_grafica$municipio)


bd_grafica %>%
  group_by(class) %>%
  summarise(min = min(limite_inferior_de_confianza),
            max = max(limite_superior_de_confianza))

bd_grafica %>%
  ggplot(aes(x = municipio,
             y = valor,
             fill = class)) +
  geom_col() +
  geom_errorbar(aes(ymin=limite_inferior_de_confianza,
                    ymax=limite_superior_de_confianza),
                color = "red", width = 0.75) +
  geom_text(aes(label = round(valor, 1), color = class),
            vjust = -0.8, family = "Poppins", fontface = "bold") +
  geom_hline(yintercept = c(11.7, 3.36),
             linetype = 2,
             size = 1,
             color = "gold") +
  scale_fill_manual(values = c("#F25F5C", "#70c1b3")) +
  scale_y_continuous(breaks = c(6, 9, 12, 17),
                     expand = expansion(c(0,0.1), 0.1),
                     labels = c("Primaria\nterminada",
                                "Secundaria\nterminada",
                                "Preparatoria\nterminada", "Educación\nSuperior\nterminada")) +
  theme(legend.position = "bottom",
        strip.text.x = element_text(
          size = 10, face = "bold", hjust = 0.5, color = "465775"),
        strip.background = element_rect(color="#fff59e",
                                        fill="#fff59e",
                                        size=1.5,
                                        linetype="solid"),
        text = element_text(family = "Poppins", color = "#465775"),
        plot.title = element_text(family = "Poppins", color = "#465775", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(family = "Poppins", color = "#465775", hjust = 0.5),
        legend.title = element_text(family = "Poppins", color = "#465775")) +
  # c("#F25F5C", "#70c1b3")
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   color = c(rep("#70c1b3",6),
                                             rep("#F25F5C",6))),
        axis.text.y = element_text(angle = 0, hjust = 1,color = "#465775"),
        legend.position = "none") +
  labs(x = "", y = "",
       title = "Grado promedio de escolaridad.\n5 Municipios con los grados más altos y más bajos.\nMéxico, 2015",
       subtitle = "Average educational degree. Top 5 & Bottom 5 Municipalities in Mexico. 2015.",
       caption = "Fuente: INEGI. Tabulados de Educación la encuesta intercensal 2015.\n@JuvenalCamposF - #30DayChartChallenge - Unc + Education\n1 a 6 años: Primaria. 7 a 9 años: Secundaria. 9 a 12 años: Preparatoria y en adelante educación superior.")
# +
#   coord_polar(start = 1.5708)

