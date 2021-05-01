Sys.setlocale("LC_ALL", "es_ES.UTF-8")

# Librerias ----
library(tidyquant)
library(tidyverse)
library(fs)
library(ggtext)
library(ggshadow)

# Datos ----
btc = read_csv("01_Datos/BTC-USD.csv")
btc[,2:7] = lapply(btc[,2:7], as.numeric)

btc = btc %>%
  rename(date = Date,
         adjusted = `Adj Close`) %>%
  filter(!is.na(adjusted)) %>%
  select(date, adjusted) %>%
  mutate(mavg_short = rollmean(adjusted, k = 5, na.pad = TRUE, align = "right")) %>%
  mutate(mavg_long  = rollmean(adjusted, k = 50, na.pad = TRUE, align = "right"))


btc %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(size = 2) +
  geom_glowline(aes(y = mavg_short),
                color = "blue", linetype = 1, size = 1) +
  geom_glowline(aes(y = mavg_long),
                color = "red", linetype = 1, size = 1) +
  labs(x = "", y = "Precio Ajustado (USD)",
       title = "<b style = 'color:#b59407;'>Bitcoin</b><br>Precio de la Criptomoneda durante el último año.",
       subtitle = "Precio, <b style = 'color:red;'>media móvil a 50 dias</b> y <b style = 'color:blue;'>media móvil a 20 días</b>",
       caption = "<b>Fuente:</b> Yahoo Finance.<br>#30DayChartChallenge - 28: Unc + Future") +
  theme_minimal() +
  theme(plot.subtitle = element_markdown(hjust = 0.5),
        plot.title = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(hjust = 0.5),
        text = element_text(family = "Poppins")) +
  scale_y_continuous(labels = scales::dollar_format())


