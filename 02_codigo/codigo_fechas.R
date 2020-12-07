p_load(lubridate, zoo)
p_load(lubridate, tidyverse)

muertes_mx_st <- 
  read_csv("https://raw.githubusercontent.com/mariorz/covid19-mx-time-series/master/data/covid19_deaths_mx.csv") %>% 
  pivot_longer(-Estado, names_to = "fecha_corte", values_to = "muertes") %>% 
  mutate(fecha_corte = dmy(fecha_corte))
#https://r4ds.had.co.nz/dates-and-times.html

muertes_mx_st %>% 
  filter(Estado == "Jalisco") %>% 
  group_by(Estado) %>% 
  mutate(muertes_por_dia = muertes - lag(muertes),
         media_movil_7 = rollmean(x = muertes_por_dia, 
                                  k = 7, 
                                  align = "right", 
                                  fill = NA)) %>%  #promedio movvil
  ungroup() %>% 
  ggplot(aes(x = fecha_corte, y = muertes_por_dia)) +
  geom_line(alpha = 0.8) +
  geom_line(aes(x = fecha_corte, y = media_movil_7), color = "salmon", size = 1) +
  theme_minimal()


muertes_mx_st %>% 
  filter(Estado == "Jalisco") %>% 
  group_by(Estado) %>% 
  mutate(muertes_por_dia = muertes - lag(muertes),
         media_movil_7 = rollmean(x = muertes_por_dia, 
                                  k = 7, 
                                  align = "right", 
                                  fill = NA)) %>%  #promedio móvil
  ungroup() %>% 
  ggplot(aes(x = fecha_corte, y = muertes_por_dia)) +
  geom_line(alpha = 0.8) +
  geom_line(aes(x = fecha_corte, y = media_movil_7), color = "salmon", size = 1) +
  scale_x_date(date_breaks = "1 month",
              date_labels = "%B %d")+
  theme_minimal()
