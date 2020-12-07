###
### Código clase {ggplot2}, parte 4
###
### Código elaborado por Sebastián Garrido de Sierra

### Paquetes ----
library(pacman)
p_load(ggExtra, 
       hexbin, 
       janitor, 
       lubridate, 
       scales, 
       sf, # mapas
       tidyverse, 
       treemapify)

### Setup ----
#windows
Sys.setlocale("LC_ALL", "Spanish")
#Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

### Bases de datos ----

# Número mensual de homicidios entre 2015 y agosto de 2018, por estado ----
homicidios <- read_csv("http://segasi.com.mx/clases/cide/datos/homicidios_mmesuales_por_edo_2015_2018.csv")

# Datos de jugadores de Rusia 2018 ----
mundial <- read_csv("http://segasi.com.mx/clases/cide/datos/wc2018_player_stats.csv")

# Resultados de la elección presidencial de 2018, por distrito ----
presidente <- read_csv("http://segasi.com.mx/clases/cide/datos/cp_2018_dtto.csv")

# Número de carpetas de investigación abiertas por homicidio doloso en cada colonia y alcaldía de la CDMX entre enero de 2016 y diciembre de 2019 ----
ci_hom_dol <- read_csv("http://segasi.com.mx/clases/cide/datos/num_ci_hom_dol_por_colonia_alcaldia.csv")

# Número de carpetas de investigación abiertas por robo en la vía pública con violencia en la CDMX entre enero y noviembre de 2020
ci_robos <- 
  read_csv("http://segasi.com.mx/clases/cide/datos/ci_robo_via_publica_ene_nov_2020.csv")



### Diagramas de dispersión ----

## Alternativas para lidiar con un gran número de observaciones ----

# Punto de partida
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point()

# Ej. 1
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(alpha = 1/100) 

# Ej. 2
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_bin2d(color = "white")

# Ej. 3
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_hex(color = "white") # Hace falta instalar y cargar {hexbin}
  

## Análisis de distribuciones univariadas a partir de un diagrama de dispersión ----

# opción 1
diamonds %>% 
  ggplot(aes(x=price))+
  geom_histogram(color="white",
                 #bins = 10,
                 breaks = seq(0,25000,2500)
                 )


# opción 2
diamonds %>% 
  ggplot(aes(x=price))+
  geom_density(fill = "steelblue",
                 #bins = 10,
                 #breaks = seq(0,25000,2500)
  )

# opción 2
diamonds %>% 
  ggplot(aes(x=price))+
  geom_boxplot(outlier.colour = "salmon"
               #bins = 10,
               #breaks = seq(0,25000,2500)
  )

presidente %>% 
  ggplot(aes(x=v_amlo,
             y=edo_min))+
  geom_boxplot(color="red")


# Punto de partida
presidente %>%
  ggplot(aes(x = v_amlo, y = v_anaya)) +
  geom_point(alpha = 0.7)


# Con geom_rug()
presidente %>%
  ggplot(aes(x = v_amlo, y = v_anaya)) +
  geom_point(alpha = 0.7) +
  geom_rug(color = "grey30",
           alpha = 0.7,
           sides = "tr")

# Con ggMarginal() + type = "histogram"
p <-
  presidente %>%
  ggplot(aes(x = v_amlo, y = v_anaya)) +
  geom_point(alpha = 0.7)

p

p %>%
  ggMarginal(type = "histogram",
             margins = "both",
             fill = "steelblue",
             color = "white")


# Con ggMarginal() + type = "density"
p %>%
  ggMarginal(type = "density",
             margins = "both",
             fill = "steelblue",
             color = "salmon")

# Con ggMarginal() + type = "densigram"
p %>%
  ggMarginal(type = "densigram",
             margins = "both",
             fill = "steelblue",
             color = "salmon")

# Con ggMarginal() + type = "boxplot"
p %>%
  ggMarginal(type = "boxplot",
             margins = "both",
             fill = "steelblue",
             color = "salmon")


# Con ggMarginal() + type = "violin"
p %>%
  ggMarginal(type = "violin",
             margins = "both",
             fill = "steelblue",
             color = "white")



### Heatmaps ----
presidente %>% 
  # Calcular total de votos recibidos por AMLO, Meade, Anaya y El Bronco en cada estado
  group_by(edo_min) %>% 
  summarise_at(.vars = c("v_amlo", "v_meade", "v_anaya", "v_bronco"), 
               list(edo = sum)) %>% 
  ungroup() %>% 
  # Transformar estructura de la base de datos
  pivot_longer(v_amlo_edo:v_bronco_edo,
               names_to = "candidato",
               values_to = "votos") %>% 
  # Graficar
  ggplot(aes(x = candidato, 
             y = fct_rev( edo_min), 
             fill = votos
             )) +
  geom_tile(color="white")+
  scale_fill_gradient(low="white", high = "salmon")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  theme_minimal()+
  theme(
    panel.grid = element_blank()
  )


### Matriz de gráficas ----
mundial %>%
  ggplot() +
  geom_point(aes(x = altura,
                 y = peso,
                 color = posicion)) +
  facet_wrap(~ posicion)


### Treemap ----

# Revisar estructura de los datos
ci_hom_dol

ci_hom_dol %>% 
  group_by(alcaldia) %>% 
  summarise(total_ci = sum(num_ci)) %>% 
  ungroup() %>% 
  ggplot(aes(area= total_ci,
             fill = total_ci,
             subgroup = alcaldia,
             label= alcaldia)) +
  geom_treemap()+
  geom_treemap_subgroup_text(color = "white")

# Revisar estructura de los datos
ci_hom_dol %>% 
  group_by(alcaldia) %>% 
  summarise(total_ci = sum(num_ci)) %>% 
  ungroup() %>% 
  ggplot(aes(area = total_ci,
             fill = total_ci,
             subgroup = alcaldia,
             label = alcaldia)) +
  geom_treemap() +
  geom_treemap_subgroup_text(color = "white") +
  scale_fill_gradient(low = "grey90", high = "salmon")

# Graficar
ci_hom_dol %>% 
  ggplot(aes(area = num_ci, 
             fill = num_ci, 
             subgroup = alcaldia, # Este es un elemento estético nuevo, especiífico de geom_treemap()
             label = alcaldia)) +
  geom_treemap(col = "white") +
  geom_treemap_subgroup_border(size = 1) +
  geom_treemap_subgroup_text(color = "white") +
  scale_fill_gradient(low = "grey90", high = "salmon")
  

### Mapas ----

## Importar shp de alcaldías ----

alcaldias_sf <- 
  st_read("./01_datos/alcaldias_adip/alcaldias.shp") %>% 
  st_transform(.,  crs = 4326)

alcaldias_sf %>% 
  filter(cve_mun == "010")

alcaldias_sf %>% 
  ggplot()+
  geom_sf()

### Mapa puntual ----

ci_robos %>% 
  ggplot(aes(x= longitud,
             y = latitud))+
  geom_point()

# Primero el poligono
ci_robos %>% 
  ggplot()+
  geom_sf(data = alcaldias_sf)+
  geom_point(aes(x= longitud,
                 y = latitud),
             alpha= 0.2,
             size = 0.5)

## hxagonos
ci_robos %>% 
  ggplot()+
  geom_sf(data = alcaldias_sf)+
  geom_hex(aes(x= longitud,
                 y = latitud),
             )+
  theme_void()

# densidad
ci_robos %>% 
  ggplot()+
  geom_sf(data = alcaldias_sf)+
  geom_density2d(aes(x= longitud,
               y = latitud),
  )

# densidad
ci_robos %>% 
  ggplot()+
  geom_sf(data = alcaldias_sf)+
  geom_density2d_filled(aes(x= longitud,
                     y = latitud),
  )



# primero los puntos -> el mapa los va a tapar

ci_robos %>% 
  ggplot()+
  geom_point(aes(x= longitud,
                 y = latitud),
             alpha= 0.2) +
  geom_sf(data = alcaldias_sf)

## ahora ocn densidad


ci_robos %>% 
  ggplot()+
  geom_density2d_filled(aes(x= longitud,
                            y = latitud),
  )+
  geom_sf(data = alcaldias_sf,
          fill = "transparent",
          color = "salmon")+
  #theme_map()+
  theme_void()


## mapa cloropletico

alcaldias_sf %>% 
  ggplot()+
  #geom_sf(aes(fill= gid))
  geom_sf(aes(fill= nomgeo ))


ci_robos %>%
  filter(!is.na(latitud)) %>% 
  group_by(alcaldia_hechos) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  mutate (alcaldia_hechos = str_to_title(alcaldia_hechos),
          alcaldia_hechos = str_replace(alcaldia_hechos,))


ci_robos_mpo <- 
  ci_robos %>% 
  filter(!is.na(latitud)) %>% 
  group_by(alcaldia_hechos) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  mutate(alcaldia_hechos = str_to_title(alcaldia_hechos),
         alcaldia_hechos = str_replace(alcaldia_hechos, "Alvaro Obregon", "Álvaro Obregón"),
         alcaldia_hechos = str_replace(alcaldia_hechos, "Juarez", "Juárez"),
         alcaldia_hechos = str_replace(alcaldia_hechos, "Coyoacan", "Coyoacán"),
         alcaldia_hechos = str_replace(alcaldia_hechos, "Tlahuac", "Tláhuac"),
         alcaldia_hechos = str_replace(alcaldia_hechos, " A ", " A. "),
         alcaldia_hechos = str_replace(alcaldia_hechos, " A ", " A. "),
         alcaldia_hechos = str_replace(alcaldia_hechos, "Cuauhtemoc", "Cuauhtémoc"),
         alcaldia_hechos = str_replace(alcaldia_hechos, " De ", " de "))



alcaldias_sf %>% 
  left_join(y = ci_robos_mpo, by = c("nomgeo" = "alcaldia_hechos")) %>% 
  ggplot() +
  geom_sf(aes(fill = num_ci))+
  theme_void()
