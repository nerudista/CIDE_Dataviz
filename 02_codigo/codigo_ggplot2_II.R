###
### Código clase {ggplot2}, parte 2
###
### Elaborado por Sebastián Garrido de Sierra

### Paquetes mencionados en la láminas ----
library(pacman)
p_load(extrafont, ggrepel, ggthemes, janitor, RColorBrewer, scales, tidyverse,lubridate
)

extrafont::font_import()
# con este comando Windows me reconoce las fuentes
loadfonts(device = "win")

### Setup ----
#Mac
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
#windows
Sys.setlocale("LC_ALL", "Spanish")
options(scipen=999) # Prevenir notación científica

### Bases de datos ----

# Número mensual de homicidios entre 2015 y agosto de 2018, por estado ----
homicidios <- read_csv("http://segasi.com.mx/clases/cide/datos/homicidios_mmesuales_por_edo_2015_2018.csv")

# Datos de jugadores de Rusia 2018 ----
mundial <- read_csv("http://segasi.com.mx/clases/cide/datos/wc2018_player_stats.csv")

# Resultados de la elección presidencial de 2018, por distrito ----
presidente <- read_csv("http://segasi.com.mx/clases/cide/datos/cp_2018_dtto.csv")

# Iniciativas legislativas presentadas por los últimos cuatro presidentes en los primeros 14 meses de su respectiva administración.
iniciativas <- 
  read_csv("http://segasi.com.mx/clases/cide/datos/bd_iniciativas_primeros_14_meses.csv") %>% 
  clean_names()


### Títulos y notas al pie ----

# Versión 1. Título se sale de la gráfica :'(
presidente %>%
  rename(total_votos = total_votos_calculados) %>% 
  mutate(anaya_gano = ifelse(v_anaya > v_meade, "Sí", "No")) %>% 
  ggplot(aes(x = v_anaya,
             y = v_meade,
             size = total_votos,
             color = anaya_gano)) +
  geom_point(alpha = 0.5) +
  labs(title = "Relación entre el número de votos obtenidos por  Anaya y Meade en la elección presidencial de 2018",
          subtitle = "Cada punto representa un distrito.",
          x = "Votos recibidos por Anaya",
          y = "Votos recibidos por Meade",
          color = "¿Ganó Anaya?",
          size = "Total de votos\nen el distrito")


# Versión 2. Primera opción para poner título en dos líneas: \n
presidente %>%
  rename(total_votos = total_votos_calculados) %>% 
  mutate(anaya_gano = ifelse(v_anaya > v_meade, "Sí", "No")) %>% 
  ggplot(aes(x = v_anaya,
             y = v_meade,
             size = total_votos,
             color = anaya_gano)) +
  geom_point(alpha = 0.5) +
  labs(title = "Relación entre el número de votos obtenidos por Anaya y Meade en la elección\npresidencial de 2018",
          subtitle = "Cada punto representa un distrito.",
          x = "Votos recibidos por Anaya",
          y = "Votos recibidos por Meade",
          color = "¿Ganó Anaya?",
          size = "Total de votos\nen el distrito")


# Versión 3. Segunda opción para poner título en dos líneas: str_wrap(, width= XX)
presidente %>%
  rename(total_votos = total_votos_calculados) %>% 
  mutate(anaya_gano = ifelse(v_anaya > v_meade, "Sí", "No")) %>% 
  ggplot(aes(x = v_anaya,
             y = v_meade,
             size = total_votos,
             color = anaya_gano)) +
  geom_point(alpha = 0.5) +
  labs(title = str_wrap("Relación entre el número de votos obtenidos por Anaya y Meade en la elección presidencial de 2018", width = 40),
          subtitle = "Cada punto representa un distrito.",
          x = "Votos recibidos por Anaya",
          y = "Votos recibidos por Meade",
          color = "¿Ganó Anaya?",
          size = "Total de votos\nen el distrito")

### Temas ----

# Para cambiar casi cualquier elemento de la gráfica, tenemos que modificar todo o parte del tema con theme().

# Dos tipos de cambios:

# Generales, eligiendo un tema específico: theme_nombre()

# Particulares, eligiendo un elemento específico en un tema: theme(plot.title = element_text())

# Ejemplo de cambio general de look and feel: 

presidente %>%
  rename(total_votos = total_votos_calculados) %>% 
  mutate(anaya_gano = ifelse(v_anaya > v_meade, "Sí", "No")) %>% 
  ggplot(aes(x = v_anaya,
             y = v_meade,
             size = total_votos,
             color = anaya_gano)) +
  geom_point(alpha = 0.5) +
  labs(title = str_wrap("Relación entre el número de votos obtenidos por  Anaya y Meade en la elección presidencial de 2018", width = 80),
       subtitle = "Cada punto representa un distrito.",
       x = "Votos recibidos por Anaya",
       y = "Votos recibidos por Meade",
       color = "¿Ganó Anaya?",
       size = "Total de votos\nen el distrito") +
  theme_bw()



# Catálogo de cosas que podemos modificar via theme()

# theme(line, rect, text, title, aspect.ratio, axis.title, axis.title.x,
#       axis.title.x.top, axis.title.y, axis.title.y.right, axis.text, axis.text.x,
#       axis.text.x.top, axis.text.y, axis.text.y.right, axis.ticks, axis.ticks.x,
#       axis.ticks.y, axis.ticks.length, axis.line, axis.line.x, axis.line.y,
#       legend.background, legend.margin, legend.spacing, legend.spacing.x,
#       legend.spacing.y, legend.key, legend.key.size, legend.key.height,
#       legend.key.width, legend.text, legend.text.align, legend.title,
#       legend.title.align, legend.position, legend.direction, legend.justification,
#       legend.box, legend.box.just, legend.box.margin, legend.box.background,
#       legend.box.spacing, panel.background, panel.border, panel.spacing,
#       panel.spacing.x, panel.spacing.y, panel.grid, panel.grid.major,
#       panel.grid.minor, panel.grid.major.x, panel.grid.major.y, panel.grid.minor.x,
#       panel.grid.minor.y, panel.ontop, plot.background, plot.title, plot.subtitle,
#       plot.caption, plot.margin, strip.background, strip.placement, strip.text,
#       strip.text.x, strip.text.y, strip.switch.pad.grid, strip.switch.pad.wrap, ...,
#       complete = FALSE, validate = TRUE)



### Crear y guardar temas ----

#  Ejemplo

mi_tema <- theme_minimal() +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(linetype = "dashed"),
        axis.title = element_text(size = 12, hjust = 1, color = "#a50f15", face = "bold"),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 15, 0, 0)))


presidente %>%
  rename(total_votos = total_votos_calculados) %>% 
  mutate(anaya_gano = ifelse(v_anaya > v_meade, "Sí", "No")) %>% 
  ggplot(aes(x = v_anaya,
             y = v_meade,
             size = total_votos,
             color = anaya_gano)) +
  geom_point(alpha = 0.5) +
  labs(title = str_wrap("Relación entre el número de votos obtenidos por  Anaya y Meade en la elección presidencial de 2018", width = 60),
       subtitle = "Cada punto representa un distrito.",
       x = "Votos recibidos por Anaya",
       y = "Votos recibidos por Meade",
       color = "¿Ganó Anaya?",
       size = "Total de votos\nen el distrito") +
  mi_tema



presidente %>%
  rename(total_votos = total_votos_calculados) %>% 
  mutate(anaya_gano = ifelse(v_anaya > v_meade, "Sí", "No")) %>% 
  ggplot(aes(x = v_anaya,
             y = v_meade,
             size = total_votos,
             color = anaya_gano)) +
  geom_point(alpha = 0.5) +
  labs(title = str_wrap("Relación entre el número de votos obtenidos por  Anaya y Meade en la elección presidencial de 2018", width = 80),
       subtitle = "Cada punto representa un distrito.",
       x = "Votos recibidos por Anaya",
       y = "Votos recibidos por Meade",
       color = "¿Ganó Anaya?",
       size = "Total de votos\nen el distrito")+
  theme(
    plot.title = element_text(family = "Reem Kufi")
  )


### Escalas ----

# Las escalas (scale_) nos permiten ajustar diversos aspectos relacionados con los elementos estéticos (aes()) de una gráfica.
# 
# Todas las escalas se construyen igual:
#   
#   scale_[aes]_[nombre escala]()

# El [aes] en scale_[aes]_[nombre escala]() puede referise a: 
#   
# - Canales de posición: x | y 
# 
#   - scale_x_ | scale_y_  
# 
# - Canales de color: colour | fill | alpha
# 
#   - scale_colour_ | scale_fill_ | scale_alpha_  
# 
# - Canales de tamaño: size | radius 
# 
#   - scale_size_ | scale_radius_  
# 
# - Canales variaditos: shape| linetype
# 
#   - scale_shape_ | scale_linetype_  


# El [nombre escala] en scale_[aes]_[nombre escala]() dependerá de:
#   
#   1. El [aes]
# 
# 
#   
#   2. El tipo de variable] que asignaron al  [aes]
# 
# 
#   
#   Por ejemplo, para scale_x/y_ pueden ser: 
#   
#   
#   
#   - continuous | discrete | log10 | sqrt | date | ...
# 
# 
# Para scale_color/fill/alpha pueden ser
# 
# 
#   
#   - continuous | discrete | manual | ...


# Ejemplo con scale_x/y_continuous() ----

# scale_x/y_continuous() nos permiten modificar diferentes elementos de los ejes:
#   
#   - breaks = posición donde aparecen las etiquetas del eje
# 
#   - limits = límites del eje
# 
#   - labels = etiquetas que apareceran en los breaks
# 
#   - position = "top", "right", "bottom" o "left"

# Gráfica original
presidente %>%
  ggplot(aes(x = v_anaya,
             y = v_meade)) +
  geom_point(alpha = 0.5)


# Gráfica con cambio en breaks
presidente %>%
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 100000, 10000))

# Gráfica con cambio en breaks + limits
presidente %>%
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 100000, 10000),
                 limits =  c(0, 100000)) # Para forzar a que aparezca 0


# Gráfica con cambio en breaks en intervalos no constantes
presidente %>%
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(breaks = c(seq(0, 50000, 10000),  
                      c(75000, 100000, 25000)),
                 limits =  c(0, 100000))

# Gráfica con cambio en breaks en intervalos no constantes + comas para separar miles
presidente %>%
  ggplot(aes(x = v_anaya,
             y = v_meade)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(breaks = c(seq(0, 50000, 10000),  
                      seq(75000, 100000, 25000)),
                 limits = c(0, 100000),
                 labels = comma) +
  scale_y_continuous(labels = comma)


presidente %>%
  ggplot(aes(x = v_anaya,
             y = v_meade/100000)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(breaks = c(seq(0, 50000, 10000),  
                                seq(75000, 100000, 25000)),
                     limits = c(0, 100000),
                     labels = comma) +
  scale_y_continuous(labels = percent_format())



# Gráfica con cambio en breaks + labels 
presidente %>%
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(breaks = c(seq(0, 50000, 10000),  
                      seq(75000, 100000, 25000)),
                 limits = c(0, 100000),
                 labels = 1:8)

# Gráfica con cambio en breaks + labels de texto
presidente %>%
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(breaks = c(seq(0, 50000, 10000),  
                      seq(75000, 100000, 25000)),
                 limits = c(0, 100000),
                 labels = c("Poquito", "", "", "", 
                            "", "", "", "Muchito"))

# Gráfica con cambio en posición de ejes
presidente %>%
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(alpha = 0.5) +
  scale_x____(___ = c(seq(0, 50000, 10000),  
                      ___(75000, 100000, 25000)),
                 ___ = c(0, 100000),
                 ___ = "top")


# Ejemplo con scale_color/fill_manual() ----

# Hay varias formas de cambiar los colores que ggplo2 usa por default para los puntos líneas y áreas.

# Entre otras cosas, `scale_colour_manual()` nos permiten modificar:

# El color asignado a cada valor de una variable discreta con .hl_bold[`values =`]
# 
# Las etiquetas que aparecen en la leyenda de colores con .hl_bold[`labels =`]


# Gráfica inicial 
presidente %>%
  mutate(anaya_gano = ifelse(v_anaya > v_meade, "Sí", "No")) %>% 
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(aes(color = anaya_gano), 
             alpha = 0.5)  

# Gráfica con colores específicos
presidente %>%
  mutate(anaya_gano = ifelse(v_anaya > v_meade, "Sí", "No")) %>% 
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(aes(color = anaya_gano), 
             alpha = 0.5) +
  scale_color_manual(values =  c("salmon", "steelblue"))

# Gráfica con colores + etiquetas específicos
presidente %>%
  mutate(anaya_gano = ifelse(v_anaya > v_meade, "Sí", "No")) %>% 
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(aes(color = anaya_gano), 
             alpha = 0.5) +
  scale_color_manual(values = c("red", "steelblue"),
                     labels = c("Ganó Pepe", "Ganó Ricky"))


# Ejemplo con scale_color/fill_brewer() ----

#  Requiere que el paquete `RColorBrewer` esté instalado y cargado.

#  RColorBrewer` ofrece varias paletas de colores precargadas.

# Gráfica con paleta de RColorBrewer
presidente %>%
  mutate(anaya_gano = ifelse(v_anaya > v_meade, "Sí", "No")) %>% 
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(aes(color = anaya_gano), 
             alpha = 0.5) +
  scale_colour_brewer(palette =  "Set1")

# Ejemplo con scale_color/fill_gradient() ----

# Si asignamos una variable numérica color o fill, debemos usar scale_colour_gradient() para modificarlo.

presidente %>%
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(aes(color = total_votos_calculados),
             alpha = 0.5)

# Gráfica inicial
presidente %>%
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(aes(___ = total_votos_calculados), 
             alpha = 0.5)

# Gráfcia con color modificado
presidente %>%
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(aes(___ = total_votos_calculados), 
             alpha = 0.5) +
  scale_color___(__ = "white", high = "steelblue")

### Leyendas ----

# Para reubicar la leyenda debemos ir a `theme` y usar alguna de las siguientes opciones:

# grafica + 
#   theme(legend.position = "left")
# 
# grafica + 
#   theme(legend.position = "top")
# 
# grafica + 
#   theme(legend.position = "bottom")
# 
# grafica + 
#   theme(legend.position = "right")
# 
# grafica + 
#   theme(legend.position = c(0.5, 0.5))

presidente %>%
  ggplot(aes(x = v_anaya, y = v_meade)) +
  geom_point(aes(color = total_votos_calculados), 
             alpha = 0.5) +
  scale_color_gradient(low = "white", high = "steelblue") +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"))


### Incluir texto en gráficas a partir de valores de variables ----

presidente %>%
  ggplot(aes(x = v_anaya,
             y = v_meade)) +
  geom_point()+
  geom_text(aes(label= nombre_distrito))+
  theme_stata()


presidente %>%
  mutate(etiqueta_texto = ifelse(test = v_anaya>= 75e3 | v_meade >= 60e3,
                                 yes= nombre_distrito,
                                 no = "")) %>% 
  ggplot(aes(x = v_anaya,
             y = v_meade)) +
  geom_point()+
  geom_text(aes(label= etiqueta_texto))+
  theme_stata()

# ggrepel
presidente %>%
  mutate(etiqueta_texto = ifelse(test = v_anaya>= 75e3 | v_meade >= 60e3,
                                 yes= nombre_distrito,
                                 no = "")) %>% 
  ggplot(aes(x = v_anaya,
             y = v_meade)) +
  geom_point()+
  geom_text_repel(aes(label= etiqueta_texto))


# con stringr
presidente %>%
  mutate(etiqueta_texto = ifelse(test = v_anaya>= 75e3 | v_meade >= 60e3,
                                 yes= nombre_distrito,
                                 no = ""),
         etiqueta_texto= str_to_title(string = etiqueta_texto),
         etiqueta_texto= str_replace(string = etiqueta_texto,
                                     pattern = "Leon",
                                     replacement = "León"),
         etiqueta_texto = str_wrap(etiqueta_texto,20)) %>% 
  select (v_meade,v_anaya,nombre_distrito,etiqueta_texto) %>% 
  filter(etiqueta_texto != "") %>% 
  #print(v=Inf) %>% 
  ggplot(aes(x = v_anaya,
             y = v_meade)) +
  geom_point()+
  geom_text_repel(aes(label= etiqueta_texto))

  #ejemplo Sebastian
presidente %>%
  mutate(etiqueta_texto = ifelse(test = v_anaya >= 75000 | v_meade >= 60000, yes = nombre_distrito, no = ""),
         etiqueta_texto = str_to_title(string = etiqueta_texto),
         etiqueta_texto = str_replace(string = etiqueta_texto, 
                                      pattern = "eon", 
                                      replacement = "eón"),
         etiqueta_texto = str_replace(string = etiqueta_texto, 
                                      pattern = " De ", 
                                      replacement = " de "),
         etiqueta_texto = str_wrap(string = etiqueta_texto, width = 20)) %>%
  # select(v_meade, v_anaya, nombre_distrito, etiqueta_texto) %>%
  # filter(etiqueta_texto != "") %>%
  # print(n = Inf)
  ggplot(aes(x = v_anaya,
             y = v_meade)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = etiqueta_texto), size = 7, color = "blue")

# en versiones previas a ggplot 3.3, necesito este código porque el eje x no permite 
# variables catgóricas

presidente %>% 
  mutate(por_v_amlo = v_amlo/total_votos_calculados*100,
         por_v_amlo = round(x=por_v_amlo, digits = 1),
         etiqueta_barras = str_c(por_v_amlo, "%", sep="")) %>% 
  filter (edo_min == "Oaxaca") %>% 
  select (nombre_distrito,por_v_amlo,etiqueta_barras) %>% 
  ggplot(aes(y=por_v_amlo, x=nombre_distrito)) +
  geom_col()+
  geom_text(aes(label=etiqueta_barras, hjust = 1.2, fontface= "bold")) +
 coord_flip()

# ejemplo que sí corrio en su equipo pero tiene ggplot 3.3
presidente %>% 
  mutate(por_v_amlo = v_amlo/total_votos_calculados*100,
         por_v_amlo = round(x = por_v_amlo, digits = 1),
         etiqueta_texto = str_c(por_v_amlo, "%", sep = "")) %>% 
  filter(edo_min == "Oaxaca") %>% 
  select(nombre_distrito, por_v_amlo, etiqueta_texto) %>% 
  ggplot(aes(y = por_v_amlo, y = nombre_distrito)) +
  geom_col() +
  geom_text(aes(label = etiqueta_texto), hjust = -0.2, color = "blue", fontface = "bold") +
  scale_x_continuous(limits = c(0, 80)) +
  theme(axis.title.x = element_text(face = "bold"))


# otro ejemplo

diamonds %>% 
  group_by(cut, color) %>% 
  summarise(num_obs = n()) %>% 
  ungroup() %>% 
  group_by(cut) %>% 
  mutate(total_obs = sum(num_obs)) %>% 
  ungroup() %>% 
  ggplot(aes(x = cut, y = num_obs, fill = color)) +
  geom_col() +
  geom_text(aes(label = num_obs, 
                group = color, 
                color = ifelse(color == "D", "white", "black")),
            position = position_stack(vjust = 0.5),
  ) +
  geom_text(aes(label = total_obs, y = total_obs)) +
  scale_color_manual(values = c("white", "black"), guide = F)

####################
foo <- 
  tibble(fecha = seq.Date(from = as_date("2020-01-01"), to = as_date("2020-12-01"), by = "1 month"),
         oaxaca = rnorm(n = 12, mean = 24, sd = 5),
         cdmx = rnorm(n = 12, mean = 24, sd = 5)) 

foo %>% 
  pivot_longer(c(oaxaca, cdmx), names_to = "entidad", values_to = "porcentaje") %>% 
  ggplot(aes(x = fecha, y = porcentaje, color = entidad)) +
  geom_line()

##########
# Ejemplo con gráfica de barras 1 ----
iniciativas %>% 
  group_by(presidente) %>% 
  summarise(num = n()) %>% 
  ungroup() %>%  
  ggplot(aes(x = presidente, 
             y = num)) +
  geom_col() +
  geom_text(aes(____ = ____))


# Ejemplo con gráfica de dispersión ----

# Toma 1
presidente %>% 
  ggplot(aes(x = v_anaya, 
             y = v_meade)) +
  geom_point() + 
  geom_text(aes(___ = ____))

# Toma 2
presidente %>% 
  ggplot(aes(x = v_anaya, 
             y = v_meade)) +
  geom_point() + 
  geom_text____(aes(___ = ____))

# Toma 3
presidente %>% 
  mutate(etiqueta = ___) %>% 
  ggplot(aes(x = v_anaya, 
             y = v_meade)) +
  geom_point() + 
  geom_text(aes(___ = ____))



# Ejemplo con gráfica de líneas ----

# Toma 1
homicidios %>%
  # Filtrar para sólo quedarnos con obs. de cuatro edos.
  filter(edo %in% c("Chihuahua", "Guanajuato", "Yucatán", "Ciudad de México")) %>% 
  ggplot() +
  geom_line(aes(x = fecha,
                y = homicidios_mensuales,
                group = edo)) +
  geom_text(aes(___ = ___))


# Toma 2
homicidios %>%
  # Filtrar para sólo quedarnos con obs. de cuatro edos.
  filter(edo %in% c("Chihuahua", "Guanajuato", "Yucatán", "Ciudad de México")) %>% 
  mutate(etiqueta = ___) %>% 
  ggplot() +
  geom_line(aes(x = fecha,
                y = homicidios_mensuales,
                group = edo)) +
  geom_text(aes(___ = ___))


### Guardar gráficas ----

# Para guardar las gráficas generas con ggplot2 debemos usar ggsave()

# Por default, `ggsave()` guarda:
#
#   - La última gráfica generada con `ggplot2`
#   
#   - Usando las dimensiones del panel de gráficas en RStudio
#
#   - Lo hace en el directorio de trabajo de la sesión

#   Esto se puede modificar con los argumentos de ggsave()

# ggsave("nombre_del_archivo",        # Nombre con que queremos guardar la gráfica
#        plot = nombre_de_grafica,    # En su caso, nombre de objeto que contiene gráfica
#        device = NULL,               # Formato de imagen: .png, .pdf, etc
#        path = NULL,                 # Ruta al folder donde queremos guardarla
#        scale = 1,                   # Escala de la gráfica
#        width = NA,                  # Ancho de la gráfica
#        height = NA,                 # Alto de la gráfica
#        units = c("in", "cm", "mm")) # Unidades ancho/alto


presidente %>% 
  mutate(por_v_amlo = v_amlo/total_votos_calculados*100,
         por_v_amlo = round(x=por_v_amlo, digits = 1),
         etiqueta_barras = str_c(por_v_amlo, "%", sep="")) %>% 
  filter (edo_min == "Oaxaca") %>% 
  select (nombre_distrito,por_v_amlo,etiqueta_barras) %>% 
  ggplot(aes(y=por_v_amlo, x=nombre_distrito)) +
  geom_col()+
  annotate(geom="text", label="Mi mamá me mima", x=6, y=120)+
  geom_text(aes(label=etiqueta_barras, hjust = 1.2, fontface= "bold")) +
  coord_flip()+
  ggsave("01_datos/mi_primer_grafica.png",
         width = 16,
         height = 9)
