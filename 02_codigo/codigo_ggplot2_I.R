###
### Código clase {ggplot2}, parte 1
###
### Elaborado por Sebastián Garrido de Sierra

### Paquetes mencionados en la láminas ----
library(pacman)
p_load(janitor, tidyverse) 


### Setup ----
#Mac
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
#windows
Sys.setlocale("LC_ALL", "Spanish")

options(scipen=999) # Prevenir notación científica

### Bases de datos ----

# Número mensual de homicidios entre 2015 y agosto de 2018, por estado
homicidios <- 
  read_csv("http://segasi.com.mx/clases/cide/datos/homicidios_mmesuales_por_edo_2015_2018.csv")

# Datos de jugadores de Rusia 2018
mundial <- 
  read_csv("http://segasi.com.mx/clases/cide/datos/wc2018_player_stats.csv")

# Resultados de la elección presidencial de 2018, por distrito
presidente <- 
  read_csv("http://segasi.com.mx/clases/cide/datos/cp_2018_dtto.csv")

# Iniciativas legislativas presentadas por los últimos cuatro presidentes en los primeros 14 meses de su respectiva administración.
iniciativas <- 
  read_csv("http://segasi.com.mx/clases/cide/datos/bd_iniciativas_primeros_14_meses.csv") %>% 
  clean_names()

### {ggplot2} ----

# Para hacer una gráfica en ggplot2 debemos especificar al menos cuatro cosas:
#   
# nombre_data_frame %>%  # 1) Nombre del df que usaremos
#   
#   ggplot() +           # 2) Abrimos canvas para dibujar
#   
#   aes(x = var_1,       # 3) Asignamos variable(s) a elemento(s) 
#       y = var_2,       #    estético(s) o canale(s)
#       color = var_3, 
#       fill = var_4,
#       size = var_5,
#       etc.) +
#   
#     geom_[tipo]()      # 4) Elegimos objeto geométrico o marca
#                                        para representar la(s) variable(s)

 
# Ejemplo 1

presidente %>% 
ggplot() +
aes(x = v_anaya,
    y = v_meade) +
geom_point()

# ejemplo by us
presidente %>% 
  rename(t_votos = total_votos_calculados) %>% 
  mutate(anaya_gano = ifelse(v_anaya > v_meade,"Sí","No")) %>% 
  ggplot() +
  aes(x = v_anaya,
      y = v_meade,
      size = t_votos,
      color=anaya_gano
    ) +
  geom_point(alpha=0.5
             )


### aes() por aquí y por allá ----

# Opción 1 - aes() solita

presidente %>%
  ggplot() +
  aes(x = v_anaya,
      y = v_meade) +
  geom_point()

# Opción 2 - aes() dentro de ggplot(). Los elementos estéticos que definan aquí afectaran a TODAS las capas 

presidente %>%
  ggplot(aes(x = v_anaya, 
             y = v_meade))  +
  geom_abline()


# Opción 3 - aes() dentro del geom_(). Los elementos estéticos que definan aquí SOLO afectaran a la capa en donde los coloquen

presidente %>%
  ggplot()  +
  geom_point(aes(x = v_anaya, 
                 y = v_meade))


# Comparación:

presidente %>%
  ggplot()  +
  geom_point(aes(x = v_anaya, 
                 y = v_meade)) +
  geom_smooth(aes(x = v_anaya, 
                  y = v_meade))


presidente %>%
  ggplot(aes(x = v_anaya, 
             y = v_meade))  +
  geom_point() +
  geom_smooth()


presidente %>%
  ggplot()  +
  geom_point(aes(x = v_anaya, 
                 y = v_meade)) +
  geom_smooth()


### ¿Por qué importa donde ponemos el aes()?

# Tibble de chocolate
chocolate <- 
  tibble(año = 2000:2019,
         casos = 21:40, 
         decada = c(rep(x = "primera", times = 10),
                    rep(x = "segunda", times = 10)))

# Comencémos poniendo la función aes() dentro de ggplot()
chocolate %>% 
  ggplot(aes(x = año, y = casos)) +
  geom_line() +
  geom_point()

# Ahora movamos la función aes() a geom_line()
chocolate %>% 
  ggplot() +
  geom_line(aes(x = año, y = casos)) +
  geom_point()

# Ahora movamos la función aes() a geom_point()
chocolate %>% 
  ggplot() +
  geom_line() +
  geom_point(aes(x = año, y = casos))

# Ahora pongamos la función aes() tanto dentro de geom_line() como geom_point()
chocolate %>% 
  ggplot() +
  geom_line(aes(x = año, y = casos)) +
  geom_point(aes(x = año, y = casos))


# En suma, la ubicación de la función aes() importa porque [te toca completar la frase]

# Noten ahora como cambia el resultado en cada uno de los siguientes ejemplos

# Ej. 1
chocolate %>% 
  ggplot(aes(x = año, y = casos, color = decada)) +
  geom_line() +
  geom_point()

# Ej. 2
chocolate %>% 
  ggplot(aes(x = año, y = casos)) +
  geom_line(aes(color = decada)) +
  geom_point()

# Ej. 3
chocolate %>% 
  ggplot(aes(x = año, y = casos)) +
  geom_line() +
  geom_point(aes(color = decada))


# La diferencia entre el primer, segundo y tercer ejemplo es que ____ 

### ¿Dentro o fuera de aes()? ----

## Cuando comienzas a usar ggplot2 siempre hay un dilema en torno a dónde definir un elemento estético.

# Con color = decada ADENTRO de aes()
chocolate %>% 
  ggplot(aes(x = año, y = casos)) +
  geom_point(aes(color = decada))

# Con color = decada AFUERA de aes()
chocolate %>% 
  ggplot(aes(x = año, y = casos)) +
  geom_point(color = decada)

# :'(

## Otra forma de analizar qué está pasando

# Primero, usen colors() para conocer la gama de colores que pueden especificar por NOMBRE en R 

colors()

# Vamos a trabajar con dos: purple y seagreen

# Con color = "purple" AFUERA de aes()
chocolate %>% 
  ggplot() +
  geom_point(aes(x = año, y = casos), # En esta línea termina aes()
             color = "purple")

# Con color = "seagreen" AFUERA de aes()
chocolate %>% 
  ggplot() +
  geom_point(aes(x = año, y = casos), # En esta línea termina aes()
             color = "seagreen")

# Ahora con color = "purple" DENTRO de aes()
chocolate %>% 
  ggplot() +
  geom_point(aes(x = año, y = casos, 
                 color = "purple")) # En esta línea termina aes()
# Noten la diferencia en la 
# alineación de color

# Ahora con color = "seagreen" DENTRO de aes()
chocolate %>% 
  ggplot() +
  geom_point(aes(x = año, y = casos, 
                 color = "seagreen")) 

# :/

## Otra comparación más...

# Con color = "seagreen" AFUERA de aes()
chocolate %>% 
  ggplot() +
  geom_point(aes(x = año, y = casos), 
             color = "seagreen") 

# Con color = década ADENTRO de aes()
chocolate %>% 
  ggplot() +
  geom_point(aes(x = año, y = casos,
                 color = decada))


# Moraleja: 

# Si quiero que un elemento estético dependa de los valores de una VARIABLE (donde los valores son DIFERENTES para cada unidad), entonces debo definirlo DENTRO de aes(). 

# En cambio, si quiero que un elemento estético dependa de una CONSTANTE (donde los valores son IGUALES para todas las unidades), entonces debo definirlo FUERA de aes(). 


### Argumento position ----

# position es un argumento que nos permite especificar cómo ordenar los puntos y las barras en ciertos geom_.

# Es particularmente útil con:

# geom_point()
# geom_col()
# geom_bar()

# Ejemplo con geom_point() ----

# En geom_point() el default es position = "identity". Comparen:

# Sin mencionar explícitamente position = "identity"
presidente %>%
  ggplot() +
  geom_point(aes(x = v_amlo,
                 y = edo_min),
             alpha = 0.5)


# Mencionando explícitamente position = "identity"
presidente %>%
  ggplot() +
  geom_point(aes(x = v_amlo,
                 y = edo_min),
             alpha = 0.5,
             position = "identity")


# Ahora, vean cómo cambia la cosaa cuando usamos position = "jitter"

presidente %>%
  ggplot() +
  geom_point(aes(x = v_amlo,
                 y = edo_min),
             alpha = 0.5,
             position = "jitter")


# geom_jitter() es un shortcut para geom_point() + position = "jitter"

presidente %>%
  ggplot() +
  geom_jitter(aes(x = v_amlo, 
                  y = edo_min),
              alpha = 0.5)

# Una ventaja de usar geom_jitter() es que pueden controlar la cantidad de ruido aleatorio que ggplot2 introduce a los datos vertical y horizontalmente con los argumentos width y height

presidente %>%
  ggplot() +
  geom_jitter(aes(x = v_amlo, y = edo_min),
              alpha = 0.5,
              height = 0.5)

# Ejemplo con geom_col() ----

# Primero, calculemos el número de iniciativas legislativas de reforma constitucional y a leyes secundarias propuestas por cada presidente
iniciativas %>% 
  # El mágico mundo de {dplyr}
  group_by(presidente, subclasificacion) %>% 
  summarise(num = n()) %>% 
  ungroup()


# Ahora sí, hagamos nuestra primera gráfica de columnas...
iniciativas %>% 
  # El mágico mundo de {dplyr}
  group_by(presidente, subclasificacion) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  # El mágico mundo de {ggplot2}
  ggplot(aes(x = presidente, y = num)) +
  geom_col()


# Corrijamos para que se distinga la región de cada tipo de iniciativa dentro de las barras
iniciativas %>% 
  # El mágico mundo de {dplyr}
  group_by(presidente, subclasificacion) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  # El mágico mundo de {ggplot2}
  ggplot(aes(x = presidente, 
             y = num,
             color = "red")) +
  geom_col()


# Veamos que pasa con position = "fill"
iniciativas %>% 
  # El mágico mundo de {dplyr}
  group_by(presidente, subclasificacion) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  # El mágico mundo de {ggplot2}
  ggplot(aes(x = presidente, 
             y = num,
             fill = subclasificacion)) +
  geom_col(position = "fill")


# Ahora con position = "dodge"
iniciativas %>% 
  # El mágico mundo de {dplyr}
  group_by(presidente, subclasificacion) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  # El mágico mundo de {ggplot2}
  ggplot(aes(x = presidente, 
             y = num,
             fill = subclasificacion)) +
  geom_col(position = "dodge")



### Función coord_ ----

# Existen tres sistemas de coordenadas para crear una visualización:

# - Cartesianas (ejes perpendiculares) - El defaul en ggplot2
# - Paralelas
# - Polares

# Entre otras cosas, la función coord_ nos sirve apra 

# - Invertir ejes
# - Hacer gráficas de pay y donas (y algunas atrocidades)

## coord_flip() ----
# Gráfica de columnas original
presidente %>%
  ggplot() +
  geom_col(aes(x = edo_min, # OJO: siempre deben asignar variable cateógrica o character a x
               y = v_amlo),
           fill = "steelblue")

# Gráfica de columnas con ejes invertidos
presidente %>%
  ggplot() +
  geom_col(aes(x = edo_min,
               y = v_amlo),
           fill = "steelblue",
           color="red") +
  coord_flip()

## coord_polar() ----

# Primero, creemos un tibble
foo <- 
  tibble(grupo = c("Mujeres", "Hombres", "Niños"),
         numero_abordo = c(25, 25, 50))

# Ahora, ustedes comiencen a modificar el siguiente código 
foo %>% 
  ggplot(aes(x = "" , y = numero_abordo, fill = grupo)) +
  geom_col()+
  coord_polar(theta = "y", start = 0)+
  theme_void()
  
# Donut chart 
foo %>% 
  ggplot(aes(x = 2 , y = numero_abordo, fill = grupo)) +
  geom_col(width = .4)+
  coord_polar(theta = "y", start = 0)+
  #theme_void()+
  xlim(0.5,2.5)



### Función facet_ -----

# La función facet_ nos permite generar gráficas para subconjuntos de nuestros datos.

# Hay dos tipos de facet_:

# - facet_wrap(): útil para generar facetas usando una sola variable discreta.
# - facet_grid(): permite generar facetas usando dos variables discretas.

# Ejemplo 1 - Con datos del df mundial

# Gráfica de relación entre altura y peso de jugadores del mundial de Rusia 2018

# Versión 1
mundial %>%
  ggplot() +
  geom_point(aes(x = altura,
                 y = peso)) 

# Versión 2, (i) asignando posicion al canal de color
mundial %>%
  ggplot() +
  geom_point(aes(x = altura,
                 y = peso,
                 color = posicion)) 

# Versión 3, (i) asignar posicion al canal de color; (ii) generar una faceta por tipo de posición 
mundial %>%
  ggplot() +
  geom_point(aes(x = altura,
                 y = peso,
                 color = posicion)) +
  facet_wrap( ~ posicion, ncol=4) 

# hagamos una regresión
mundial %>%
  ggplot(aes(x = altura,
             y = peso,
             color=posicion
  )) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_wrap( ~ posicion, ncol=2) 

  # otro ejemplo
mtcars %>% 
  ggplot(aes(x=wt,
             y=mpg,
             color = as.factor(cyl)))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap( ~ as.factor(cyl))


# Ejemplo 2 - Con datos del df homicidios

# homicidios contiene el número de homicidios mensuales registrados en cada estado. Es un dataframe que construí a partir de los datos que publica el SNSP y cubre el período enero de 2015 a agosto de 2018

# Hagamos nuestra primera gráfica de líneas con geom_line()

# Versión 1
homicidios %>%
  ggplot() +
  geom_line(aes(x = fecha,
                   y = homicidios_mensuales)) 

# No guta :'(

# ¿Cuál es el problema? 
# ¿Cuántas observaciones tenemos por año?
# ¿Cuál es el valor máximo y mínimo de la línea horizontal que aparece en cada mes?
# ¿Cuál es el valor de la línea que va de un mes a otro?
# ¿Qué hacemos?


# Versión 2, (i) especificar cómo agrupar las líneas usando el argumento group DENTRO de aes()
homicidios %>%
  ggplot() +
  geom_line(aes(x = fecha,
                y = homicidios_mensuales, 
                group = edo)) #este group hace que se haga la línea bien

# Mochnaiser :)

# Versión 3, (i) especificar cómo agrupar las líneas; (ii) generar una faceta por estado

homicidios %>%
  ggplot() +
  geom_line(aes(x = fecha,
                y = homicidios_mensuales,
                group = edo)) +
  facet_wrap(~ edo, ncol=8, scales = "free_x") 
  


# Versión 4, (i) especificar cómo agrupar las líneas; (ii) generar una faceta por estado; (iii) especificar cuántos renglones/columnas queremos que tenga la matriz de gráficas

homicidios %>%
  ggplot() +
  geom_line(aes(x = fecha,
                y = homicidios_mensuales,
                ___ = edo)) +
  facet_wrap(~ ___, 
             ncol = 8) 