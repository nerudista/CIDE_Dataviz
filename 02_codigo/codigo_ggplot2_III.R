###
### Código clase {ggplot2} + {forcats}
###
### Elaborado por Sebastián Garrido de Sierra

### Paquetes mencionados en la láminas ----
library(pacman)
p_load(extrafont, ggrepel, ggthemes, janitor, RColorBrewer, scales, tidyverse)

### Setup ----
#Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica
#windows
Sys.setlocale("LC_ALL", "Spanish")
options(scipen=999) # Prevenir notación científica


### Importar base(s) de datos ----
# CEMABE CDMX
cemabe_df <- read_csv("http://segasi.com.mx/clases/cide/vis_man/datos/cemabe_cdmx.csv", col_types = cols(ID_INM = col_character()), 
                      locale = locale(asciify = TRUE))

# Gastos de campaña del proceso federal 2018
gastos <- 
  read_csv("http://segasi.com.mx/clases/cide/datos/gastso_campania_2018_por_rubro.csv") %>% 
  # Cambiar formato de la variable `Total Gastos` para que sea numeric
  mutate(`Total Gastos` = str_replace(`Total Gastos`, "\\$", ""),
         `Total Gastos` = str_replace_all(`Total Gastos`, ",", ""),
         `Total Gastos` = as.numeric(`Total Gastos`)) 


### {forcats} ----

# {forcats} nos permite transformar dos cosas de los factores:
# - El **orden** de los niveles
# - El **valor** de cada categoría


gastos %>% 
  count(Siglas,sort=T) %>% 
  ggplot(aes(x=n,y=Siglas))+
  geom_col()

gastos %>% 
  ggplot(aes(y=Siglas))+
  geom_bar()

# Reordenar factores con fct_infreq() ----

# Gráfica del número de candidatos que han reportado gastos al INE, v. 1
# Sin fct_infreq()
gastos %>% 
  ggplot(aes(x = Siglas)) +
  geom_bar() +
  coord_flip() + 
  labs(title = "Núm. de candidatos de cada fuerza política que reportaron\ngastos al INE", 
       x = NULL) +
  theme_minimal()

# Gráfica del número de candidatos que han reportado gastos al INE, v. 2
# Con fct_infreq()
gastos %>% 
  ggplot(aes(x = fct_infreq(Siglas))) +
  geom_bar() +
  coord_flip() + 
  labs(title = "Núm. de candidatos de cada fuerza política que reportaron\ngastos al INE", 
       x = NULL) +
  theme_minimal()

# Gráfica del número de candidatos que han reportado gastos al INE, v. 3
# Con fct_infreq() + fct_rev()
gastos %>% 
  ggplot(aes(x = fct_rev(fct_infreq(Siglas)))) +
  geom_bar() +
  coord_flip() + 
  labs(title = "Núm. de candidatos de cada fuerza política que reportaron\ngastos al INE", 
       x = NULL) +
  theme_minimal()


# Reordenar factores con fct_infreq() ----

# Gráfica del número de alumnos por delegación, v.1 
# Sin fct_reorder()
cemabe_df %>% 
  filter(P166 < 9000) %>%  
  group_by(NOM_MUN) %>% 
  summarise(num_alumnos = sum(P166)) %>% 
  ungroup() %>%
  ggplot(aes(x = NOM_MUN, y = num_alumnos)) + 
  geom_col() +
  labs(title = "Núm. de alumnos por delegación", 
       x = NULL) +
  coord_flip() +  theme_minimal()


# Gráfica del número de alumnos por delegación, v.2
# Con fct_reorder()
cemabe_df %>% 
  filter(P166 < 9000) %>%  
  group_by(NOM_MUN) %>% 
  summarise(num_alumnos = sum(P166)) %>% 
  ungroup() %>%
  mutate(NOM_MUN = fct_reorder(NOM_MUN,           # Aquí reordeno  
                               num_alumnos)) %>% 
  ggplot(aes(x = NOM_MUN, y = num_alumnos)) + 
  geom_col() +
  labs(title = "Núm. de alumnos por delegación", 
       x = NULL) +
  coord_flip() +  theme_minimal()


# Reordenar factores con fct_inorder() ----

# fct_inorder define el orden de los factores de acuerdo con el orden con que aparecen en el df.

# Gráfica del número de centros educativos por delegación, v.1
# Sin fct_infreq()
cemabe_df %>% 
  ggplot(aes(x = NOM_MUN)) +  # Aquí ggplot2 calcula ..count..
  geom_bar() +
  labs(x = NULL) +
  coord_flip() +
  theme_minimal()

# Reordenemos aleatoriamiente los renglones y guardemos transformación foo
foo <- cemabe_df %>% 
  slice(sample(1:n())) 

# slice() sirve para seleccionar renglones por posición
# sample() toma una muestra aleatoria del tamaño del df

# Comparar los dos data frames
cemabe_df %>% select(NOM_MUN) %>% head()
foo %>% select(NOM_MUN) %>% head()


# si uso foo, vuelve a calcular frecuencias , los ordena alfateticamente
# y les pone como factor
foo %>% 
  ggplot(aes(x = NOM_MUN)) +  # Aquí ggplot2 calcula ..count..
  geom_bar() +
  labs(x = NULL) +
  coord_flip() +
  theme_minimal()



# Gráfica del número de centros educativos por delegación, v.2
# Con fct_inorder()
foo %>% 
  mutate(NOM_MUN = fct_inorder(NOM_MUN)) %>%  #esto desordena
  ggplot(aes(x = NOM_MUN)) + 
  geom_bar() +
  labs(x = NULL) +
  coord_flip() +
  theme_minimal() 


# Reordenar factores con fct_relevel() ----
# fct_relevel() nos permite reordenar niveles de un fatcor siguiendo un criterio arbitrario.

# Gráfica de cuánto ha gastado cada candidato presidencial, v. 1
# Sin fct_relevel()
gastos %>% 
  filter(`Cargo de Elección` == "PRESIDENTE") %>% 
  ggplot(aes(x = str_wrap(`Nombre Completo`,width = 20), y = `Total Gastos`)) +
  geom_col() +
  labs(title = "Gastos totales de los candidatos presidenciales",
       x = NULL) +
  coord_flip()

# Gráfica de cuánto ha gastado cada candidato presidencial, v. 2
# Con fct_relevel()
gastos %>% 
  filter(`Cargo de Elección` == "PRESIDENTE") %>%
  mutate(`Nombre Completo` = fct_relevel(`Nombre Completo`, 
                                         "ANDRES MANUEL LOPEZ OBRADOR",
                                         "RICARDO ANAYA CORTES",
                                         "JOSE ANTONIO MEADE KURIBREÑA",
                                         "MARGARITA ESTER ZAVALA GOMEZ DEL CAMPO",
                                         "JAIME HELIODORO RODRIGUEZ CALDERON")) %>% 
  ggplot(aes(x = `Nombre Completo`, y = `Total Gastos`)) +
  geom_col() +
  labs(title = "Gastos totales de los candidatos presidenciales",
       x = NULL) +
  coord_flip()

# Recodificar categorías con fct_recode() ----

# Gráfica del núm. de candidatos de cada fuerza política que reportaron gastos al INE, v. 1
# Sin fct_recode()
gastos %>% 
  ggplot(aes(x = Siglas)) +
  geom_bar() +
  coord_flip() + labs(x = NULL) +
  theme_minimal()

# Gráfica del núm. de candidatos de cada fuerza política que reportaron gastos al INE, v. 2
# Con fct_recode()
gastos %>% 
  mutate(Siglas = fct_recode(Siglas,
                             "JHH" = "PT-MORENA-ENCUENTRO SOCIAL", 
                             "PMalF" = "PAN-PRD-MOVIMIENTO CIUDADANO",
                             "TPM" = "PRI-PVEM-NUEVA ALIANZA",
                             "MC" = "MOVIMIENTO CIUDADANO",
                             "PANAL" = "NUEVA ALIANZA",
                             "PES" = "ENCUENTRO SOCIAL")) %>% 
  ggplot(aes(x = Siglas)) +
  geom_bar() +
  coord_flip() + labs(x = NULL) +
  theme_minimal()


# Recodificar categorías con fct_collapse() ----

# Gráfica del núm. de candidatos de cada fuerza política que reportaron gastos al INE
# Con fct_collapse()
gastos %>% 
  mutate(Siglas = fct_collapse(Siglas,
                               JHH = c("ENCUENTRO SOCIAL", 
                                       "MORENA", 
                                       "PT-MORENA-ENCUENTRO SOCIAL"),
                               PMalF = c("MOVIMIENTO CIUDADANO", 
                                         "PAN", 
                                         "PAN-PRD-MOVIMIENTO CIUDADANO", "PRD"),
                               TPM = c("NUEVA ALIANZA",
                                       "PRI", 
                                       "PVEM", 
                                       "PRI-PVEM-NUEVA ALIANZA"))) %>% 
  ggplot(aes(x = Siglas)) +
  geom_bar() +
  coord_flip() + labs(x = NULL) + theme_minimal()

# fct_lump

gastos %>% 
  mutate(Siglas = fct_lump( Siglas,n=5,
                            other_level = "Otros")) %>% 
  ggplot(aes(x = Siglas)) +
  geom_bar() +
  coord_flip() + labs(x = NULL) +
  theme_minimal()
---------------
  
  mundial <- read_csv("http://segasi.com.mx/clases/cide/datos/wc2018_player_stats.csv")


mundial %>%
  ggplot() +
  geom_point(aes(x = altura,
                 y = peso,
                 color = posicion)) +
  facet_wrap(~ posicion)

foo <- c("GK","DF","MF","FW")

mundial %>% 
  mutate(posicion = fct_relevel(posicion,foo)) %>% 
  ggplot() +
  geom_point(aes(x=altura,
             y=peso,
             color=posicion))+
  facet_wrap(~ posicion)
  

cemabe_df %>% 
  ggplot(aes(x = NOM_MUN)) + 
  geom_bar() +
  labs(title = "Núm. de alumnos por delegación", 
       x = NULL) +
  coord_flip() +  theme_minimal()

foo <- cemabe_df %>% 
  slice(sample(1:n()))

foo
