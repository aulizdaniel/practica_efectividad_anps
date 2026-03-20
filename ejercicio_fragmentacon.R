
#Script para hacer la evaluación de un ANP
#El script proporciona un flujo de trabajo para determinar si un ANP puede 
#servir para reducir la fragemntación o no


#Autor: Dr. Daniel Auliz-Ortiz
#Laboratorio de Geografía de la biodiversidad, Instituto de Biología
#contacto: dauliz@cieco.unam.mx

#Dra Aline Pingarroni
# FES Iztacala, UNAM
#a_pingarroni@iztacala.unam.mx 


#Paquetes necesarios

#recuerda que puedes instalar el paquete que no tengas usando install.packages()

library(tidyverse)#manipulaDatos
library(sf)#Datos tipo shape
library(terra)#Raster
library(margins)#Efectos marginales
library(landscapemetrics)#Composicion y configuracion
library(exactextractr)#resumen de variables raster


#leer archivo raster de resultados

bosque <- rast("spatial/resultados/los_tux_bosque_2025.tif")


#Polígono de la reserva de la biosfera Los tuxtlas
pol_tux <-read_sf("spatial/LosTuxtlas1998.shp") %>% 
  st_transform(st_crs(bosque))

#cargamos el polígono del buffer alrededor de la reserva
buff_tux <- read_sf("spatial/buffer_los_tuxtlas.shp")



#agregamos una variable que defina si el sitio cuneta con protección o no a cada parte de los polígonos
pol_tux$prot <- "Protected"
buff_tux$prot <- "Unprotected"

#unimos ambos polígonos para tener uno, peor que distinga afuera y adentro
pol_tux_full <- bind_rows(pol_tux, buff_tux) %>% 
  select(prot)


pol_tux_full


#siembro semillas para hacer los resultados replicables
set.seed(123)

#siembro 30 puntos dispuestos al azar en el polígono que quiero analizar
p_in <- st_sample(pol_tux, 30, type= "random")

#visualizamos los puntos
plot(p_in)

#siembro semillas para hacer los resultados replicables
set.seed(123)

#siembro 30 puntos dispuestos al azar en el polígono que quiero analizar
p_out <- st_sample(buff_tux, 30, type= "random")

#visualizamos los puntos
plot(p_out)


#Necesitamos que cada punto tenga información de si está afuera o adentro de la reserva
#para eso hacemos la operación de intersección que le confiere los atributos del polígono a los puntos
p_in <- st_intersection( pol_tux_full, p_in)

p_out <- st_intersection( pol_tux_full, p_out)

#visualizamos que tenga los atributos
p_in %>% plot

p_out%>% plot


#realizamos buffers alrededor de cada punto con un radio de 3 km
buff_in <- st_buffer(p_in, 3000)

#visualizamos los buffer
buff_in %>% plot

buff_out<- st_buffer(p_out, 3000)

buff_out %>% plot

#para tener certeza de lo que pasa en cada buffer les asingamos un identificador numérico único (id)
#números del 1 al 30 como identificador para los buffer dentro
buff_in$id <- 1:30

#números del 31 al 60 como identificador para los buffer fuera
buff_out$id <- 31:60


#para agilizar el flujo de trabajo vamos a crear funciones que nos dejan hacer múltiples pasos en poco tiempo y líneas de código

#la primer función permite recortar el raster de bosque a las dimensiones de los buffer, se llama cortar

cortar <- function(pol, ra){
  gg <- ra %>% 
    crop(pol) %>% #recorta a la extensión
    mask(pol)     #aplica una máscara
  
  return(gg)      #devuelve el ráster recortado
}

#la segunda función permite medir el número de parches de bosque en cada buffer, se llama medir_fragment
medir_fragment <- function(pol, ra, part= "none"){
  
  gg <- cortar(pol, ra)  #recorta el ráster
  
  df_pol <- pol %>%      #convierte el polígono en data frame
    as.data.frame()
  
  df <- lsm_c_np(gg) %>%   #mide el número de parches
    mutate(id = df_pol$id,   #agrego identificadores únicos
           prot = df_pol$prot) %>% 
    filter(class == 1) %>%  #filtro lo que pasa solo en el bosque
    select(id, prot, value) %>%  #ordenos mis datos
    mutate(parte = part) %>% #agrego la parte que estoy midiendo
    rename(np = value) #cambio el nombre a la varaiable a np (número de parches)
  
  return(df)   #devuelvo el dato completo
  
}

#Podría aplicar la función manualmente a cada uno de los 60 paisajes de bosque, pero eso levaría tiempo
#para agilizarlo R puede aplicar una misma función a una lista, o serie de objetos (polígonos en este caso
#para ello utilizamos la función map

#aquí lo que se pide es aplicar cortar a cada renglón de buffer, esto hace que recorte el ráster en cada buffer
paisajes_in<- map(1:nrow(buff_in), ~ cortar(buff_in[.x, ], bosque)) 

paisajes_out<- map(1:nrow(buff_in), ~ cortar(buff_out[.x, ], bosque)) 



#pueden visualizar los paisajes de ls eiguiente forma. Si cambian el número en los corchetes pueden
#cambiar el raster que visualizan, como son 30 dentro y 30 fuera, pueden cambiar hasta el 30 en cada parte

plot(paisajes_in[[1]], col = c("gray", "darkgreen"))

plot(paisajes_out[[7]], col = c("gray", "darkgreen"))


#con las siguientes líneas vamos a calcular el número de parche spor cada paisaje y lo va a devolver todo
#en un formato que es muy comprensible
fragment_in<- map(1:nrow(buff_in), ~ medir_fragment(buff_in[.x, ], bosque, part ="dentro")) %>% 
  bind_rows()

fragment_out <- map(1:nrow(buff_out), ~ medir_fragment(buff_out[.x, ], bosque, part= "fuera")) %>% 
  bind_rows()


#visualizo los datos
fragment_in

#visualizo los datos
fragment_out

#juntos las mediciones afuera ya dentro en un solo objeto
df_frag <- bind_rows(fragment_in, fragment_out)


df_frag

#visualizo si hay diferencias en los números de parches en ambos sitios
df_frag %>% 
  ggplot(aes(x= parte, y= np, fill=parte))+
  geom_boxplot()

###################################
#vamos acalcular cuál es el promedio de diferentes variables en cada uno de los buffer o pasiajes


#para ello primero tengo que unir los paisajes en un solo objeto
buff_all <- bind_rows(buff_in, buff_out)

#la ventaja es que ya tienen identificadores únicos y puedo ver si cada uno es protejido o no
buff_all


#cargo variables
slope_tux <- rast("spatial/slope_tuxtlas.tif") #pendiente

road_tux <- rast("spatial/road_dis_tuxtlas.tif")  #distancia a carreteras

elev_tux <- rast("spatial/elevation_tuxtlas.tif") #altitud

citi_tux <- rast("spatial/cities_dis_tuxtlas.tif") #distancia a ciudades


#ahora vamos acrear una función que haga uso de la función exact_extract para calcular promedios de un raster en una zona
#sin embargo, la función que queremos nos pedmite editar muchas cosas, y saber a qué paisaje se refiere cada medición (id)
#además de etiquetar qué variable estamos midiendo

promedio_raster <- function(pol, ra, ch = "var"){
  
  df_pol <- pol %>% as.data.frame() #convierto el polígono a dataframe
  
  prom <- data.frame(exact_extract(ra, pol, "mean"), id= df_pol$id) #creo un df con el valor promedio del raster y el id
  
  colnames(prom) <- c(ch, "id") #le pongo nombre a la variable que estoy midiendo, y el id lo dejo como id

  return(prom) #devuelvo la medición
}


#medimos los promedios de cada variable por cada paisaje y lo devuelvo en un formato amigable con un id por paisaje
df_slope <- map(1:nrow(buff_all), ~ promedio_raster(buff_all[.x, ], slope_tux, "slope")) %>% 
  bind_rows()

df_slope

#medimos los promedios de cada variable por cada paisaje y lo devuelvo en un formato amigable con un id por paisaje
df_road <- map(1:nrow(buff_all), ~ promedio_raster(buff_all[.x, ], road_tux, "road")) %>% 
  bind_rows()

df_road


#medimos los promedios de cada variable por cada paisaje y lo devuelvo en un formato amigable con un id por paisaje
df_elev <- map(1:nrow(buff_all), ~ promedio_raster(buff_all[.x, ], elev_tux, "elev")) %>% 
  bind_rows()

df_elev


#medimos los promedios de cada variable por cada paisaje y lo devuelvo en un formato amigable con un id por paisaje
df_citi <- map(1:nrow(buff_all), ~ promedio_raster(buff_all[.x, ], citi_tux, "cities")) %>% 
  bind_rows()

df_citi

#juntamos toda la información en un solo objeto, número de parches y variables explicativas
df_frag_full <- left_join(df_frag, df_slope, by= "id") %>% 
  left_join(df_road) %>% 
  left_join(df_elev) %>% 
  left_join(df_citi)

df_frag_full



#designo los niveles de la variable
df_frag_full$prot <- factor(df_frag_full$prot, levels = c("Unprotected", "Protected"))

#constanto los niveles
levels(df_frag_full$prot)

#realizo un modelo linear generalizado para ver qué explica mejor el número de parches de bosque.
# Uso distribución de poisson por tratarse de datos de conteo
modelo_01 <- glm(np ~ prot +slope+road+elev+cities, data = df_frag_full, family = "poisson")

summary(modelo_01)#resultados del modelo


#visualizo los efectos de cada variable
cplot(modelo_01, "prot")
cplot(modelo_01, "slope")
cplot(modelo_01, "elev")
cplot(modelo_01, "cities")

#Finalmente calculamos el efecto marginal de la protección
#Ello es el efecto en mantener paisajes menos fragmentados  por parte de la reserva dejando fijas el resto de variables
margins(modelo_01, variables = "prot") 




