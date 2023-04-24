# script para hacer nubes de palabras sobre temas estrategicos ############

# librerias ###########

library(tidyverse)
library(tidytext) 
library(tm)
#library(wordcloud) # la llamo despues, pero se necesita instalada
#library(readxl) # la llamo despues, pero se necesita instalada
library(reshape2)

# lectura de datos ###########################

# nota: respecto del excel original, se hicieron tres modificaciones menores a la base
# 1: se eliminaron las filas de "titulo" que se encontraban sobre la base que contiene los temas estrategicos
# 2: se eliminó una fila vacía entre los nombres de las columnas de la tabla ("institución", "tema 1", etc) y el contenido de dichas columnas
# 3: se movió de lugar un tema estratégico de INVAP ubicado en una celda unificada entre temas 1-tema 4, se lo colocó en la casilla tema 5, que estaba vacía

datos_originales <- readxl::read_excel("TEMAS ESTRATEGICOS - CA CCT PN.xlsx")

# transformaciones a los datos #############

# requerimos pasar la data a formato long si queremos su analis conjunto 

data_long <- datos_originales %>% 
  pivot_longer(cols = starts_with("TEMA"),
               names_to = "NR TEMA",
               values_to = "TEXTO") %>% 
  subset(!is.na(TEXTO)) # eliminamos celdas vacías, correspondientes a instituciones que enviaron menos de 6 temas

# usamos funcion predeterminada para extraer una fila por palabra de nuestra variable TEXTO, la función es del paquete "tidytext"

data_long_palabras <- data_long %>% 
  unnest_tokens(PALABRAS, TEXTO)

# filtramos palabras muy comunes. Esto lo hacemos con auxilio de una base predeterminada en español, del paquete "tm"

palabras_a_borrar <- stopwords("spanish")

data_long_palabras_limpia <- data_long_palabras %>% 
  subset(!PALABRAS%in%palabras_a_borrar)

# finalmente creamos una matriz con la cuenta de la frequencia de las palabras
data_long_palabras_limpia_matriz <- data_long_palabras_limpia   %>% 
  count(PALABRAS, sort=TRUE)
        
# creamos nube #############

# simple 

set.seed(1234) # para reproductibilidad 

wordcloud::wordcloud(words = data_long_palabras_limpia_matriz$PALABRAS, 
                     freq = data_long_palabras_limpia_matriz$n, 
                     min.freq = 1,           
                     max.words=200, 
                     random.order=FALSE, 
                     rot.per=0.35,            
                     colors="blue")


# separado por institucion. queda feo porque son demasiadas instituciones

data_long_palabras_limpia_matriz_agrupada <- data_long_palabras_limpia  %>%
  group_by(PALABRAS, INSTITUCION) %>% 
  dplyr::mutate(n =  dplyr::n()) %>% 
  acast(PALABRAS ~ INSTITUCION, value.var = "n", fill = 0) # un formato particular requerido

wordcloud::comparison.cloud(data_long_palabras_limpia_matriz_agrupada ,
                           # colors = c("lightblue", "blue"),
                            max.words = 100,
                            title.size=NULL)