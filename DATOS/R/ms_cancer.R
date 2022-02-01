####LIBRERÍAS####
library(readr)
library(plyr)
library(rakeR)

##Se abren los archivos requeridos
casen_gs <- read_delim("DATOS/CSV/casen_gs.csv",";",
                       locale = locale(encoding = "LATIN1"),
                       escape_double = FALSE, trim_ws = TRUE)


##Se genera df únicamente con las columnas de interés y se modifican los nombres
casen_DF = subset(casen_gs, select = c("comuna", "edad", "esc", "sexo", "e6a", "s28"))
names(casen_DF) = c("comuna","edad","escolaridad","sexo","e6a","enfermedad")

##Se imputan datos de escolaridad

#Vector que contiene las filas en donde se encuentran los NA de escolaridad
index_esc_na=which(is.na(casen_DF$escolaridad)==T) 

#Regresión lineal
educ_lm = lm(escolaridad ~ e6a, casen_DF[-index_esc_na,])

#Predicción
educ_pred = predict.lm(educ_lm, casen_DF[index_esc_na,])

#Se pasan los resultados a la columna del DF 
casen_DF$escolaridad = as.integer(casen_DF$escolaridad)
casen_DF$escolaridad[index_esc_na] = as.integer(educ_pred)

