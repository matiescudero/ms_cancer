####LIBRERÍAS####
library(readr)
library(plyr)
library(rakeR)

####INPUTS####
casen_gs <- read_delim("DATOS/CSV/casen_gs.csv",";",
                       locale = locale(encoding = "LATIN1"),
                       escape_double = FALSE, trim_ws = TRUE)

####CASEN####

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

#Se eliminan los registros en donde la gente no respondió
casen_DF = casen_DF[!(casen_DF$enfermedad==99),]

#Se genera columna booleana en la que se indica si la persona ha estado en 
#tratamiento por algún tipo de cáncer o no (incluye todos tipos de cáncer y leucemia)
casen_DF$cancer = ifelse(casen_DF$enfermedad %in% c(8,10:14,18), 1,0)

#Se limita la edad de los registros, ya que únicamente el 6% de las personas en
#tratamiento por cancer tienen menos de 30 años
casen_DF = casen_DF[!(casen_DF$edad<30),]


###Reclasificación###

##Escolaridad##

#Se generan rangos para los años de escolaridad
index_0=which(casen_DF$escolaridad==0)
index_1_8=which(casen_DF$escolaridad>0&casen_DF$escolaridad<=8)
index_8_12=which(casen_DF$escolaridad>8&casen_DF$escolaridad<=12)

#Se reclasifica la variable de escolaridad
casen_DF$escolaridad[index_0]="esco_0" 
casen_DF$escolaridad[index_1_8]="esco_1_8"
casen_DF$escolaridad[index_8_12]="esco_8_12"
casen_DF$escolaridad[-c(index_0,index_1_8,index_8_12)]="esco_mayor_12"

#Se transforma la columna a string 
casen_DF$escolaridad = as.character(casen_DF$escolaridad)


##Edad##

#Se generan rangos etarios
index_30_40 = which(casen_DF$edad>=30&casen_DF$edad<40)
index_40_50 = which(casen_DF$edad>=40&casen_DF$edad<50)
index_50_60 = which(casen_DF$edad>=50&casen_DF$edad<60)
index_60_70 = which(casen_DF$edad>=60&casen_DF$edad<70)
index_70_80 = which(casen_DF$edad>=70&casen_DF$edad<80)


#Se reclasifica la edad en base al rango etario preestablecido
casen_DF$edad[index_30_40]="edad_30_40"
casen_DF$edad[index_40_50]="edad_40_50"
casen_DF$edad[index_50_60]="edad_50_60"
casen_DF$edad[index_60_70]="edad_60_70"
casen_DF$edad[index_70_80]="edad_70_80"
casen_DF$edad[-c(index_30_40,index_40_50,index_50_60,index_60_70,index_70_80)]="edad_mayor_80"

#Se transforma la columna a string
casen_DF$edad = as.character(casen_DF$edad)


##Sexo##

#index qué indica posición de registros cuyo sexo sea femenino
index_mujer_casen=which(casen_DF$sexo==2)

#Se cambian los 2 por "sexo_f" y los 1 por "sexo_m"
casen_DF$sexo= as.character(casen_DF$sexo)
casen_DF$sexo[index_mujer_casen]="sexo_f"
casen_DF$sexo[-index_mujer_casen]="sexo_m"

#Se transforma la columna a string
casen_DF$sexo = as.character(casen_DF$sexo)


##DF final para CASEN##

#Se añade ID a cada registro
casen_DF$ID = as.character(seq.int(nrow(casen_DF)))

#Se dejan únicamente las columnas de interés
casen_DF = casen_DF[,c("ID","comuna","edad","escolaridad","sexo","enfermedad","cancer")]


#Se separa la casen generada por comunas
casen_comunas = dlply(casen_DF,.(comuna))

####CENSO####
