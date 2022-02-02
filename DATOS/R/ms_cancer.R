####LIBRERÍAS####
library(readr)
library(plyr)
library(rakeR)
library(data.table)

####INPUTS####
casen_gs <- read_delim("DATOS/CSV/casen_gs.csv",";",
                       locale = locale(encoding = "LATIN1"),
                       escape_double = FALSE, trim_ws = TRUE)

censo_gs = read.csv2("DATOS/CSV/data_censo_gs_red.csv", sep=";")

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

##Pequeña limpieza##
censo_gs$COMUNA = factor(censo_gs$COMUNA) 

#Se genera una tabla que contenga la población de cada zona censal
POB_ZC = count(censo_gs,"GEOCODIGO")
POB_ZC$GEOCODIGO = as.character(POB_ZC$GEOCODIGO)

#Filtro para dejar únicamente mayores de 30 años
censo_gs = censo_gs[!(censo_gs$EDAD<30),]

###Agregación de información###

##Edad##

#Se generan dataframes con edad agregada por zona censal
pos=3
edad_30_40=aggregate(EDAD ~ GEOCODIGO+COMUNA,data = censo_gs,FUN=function (a) { length( which(a>=30&a<40)) })
edad_40_50=aggregate(EDAD ~ GEOCODIGO+COMUNA,data = censo_gs,FUN=function (a) { length( which(a>=40&a<50)) })
edad_50_60=aggregate(EDAD ~ GEOCODIGO+COMUNA,data = censo_gs,FUN=function (a) { length( which(a>=50&a<60)) })
edad_60_70=aggregate(EDAD ~ GEOCODIGO+COMUNA,data = censo_gs,FUN=function (a) { length( which(a>=60&a<70)) })
edad_70_80=aggregate(EDAD ~ GEOCODIGO+COMUNA,data = censo_gs,FUN=function (a) { length( which(a>=70&a<80)) })
edad_mayor_80=aggregate(EDAD ~ GEOCODIGO+COMUNA, data = censo_gs, FUN = function (a) { length( which(a>=80)) })

#se cambian los nombres de las columnas de edad de los distintos df's
names(edad_30_40)[pos]="edad_30_40"
names(edad_40_50)[pos]="edad_40_50"
names(edad_50_60)[pos]="edad_50_60"
names(edad_60_70)[pos]="edad_60_70"
names(edad_70_80)[pos]="edad_70_80"
names(edad_mayor_80)[pos]="edad_mayor_80"

##Escolaridad##

#Se generan dataframes con escolaridad agregada por zona censal
esco_0=aggregate(ESCOLARIDAD ~ GEOCODIGO+COMUNA, data = censo_gs,FUN= function (a) { length( which(a==0)) })
esco_1_8=aggregate(ESCOLARIDAD ~ GEOCODIGO+COMUNA, data = censo_gs,FUN = function (a) { length( which(a>0&a<=8)) })
esco_8_12=aggregate(ESCOLARIDAD ~ GEOCODIGO+COMUNA, data = censo_gs,FUN = function (a) { length( which(a>=9&a<=12)) })
esco_mayor_12=aggregate(ESCOLARIDAD ~ GEOCODIGO+COMUNA, data = censo_gs,FUN = function (a) { length( which(a>12)) })

#se cambian los nombres de las columnas de escolaridad de los distintos df's
names(esco_0)[pos]="esco_0"
names(esco_1_8)[pos]="esco_1_8"
names(esco_8_12)[pos]="esco_8_12"
names(esco_mayor_12)[pos]="esco_mayor_12"

##Sexo##

#Se generan dataframes con sexo agregado por zona censal
sexo_m=aggregate(SEXO ~ GEOCODIGO+COMUNA, data = censo_gs, FUN = function (a) { length( which(a==1)) })
sexo_f=aggregate(SEXO ~ GEOCODIGO+COMUNA, data = censo_gs, FUN = function (a) { length( which(a==2)) })

#se cambian los nombres de las columnas de sexo de los distintos df's
names(sexo_m)[pos]="sexo_m"
names(sexo_f)[pos]="sexo_f"

###Tabla Consolidada###

#Se consolidan todas las variables generadas anteriormente
tabla_consolidada=Reduce(function(x,y) merge(x = x, y = y, by = "GEOCODIGO"), list(edad_30_40,edad_40_50,edad_50_60,edad_60_70,edad_70_80,edad_mayor_80,esco_0,esco_1_8,esco_8_12,esco_mayor_12,sexo_f,sexo_m)) 

#Se seleccionan únicamente las columnas con las cuales se va a trabajar
tabla_consolidada=tabla_consolidada[,c(1,3,5,7,9,11,13,15,17,19,21,23,25,20)] 

#Se cambia el nombre de la columna de comuna
names(tabla_consolidada)[14]="COMUNA"

#Se transforma el campo de geocodigo a string
tabla_consolidada$GEOCODIGO=as.character(tabla_consolidada$GEOCODIGO)

#Se Separa el Data Frame en comunas
cons_censo_comunas=dlply(tabla_consolidada,.(COMUNA))


####MICROSIMULACIÓN####

#vector con columnas que se utilizarán para microsimular
vars=c("edad","escolaridad","sexo")

#el algoritmo le asigna un valor a la cantidad de hipertensos por zona censal 
#en base a los datos utilizados de la encuesta CASEN. El resultado se almacena
#en una lista de listas, en donde cada una de ellas pertence a una comuna.

sim_list = list()

for (i in 1:34){
  pesos=weight(cons = cons_censo_comunas[[i]][,-14], inds = casen_comunas[[i]][,-2],vars=vars)
  pesos_int=integerise(pesos, inds = casen_comunas[[i]][,-2])
  names(pesos_int)[7]="GEOCODIGO"
  sim_list[[i]]=pesos_int
}

#Se unen las listas 
sim_df = rbindlist(sim_list)

#Se elimina a columna de enfermedades
sim_df$enfermedad = NULL 

#Se agregan los casos de cancer a nivel zona censal
df_cancer = aggregate(cancer ~ GEOCODIGO, data= sim_df, FUN = function(a) {length(which(a==1))})

####TABLA FINAL####

#Se añade la columna de población a cada zona censal
cancer_zc = join(df_cancer,POB_ZC, "GEOCODIGO")

#Se cambia el nombre de columnas
colnames(cancer_zc) = c('geocodigo', 'pob_cancer', 'pob_tot')

#Tasa de gente con cáncer cada 100.000 personas
cancer_zc$tasa_cancer = cancer_zc$pob_cancer / cancer_zc$pob_tot * 100000

####OUTPUT####
write.csv(cancer_zc, 'SALIDAS/cancer_zc.csv', row.names = FALSE)

