
# IMPORTACION DE LIBRERIAS 
library(readr)
library(readxl)
library(dplyr)
library(dummies)       #no sirve
library(fastDummies)   #esta si
library(dummy)
library(scales)
library(stringr)
library(tidyr)
library(nortest)
library(ggplot2)

# IMPORTACION DEL DATASET
calidadAire <- read.delim("E:/este semestre UPTC/ELECTIVA I/segundo corte/proyecto/Mediciones_calidad_aire.csv",sep= "," , na.strings = c("", " ","  ","   ","     ", "null"))
View(calidadAire)

# TIPO DE VARIBALE DE CADA COLUMNA
class(calidadAire$id)
class(calidadAire$name)
class(calidadAire$recvTime)
class(calidadAire$NO2)
class(calidadAire$O3)
class(calidadAire$SO2)
class(calidadAire$CO)
class(calidadAire$CO2)
class(calidadAire$PM10)
class(calidadAire$PM25)
class(calidadAire$PM1)

# TAMAÑO DEL DATASET (FILAS Y COLUMNAS)
dim(calidadAire)

# RESUMEN ESTADISTICO
summary(calidadAire)

# TRANSFORMACION DE VARIABLES


# DIVISION DE LA COLUMNA O VARIABLE:  name -> (Sensor_Zona)
calidadAire<-separate(calidadAire,name,c("Sensor","Zona"))
# DIVISION DE LA COLUMNA O VARIABLE:  rcvTime -> (fecha T hora)
calidadAire<-separate(calidadAire,recvTime, c("fecha", "hora"), sep = "T")
# DIVISION DE LA COLUMNA O VARIABLE:  fecha -> (anio-mes-dia)
calidadAire<-separate(calidadAire,fecha, c("anio", "mes","dia"), sep = "-")
# DIVISION DE LA COLUMNA O VARIABLE:  hora -> (hora:minutos:segundos)
calidadAire<-separate(calidadAire,hora, c("hora","minutos","segundos"), sep = ":")
View(calidadAire)

# ELIMINAMOS EL CARACTER Z EN LOS SEGUNDOS
calidadAire$segundos<-gsub("Z", "", calidadAire$segundos)
View(calidadAire)


# REVISO EL TIPO DE VARAIBLE DE LAS NUEVAS VARIABLES
class(calidadAire$Sensor)
class(calidadAire$Zona)
class(calidadAire$anio)
class(calidadAire$mes)
class(calidadAire$dia)
class(calidadAire$hora)
class(calidadAire$minutos)
class(calidadAire$segundos)
# CONVIERTO LAS VARAIBLES QUE NECESITO
calidadAire$anio <- as.numeric(calidadAire$anio)
calidadAire$mes <- as.numeric(calidadAire$mes)
calidadAire$dia <- as.numeric(calidadAire$dia)
calidadAire$hora <- as.numeric(calidadAire$hora)
calidadAire$minutos <- as.numeric(calidadAire$minutos)
calidadAire$segundos <- as.numeric(calidadAire$segundos)

summary(calidadAire)


# MIRO LAS CATEGORIAS RESPECTO A LOS SENSORES
unique(calidadAire$Sensor)
# RESULTADO -> "SondaCO2","Particulas","Gases","SondaCO22","gases","particula","sonda"

# CON LO ANTERIOR VALIDO VALORES SEGUN EL TIPO DE SENSOR

# PARA GASES (NO2, O3, SO2, CO)
gases<-filter(calidadAire, Sensor==("Gases")|Sensor==("gases"))
gases<-select(gases,-CO2,-PM1,-PM10,-PM25)
View(gases)
# CALCULO DE VALORES NA EN CADA COLUMNA PARA LOS GASES
(sum(is.na(gases$NO2)))
(sum(is.na(gases$O3)))
(sum(is.na(gases$SO2)))
(sum(is.na(gases$CO)))
# COMO LOS NA DAN 0 NO NESECITO IMPUTAR 

# PARA SONDA (GAS CO2)
sonda<-filter(calidadAire, Sensor==("SondaCO2")|Sensor==("SondaCO22")|Sensor==("sonda"))
sonda<-select(sonda,-NO2,-O3,-SO2,-CO,-PM1,-PM10,-PM25)
View(sonda)
# CALCULO DE VALORES NA EN CADA COLUMNA O VARIABLE PARA SONDA CO2
(sum(is.na(sonda$CO2)))
# COMO LOS NA DAN 0 NO NESECITO IMPUTAR

# PARA PARTICULAS (PM1, PM10, PM25)
particulas<-filter(calidadAire, Sensor==("Particulas")|Sensor==("particula"))
particulas<-select(particulas,-NO2,-O3,-SO2,-CO,-CO2)
View(particulas)
# CALCULO DE VALORES NA EN CADA COLUMNA O VARIABLE PARA LAS PARTICULAS
(sum(is.na(particulas$PM10)))
(sum(is.na(particulas$PM25)))
(sum(is.na(particulas$PM1)))
# COMO LOS NA DAN 0 NO NESECITO IMPUTAR


# [ANALISIS BIVARIADO]


# PARA LA HIPOTESIS UNO ____________________________________________________________________________________

# CORRELACION DE VARIABLES (LINEALES)
# QUE MESES EXISTEN EN EL DATASET
unique(gases$mes)
# FILTRO LOS DATOS DE NOVIMBRE YA QUE ES EL MES A EVALUAR
gasesNoviembre<-filter(gases, mes==("11"))
View(gasesNoviembre)

# PASO 1 -> TEST O PRUEBA DE NORMALIDAD

# TEST KOLMOGOROV 
lillie.test(gasesNoviembre$dia)        # MAS DE 50 DATOS
lillie.test(gasesNoviembre$CO)         # MAS DE 50 DATOS

# PASO 2 -> ANALIZO EL RESULTADO DEL TEST

# P-VALOR -> MENOR A 0.05, LA DISTRIBUCION NO ES NORMAL
# POR TAL MOTIVO SE USAN LAS PRUEBAS NO PARAMETRICAS (SPEARMAN Y KENDALL)
# SI LA DISTRIBUCION FUESE NORMAL APLICO PRUEBAS PARAMETRICS (PEARSON)

# GENERO EL HISTOGRAMA -> PARA ANALIZAR EL COMPORTAMTIENTO DE LAS VARIABLES SELECCIONADAS

# VARAIBLE 1
hist(gasesNoviembre$dia)
# VARIABLE 2
hist(gasesNoviembre$CO)

# PASO 3 -> APLICO CORRELACION BASADOS EN LOS ANTERIORES PASOS
# DIA VS GAS CO
cor(x=gasesNoviembre$dia, y= gasesNoviembre$CO,method = "kendall")
cor(x=gasesNoviembre$dia, y= gasesNoviembre$CO,method = "spearman")

# PASO 4 -> EVALUO LAS DOS VARIABLES POR MEDIO DE UN GRAFICO PARA ENTENDER LA DISPERSION VISUALMENTE  

plot(x=gasesNoviembre$dia, y= gasesNoviembre$CO,col='red')

# FIN DEL ANALISIS UNO____________________________________________________________________________________



# PARA LA HIPOTESIS DOS____________________________________________________________________________________

# CORRELACION DE VARIABLES (LINEALES)

# PASO 1 -> TEST O PRUEBA DE NORMALIDAD

# TEST KOLMOGOROV 
lillie.test(gases$NO2)        # MAS DE 50 DATOS
lillie.test(gases$O3)        # MAS DE 50 DATOS

# PASO 2 -> ANALIZO EL RESULTADO DEL TEST

# P-VALOR -> MENOR A 0.05, LA DISTRIBUCION NO ES NORMAL
# POR TAL MOTIVO SE USAN LAS PRUEBAS NO PARAMETRICAS (SPEARMAN Y KENDALL)
# SI LA DISTRIBUCION FUESE NORMAL APLICO PRUEBAS PARAMETRICAS (PEARSON)

# GENERO EL HISTOGRAMA -> PARA ANALIZAR EL COMPORTAMTIENTO DE LAS VARIABLES SELECCIONADAS

# VARAIBLE 1
hist(gases$NO2)
# VARIABLE 2
hist(gases$O3)

# PASO 3 -> APLICO CORRELACION BASADOS EN LOS ANTERIORES PASOS
# NO2 VS GAS O3
cor(x=gases$NO2, y= gases$O3,method = "kendall")
cor(x=gases$NO2, y= gases$O3,method = "spearman")

# PASO 4 -> EVALUO LAS DOS VARIABLES POR MEDIO DE UN GRAFICO PARA ENTENDER LA DISPERSION VISUALMENTE  

plot(x=gases$NO2, y= gases$O3,col='blue')

# FIN DEL ANALISIS DOS____________________________________________________________________________________



# PARA LA HIPOTESIS TRES____________________________________________________________________________________

# CREO UNA COLUMNA QUE ME REPRESENTA LAS ZONAS CON UN EQUIVALENTE NUMERICO
particulas$ZonaNumerica <- as.numeric(factor(particulas$Zona, levels = unique(particulas$Zona)))
# CAMBIO DE POSICION EN EL DATASET DE LA COLUMNA CREADA
particulasReordenadas<-relocate(particulas, ZonaNumerica, .after = Sensor)
View(particulasReordenadas)

# CORRELACION DE VARIABLES (LINEALES)

# PASO 1 -> TEST O PRUEBA DE NORMALIDAD

# TEST KOLMOGOROV 
lillie.test(particulasReordenadas$ZonaNumerica)         # MAS DE 50 DATOS
lillie.test(particulasReordenadas$PM25)                 # MAS DE 50 DATOS

# PASO 2 -> ANALIZO EL RESULTADO DEL TEST

# P-VALOR -> MENOR A 0.05, LA DISTRIBUCION NO ES NORMAL
# POR TAL MOTIVO SE USAN LAS PRUEBAS NO PARAMETRICAS (SPEARMAN Y KENDALL)
# SI LA DISTRIBUCION FUESE NORMAL APLICO PRUEBAS PARAMETRICAS (PEARSON)

# GENERO EL HISTOGRAMA -> PARA ANALIZAR EL COMPORTAMTIENTO DE LAS VARIABLES SELECCIONADAS

# VARAIBLE 1
hist(particulasReordenadas$ZonaNumerica)
# VARIABLE 2
hist(particulasReordenadas$PM25)

# PASO 3 -> APLICO CORRELACION BASADOS EN LOS ANTERIORES PASOS
# ZONANUMERICA VS PARTICULA PM25
cor(x=particulasReordenadas$ZonaNumerica, y= particulasReordenadas$PM25,method = "kendall")
cor(x=particulasReordenadas$ZonaNumerica, y= particulasReordenadas$PM25,method = "spearman")

# PASO 4 -> EVALUO LAS DOS VARIABLES POR MEDIO DE UN GRAFICO PARA ENTENDER LA DISPERSION VISUALMENTE  

plot(x=particulasReordenadas$ZonaNumerica, y= particulasReordenadas$PM25,col='orange')

# FIN DEL ANALISIS TRES____________________________________________________________________________________


# PARA LA HIPOTESIS CUATRO____________________________________________________________________________________

# ANALIZO QUE HORAS SE TRABAJAN EN EL DATA SET
unique(gases$hora)

# CORRELACION DE VARIABLES (LINEALES)

# PASO 1 -> TEST O PRUEBA DE NORMALIDAD

# TEST KOLMOGOROV 
lillie.test(gases$hora)        # MAS DE 50 DATOS
lillie.test(gases$NO2)        # MAS DE 50 DATOS

# PASO 2 -> ANALIZO EL RESULTADO DEL TEST

# P-VALOR -> MENOR A 0.05, LA DISTRIBUCION NO ES NORMAL
# POR TAL MOTIVO SE USAN LAS PRUEBAS NO PARAMETRICAS (SPEARMAN Y KENDALL)
# SI LA DISTRIBUCION FUESE NORMAL APLICO PRUEBAS PARAMETRICAS (PEARSON)

# GENERO EL HISTOGRAMA -> PARA ANALIZAR EL COMPORTAMTIENTO DE LAS VARIABLES SELECCIONADAS

# VARAIBLE 1
hist(gases$hora)
# VARIABLE 2
hist(gases$NO2)

# PASO 3 -> APLICO CORRELACION BASADOS EN LOS ANTERIORES PASOS
# HORA VS GAS NO2
cor(x=gases$hora, y= gases$NO2,method = "kendall")
cor(x=gases$hora, y= gases$NO2,method = "spearman")

# PASO 4 -> EVALUO LAS DOS VARIABLES POR MEDIO DE UN GRAFICO PARA ENTENDER LA DISPERSION VISUALMENTE  

plot(x=gases$hora, y= gases$NO2,col='purple')

# FIN DEL ANALISIS CUATRO____________________________________________________________________________________



# PARA LA HIPOTESIS CINCO____________________________________________________________________________________


# CORRELACION DE VARIABLES (LINEALES)

# PASO 1 -> TEST O PRUEBA DE NORMALIDAD

# TEST KOLMOGOROV 
lillie.test(gases$mes)         # MAS DE 50 DATOS
lillie.test(gases$O3)          # MAS DE 50 DATOS

# PASO 2 -> ANALIZO EL RESULTADO DEL TEST

# P-VALOR -> MENOR A 0.05, LA DISTRIBUCION NO ES NORMAL
# POR TAL MOTIVO SE USAN LAS PRUEBAS NO PARAMETRICAS (SPEARMAN Y KENDALL)
# SI LA DISTRIBUCION FUESE NORMAL APLICO PRUEBAS PARAMETRICAS (PEARSON)

# GENERO EL HISTOGRAMA -> PARA ANALIZAR EL COMPORTAMTIENTO DE LAS VARIABLES SELECCIONADAS

# VARAIBLE 1
hist(gases$mes)
# VARIABLE 2
hist(gases$O3)

# PASO 3 -> APLICO CORRELACION BASADOS EN LOS ANTERIORES PASOS
# MES VS GAS O3
cor(x=gases$mes, y= gases$O3,method = "kendall")
cor(x=gases$mes, y= gases$O3,method = "spearman")

# PASO 4 -> EVALUO LAS DOS VARIABLES POR MEDIO DE UN GRAFICO PARA ENTENDER LA DISPERSION VISUALMENTE  

plot(x=gases$mes, y= gases$O3,col='black')

# FIN DEL ANALISIS CINCO____________________________________________________________________________________
