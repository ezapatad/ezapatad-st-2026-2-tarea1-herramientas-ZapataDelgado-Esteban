rm(list=ls())
#Se cargan la librerías permitidas
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(patchwork)

#Funcion de usuario que recibe un conjunto de datos x cargados en ts o csv
#la función recibe las variables
#X: conjunto de datos escrito en ts o csv
#fuente: tipo de archivo cargado en forma de string {"ts", "csv" separados por ;}
#unidad: unidad de los datos
#La función retorna un objeto tipple donde se almacenan el tiempo (t) de manera discreta, las fechas y valores de la serie
#además guarda atributos sobre el objeto donde se almacenan la frecuencia, la fuente y la unidad de esta
leer_serie <- function(x, fuente, unidad) {
  
  #Se verifica la naturaleza del archivo
  if (inherits(x, "ts")) {
    
    #Se obtiene la frecuencia
    frecuencia <- frequency(x)
    y <- as.numeric(x)
    
    #Proceso para obtener las fechas en objetos ts
    if (frecuencia == 12) {
      fecha_inicio <- as.Date(paste(start(x)[1], start(x)[2], "01", sep = "-"))
      fecha <- seq.Date(from = fecha_inicio, length.out = length(x), by = "1 month")
      
    } else if (frecuencia == 4) {
      mes_inicio <- (start(x)[2] - 1) * 3 + 1
      fecha_inicio <- as.Date(paste(start(x)[1], mes_inicio, "01", sep = "-"))
      fecha <- seq.Date(from = fecha_inicio, length.out = length(x), by = "3 months")
      
    } else if (frecuencia == 1) {
      fecha_inicio <- as.Date(paste(start(x)[1], "01", "01", sep = "-"))
      fecha <- seq.Date(from = fecha_inicio, length.out = length(x), by = "1 year")
      
    } else if (frecuencia == 365 || frecuencia == 366) {
      fecha_inicio <- as.Date(paste0(start(x)[1], "-01-01")) + (start(x)[2] - 1)
      fecha <- seq.Date(from = fecha_inicio, length.out = length(x), by = "1 day") # Parentesis corregido
      
    } else if (frecuencia == 3) {
      mes_inicio <- (start(x)[2] - 1) * 4 + 1
      fecha_inicio <- as.Date(paste(start(x)[1], mes_inicio, "01", sep = "-"))
      fecha <- seq.Date(from = fecha_inicio, length.out = length(x), by = "4 months")
      
    } else {
      stop("¡¡ERROR!!, Frecuencia del objeto 'ts' no soportada.")
    }
    
  } else if (is.character(x) && endsWith(tolower(x), ".csv")) {
    data <- read.csv(x, sep = ";")
    
    fecha <- as.Date(data$fecha)
    y <- as.numeric(data$valor)
    
    dias_promedio <- median(as.numeric(diff(fecha)), na.rm = TRUE)
    
    if (dias_promedio >= 0.8 && dias_promedio <= 1.2) {
      frecuencia <- 365
    } else if (dias_promedio >= 28 && dias_promedio <= 31) {
      frecuencia <- 12
    } else if (dias_promedio >= 89 && dias_promedio <= 92) {
      frecuencia <- 4
    } else if (dias_promedio >= 120 && dias_promedio <= 123) {
      frecuencia <- 3
    } else if (dias_promedio >= 365 && dias_promedio <= 366) {
      frecuencia <- 1
    } else {
      stop("¡¡ERROR!!, La frecuencia detectada en el CSV no corresponde a un intervalo estándar.")
    }
    
  } else {
    stop("¡¡ERROR!!, x debe ser un objeto ts o una ruta a un archivo .csv")
  }
  
  # 2. VALIDACIONES DE FECHAS (CRECIENTES Y EQUIESPACIADAS)
  dias <- as.numeric(diff(fecha))
  
  #Si la diferencia de dos pares de días separados por una unidad de tiempo es menor o igual a cero
  if (any(dias <= 0)) {
    stop("¡¡ERROR!!, Las fechas no son crecientes, posible desorden en los datos.")
  }
  
  #Se verifica que la serie sea creciente
  if (frecuencia == 365 || frecuencia == 366 || frecuencia == 7) {
    if (any(dias != 1)) {
      stop("¡¡ERROR!!, Las fechas no corresponden a una serie diaria continua (hay días faltantes).")
    }
  } else if (frecuencia == 12) {
    if (any(dias < 28 | dias > 31)) {
      stop("¡¡ERROR!!, Las fechas no corresponden a una serie mensual continua.")
    }
  } else if (frecuencia == 4) {
    if (any(dias < 89 | dias > 92)) {
      stop("¡¡ERROR!!, Las fechas no corresponden a una serie trimestral continua.")
    }
  } else if (frecuencia == 3) {
    if (any(dias < 120 | dias > 123)) {
      stop("¡¡ERROR!!, Las fechas no corresponden a una serie cuatrimestral continua.")
    }
  } else if (frecuencia == 1) {
    if (any(dias < 365 | dias > 366)) {
      stop("¡¡ERROR!!, Las fechas no corresponden a una serie anual continua.")
    }
  }
  
  # 3. SALIDA EN FORMATO TIBBLE Y ASIGNACIÓN DE ATRIBUTOS
  lectura <- tibble(t = 1:length(y), fecha = fecha, y = y)
  
  attr(lectura, "frecuencia") <- frecuencia
  attr(lectura, "fuente") <- fuente
  attr(lectura, "unidad") <- unidad
  
  lectura
}


# #Ejemplo de aplicación funcion leer_serie()
# t = 1:36
# x = 45 + 2*t + 2*cos(2*pi/3*t)+rnorm(36)
# 
# x = ts(x,frequency = 12,start=c(2005,8))
# 
# datos_ejemplo = leer_serie(x,"DANE","pesos")
# attr(datos_ejemplo,"fuente")
# attr(datos_ejemplo,"frecuencia")
