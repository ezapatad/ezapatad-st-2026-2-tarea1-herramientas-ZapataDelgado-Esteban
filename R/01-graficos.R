rm(list=ls())
#Se cargan la librerías permitidas
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(stats) #Librería usada para calcular pacf
source("C:/Users/betid/OneDrive/Documents/Universidad/Semestre 2026-2/Series de Tiempo Univariadas/Tareas/Tarea1/st-2026-2-tarea-1-herramientas-ZapataDelgado-Esteban/R/00-lectura.R")


#Función de usuario graficar_serie, que recibe las variables datos,tiulo
#datos : variable tipo tipple donde se guardan en columnas el tiempo (t), fechas (fecha) y el valor de la serie(y)
#La función retorna una grafica de la serie, con ejes rotulados y un caption donde se imprime la fuente y el número de observaciones
graficar_serie <- function(datos,titulo){
  
  #Se extraen los datos o atributos del objeto datos
  unidad = attr(datos,"unidad")
  frecuencia = attr(datos,"frecuencia")
  fuente = attr(datos,"fuente")
  y = datos$y
  fecha= datos$fecha
  
  
  #Se construyen los gráficos
  ggplot(datos, aes(x=fecha,y=y)) + 
    geom_line(color="blue") + 
    labs(tittle=titulo,x = "Tiempo [fecha]",y=paste("Serie[",unidad,"]"),caption = paste("Fuente:",fuente,"/ Número de observaciones:",nrow(datos)))
  
}

#Función de usuario correlograma, que recibe las variables datos, m
#datos: variable tipo tipple donde se guardan en columnas el tiempo (t), fechas (fecha) y el valor de la serie(y)
#m: Máximo rezago a tener en cuenta en la ACF
#La función retorna un gráfico partido, donde se muestra la ACF y la PACF, además de un mensaje de prevención para el error máximo cometido sobre
#la función acf de R
correlograma <- function(datos, m = NULL){
  #Se cargan los datos
  y = datos$y
  T_y = length(y)
  
  #Se verifica que m sea nulo, y se calcula
  if (!is.null(m) && m > (T_y - 1)){
    m = NULL
  }
  if (is.null(m)){
    m = min(floor(T_y / 4), 24)
  }
  
  #Se calcula los valores de la ACF
  #Se crea el arreglo o vector donde se almacenan los datos
  y_barra = mean(y)
  c0 = sum((y - y_barra)^2) / T_y
  acf_mano = c()
  
  #Con el for se calcula la ACF con divisor unico T
  for (k in 1:m) {
    ck <- sum((y[1:(T_y - k)] - y_barra) * (y[(k + 1):T_y] - y_barra)) / T_y
    acf_mano[k] <- ck / c0
  }
  
  #Se obtienen los valores de la pacf con la librería stats
  pacf_mano = as.numeric(pacf(y, lag.max = m, plot = FALSE)$acf)
  
  #Se busca la máxima diferencia absoluta de la ACF a mano y la ACF de R
  acf_R = as.numeric(acf(y, lag.max = m, plot = FALSE)$acf[-1])
  max_diff = max(abs(acf_mano - acf_R))
  cat("Máxima diferencia absoluta encontrada:", max_diff, "\n")
  
  #Se verifica que la máxima diferencia sea menor de 10^(-12)
  if(max_diff < 1e-12){
    cat("La diferencia máxima entre la acf a mano y en r, es menor a 10^-12\n")
  } else{
    warning("La diferencia máxima entre la acf a mano y en r, es mayor a 10^-12")
  }
  
  #Gráfica de las funciones de autocorrelación
  #Banda asintótica
  ci = 0.95
  banda = qnorm((1 + ci) / 2) / sqrt(T_y)
  
  df_grafico = tibble(h = 1:m, acf = acf_mano, pacf = pacf_mano)
  
  grafico1 = ggplot(df_grafico, aes(x = h, y = acf)) + 
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-banda, banda), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = h, yend = 0), color = "black") +
    labs(title = "Gráfica ACF", x = "Rezago (h)", y = "ACF") +
    scale_x_continuous(breaks = 1:m) +
    theme_minimal()
  
  grafico2 = ggplot(df_grafico, aes(x = h, y = pacf)) + 
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-banda, banda), linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = h, yend = 0), color = "black") +
    labs(title = "Gráfica PACF", x = "Rezago (h)", y = "PACF") +
    scale_x_continuous(breaks = 1:m) +
    theme_minimal()
  
  return(grafico1 / grafico2)
}


# #Ejemplo de aplicación funcion leer_serie()
# t = 1:36
# x = 45 + 2*t + 2*cos(pi/2*t)+rnorm(36)
# 
# x = ts(x,frequency = 12,start=c(2005,8))
# 
# datos_ejemplo = leer_serie(x,"DANE","pesos")
# 
# 
# graficar_serie(datos_ejemplo,"Hola mundo")
# correlograma(datos_ejemplo)
