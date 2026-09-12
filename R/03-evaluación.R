rm(list=ls())
#Se cargan la librerías permitidas
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(patchwork)
source("C:/Users/betid/OneDrive/Documents/Universidad/Semestre 2026-2/Series de Tiempo Univariadas/Tareas/Tarea1/st-2026-2-tarea-1-herramientas-ZapataDelgado-Esteban/R/02-metodos.R")

#Función de usuario ljung_box() la cual depende de las variables r,T_obs,m,p
#r: arreglo númerico donde se guardan los valores de la acf
#T_obs: Tamaño de la serie en el periodo de entrenamiento (entero positivo)
#m: Tamaño de la ventana para evaluar la prueba Ljung-Box
#p: Número de parámetros estimados en el método usado
#Retorna: 
#lista donde se almacena la conclusión de la prueba, el estadpistico calculado,grados de libertad, valor crítico 
ljung_box <- function(r,T_obs,m,p){
  stopifnot("El tamaño de datos observado T debe ser positivo"= T_obs>0 && as.integer(T_obs),
            "El número de valores de autocorrelación m a tratar debe ser entero porsitivo" = m>0 && as.integer(m),
            "El número de parámetros p debe ser entero porsitivo" = p>0 && as.integer(p)
            ) 
  
  #Prueba H0: r1 = r2 = ... = rm = 0 vs H1: algún ri!=0
  #Creación del estadístico
  h = 1:m
  Qm = T_obs*(T_obs + 2)*sum((r[h]^2)/(T_obs-h))
  
  #Estadístico distrubuye chi2 con m-p grados de libertad
  
  #Región de rechazo
  X2_crit = qchisq(0.05,df = m-p,lower.tail=FALSE)
  
  #Valor-p
  p_val = pchisq(Qm,df = m-p,lower.tail = FALSE)
  
  
  if (p_val<0.05){
    if (X2_crit<Qm){
      conclusion = "Se rechaza Ho"  
    }
  } else {
    conclusion = "No se rechaza Ho"
  }
  resultados = list(estadistico = Qm, df = m-p,X2_crit = X2_crit,p_val = p_val,conclusion)
  
  return(resultados)
  
}




#Ejemplo
t = 1:100
x = 5 + 4*t +5.5*t^2+2*cos(pi/3*t) + rnorm(100)

x = ts(x,start = c(2005,8),frequency = 4)

leer_serie(x,"DANE","$")
