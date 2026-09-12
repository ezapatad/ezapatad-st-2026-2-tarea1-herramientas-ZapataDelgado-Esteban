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
ljung_box = function(r, T_obs, m, p = 0){
  stopifnot("El tamaño de datos observado T debe ser positivo" = is.numeric(T_obs) && T_obs > 0 && T_obs == as.integer(T_obs),
            "El número de valores de autocorrelación m a tratar debe ser entero porsitivo" = is.numeric(m) && m > 0 && m == as.integer(m),
            "El número de parámetros p debe ser entero porsitivo" = is.numeric(p) && p >= 0 && p == as.integer(p),
            "Los rezagos m deben ser menores a T_obs" = m < T_obs,
            "Grados de libertad m - p deben ser estrictamente positivos" = (m - p) > 0
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
  
  # Control de seguridad para evitar 'valor ausente donde TRUE/FALSE es necesario'
  if (!is.na(p_val) && p_val < 0.05){
    conclusion = "Se rechaza Ho"  
  } else {
    conclusion = "No se rechaza Ho"
  }
  resultados = list(estadistico = Qm, df = m-p, X2_crit = X2_crit, p_val = p_val, conclusion = conclusion)
  
  return(resultados)
}



##Función de usuario jarque_bera() la cual depende de las variables e
#e: vector de residuos del modelo usado
#Retorna: 
#lista donde se almacena la conclusión de la prueba, el estadistico calculado,grados de libertad, valor crítico 
jarque_bera = function(e) {
  stopifnot(
    "El vector de errores 'e' debe ser numérico." = is.numeric(e),
    "El vector 'e' no debe contener valores missing (NA)." = !any(is.na(e)),
    "El vector 'e' debe tener al menos 4 observaciones." = length(e) >= 4
  )
  
  N = length(e)
  e_centrado = e - mean(e)
  
  # Momentos centrales: Asimetría (A) y Kurtosis (K)
  m2 = mean(e_centrado^2)
  m3 = mean(e_centrado^3)
  m4 = mean(e_centrado^4)
  
  A = m3 / (m2^(3/2))
  K = m4 / (m2^2)
  
  # Estadístico JB = (N/6)*[A^2+((K-3)^2)/4]
  JB = (N/6)*(A^2+((K-3)^2)/4)
  
  df_val = 2
  X2_crit = qchisq(0.05, df = df_val, lower.tail = FALSE)
  p_val = pchisq(JB, df = df_val, lower.tail = FALSE)
  
  return(list(
    estadistico = JB,
    df = df_val,
    valor_critico_5pct = X2_crit,
    p_valor = p_val
  ))
}



#Ejemplo
t = 1:100
x = 5 + 1.5*t +0.3*t^2+2*cos(pi/3*t) + rnorm(100,4,36)

x = ts(x,start = c(2005,8),frequency = 4)

data = leer_serie(x,"DANE","$")
graficar_serie(data,"Gráfica serie")
ajustar_tendencia(data$y,tipo = "cuadratica")
p = ajustar_tendencia(data$y,tipo="cuadratica")$p

acf_mano = correlograma(datos = data)$acf_mano

ljung_box(r = acf_mano,T_obs = length(data$y),m = 24,p = p)
Box.test(data$y,lag=24,type="Ljung-Box",fitdf = p)
