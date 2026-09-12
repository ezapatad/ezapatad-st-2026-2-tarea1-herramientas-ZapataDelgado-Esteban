rm(list=ls())
#Se cargan la librerías permitidas
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(stats)
source("https://cdn.jsdelivr.net/gh/ezapatad/ezapatad-st-2026-2-tarea1-herramientas-ZapataDelgado-Esteban@main/R/00-lectura.R", encoding = "UTF-8")
source("https://cdn.jsdelivr.net/gh/ezapatad/ezapatad-st-2026-2-tarea1-herramientas-ZapataDelgado-Esteban@main/R/01-graficos.R", encoding = "UTF-8")
#Función de usuario ajustar_media() la cual depende de las variables y,h
#y: arreglo númerico que muestra los valores que toma la serie en los T primeros periodos
#h: número de periodos de tiempos luego de la región de entrenamiento, es decir pronóstico máximo Y_T+h
#Retorna: 
#yhat: vector de la misma longitud que y en el cual yhat[t] es el pronóstico de Yt hecho
# con la información disponible hasta t − 1; las posiciones de calentamiento son NA.
# pronosticar, una función de un entero h que devuelve los h pronósticos extramuestrales
# Y_T +1, . . . , Y_T+h a partir del último dato.
#parametros, lista nombrada con los parámetros usados y, cuando aplique, los estados finales
#del método.
ajustar_media <- function(y){
  stopifnot(
    "El argumento 'y' debe ser un vector numérico." = is.numeric(y),
    "El vector 'y' no debe contener valores missing (NA)." = !any(is.na(y)),
    "El vector 'y' debe tener al menos 2 observaciones." = length(y) >= 2
  )
  
  T_obs = length(y)
  
  # yhat[t] es la media acumulada de los datos hasta t-1
  yhat = c(NA, cumsum(y[1:(T_obs - 1)]) / 1:(T_obs - 1))
  
  pronosticar_f <- function(h){
    stopifnot("El horizonte 'h' debe ser un entero positivo." = is.numeric(h) && length(h) == 1 && h > 0 && h == as.integer(h))
    rep(mean(y), h)
  }
  
  parametros = list(metodo = "Media Simple", T_obs = T_obs, y_barra = mean(y))
  
  return(list(yhat = yhat, pronosticar = pronosticar_f, parametros = parametros,p=0))
}


#Función de usuario ajustar_mm() la cual depende de las variables y y k
#y: arreglo númerico que muestra los valores que toma la serie en los T primeros periodos
#k: Tamaño de ventana para realizar la media móvil
#Retorna: 
#yhat: vector de la misma longitud que y en el cual yhat[t] es el pronóstico de Yt hecho
# con la información disponible hasta t − 1; las posiciones de calentamiento son NA.
# pronosticar, una función de un entero h que devuelve los h pronósticos extramuestrales
# Y_T +1, . . . , Y_T+h a partir del último dato.
#parametros, lista nombrada con los parámetros usados y, cuando aplique, los estados finales
#del método.
ajustar_mm <- function(y, k){
  stopifnot(
    "El argumento 'y' debe ser un vector numérico." = is.numeric(y),
    "El vector 'y' no debe contener valores missing (NA)." = !any(is.na(y)),
    "El vector 'y' debe tener al menos 2 observaciones." = length(y) >= 2,
    "El tamaño de ventana 'k' debe ser un entero positivo." = is.numeric(k) && length(k) == 1 && k > 0 && k == as.integer(k),
    "La ventana 'k' no puede ser mayor que la longitud de la serie." = k <= length(y)
  )
  T_obs = length(y)
  
  #estimaciones en la zona de entreanmiento
  #Se crea las primeras k+1 estimaciones
  yhat = numeric(T_obs)
  yhat[1:k] = NA
  
  # Primer pronóstico disponible en t = k + 1 con la información disponible hasta t = k
  if (T_obs >= k + 1) {
    yhat[k + 1] <- mean(y[1:k])
    
    # Cálculo recursivo para t desde k + 2 hasta T_obs
    if (T_obs >= k + 2) {
      for (t in (k + 2):T_obs) {
        yhat[t] <- yhat[t-1] + 1/k*(y[t-1] - y[t-1-k])
      }
    }
  }
  
  #Se calcula la media movil para la zona de pronostico
  media_movil_final = mean(y[(T_obs - k + 1):T_obs])
  
  pronosticar_fx = function(h){
    stopifnot("El horizonte 'h' debe ser un entero positivo." = is.numeric(h) && length(h) == 1 && h > 0 && h == as.integer(h))
    rep(media_movil_final, h)
  }
  
  parametros = list(metodo = "mm", k = k, T_obs = T_obs, media_movil_final = media_movil_final)
  
  return(list(yhat = yhat, pronosticar = pronosticar_fx, parametros = parametros,p=0))
}
  

#Función de usuario ajustar_ses() la cual depende de las variables y y alpha
#y: arreglo númerico que muestra los valores que toma la serie en los T primeros periodos
#alpha: Parámetro alpha para la ponderacion del método
#Retorna: 
#yhat: vector de la misma longitud que y en el cual yhat[t] es el pronóstico de Yt hecho
# con la información disponible hasta t − 1; las posiciones de calentamiento son NA.
# pronosticar, una función de un entero h que devuelve los h pronósticos extramuestrales
# Y_T +1, . . . , Y_T+h a partir del último dato.
#parametros, lista nombrada con los parámetros usados y, cuando aplique, los estados finales
#del método.
ajustar_ses <- function(y, alpha) {
  stopifnot(
    "El argumento 'y' debe ser un vector numérico." = is.numeric(y),
    "El vector 'y' no debe contener valores missing (NA)." = !any(is.na(y)),
    "El vector 'y' debe tener al menos 2 observaciones." = length(y) >= 2,
    "El parámetro 'alpha' debe ser un escalar numérico." = is.numeric(alpha) && length(alpha) == 1,
    "El parámetro 'alpha' debe estar estrictamente en el intervalo (0, 1)." = alpha > 0 && alpha < 1
  )
  
  T_obs = length(y)
  
  yhat = numeric(T_obs)
  yhat[1] = NA
  yhat[2] = y[1]
  
  if (T_obs > 2) {
    for (t in 3:T_obs) {
      yhat[t] = alpha * y[t - 1] + (1 - alpha) * yhat[t - 1]
    }
  }

  yhat_final = alpha*y[T_obs] + (1-alpha)*yhat[T_obs]
  
  #SES escrito en la forma de correción de error
  yhat_err = numeric(T_obs)
  yhat_err[1] = NA
  yhat_err[2] = y[1]
  
  if (T_obs > 2) {
    for (t in 3:T_obs) {
      e_prev = y[t-1] - yhat_err[t-1]
      yhat_err[t] = yhat_err[t-1] + alpha*e_prev
    }
  }
  
  max_diff = max(abs(yhat[-1] - yhat_err[-1]))
  stopifnot("La forma de promedio ponderado y corrección de error no coinciden numéricamente." = max_diff < 1e-12)
  
  
  parametros = list(metodo = "ses",alpha = alpha,T_obs = T_obs,nivel_final = yhat_final)
  
  pronosticar_fx <- function(h) {
    stopifnot("El horizonte 'h' debe ser un entero positivo." = is.numeric(h) && length(h) == 1 && h > 0 && h == as.integer(h))
    rep(yhat_final, h)
  }
  

  return(list(yhat = yhat,pronosticar = pronosticar_fx,parametros = parametros),p=1)
}


#Función de usuario ajustar_dmm() la cual depende de las variables y y k
#y: arreglo númerico que muestra los valores que toma la serie en los T primeros periodos
#k: número entero positivo menor a T que indica el tamaño de la ventana en la zona de calentamiento
#Retorna: 
#yhat: vector de la misma longitud que y en el cual yhat[t] es el pronóstico de Yt hecho
# con la información disponible hasta t − 1; las posiciones de calentamiento son NA.
# pronosticar, una función de un entero h que devuelve los h pronósticos extramuestrales
# Y_T +1, . . . , Y_T+h a partir del último dato.
#parametros, lista nombrada con los parámetros usados y, cuando aplique, los estados finales
#del método.
ajustar_dmm <- function(y, k) {
  stopifnot(
    "El argumento 'y' debe ser un vector numérico." = is.numeric(y),
    "El vector 'y' no debe contener valores missing (NA)." = !any(is.na(y)),
    "El tamaño de ventana 'k' debe ser un entero mayor o igual a 2." = is.numeric(k) && length(k) == 1 && k >= 2 && k == as.integer(k),
    "La serie debe tener al menos (2k - 1) observaciones para el calentamiento." = length(y) >= (2 * k - 1)
  )
  
  T_obs = length(y)
  calentamiento = 2*k-1
  
  
  ajust_1 = ajustar_mm(y, k)
  
  #Se crea variable mm que guarde los valores no nulos y el valor para la zona de pronóstico
  mm = c(ajust_1$yhat[-1], ajust_1$parametros$media_movil_final)
  
  #Se crea arreglo donde se guardarán las dobles medias móviles
  dmm = numeric(T_obs)
  dmm[1:(calentamiento - 1)] = NA
  for (t in calentamiento:T_obs) {
    dmm[t] = mean(mm[(t-k+1):t])
  }

  yhat = numeric(T_obs)
  yhat[1:calentamiento] = NA
  
  if (T_obs > calentamiento) {
    for (t in (calentamiento + 1):T_obs) {
      prev_t = t - 1
      E_prev = 2 * mm[prev_t] - dmm[prev_t]
      beta1_prev = (2/(k-1))*(mm[prev_t] - dmm[prev_t])
      
      # Pronóstico 1-paso adelante
      yhat[t] = E_prev + beta1_prev*1
    }
  }
  
  E_T = 2*mm[T_obs] - dmm[T_obs]
  beta1_T = (2/(k - 1))*(mm[T_obs] - dmm[T_obs])
  
  parametros = list(metodo = "dmm",k = k,T_obs = T_obs,calentamiento = calentamiento,E_T = E_T,beta1_T = beta1_T)
  
  #Función interna de pronóstico
  pronosticar_fx = function(h) {
    stopifnot("El horizonte 'h' debe ser un entero positivo." = is.numeric(h) && length(h) == 1 && h > 0 && h == as.integer(h))
    vector_h = 1:h
    E_T + beta1_T*vector_h
  }

  return(list(yhat = yhat,pronosticar = pronosticar_fx,parametros = parametros,p=0))
}


#Función de usuario ajustar_tendencia() la cual depende de las variables y y tipo
#y: arreglo númerico que muestra los valores que toma la serie en los T primeros periodos
#tipo: string donde se almacena el tipo de tendencia a ajustar ('lineal', 'cuadratica','exponencial')
#Retorna: 
#yhat: vector de la misma longitud que y en el cual yhat[t] es el pronóstico de Yt hecho
# con la información disponible hasta t − 1; las posiciones de calentamiento son NA.
# pronosticar, una función de un entero h que devuelve los h pronósticos extramuestrales
# Y_T +1, . . . , Y_T+h a partir del último dato.
#parametros, lista nombrada con los parámetros usados y, cuando aplique, los estados finales
#del método.
ajustar_tendencia <- function(y, tipo = c("lineal", "cuadratica", "exponencial"), corregir_sesgo = FALSE) {
  stopifnot(
    "El argumento 'y' debe ser un vector numérico." = is.numeric(y),
    "El vector 'y' no debe contener valores missing (NA)." = !any(is.na(y)),
    "El vector 'y' debe tener al menos 3 observaciones." = length(y) >= 3
  )
  
  T_obs = length(y)
  t = 1:T_obs
  
  # Se transforma la tendencia exponencial a una lineal por medio del logaritmo
  # Yt=B0e^(B1*t) -----> Ln(Yt)=a + B1*t
  if (tipo == "exponencial") {
    stopifnot("La tendencia exponencial requiere valores strictly positivos (y > 0)." = all(y > 0))
    y_lm = log(y)
  } else {
    y_lm = y
  }
  
  # Se crea la matrix de diseño para el modelo lm
  if (tipo == "lineal") {
    X = cbind(rep(1, T_obs), t)
    p_para=2
  } else if (tipo == "cuadratica") {
    X = cbind(rep(1, T_obs), t, t^2)
    p_para=3
  } else if (tipo == "exponencial") {
    X = cbind(rep(1, T_obs), t)
    p_para=2
  }
  
  # Número de variables predictoras (incluye el intercepto)
  p = ncol(X)
  
  # Estimación de bheta por medio de MCO
  bheta_hat = as.vector(solve(crossprod(X), crossprod(X, y_lm)))
  
  # Estimaciones de la serie en la escala de estimación (lineal o logarítmica)
  Y_hat_reg = as.vector(X %*% bheta_hat)
  residuos = y_lm - Y_hat_reg
  
  # Pruebas de hipótesis
  # Sobre los residuos
  df_res = T_obs - p
  s2_ln = sum(residuos^2) / df_res
  
  # R2
  SST = sum((y_lm - mean(y_lm))^2)
  SSE = sum(residuos^2)
  R2 = 1 - (SSE / SST)
  
  # Prueba Durbin_Watson
  dw_stat = sum(diff(residuos)^2) / sum(residuos^2)
  
  # ----------------------------------------------------------------------------
  # Errores estándar Ordinarios y Robustos (HAC / Bartlett)
  # ----------------------------------------------------------------------------
  XtX_inv = solve(crossprod(X))
  se_ols = sqrt(diag(s2_ln * XtX_inv))
  
  # Rezagos según fórmula: floor(4 * (T / 100)^(2/9))
  m_lags = floor(4 * (T_obs / 100)^(2 / 9))
  
  # Matriz de covarianza HAC con Kernel de Bartlett
  V_matrix = matrix(0, nrow = p, ncol = p)
  for (i in 1:T_obs) {
    u_i = X[i, , drop = FALSE] * residuos[i]
    V_matrix = V_matrix + crossprod(u_i)
  }
  
  if (m_lags > 0) {
    for (l in 1:m_lags) {
      w_l = 1 - (l / (m_lags + 1))
      Gamma_l = matrix(0, nrow = p, ncol = p)
      for (i in (l + 1):T_obs) {
        u_i = X[i, , drop = FALSE] * residuos[i]
        u_il = X[i - l, , drop = FALSE] * residuos[i - l]
        Gamma_l = Gamma_l + crossprod(u_i, u_il)
      }
      V_matrix = V_matrix + w_l * (Gamma_l + t(Gamma_l))
    }
  }
  
  var_hac = XtX_inv %*% V_matrix %*% XtX_inv
  se_hac = sqrt(diag(var_hac))
  
  t_stat_hac = bheta_hat / se_hac
  p_val_hac = 2 * (1 - pt(abs(t_stat_hac), df = df_res))
  
  # Tabla de coeficientes
  if (tipo == "lineal"){
    nombres_coef = c("beta0", "beta1")
  } else if (tipo == "cuadratica"){
    nombres_coef = c("beta0", "beta1", "beta2")
  } else{
    nombres_coef = c("a (beta0)", "theta (beta1)")
  }
    
  
  tabla_coeficientes = tibble(coeficientes = nombres_coef,estimacion = bheta_hat,SE_Ordinario = se_ols,SE_Robusto_HAC = se_hac,Estadistico_t = t_stat_hac,Valor_p_HAC = p_val_hac)
  
  
  # Diagnósticos de Residuos (Ljung-Box con m - p gl y Jarque-Bera)
  m_lb = max(p + 1, floor(10 * log10(T_obs)))
  lb_test = Box.test(residuos, lag = m_lb, type = "Ljung-Box", fitdf = p)
  
  # Jarque-Bera manual para evitar librerías externas
  Asimetria = mean(residuos^3) / (mean(residuos^2)^(3/2))
  Kurtosis = mean(residuos^4) / (mean(residuos^2)^2)
  JB_estadistico = (T_obs / 6) * (Asimetria^2 + ((Kurtosis - 3)^2) / 4)
  JB_pval = 1 - pchisq(JB_estadistico, df = 2)
  
  #Factor de sesgo o correción y devolución de la serie logaritmica a la escala original
  if(tipo=="exponencial" && corregir_sesgo==TRUE){
    factor_sesgo = exp(s2_ln/2)
  } else {
    factor_sesgo = 1
  }
  
  Y_hat = if (tipo == "exponencial") {
    exp(Y_hat_reg) * factor_sesgo
  } else {
    Y_hat_reg
  }
  
  #Parametros usados
  parametros = list(
    metodo = paste("Tendencia", tipo),
    T_obs = T_obs,
    tabla_coeficientes = tabla_coeficientes,
    bheta_hat = bheta_hat,
    R2 = R2,
    s2_ln = s2_ln,
    dw_stat = dw_stat,
    corregir_sesgo = corregir_sesgo,
    diagnostico_residuos = list(
      ljung_box = list(estadistico = lb_test$statistic, p_valor = lb_test$p.value, df = m_lb - p),
      jarque_bera = list(estadistico = JB_estadistico, p_valor = JB_pval)
    )
  )
  
  pronosticar_f = function(h) {
    stopifnot("El horizonte 'h' debe ser un entero positivo." = is.numeric(h) && length(h) == 1 && h > 0 && h == as.integer(h))
    
    t_futuro = (T_obs + 1):(T_obs + h)
    
    if (tipo == "lineal") {
      X_fut = cbind(1, t_futuro)
      pred = as.vector(X_fut %*% bheta_hat)
    } else if (tipo == "cuadratica") {
      X_fut = cbind(1, t_futuro, t_futuro^2)
      pred = as.vector(X_fut %*% bheta_hat)
    } else if (tipo == "exponencial") {
      X_fut = cbind(1, t_futuro)
      pred = exp(as.vector(X_fut %*% bheta_hat))*factor_sesgo
    }
    
    return(pred)
  }
  
  return(list(yhat = Y_hat,pronosticar = pronosticar_f,parametros = parametros,p = p_para))
}

#Función de usuario ajustar_holt() la cual depende de las variables y, alpha y bheta
#y: arreglo númerico que muestra los valores que toma la serie en los T primeros periodos
#alpha: número tipo float entre 0 y 1, parámetros del modelo
#beta: número tipo float entre 0 y 1, parámetros del modelo
#Retorna: 
#yhat: vector de la misma longitud que y en el cual yhat[t] es el pronóstico de Yt hecho
# con la información disponible hasta t − 1; las posiciones de calentamiento son NA.
# pronosticar, una función de un entero h que devuelve los h pronósticos extramuestrales
# Y_T +1, . . . , Y_T+h a partir del último dato.
#parametros, lista nombrada con los parámetros usados y, cuando aplique, los estados finales
#del método
ajustar_holt = function(y, alpha, beta) {
  stopifnot(
    "El argumento 'y' debe ser un vector numérico." = is.numeric(y),
    "El vector 'y' no debe contener valores missing (NA)." = !any(is.na(y)),
    "El vector 'y' debe tener al menos 3 observaciones." = length(y) >= 3,
    "El parámetro 'alpha' debe ser un escalar numérico en (0, 1)." = is.numeric(alpha) && length(alpha) == 1 && alpha > 0 && alpha < 1,
    "El parámetro 'beta' debe ser un escalar numérico en (0, 1)." = is.numeric(beta) && length(beta) == 1 && beta > 0 && beta < 1
  )
  
  T_obs = length(y)
  
  # Forma de ecuaciones de nivel y pendiente (Promedio Ponderado)
  L = numeric(T_obs)
  Tend = numeric(T_obs)
  
  L[1] = y[1]
  Tend[1] = y[2] - y[1]
  
  for (t in 2:T_obs) {
    L[t] = alpha * y[t] + (1 - alpha) * (L[t - 1] + Tend[t - 1])
    Tend[t] = beta * (L[t] - L[t - 1]) + (1 - beta) * Tend[t - 1]
  }
  
  # Forma de corrección de error
  L_err = numeric(T_obs)
  Tend_err = numeric(T_obs)
  
  # CORRECCIÓN 1: Inicialización obligatoria
  L_err[1] = y[1]
  Tend_err[1] = y[2] - y[1]
  
  for (t in 2:T_obs) {
    yhat_prev = L_err[t - 1] + Tend_err[t - 1]
    et = y[t] - yhat_prev
    
    L_err[t] = L_err[t - 1] + Tend_err[t - 1] + alpha * et
    Tend_err[t] = Tend_err[t - 1] + alpha * beta * et
  }
  
  # Verificación de equivalencia numérica
  max_diff_L = max(abs(L - L_err))
  max_diff_T = max(abs(Tend - Tend_err))
  
  stopifnot(
    "La forma de promedio ponderado y de corrección de error para Nivel (L) no coinciden numéricamente." = max_diff_L < 1e-12,
    "La forma de promedio ponderado y de corrección de error para Pendiente (T) no coinciden numéricamente." = max_diff_T < 1e-12
  )
  
  # Estimación de y en la zona de entrenamiento (1-paso adelante)
  yhat = numeric(T_obs)
  yhat[1:2] = NA
  
  if (T_obs >= 3) {
    for (t in 3:T_obs) {
      yhat[t] = L[t - 1] + Tend[t - 1]
    }
  }
  
  # CORRECCIÓN 3: Normalización de variables para la lista y la subfunción
  L_final = L[T_obs]
  T_final = Tend[T_obs]
  
  parametros = list(
    metodo = "holt",
    alpha = alpha,
    beta = beta,
    T_obs = T_obs,
    L_t = L,
    T_hat_t = Tend,
    L_final = L_final,
    T_final = T_final,
    max_diff_L = max_diff_L,
    max_diff_T = max_diff_T
  )
  
  pronosticar_f = function(h) {
    stopifnot(
      "El horizonte 'h' debe ser un entero positivo." = is.numeric(h) && length(h) == 1 && h > 0 && h == as.integer(h)
    )
    vector_h = 1:h
    L_final + T_final * vector_h
  }
  
  return(list(yhat = yhat,pronosticar = pronosticar_f,parametros = parametros,p=2))
}


#Función de usuario optimizar() la cual depende de las variables y, metodo, y rejilla
#y: arreglo númerico que muestra los valores que toma la serie en los T primeros periodos
#metodo: string que indica el metodo utilizado {mm,dmm,ses,holt}
#rejilla: rango en el que se mueve el parametro del metodo
#Retorna: 
#rejilla_completa: rango donde se movió el parametro que se vario para optimizar
#optimo: valor(es) óptimo(s) del parametro(s)
#metodo: método utilizado (mismo que el ingresado como variable a la función),
#en_borde: variable booleana que retorna TRUE o FALSE si es óptimo encontrado se encuentra en un extremo de la rejilla,
#mensaje_borde: str de advertencia que indica si el óptimo encontrado esta en un extremo de la rejilla
#grafico: figura con ggplot donde se muestra el MSE contra la rejilla, varia segun el metodo empleado
optimizar = function(y, metodo = c("mm", "dmm", "ses", "holt"), rejilla = NULL) {
  
  stopifnot(
    "El argumento 'y' debe ser un vector numérico." = is.numeric(y),
    "El vector 'y' no debe contener valores missing (NA)." = !any(is.na(y))
  )
  
  # 1. Creación de la rejilla según el método
  if (is.null(rejilla)) {
    if (metodo %in% c("mm", "dmm")) {
      rejilla = expand.grid(k = 2:12)
    } else if (metodo == "ses") {
      rejilla = expand.grid(alpha = seq(0.02, 0.98, by = 0.02))
    } else if (metodo == "holt") {
      seq_par = seq(0.05, 0.95, by = 0.05)
      rejilla = expand.grid(alpha = seq_par, beta = seq_par)
    }
  }
  
  n = nrow(rejilla)
  mse_vec = numeric(n)
  
  # 2. Evaluación del modelo sobre la rejilla
  for (i in 1:n) {
    if (metodo == "mm") {
      fit = ajustar_mm(y, k = rejilla$k[i])
    } else if (metodo == "dmm") {
      fit = ajustar_dmm(y, k = rejilla$k[i])
    } else if (metodo == "ses") {
      fit = ajustar_ses(y, alpha = rejilla$alpha[i])
    } else if (metodo == "holt") {
      fit = ajustar_holt(y, alpha = rejilla$alpha[i], beta = rejilla$beta[i])
    }
    
    
    residuos = y - fit$yhat
    residuos_validos = residuos[!is.na(residuos)]
    
    
    mse_vec[i] = mean(residuos_validos^2)
  }
  
  # 3. Búsqueda del óptimo
  rejilla_completa = rejilla
  rejilla_completa$MSE = mse_vec
  
  # CORRECCIÓN 2: Uso consistente del índice ('indice_optimo')
  indice_optimo = which.min(mse_vec)
  optimo = rejilla_completa[indice_optimo, , drop = FALSE]
  
  # 4. Verificación de bordes
  en_borde = FALSE
  mensaje_borde = NULL
  
  if (metodo == "ses") {
    alpha_opt = optimo$alpha
    if (alpha_opt == min(rejilla$alpha) || alpha_opt == max(rejilla$alpha)) {
      en_borde = TRUE
      if (alpha_opt == max(rejilla$alpha)) {
        mensaje_borde = "ADVERTENCIA: alpha óptimo en el borde superior (alpha -> 1). Indica pronóstico ingenuo. La serie NO es estacionaria en media."
      } else {
        mensaje_borde = "ADVERTENCIA: alpha óptimo en el borde inferior (alpha -> 0). La serie favorece la media global constante."
      }
    }
  } else if (metodo == "holt") {
    alpha_opt = optimo$alpha
    beta_opt = optimo$beta
    if (alpha_opt %in% range(rejilla$alpha) || beta_opt %in% range(rejilla$beta)) {
      en_borde = TRUE
      mensaje_borde = "ADVERTENCIA: Uno o ambos parámetros (alpha, beta) se encuentran en el borde de la rejilla declarada."
    }
  }
  
  # 5. Generación del gráfico de la rejilla vs MSE
  grafico = NULL
  
  if (metodo %in% c("mm", "dmm")) {
    grafico = ggplot(rejilla_completa, aes(x = k, y = MSE)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "blue", size = 2) +
      geom_point(data = optimo, aes(x = k, y = MSE), color = "red", size = 4) +
      geom_vline(xintercept = optimo$k, linetype = "dashed", color = "red", alpha = 0.7) +
      scale_x_continuous(breaks = rejilla$k) +
      labs(
        title = paste0("Curva de MSE vs. k (", toupper(metodo), ")"),
        subtitle = paste0("Óptimo: k = ", optimo$k, " | MSE min = ", round(optimo$MSE, 4)),
        x = "Tamaño de Ventana (k)",
        y = "MSE (1-paso)"
      ) +
      theme_minimal()
    
  } else if (metodo == "ses") {
    p = ggplot(rejilla_completa, aes(x = alpha, y = MSE)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "blue", size = 2) +
      geom_point(data = optimo, aes(x = alpha, y = MSE), color = "red", size = 4) +
      geom_vline(xintercept = optimo$alpha, linetype = "dashed", color = "red", alpha = 0.7) +
      labs(
        title = expression(paste("Curva de MSE vs. ", alpha, " (SES)")),
        subtitle = paste0("Óptimo: alpha = ", optimo$alpha, " | MSE min = ", round(optimo$MSE, 4)),
        x = expression(alpha),
        y = "MSE (1-paso)"
      ) +
      theme_minimal()
    
    if (en_borde && optimo$alpha == max(rejilla$alpha)) {
      p = p + annotate(
        "text", x = optimo$alpha - 0.05, y = max(rejilla_completa$MSE),
        label = mensaje_borde, color = "red", fontface = "italic", hjust = 1, size = 3.5
      )
    }
    grafico = p
    
  } else if (metodo == "holt") {
    grafico = ggplot(rejilla_completa, aes(x = alpha, y = beta, fill = MSE)) +
      geom_tile() +
      geom_point(data = optimo, aes(x = alpha, y = beta), color = "red", shape = 4, size = 5, stroke = 2) +
      scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
      labs(
        title = expression(paste("Mapa de MSE sobre la Rejilla (", alpha, ", ", beta, ") — Holt Lineal")),
        subtitle = paste0("Óptimo: alpha = ", optimo$alpha, ", beta = ", optimo$beta, " | MSE min = ", round(optimo$MSE, 4)),
        x = expression(alpha),
        y = expression(beta),
        fill = "MSE"
      ) +
      theme_minimal() +
      theme(panel.grid = element_blank())
  }
  
  return(list(
    rejilla_completa = rejilla_completa,
    optimo = optimo,
    metodo = metodo,
    en_borde = en_borde,
    mensaje_borde = mensaje_borde,
    grafico = grafico
  ))
}


