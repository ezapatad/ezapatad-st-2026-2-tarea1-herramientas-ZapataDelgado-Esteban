\#Archivo README.md 

1. **Qué contiene el repositorio**



| Archivo | Contenido | Dependencias |

| :--- | :--- | :--- |

| `R/00-lectura.R` | Implementación de `leer\_serie()` para objetos `ts` o `.csv` con fechas equiespaciadas. | `base`, `stats`, `tibble` |

| `R/01-graficos.R` | Implementación de `graficar\_serie()` y `correlograma()` (ACF/PACF a mano, $m=\\min\\{\\lfloor T/4\\rfloor,24\\}$). | `ggplot2`, `patchwork`, `stats` |

| `R/02-metodos.R` | Ocho métodos de pronóstico (`ajustar\_media`, `ajustar\_mm`, `ajustar\_ses`, `ajustar\_dmm`, `ajustar\_tendencia` \[lineal, cuadrática, exponencial], `ajustar\_holt`) y función `optimizar()`. | `base` |

| `R/03-evaluacion.R` | Funciones `medidas()`, `ljung\_box()`, `jarque\_bera()`, `durbin\_watson()` y `validar\_errores()`. | `stats`, `ggplot2`, `patchwork` |

| `ejemplos/ejemplos.R` | Script automatizado que carga el paquete/funciones y corre los 8 ejemplos en orden. | `base`, `ggplot2` |

| `informe/informe.qmd` | Informe detallado en Quarto con el análisis riguroso de las 8 series y pruebas de hipótesis completas. | `base`, `ggplot2` |

| `informe/informe.html` | Documento renderizado del informe de la Tarea 1. | N/A |

| `figs/` | Carpeta de salida con todas las figuras generadas por `ejemplos.R`. | N/A |

| `sesion-info.txt` | Salida de `sessionInfo()` capturada automáticamente al finalizar la ejecución de la entrega. | `base` |





**2. ¿Cómo se corre?**



Para clonar el repositorio y ejecutar la verificación completa de forma automatizada desde una instalación limpia de R (versión 4.3 o superior), ejecute los siguientes comandos en su terminal o consola de R:



\# Clonar el repositorio y acceder a la carpeta

git clone \[https://github.com/tu-usuario/st-2026-2-tarea1-herramientas-apellidos-nombres.git](https://github.com/tu-usuario/st-2026-2-tarea1-herramientas-apellidos-nombres.git)

cd st-2026-2-tarea1-herramientas-apellidos-nombres





**3. Ejemplo de como usar las funciones**

\# a) Cargar módulos del repositorio

source("R/00-lectura.R")

source("R/01-graficos.R")

source("R/02-metodos.R")

source("R/03-evaluacion.R")



\# b) Cargar y estructurar la serie de tiempo 

datos = leer\_serie(Serie, fuente = "UniversidadNacional", unidad = "N")



\# c) Optimización de parámetros alpha y beta sobre la rejilla por MSE 1-paso

res\_opt = optimizar(datos$y, metodo = "holt")



\# d) Ajustar el modelo final con los parámetros óptimos encontrados

fit\_holt = ajustar\_holt(y = datos$y, alpha = res\_opt$optimo$alpha, beta = res\_opt$optimo$beta)



\# 5. Generar pronósticos extramuestrales a h = 5 pasos adelante

pronosticos = fit\_holt$pronosticar(h = 5)



\# Visualizar resultados

print(fit\_holt$parametros)

print(pronosticos)





**4. Convenciones**

1\. Inicializaciones:

&#x20;  - Media simple: Se inicializa de forma recursiva como Yhat\_2 = Y\_1.

&#x20;  - Media móvil: Calentamiento de k períodos (los primeros k valores toman NA).

&#x20;  - Suavizamiento exponencial simple (SES): Inicialización de nivel en Yhat\_2 = Y\_1.

&#x20;  - Doble media móvil: Calentamiento de 2k - 1 períodos (los primeros 2k - 1 valores toman NA).

&#x20;  - Tendencias (lineal, cuadrática, exponencial): Calentamiento de 0 observaciones. La versión exponencial linealizada ln(Y\_t) = a + theta\*t + e\_t produce la mediana

condicional exp(a\_hat + theta\_hat\*t)

&#x20;  - Holt lineal: Estado inicial de nivel L\_1 = Y\_1 y tendencia T\_1 = Y\_2 - Y\_1



2\. Divisor de la ACF y Banda de Confianza:

&#x20;  - Divisor único T

&#x20;  - Banda del Correlograma: Basada en el resultado asintótico de Bartlett para ruido blanco i.i.d.: +- z\_{1-alpha/2} / sqrt(n) aprox. +- 1.96 / sqrt(n), donde n representa la cantidad de datos de la serie o errores graficados (reproduciendo la línea exacta de plot.acf: qnorm((1 + ci)/2) / sqrt(x$n.used)).



3\. Escalamiento del MASE:

&#x20;  - El factor de escala corresponde al error absoluto medio del pronóstico ingenuo (naive o naive estacional) dentro del tramo de estimación:

&#x20;    Escala = (1 / (T\_train - 1)) \* sum\_{t=2}^{T\_train} |Y\_t - Y\_{t-1}|





**5. Uso de asistes de inteligencia artificial**

Para la función leer\_serie(), tuve un pequeño problema encontrando cómo obtener las fechas de las series cargadas con el objeto ts, llegando a lo siguiente:



fechas = as.Date(time(x))



Por lo anterior, lo que el programa me arrojó error, así que le pregunté a la IA cómo podría corregirlo y me entregó lo siguiente: 

if (frecuencia == 12) {

&#x20;     fecha\_inicio <- as.Date(paste(start(x)\[1], start(x)\[2], "01", sep = "-"))

&#x20;     fecha <- seq.Date(from = fecha\_inicio, length.out = length(x), by = "1 month")

&#x20;     

&#x20;   } else if (frecuencia == 4) {

&#x20;     mes\_inicio <- (start(x)\[2] - 1) \* 3 + 1

&#x20;     fecha\_inicio <- as.Date(paste(start(x)\[1], mes\_inicio, "01", sep = "-"))

&#x20;     fecha <- seq.Date(from = fecha\_inicio, length.out = length(x), by = "3 months")

&#x20;     

&#x20;   } else if (frecuencia == 1) {

&#x20;     fecha\_inicio <- as.Date(paste(start(x)\[1], "01", "01", sep = "-"))

&#x20;     fecha <- seq.Date(from = fecha\_inicio, length.out = length(x), by = "1 year")

&#x20;     

&#x20;   } else if (frecuencia == 365 || frecuencia == 366) {

&#x20;     fecha\_inicio <- as.Date(paste0(start(x)\[1], "-01-01")) + (start(x)\[2] - 1)

&#x20;     fecha <- seq.Date(from = fecha\_inicio, length.out = length(x), by = "1 day") # Parentesis corregido

&#x20;     

&#x20;   } else if (frecuencia == 3) {

&#x20;     mes\_inicio <- (start(x)\[2] - 1) \* 4 + 1

&#x20;     fecha\_inicio <- as.Date(paste(start(x)\[1], mes\_inicio, "01", sep = "-"))

&#x20;     fecha <- seq.Date(from = fecha\_inicio, length.out = length(x), by = "4 months")

&#x20;     

&#x20;   } else {

&#x20;     stop("¡¡ERROR!!, Frecuencia del objeto 'ts' no soportada.")

&#x20;   }





Se incurrió también en un error en la verificación de fechas equiespaciadas donde siempre se obtenía error, teniendo este código inicialmente

\#Fechas equiespaciadas

&#x20; if(length(unique(as.numeric(diff(fecha))))!=1){

&#x20;   stop("¡¡ERROR!!,Las fechas no son equiespaciadas")

&#x20; }



Y se llegó a esto:

dias <- as.numeric(diff(fecha))



if (frecuencia == 365 || frecuencia == 7) {

&#x20; # Serie DIARIA: la diferencia en días DEBE ser exactamente 1

&#x20; if (any(dias != 1)) {

&#x20;   stop("¡¡ERROR!!, Las fechas no corresponden a una serie diaria continua (hay días faltantes).")

&#x20; }



} else if (frecuencia == 12) {

&#x20; # Serie MENSUAL: cada mes dura entre 28 y 31 días

&#x20; if (any(dias < 28 | dias > 31)) {

&#x20;   stop("¡¡ERROR!!, Las fechas no corresponden a una serie mensual continua.")

&#x20; }



} else if (frecuencia == 4) {

&#x20; # Serie TRIMESTRAL: cada trimestre dura entre 89 y 92 días

&#x20; if (any(dias < 89 | dias > 92)) {

&#x20;   stop("¡¡ERROR!!, Las fechas no corresponden a una serie trimestral continua.")

&#x20; }



} else if (frecuencia == 3) {

&#x20; # Serie CUATRIMESTRAL: cada cuatrimestre dura entre 120 y 123 días

&#x20; if (any(dias < 120 | dias > 123)) {

&#x20;   stop("¡¡ERROR!!, Las fechas no corresponden a una serie cuatrimestral continua.")

&#x20; }



} else if (frecuencia == 1) {

&#x20; # Serie ANUAL: cada año dura 365 o 366 días (bisiesto)

&#x20; if (any(dias < 365 | dias > 366)) {

&#x20;   stop("¡¡ERROR!!, Las fechas no corresponden a una serie anual continua.")

&#x20; }

}



Al no tener una idea clara de como obtener la frecuencia para series cargadas en formato .csv, se le preguntó a la IA y se obtuvo:

\# CALCULAR LA FRECUENCIA EN EL CSV

&#x20;   dias <- as.numeric(diff(fecha))

&#x20; 

&#x20; if (any(dias <= 0)) {

&#x20;   stop("¡¡ERROR!!, Las fechas no son crecientes, posible desorden en los datos.")

&#x20; }

&#x20; 

&#x20; if (frecuencia == 365 || frecuencia == 366 || frecuencia == 7) {

&#x20;   if (any(dias != 1)) {

&#x20;     stop("¡¡ERROR!!, Las fechas no corresponden a una serie diaria continua (hay días faltantes).")

&#x20;   }

&#x20; } else if (frecuencia == 12) {

&#x20;   if (any(dias < 28 | dias > 31)) {

&#x20;     stop("¡¡ERROR!!, Las fechas no corresponden a una serie mensual continua.")

&#x20;   }

&#x20; } else if (frecuencia == 4) {

&#x20;   if (any(dias < 89 | dias > 92)) {

&#x20;     stop("¡¡ERROR!!, Las fechas no corresponden a una serie trimestral continua.")

&#x20;   }

&#x20; } else if (frecuencia == 3) {

&#x20;   if (any(dias < 120 | dias > 123)) {

&#x20;     stop("¡¡ERROR!!, Las fechas no corresponden a una serie cuatrimestral continua.")

&#x20;   }

&#x20; } else if (frecuencia == 1) {

&#x20;   if (any(dias < 365 | dias > 366)) {

&#x20;     stop("¡¡ERROR!!, Las fechas no corresponden a una serie anual continua.")

&#x20;   }

&#x20; }





Para el cálculo de la PACF con ayuda de la librería stats no fui capaz de obtener los valores numéricos de la PACF, con este código

\#Se obtienen los valores de la pacf con la librería stats

&#x20; pacf = pacf(y,lag.max=m,plot=FALSE)

Así que la IA me ayudó, y obtuve lo siguiente:

pacf(y, lag.max = m, plot = FALSE)$acf









\#Para el cálculo de la ACF a mano, el programa me estaba arrojando error, con el código:

acf\_mano = c()

&#x20; 

&#x20; #Con el for se ingresa la Corr(Y\_(t+k),Y\_t)

&#x20; for (k in 1:m) {

&#x20;   acf\_mano\[k] <- cor(y\[(k+1):length(y)],y\[1:(length(y)-k)],use = "complete.obs")

&#x20; }

La IA me lo corrigió y llegué a:

y\_barra = mean(y)

&#x20; c0 = sum((y - y\_barra)^2) / T\_y

&#x20; acf\_mano = c()

&#x20; 

&#x20; #Con el for se calcula la ACF con divisor unico T

&#x20; for (k in 1:m) {

&#x20;   ck <- sum((y\[1:(T\_y - k)] - y\_barra) \* (y\[(k + 1):T\_y] - y\_barra)) / T\_y

&#x20;   acf\_mano\[k] <- ck / c0

&#x20; }



En la inicialización de la function ajustar\_ses no el programa soltaba error, por lo que se le pidió ayuda a la IA para inicializar el o eliminar el error y arrojó:

&#x20;yhat\[2] <- y\[1]





En la función ajustar\_dmm() no supe como proceder a recorrer las medias móviles por lo que le pedí ayuda a la IA y obtuve:

for (t in calentamiento:T\_obs) {

&#x20;   dmm\[t] <- mean(mm\[(t - k + 1):t])

&#x20; }



&#x20; yhat <- numeric(T\_obs)

&#x20; yhat\[1:calentamiento] <- NA

&#x20; 

&#x20; if (T\_obs > calentamiento) {

&#x20;   for (t in (calentamiento + 1):T\_obs) {

&#x20;     prev\_t <- t - 1

&#x20;     E\_prev <- 2 \* mm\[prev\_t] - dmm\[prev\_t]

&#x20;     beta1\_prev <- (2 / (k - 1)) \* (mm\[prev\_t] - dmm\[prev\_t])

&#x20;     

&#x20;     # Pronóstico 1-paso adelante

&#x20;     yhat\[t] <- E\_prev + beta1\_prev \* 1

&#x20;   }

&#x20; }

&#x20; 

&#x20; E\_T <- 2 \* mm\[T\_obs] - dmm\[T\_obs]

&#x20; beta1\_T <- (2 / (k - 1)) \* (mm\[T\_obs] - dmm\[T\_obs])







En la función ajustar\_tendencia() se requirió error estándar robusto a autocorrelación y heterocedasticidad con núcleo

de Bartlett lo cual no lo había visto en ninguno de mis cursos anteriores por lo que opté por usar la IA y me arrojó:

XtX\_inv = solve(crossprod(X))

&#x20; se\_ols = sqrt(diag(s2\_ln \* XtX\_inv))

&#x20; 

&#x20; # Rezagos según fórmula: floor(4 \* (T / 100)^(2/9))

&#x20; m\_lags = floor(4 \* (T\_obs / 100)^(2 / 9))

&#x20; 

&#x20; # Matriz de covarianza HAC con Kernel de Bartlett

&#x20; V\_matrix = matrix(0, nrow = p, ncol = p)

&#x20; for (i in 1:T\_obs) {

&#x20;   u\_i = X\[i, , drop = FALSE] \* residuos\[i]

&#x20;   V\_matrix = V\_matrix + crossprod(u\_i)

&#x20; }

&#x20; 

&#x20; if (m\_lags > 0) {

&#x20;   for (l in 1:m\_lags) {

&#x20;     w\_l = 1 - (l / (m\_lags + 1))

&#x20;     Gamma\_l = matrix(0, nrow = p, ncol = p)

&#x20;     for (i in (l + 1):T\_obs) {

&#x20;       u\_i = X\[i, , drop = FALSE] \* residuos\[i]

&#x20;       u\_il = X\[i - l, , drop = FALSE] \* residuos\[i - l]

&#x20;       Gamma\_l = Gamma\_l + crossprod(u\_i, u\_il)

&#x20;     }

&#x20;     V\_matrix = V\_matrix + w\_l \* (Gamma\_l + t(Gamma\_l))

&#x20;   }

&#x20; }

&#x20; 

&#x20; var\_hac = XtX\_inv %\*% V\_matrix %\*% XtX\_inv

&#x20; se\_hac = sqrt(diag(var\_hac))

&#x20; 

&#x20; t\_stat\_hac = bheta\_hat / se\_hac

&#x20; p\_val\_hac = 2 \* (1 - pt(abs(t\_stat\_hac), df = df\_res))

&#x20; 



La función optimizar() tuve que apoyarme de la IA, debido a que inicialmente me acerque con el siguiente código:

optimizar = function(y, metodo, rejilla) {

&#x20; if (is.null(y)) {

&#x20;   stop("La serie y no puede ser nula")

&#x20; }

&#x20; 

&#x20;  if (missing(rejilla)) {

&#x20;   if (metodo == "mm") {

&#x20;     rejilla = data.frame(k = 2:12)

&#x20;   }

&#x20;   if (metodo == "ses") {

&#x20;     rejilla = data.frame(alpha = seq(0.02, 0.98, by = 0.02))

&#x20;   }

&#x20;   if (metodo == "holt") {

&#x20;      grid\_a = seq(0.05, 0.95, by = 0.05)

&#x20;     grid\_b = seq(0.05, 0.95, by = 0.05)

&#x20;     rejilla = data.frame(alpha = rep(grid\_a, each = length(grid\_b)),

&#x20;                          beta = rep(grid\_b, times = length(grid\_a)))

&#x20;   }

&#x20; }

&#x20; 

&#x20; # Vector para guardar resultados

&#x20; vector\_mse = c()

&#x20; 

&#x20; for (i in 1:nrow(rejilla)) {

&#x20;   

&#x20;   if (metodo == "mm") {

&#x20;     res\_fit = ajustar\_mm(y, k = rejilla$k\[i])

&#x20;   } else if (metodo == "ses") {

&#x20;     res\_fit = ajustar\_ses(y, alpha = rejilla$alpha\[i])

&#x20;   } else if (metodo == "holt") {

&#x20;     res\_fit = ajustar\_holt(y, alpha = rejilla$alpha\[i], beta = rejilla$beta\[i])

&#x20;   }

&#x20;   



&#x20;   residuos = y - res\_fit$yhat

&#x20;   mse\_i = mean(residuos^2) 

&#x20;   

&#x20;   vector\_mse = c(vector\_mse, mse\_i)  

&#x09;}

&#x20; 

&#x20;   rejilla$MSE = vector\_mse

&#x20; 

&#x20; 

&#x20; opt\_pos = min(rejilla$MSE)

&#x20; optimo = rejilla\[opt\_pos, ] 

&#x20; 

&#x20; # Revisar si cayo en el borde

&#x20; en\_borde = FALSE

&#x20; if (metodo == "ses") {

&#x20;   if (optimo$alpha == 0.98) {

&#x20;     en\_borde = TRUE

&#x20;     print("Atencion: Alpha dio en el borde superior, la serie no es estacionaria")

&#x20;   }

&#x20; }

&#x20; 

&#x20; # Graficos

&#x20; g = NULL

&#x20; 

&#x20; if (metodo == "ses") {

&#x20;   g = ggplot(rejilla, aes(x = alpha, y = MSE)) +

&#x20;     geom\_line() +

&#x20;     geom\_point() +

&#x20;      geom\_point(aes(x = optimo$alpha, y = optimo$MSE), color = "red") +

&#x20;     ggtitle("Optimizacion Alpha SES")

&#x20;     

&#x20; } else if (metodo == "holt") {

&#x20;    g = ggplot(rejilla, aes(x = alpha, y = MSE, color = as.factor(beta))) +

&#x20;     geom\_line() +

&#x20;     ggtitle("MSE en Holt para distintos Betas")

&#x20; }

&#x20; 

&#x20; # Retorno

&#x20; list(

&#x20;   tabla\_resultados = rejilla,

&#x20;   el\_optimo = optimo,

&#x20;   grafica = g

&#x20; )

}



Y obtuve el siguiente código mejorado:

optimizar = function(y, metodo = c("mm", "dmm", "ses", "holt"), rejilla = NULL) {

&#x20; 

&#x20; stopifnot(

&#x20;   "El argumento 'y' debe ser un vector numérico." = is.numeric(y),

&#x20;   "El vector 'y' no debe contener valores missing (NA)." = !any(is.na(y))

&#x20; )

&#x20; 

&#x20; # 1. Creación de la rejilla según el método

&#x20; if (is.null(rejilla)) {

&#x20;   if (metodo %in% c("mm", "dmm")) {

&#x20;     rejilla = expand.grid(k = 2:12)

&#x20;   } else if (metodo == "ses") {

&#x20;     rejilla = expand.grid(alpha = seq(0.02, 0.98, by = 0.02))

&#x20;   } else if (metodo == "holt") {

&#x20;     seq\_par = seq(0.05, 0.95, by = 0.05)

&#x20;     rejilla = expand.grid(alpha = seq\_par, beta = seq\_par)

&#x20;   }

&#x20; }

&#x20; 

&#x20; n = nrow(rejilla)

&#x20; mse\_vec = numeric(n)

&#x20; 

&#x20; # 2. Evaluación del modelo sobre la rejilla

&#x20; for (i in 1:n) {

&#x20;   if (metodo == "mm") {

&#x20;     fit = ajustar\_mm(y, k = rejilla$k\[i])

&#x20;   } else if (metodo == "dmm") {

&#x20;     fit = ajustar\_dmm(y, k = rejilla$k\[i])

&#x20;   } else if (metodo == "ses") {

&#x20;     fit = ajustar\_ses(y, alpha = rejilla$alpha\[i])

&#x20;   } else if (metodo == "holt") {

&#x20;     fit = ajustar\_holt(y, alpha = rejilla$alpha\[i], beta = rejilla$beta\[i])

&#x20;   }

&#x20;   

&#x20;   # CORRECCIÓN 1: Nombre de variable homogéneo ('residuos')

&#x20;   residuos = y - fit$yhat

&#x20;   residuos\_validos = residuos\[!is.na(residuos)]

&#x20;   

&#x20;   # MSE de 1-paso dentro de la muestra

&#x20;   mse\_vec\[i] = mean(residuos\_validos^2)

&#x20; }

&#x20; 

&#x20; # 3. Búsqueda del óptimo

&#x20; rejilla\_completa = rejilla

&#x20; rejilla\_completa$MSE = mse\_vec

&#x20; 

&#x20; # CORRECCIÓN 2: Uso consistente del índice ('indice\_optimo')

&#x20; indice\_optimo = which.min(mse\_vec)

&#x20; optimo = rejilla\_completa\[indice\_optimo, , drop = FALSE]

&#x20; 

&#x20; # 4. Verificación de bordes

&#x20; en\_borde = FALSE

&#x20; mensaje\_borde = NULL

&#x20; 

&#x20; if (metodo == "ses") {

&#x20;   alpha\_opt = optimo$alpha

&#x20;   if (alpha\_opt == min(rejilla$alpha) || alpha\_opt == max(rejilla$alpha)) {

&#x20;     en\_borde = TRUE

&#x20;     if (alpha\_opt == max(rejilla$alpha)) {

&#x20;       mensaje\_borde = "ADVERTENCIA: alpha óptimo en el borde superior (alpha -> 1). Indica pronóstico ingenuo. La serie NO es estacionaria en media."

&#x20;     } else {

&#x20;       mensaje\_borde = "ADVERTENCIA: alpha óptimo en el borde inferior (alpha -> 0). La serie favorece la media global constante."

&#x20;     }

&#x20;   }

&#x20; } else if (metodo == "holt") {

&#x20;   alpha\_opt = optimo$alpha

&#x20;   beta\_opt = optimo$beta

&#x20;   if (alpha\_opt %in% range(rejilla$alpha) || beta\_opt %in% range(rejilla$beta)) {

&#x20;     en\_borde = TRUE

&#x20;     mensaje\_borde = "ADVERTENCIA: Uno o ambos parámetros (alpha, beta) se encuentran en el borde de la rejilla declarada."

&#x20;   }

&#x20; }

&#x20; 

&#x20; # 5. Generación del gráfico de la rejilla vs MSE

&#x20; grafico = NULL

&#x20; 

&#x20; if (metodo %in% c("mm", "dmm")) {

&#x20;   grafico = ggplot(rejilla\_completa, aes(x = k, y = MSE)) +

&#x20;     geom\_line(color = "steelblue", linewidth = 1) +

&#x20;     geom\_point(color = "steelblue", size = 2) +

&#x20;     geom\_point(data = optimo, aes(x = k, y = MSE), color = "red", size = 4) +

&#x20;     geom\_vline(xintercept = optimo$k, linetype = "dashed", color = "red", alpha = 0.7) +

&#x20;     scale\_x\_continuous(breaks = rejilla$k) +

&#x20;     labs(

&#x20;       title = paste0("Curva de MSE vs. k (", toupper(metodo), ")"),

&#x20;       subtitle = paste0("Óptimo: k = ", optimo$k, " | MSE min = ", round(optimo$MSE, 4)),

&#x20;       x = "Tamaño de Ventana (k)",

&#x20;       y = "MSE (1-paso)"

&#x20;     ) +

&#x20;     theme\_minimal()

&#x20;   

&#x20; } else if (metodo == "ses") {

&#x20;   p = ggplot(rejilla\_completa, aes(x = alpha, y = MSE)) +

&#x20;     geom\_line(color = "blue", linewidth = 1) +

&#x20;     geom\_point(color = "blue", size = 2) +

&#x20;     geom\_point(data = optimo, aes(x = alpha, y = MSE), color = "red", size = 4) +

&#x20;     geom\_vline(xintercept = optimo$alpha, linetype = "dashed", color = "red", alpha = 0.7) +

&#x20;     labs(

&#x20;       title = expression(paste("Curva de MSE vs. ", alpha, " (SES)")),

&#x20;       subtitle = paste0("Óptimo: alpha = ", optimo$alpha, " | MSE min = ", round(optimo$MSE, 4)),

&#x20;       x = expression(alpha),

&#x20;       y = "MSE (1-paso)"

&#x20;     ) +

&#x20;     theme\_minimal()

&#x20;   

&#x20;   if (en\_borde \&\& optimo$alpha == max(rejilla$alpha)) {

&#x20;     p = p + annotate(

&#x20;       "text", x = optimo$alpha - 0.05, y = max(rejilla\_completa$MSE),

&#x20;       label = mensaje\_borde, color = "red", fontface = "italic", hjust = 1, size = 3.5

&#x20;     )

&#x20;   }

&#x20;   grafico = p

&#x20;   

&#x20; } else if (metodo == "holt") {

&#x20;   grafico = ggplot(rejilla\_completa, aes(x = alpha, y = beta, fill = MSE)) +

&#x20;     geom\_tile() +

&#x20;     geom\_point(data = optimo, aes(x = alpha, y = beta), color = "red", shape = 4, size = 5, stroke = 2) +

&#x20;     scale\_x\_continuous(breaks = seq(0, 1, by = 0.1)) +

&#x20;     scale\_y\_continuous(breaks = seq(0, 1, by = 0.1)) +

&#x20;     labs(

&#x20;       title = expression(paste("Mapa de MSE sobre la Rejilla (", alpha, ", ", beta, ") — Holt Lineal")),

&#x20;       subtitle = paste0("Óptimo: alpha = ", optimo$alpha, ", beta = ", optimo$beta, " | MSE min = ", round(optimo$MSE, 4)),

&#x20;       x = expression(alpha),

&#x20;       y = expression(beta),

&#x20;       fill = "MSE"

&#x20;     ) +

&#x20;     theme\_minimal() +

&#x20;     theme(panel.grid = element\_blank())

&#x20; }

&#x20; 

&#x20; return(list(

&#x20;   rejilla\_completa = rejilla\_completa,

&#x20;   optimo = optimo,

&#x20;   metodo = metodo,

&#x20;   en\_borde = en\_borde,

&#x20;   mensaje\_borde = mensaje\_borde,

&#x20;   grafico = grafico

&#x20; ))

}

