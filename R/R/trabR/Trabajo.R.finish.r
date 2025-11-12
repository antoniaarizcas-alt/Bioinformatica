# AntoniaArizbaleta_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento
getwd()
# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
# Cargar librería 'readr' para leer archivos CSV
install.packages("readr")
library(readr)

# Definir ruta al archivo CSV (incluyendo el nombre del archivo)
ruta_archivo <- "/Users/macbookair/Desktop/trabR/datos_biomed.csv"

# Leer archivo CSV y guardarlo en data frame 'datos_biomed'
datos_biomed <- read_csv(ruta_archivo)

# Mostrar  primeras filas para verificar
head(datos_biomed)

# Mostrar estructura para ver el tipo de datos y columnas
str(datos_biomed)


# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
# Mostrar primeras filas para ver cómo se ven los datos
head(datos_biomed)

# Obtener resumen estadístico por columnas
summary(datos_biomed)

# Obtener dimensiones del data frame: filas y columnas
dim(datos_biomed)  # filas = tratamientos, columnas = variables

# Obtener estructura completa: tipo de columnas y ejemplo de datos
str(datos_biomed)
# Se muestran las columnas del data frame, sus tipos de datos y ejemplos de valores

# Extraer el número de variables (columnas)
num_variables <- ncol(datos_biomed)
cat("Número de variables:", num_variables, "\n")

# Extraer número de tratamientos (filas)
num_tratamientos <- nrow(datos_biomed)
cat("Número de tratamientos:", num_tratamientos, "\n")


# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
# Cargar librería ggplot2 para gráficas avanzadas
install.packages("ggplot2")
library(ggplot2)

# Convertir datos a formato "largo" para ggplot2 con tidyr
install.packages("tidyr")
library(tidyr)

# Seleccionar columnas numéricas (Glucosa, Presion, Colesterol) y tratamiento
datos_largos <- datos_biomed %>%
  pivot_longer(cols = c(Glucosa, Presion, Colesterol),
               names_to = "Variable",
               values_to = "Valor")

# Crear boxplot: Valor vs Tto, separado por Variable
ggplot(datos_largos, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y") +  # Un boxplot por variable
  theme_minimal() +                           # minimalista limpio
  labs(title = "Boxplots por tratamiento para cada variable",
       x = "Tratamiento",
       y = "Valor")
# FarmacoA tiende a elevar colesterol, 
# Glucosa se mantiene similar en los tres grupos, 
# FarmacoB aumenta ligeramente la presión. 
# En general, no parece haber mejora clínica clara frente al placebo.


# 4. Realiza un violin plot (investiga qué es). (1 pt)
# Usar los datos en formato largo (datos_largos)
# 'Variable' = tipo de medición, 'Valor' = valor numérico, 'Tratamiento' = grupo

ggplot(datos_largos, aes(x = Tratamiento, y = Valor, fill = Tratamiento)) +
  geom_violin(trim = FALSE) +                   # violin plot, trim=FALSE muestra todo el rango
  geom_boxplot(width = 0.1, fill = "white") +   # boxplot mediana y rango intercuartílico
  facet_wrap(~Variable, scales = "free_y") +    # para separar por variable
  theme_minimal() +                             # minimalista limpio
  labs(title = "Violin plots por tratamiento para cada variable",
       x = "Tratamiento",
       y = "Valor")
# los fármacos reducen ligeramente glucosa y presión arterial,
# no pareces afectan el colesterol. 
# Cada plot indica la dispersión de los datos
# para comparar la distribución entre tratamientos.


# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
# Definir colores para cada tto
colores <- c("FarmacoA" = "blue", "FarmacoB" = "green", "Placebo" = "red")

# Crear gráfico base
plot(datos_biomed$Glucosa, datos_biomed$Presion,
     col = colores[datos_biomed$Tratamiento],  # Colorear por tto
     pch = 16,                                 # Tipo de punto sólido
     xlab = "Glucosa",                         # Etiqueta eje X
     ylab = "Presión",                         # Etiqueta eje Y
     main = "Dispersión Glucosa vs Presión")   # Título del gráfico

# Agregar leyenda en esq inf der
legend("bottomright",                                         # Posición
       legend = levels(as.factor(datos_biomed$Tratamiento)),  # Nombres ttos
       col = c("blue","green","red"),                         # Colores de cada tto
       pch = 16)                                              # Tipo de símbolo usado en el plot
# Muestra relación entre glucosa y presión arterial diferenciada por tto. 
# La mezcla de puntos muestra que no hay una tendencia clara
# permite comparar la variabilidad entre FármacoA, B y placebo

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
# Cargar ggplot2
library(ggplot2)

# Crear gráfico con facetas por tto
ggplot(datos_biomed, aes(x = Colesterol, y = Presion)) +
  geom_point(aes(color = Tratamiento), size = 2) +   # puntos de dispersión coloreados por tto
  facet_grid(. ~ Tratamiento) +                      # facetas horizontales por tto
  theme_minimal() +                                  # minimalista limpio
  labs(title = "Colesterol vs Presión por tratamiento",
       x = "Colesterol",
       y = "Presión") +
  theme(legend.position = "none")                    # quitar leyenda pq cada faceta es un tto
# separa relación colesterol y p. arterial por tto. 
# Cada panel deja ver la distribución interna de cada grupo
# compara posibles diferencias entre los pacientes que reciben FármacoA, B y placebo.

# 7. Realiza un histogramas para cada variable. (0.5 pts)
install.packages("ggplot2")
install.packages("tidyr")

library(ggplot2)
library(tidyr)

# Columnas numéricas
vars_numericas <- c("Glucosa", "Presion", "Colesterol")

# Reorganizar datos en formato largo con tidyr::pivot_longer
datos_largo <- pivot_longer(
  datos_biomed,
  cols = all_of(vars_numericas),
  names_to = "Variable",
  values_to = "Valor"
)

# Crear histogramas
ggplot(datos_largo, aes(x = Valor, fill = Variable)) +
  geom_histogram(bins = 30, alpha = 0.8, color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  labs(
    title = "Distribución de valores - Histogramas por variable",
    x = "Valor medido",
    y = "Frecuencia"
  ) +
  theme_bw() +
  theme(legend.position = "none")
# muestran distribución de cada variable
# Glucosa: distribución concentrada y mas simétrica.
# Presión y Colesterol: mas dispersión, aproximadamente normales.
# No parece haber valores atípicos extremos ni sesgos marcados
# parece que los datos son adecuados para análisis paramétricos.


# 8. Crea un factor a partir del tratamiento. Investifa factor(). (1 pt)
# variable categórica que puede tener niveles definidos
tratamiento_factor <- factor(datos_biomed$Tratamiento)
# Verificar niveles factor
levels(tratamiento_factor)


# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
# Media Glucosa por tto
media_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos_biomed, FUN = mean)
media_glucosa

# Desviación estándar Glucosa por tto
sd_glucosa <- aggregate(Glucosa ~ Tratamiento, data = datos_biomed, FUN = sd)
sd_glucosa
# Las medias de glucosa son similares entre los tres
# parece que ningun fármaco produce un efecto notable sobre los niveles de glucosa.
# En cambio las desviaciones estándar muestran variabilidad moderada
# comparable entre los grupos.


# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
# Todos los datos de Placebo
placebo <- subset(datos_biomed, Tratamiento == "Placebo")

# Todos los datos de Fármaco A
farmacoA <- subset(datos_biomed, Tratamiento == "FarmacoA")

# Todos los datos de Fármaco B
farmacoB <- subset(datos_biomed, Tratamiento == "FarmacoB")

# Verificar primeras filas de cada 
head(placebo)
head(farmacoA)
head(farmacoB)
# Placebo: los valores de Glucosa, Presión y Colesterol muestran variabilidad,
# sin valores extremos, niveles  moderados.
# Farmaco A: hay valores de Colesterol altos (242),
# Glucosa y Presión también varían
# mas dispersión que Placebo.
# Farmaco B: valores intermedios, cierta dispersión
# Glucosa a veces más baja (82.4) y Colesterol alto (220).
# cada tto tiene rangos de valores similares, con variaciones individuales, pero sin patrones extremos


# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
# Cargar librería para pruebas estadísticas
install.packages("ggpubr")
library(ggpubr)

# Evaluar normalidad con Shapiro-Wilk para Glucosa por tto
shapiro.test(placebo$Glucosa)  
shapiro.test(farmacoA$Glucosa) 
shapiro.test(farmacoB$Glucosa) 

# Comparativa de medias: ANOVA si de cumple normalidad
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos_biomed)
summary(anova_glucosa)
# La normalidad se cumple (Shapiro-Wilk p > 0.05).
# ANOVA muestra p= 0.1 (> 0.05)
# no hay diferencias significativas en la media de Glucosa entre ttos.
# Ningún fármaco tiene efecto estadísticamente significativo sobre niveles de Glucosa frente al Placebo.

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
# Ajustar modelo ANOVA
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos_biomed)

# Mostrar resumen ANOVA
summary(anova_glucosa)
# Df: grados libertad (2 para Tto, 97 para Residuals)
# Sum Sq y Mean Sq: suma de cuadrados y media de cuadrados para cada fuente de variación
# F value = 2.358: relación variabilidad entre ttos respecto a variabilidad residual
# Pr(>F) = 0.1: p-valor de la prueba F (significación estadística del efecto del tto)

# Como p-valor = 0.1 > 0.05, no hay suficiente evidencia para afirmar:
# diferencia significativa en la media de Glucosa entre los tres ttos.
# ni Farmaco A ni B tienen efecto estadísticamente significativo sobre Glucosa frente al Placebo.




