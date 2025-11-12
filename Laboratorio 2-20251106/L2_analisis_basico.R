
# Cargar librerías necesarias
install.packages("ggplot2") # si no está instalada
library(ggplot2)

# Leer el dataset
datos <- read.csv("Alzheimer_dataset.csv", sep = ";")

# Explorar estructura
head(datos)
str(datos)
summary(datos)
colSums(is.na(datos))

# Gráficos básicos
hist(datos$Edad, breaks = 20, main = "Histograma de edades", col = "lightblue")
boxplot(Tiempo_Reaccion_1 ~ Target, data = datos, col = "lightgreen")
barplot(table(datos$Sexo), col = c("pink", "lightblue"))

# Modelo de regresión logística
modelo <- glm(Target ~ Edad + Tiempo_Reaccion_1, data = datos, family = binomial)
summary(modelo)
