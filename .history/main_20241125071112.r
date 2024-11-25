# Configurar el espejo de CRAN
options(repos = c(CRAN = "https://cran.r-project.org"))

# Instalar y cargar paquetes
if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
  install.packages("haven")
  library(haven) 
} else {
  library(randomForest)
  library(haven)
}

# Proporcionar la ruta completa al archivo CSV

file_path <- "./20190718223411Ixu5S3WjSZsRS7ClOTc9m3KRW54lcJcd.s"
file_path <- "./20190718224829QlKHEHYjsnuXyJ0R0nU16Fo9SHmLER0z.sav"


if (file.exists(file_path)) {
  multas <- read.sav(file_path, header = TRUE, sep = ",")
  multas <- na.omit(multas)
  multas <- multas[, c("DEPARTAMENTO", "PEI3", "PEI4", "PEI5", "AREA")]
  print("Archivo cargado y procesado correctamente.")
} else {
  stop("El archivo no existe en la ruta especificada.")
}

multas$DEPARTAMENTO <- as.factor(multas$DEPARTAMENTO)

set.seed(100) # Semilla para reproducibilidad 
multas <- multas[sample(1:nrow(multas)), ] # Desordenar datos

# Dividir los datos en conjuntos de entrenamiento y prueba
index <- sample(1:nrow(multas), 0.7 * nrow(multas)) # 70% de los datos para entrenamiento
train <- multas[index, ] # Datos de entrenamiento
test <- multas[-index, ] # Datos de prueba

# Entrenar el modelo de bosque aleatorio
bosque <- randomForest(DEPARTAMENTO ~ PEI3 + PEI4 + PEI5 + AREA, data = train, ntree = 100, mtry = 4)

# Predecir en el conjunto de prueba
predicciones <- predict(bosque, test)

dato_nuevo <- data.frame(PEI3 = 2, PEI4 = 40, PEI5 = 2000, AREA = 2)

# Predecir en un nuevo dato
prediccion_nueva <- predict(bosque, dato_nuevo)

# Mostrar las predicciones
print(predicciones)

# Opcional: Ver las predicciones en una vista de datos (solo en RStudio o R GUI)
View(predicciones)
View(prediccion_nueva)



# Mostrar las tablas de datos
View(migracion)
View(train)
View(test)

# Importancia de las variables
importancia <- importance(bosque) # Calcula la importancia de las variables
barplot(importancia[, 1], main = "Importancia de las Variables", horiz = TRUE, col = "steelblue", las = 1)

# Gráfico de error del modelo
plot(bosque, main = "Error del Modelo vs Número de Árboles")

# Matriz de confusión
confusion_matrix <- table(test$DEPARTAMENTO, predicciones)
print(confusion_matrix)


# Gráfico de predicciones por clase
library(ggplot2)
pred_df <- data.frame(Real = test$DEPARTAMENTO, Predicho = predicciones)
ggplot(pred_df, aes(x = Real, fill = Predicho)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribución de Predicciones por Clase", x = "Clase Real", y = "Frecuencia")


