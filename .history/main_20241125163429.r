# Configurar el espejo de CRAN
options(repos = c(CRAN = "https://cran.r-project.org"))

# Instalar y cargar paquetes
if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
  install.packages("haven")
  library(haven) 
  install.packages(foreign)
  library(foreign)
} else {
  library(randomForest)
  library(haven)
}

# Proporcionar la ruta completa al archivo CSV


file_path <- "./20190718224829QlKHEHYjsnuXyJ0R0nU16Fo9SHmLER0z.sav"


# Leer el archivo SPSS
if (file.exists(file_path)) {
  multas <- read_sav(file_path)
  print(head(data)) # Ver las primeras filas del dataset
} else {
  stop("El archivo no existe en la ruta especificada.")
}

# Convertir las variables categóricas a factores
multas[] <- lapply(multas, function(x) if(is.character(x)) as.factor(x) else x)

# Definir las situaciones para las predicciones
predictions <- list(
  list(target_variable = "Sex_infractor", features = c("Étn_infractor", "Est_con_infractor", "Cond_alfab_infra", "Área_geo_infr")),
  list(target_variable = "Étn_infractor", features = c("Sex_infractor", "Est_con_infractor", "Esco_infractor", "Cond_alfab_infra")),
  list(target_variable = "Est_con_infractor", features = c("Sex_infractor", "Étn_infractor", "Esco_infractor", "Área_geo_infr")),
  list(target_variable = "Área_geo_infr", features = c("Sex_infractor", "Étn_infractor", "Esco_infractor", "Est_con_infractor"))
)

# Función para verificar si una variable tiene al menos dos observaciones en cada categoría
check_min_observations <- function(data, target_variable) {
  counts <- table(data[[target_variable]])
  return(all(counts >= 2))
}

# Función para realizar las predicciones y evaluar el modelo
run_prediction <- function(data, target_variable, features) {
  # Verificar si hay suficientes datos en cada categoría de la variable objetivo
  if (!check_min_observations(data, target_variable)) {
    return(paste("No hay suficientes datos en alguna categoría de", target_variable))
  }
  
  # Dividir los datos en conjuntos de entrenamiento y prueba
  set.seed(123)
  trainIndex <- createDataPartition(data[[target_variable]], p = .7, list = FALSE)
  trainData <- data[trainIndex,]
  testData <- data[-trainIndex,]
  
  # Entrenar el modelo de árbol de decisión
  model <- rpart(as.formula(paste(target_variable, "~ .")), data = trainData[, c(features, target_variable)], method = "class", control = rpart.control(maxdepth = 5))
  
  # Realizar predicciones en el conjunto de prueba
  predictions <- predict(model, testData[, features], type = "class")
  
  # Evaluar el modelo
  cm <- confusionMatrix(predictions, testData[[target_variable]])
  
  # Visualizar el árbol de decisión
  plot(model)
  text(model, use.n = TRUE)
  
  return(cm)
}

# Ejecutar las predicciones para cada situación
results <- lapply(predictions, function(pred) {
  run_prediction(multas, pred[["target_variable"]], pred[["features"]])
})

# Mostrar los resultados
results

Parece que el error se debe a que la función `createDataPartition` no está disponible. Esta función pertenece al paquete `caret`, que puede que no esté cargado en tu entorno. Vamos a asegurarnos de que el paquete `caret` esté instalado y cargado correctamente.

Aquí tienes el código para instalar y cargar el paquete `caret`:

```r
# Instalar el paquete caret si no está instalado
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# Cargar el paquete caret
library(caret)
```

Después de asegurarte de que el paquete `caret` esté instalado y cargado, puedes volver a ejecutar el código para realizar las predicciones. Aquí tienes el código completo con la instalación y carga del paquete `caret`:

```r
# Instalar y cargar el paquete caret
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)

# Cargar las librerías necesarias
library(foreign)
library(rpart)

# Asumiendo que ya has cargado los datos en la variable 'multas'
# multas <- read.spss("ruta/al/archivo.sav", to.data.frame = TRUE)

# Convertir las variables categóricas a factores
multas[] <- lapply(multas, function(x) if(is.character(x)) as.factor(x) else x)

# Definir las situaciones para las predicciones
predictions <- list(
  list(target_variable = "Sex_infractor", features = c("Étn_infractor", "Est_con_infractor", "Cond_alfab_infra", "Área_geo_infr")),
  list(target_variable = "Étn_infractor", features = c("Sex_infractor", "Est_con_infractor", "Esco_infractor", "Cond_alfab_infra")),
  list(target_variable = "Est_con_infractor", features = c("Sex_infractor", "Étn_infractor", "Esco_infractor", "Área_geo_infr")),
  list(target_variable = "Área_geo_infr", features = c("Sex_infractor", "Étn_infractor", "Esco_infractor", "Est_con_infractor"))
)

# Función para verificar si una variable tiene al menos dos observaciones en cada categoría
check_min_observations <- function(data, target_variable) {
  counts <- table(data[[target_variable]])
  return(all(counts >= 2))
}

# Función para realizar las predicciones y evaluar el modelo
run_prediction <- function(data, target_variable, features) {
  # Verificar si hay suficientes datos en cada categoría de la variable objetivo
  if (!check_min_observations(data, target_variable)) {
    return(paste("No hay suficientes datos en alguna categoría de", target_variable))
  }
  
  # Dividir los datos en conjuntos de entrenamiento y prueba
  set.seed(123)
  trainIndex <- createDataPartition(data[[target_variable]], p = .7, list = FALSE)
  trainData <- data[trainIndex,]
  testData <- data[-trainIndex,]
  
  # Entrenar el modelo de árbol de decisión
  model <- rpart(as.formula(paste(target_variable, "~ .")), data = trainData[, c(features, target_variable)], method = "class", control = rpart.control(maxdepth = 5))
  
  # Realizar predicciones en el conjunto de prueba
  predictions <- predict(model, testData[, features], type = "class")
  
  # Evaluar el modelo
  cm <- confusionMatrix(predictions, testData[[target_variable]])
  
  # Visualizar el árbol de decisión
  plot(model)
  text(model, use.n = TRUE)
  
  return(cm)
}

# Ejecutar las predicciones para cada situación
results <- lapply(predictions, function(pred) {
  run_prediction(multas, pred[["target_variable"]], pred[["features"]])
})

# Mostrar los resultados
results
```

Este código debería solucionar el problema de la función `createDataPartition` no encontrada. Si necesitas más ayuda o tienes alguna otra pregunta, ¡no dudes en decírmelo!