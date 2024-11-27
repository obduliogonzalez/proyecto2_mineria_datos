# Limpiar la consola
cat("\014")

# Configurar el espejo de CRAN
options(repos = c(CRAN = "https://cran.r-project.org"))

# Instalar y cargar paquetes necesarios
required_packages <- c("randomForest", "haven", "rpart", "rpart.plot", "ggplot2", "dplyr", "farver")
for(package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Leer el archivo
file_path <- "c:/Users/diego/Desktop/II/paraP/20190718224829QlKHEHYjsnuXyJ0R0nU16Fo9SHmLER0z.sav"

"C:\Users/nestor.gonzalez/Documents/GitHub/proyecto2_mineria_datos/20190718224829QlKHEHYjsnuXyJ0R0nU16Fo9SHmLER0z.sav"
if (file.exists(file_path)) {
  # Leer datos y realizar limpieza inicial
  multas <- read_sav(file_path)
  multas <- as.data.frame(multas)
  
  # Verificar las columnas disponibles
  print("Columnas disponibles en el DataFrame:")
  print(colnames(multas))
  
  # Seleccionar variables relevantes para cada predicción
  # 1. Predicción de Estado de Ebriedad
  if (all(c("Est_ebriedad_inf", "Sexo_inf", "Edad_inf", "Est_conyugal_inf", "Mes_boleta", "Área_geo_inf") %in% colnames(multas))) {
    multas_ebriedad <- multas %>%
      select(Est_ebriedad_inf, Sexo_inf, Edad_inf, Est_conyugal_inf, 
             Mes_boleta, Área_geo_inf) %>%
      na.omit()
    print("Datos de ebriedad seleccionados y limpiados.")
  } else {
    stop("Una o más columnas necesarias no existen en el DataFrame.")
  }
  
  # 2. Predicción de Área Geográfica
  if (all(c("Área_geo_inf", "Niv_escolaridad_inf", "Est_conyugal_inf", "Grupo_étnico_inf", "Edad_inf") %in% colnames(multas))) {
    multas_area <- multas %>%
      select(Área_geo_inf, Niv_escolaridad_inf, Est_conyugal_inf, 
             Grupo_étnico_inf, Edad_inf) %>%
      na.omit()
  } else {
    stop("Una o más columnas necesarias no existen en el DataFrame.")
  }
  
  # 3. Predicción de Grupo de Edad
  if (all(c("Edad_inf", "Est_conyugal_inf", "Niv_escolaridad_inf", "Área_geo_inf", "Sexo_inf", "Est_ebriedad_inf") %in% colnames(multas))) {
    multas_edad <- multas %>%
      select(Edad_inf, Est_conyugal_inf, Niv_escolaridad_inf, 
             Área_geo_inf, Sexo_inf, Est_ebriedad_inf) %>%
      na.omit()
  } else {
    stop("Una o más columnas necesarias no existen en el DataFrame.")
  }
  
  # 4. Predicción de Estado Civil
  if (all(c("Est_conyugal_inf", "Edad_inf", "Sexo_inf", "Niv_escolaridad_inf", "Área_geo_inf", "Grupo_étnico_inf") %in% colnames(multas))) {
    multas_civil <- multas %>%
      select(Est_conyugal_inf, Edad_inf, Sexo_inf, 
             Niv_escolaridad_inf, Área_geo_inf, Grupo_étnico_inf) %>%
      na.omit()
  } else {
    stop("Una o más columnas necesarias no existen en el DataFrame.")
  }
  
  print("Archivos cargados y procesados correctamente.")
} else {
  stop("El archivo no existe en la ruta especificada.")
}

# Función para entrenar y evaluar modelos
train_and_evaluate <- function(data, target_var) {
  # Convertir la variable objetivo a factor
  data[[target_var]] <- as.factor(data[[target_var]])
  
  # Desordenar datos
  set.seed(100)
  data <- data[sample(1:nrow(data)), ]
  
  # Dividir datos en entrenamiento y prueba
  index <- sample(1:nrow(data), 0.7 * nrow(data))
  train <- data[index, ]
  test <- data[-index, ]
  
  # Asegurarse de que no haya clases vacías en la variable objetivo
  train <- droplevels(train)
  test <- droplevels(test)
  
  # Crear fórmula
  predictors <- names(data)[names(data) != target_var]
  formula <- as.formula(paste(target_var, "~", paste(predictors, collapse = " + ")))
  
  # Entrenar modelo
  model <- randomForest(formula, data = train, ntree = 100)
  
  # Predicciones
  predictions <- predict(model, test)
  
  # Matriz de confusión
  conf_matrix <- table(test[[target_var]], predictions)
  
  # Calcular precisión
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # Gráficos
  par(mfrow = c(2, 2))
  
  # 1. Importancia de variables
  importance_scores <- importance(model)
  barplot(importance_scores[,1], 
          main = paste("Importancia de Variables -", target_var),
          horiz = TRUE, las = 1, col = "steelblue")
  
  # 2. Error del modelo
  plot(model, main = paste("Error vs Número de Árboles -", target_var))
  
  # 3. Distribución de predicciones
  pred_df <- data.frame(Real = test[[target_var]], Predicho = predictions)
  print(ggplot(pred_df, aes(x = Real, fill = Predicho)) +
        geom_bar(position = "dodge") +
        theme_minimal() +
        labs(title = paste("Distribución de Predicciones -", target_var),
             x = "Clase Real", y = "Frecuencia") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  
  # Restaurar configuración de gráficos
  par(mfrow = c(1, 1))
  
  return(list(
    model = model,
    confusion_matrix = conf_matrix,
    accuracy = accuracy
  ))
}

# Entrenar y evaluar los cuatro modelos
results_ebriedad <- train_and_evaluate(multas_ebriedad, "Est_ebriedad_inf")
results_area <- train_and_evaluate(multas_area, "Área_geo_inf")
results_edad <- train_and_evaluate(multas_edad, "Edad_inf")
results_civil <- train_and_evaluate(multas_civil, "Est_conyugal_inf")

# Imprimir resultados
cat("\nPrecisión de los modelos:\n")
cat("Modelo de Estado de Ebriedad:", round(results_ebriedad$accuracy * 100, 2), "%\n")
cat("Modelo de Área Geográfica:", round(results_area$accuracy * 100, 2), "%\n")
cat("Modelo de Grupo de Edad:", round(results_edad$accuracy * 100, 2), "%\n")
cat("Modelo de Estado Civil:", round(results_civil$accuracy * 100, 2), "%\n")

# Función para realizar predicciones con nuevos datos
predict_new_case <- function(model, new_data) {
  prediction <- predict(model, new_data)
  return(prediction)
}