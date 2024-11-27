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
  
  # 3. Predicción de Condición de Alfabetismo
  if (all(c("Cond_alfabetismo_inf", "Est_conyugal_inf", "Niv_escolaridad_inf", "Área_geo_inf", "Sexo_inf", "Est_ebriedad_inf") %in% colnames(multas))) {
    multas_alfabetismo <- multas %>%
      select(Cond_alfabetismo_inf, Est_conyugal_inf, Niv_escolaridad_inf, 
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
  model <- rpart(formula, data = train, method = "class")
  
  # Predicciones
  predictions <- predict(model, test, type = "class")
  
  # Matriz de confusión
  conf_matrix <- table(test[[target_var]], predictions)
  
  # Calcular precisión
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # Graficar el árbol en un nuevo dispositivo
  dev.new()
  rpart.plot(model, main = paste("Árbol de Decisión -", target_var))
  
  return(list(
    model = model,
    confusion_matrix = conf_matrix,
    accuracy = accuracy
  ))
}

# Entrenar y evaluar los cuatro modelos
results_ebriedad <- train_and_evaluate(multas_ebriedad, "Est_ebriedad_inf")
results_area <- train_and_evaluate(multas_area, "Área_geo_inf")
results_alfabetismo <- train_and_evaluate(multas_alfabetismo, "Cond_alfabetismo_inf")
results_civil <- train_and_evaluate(multas_civil, "Est_conyugal_inf")

# Imprimir resultados
cat("\nPrecisión de los modelos:\n")
cat("Modelo de Estado de Ebriedad:", round(results_ebriedad$accuracy * 100, 2), "%\n")
cat("Modelo de Área Geográfica:", round(results_area$accuracy * 100, 2), "%\n")
cat("Modelo de Condición de Alfabetismo:", round(results_alfabetismo$accuracy * 100, 2), "%\n")
cat("Modelo de Estado Civil:", round(results_civil$accuracy * 100, 2), "%\n")

# Función para realizar predicciones con nuevos datos
predict_new_case <- function(model, new_data) {
  # Convertir las variables a factores y asegurar que tengan los mismos niveles
  factor_vars <- names(new_data)
  for (var in factor_vars) {
    if (is.factor(model$frame[[var]])) {
      new_data[[var]] <- factor(new_data[[var]], levels = levels(model$frame[[var]]))
    }
  }
  prediction <- predict(model, new_data, type = "class")
  return(prediction)
}

# Ejemplos de predicciones con diferentes escenarios
escenario1 <- data.frame(Sexo_inf = "Mujer", Edad_inf = 21, Est_conyugal_inf = "Soltero", Mes_boleta = 2, Área_geo_inf = "Urbano")
escenario2 <- data.frame(Sexo_inf = "Mujer", Edad_inf = 27, Est_conyugal_inf = "Casado", Mes_boleta = 1, Área_geo_inf = "Rural")

# Convertir las variables de los escenarios a factores y asegurar que tengan los mismos niveles
escenario1 <- lapply(escenario1, as.factor)
escenario2 <- lapply(escenario2, as.factor)

# Convertir de lista a data.frame
escenario1 <- as.data.frame(escenario1)
escenario2 <- as.data.frame(escenario2)

# Asegurar que los niveles de los factores en los escenarios coincidan con los niveles en los datos de entrenamiento
for (var in names(escenario1)) {
  if (var %in% names(multas_ebriedad)) {
    levels(escenario1[[var]]) <- levels(multas_ebriedad[[var]])
    levels(escenario2[[var]]) <- levels(multas_ebriedad[[var]])
  }
}

# Asegurar que los tipos de las variables en los escenarios coincidan con los tipos en los datos de entrenamiento
for (var in names(escenario1)) {
  if (var %in% names(multas_ebriedad)) {
    if (is.factor(multas_ebriedad[[var]])) {
      escenario1[[var]] <- factor(escenario1[[var]], levels = levels(multas_ebriedad[[var]]))
      escenario2[[var]] <- factor(escenario2[[var]], levels = levels(multas_ebriedad[[var]]))
    } else {
      escenario1[[var]] <- as.numeric(escenario1[[var]])
      escenario2[[var]] <- as.numeric(escenario2[[var]])
    }
  }
}

prediccion1 <- predict_new_case(results_ebriedad$model, escenario1)
prediccion2 <- predict_new_case(results_ebriedad$model, escenario2)

cat("\nPredicciones para escenarios de Estado de Ebriedad:\n")
cat("Escenario 1:", prediccion1, "\n")
cat("Escenario 2:", prediccion2, "\n")

# Interpretaciones
cat("\nInterpretaciones:\n")
cat("El modelo de Estado de Ebriedad predice si una persona está en estado de ebriedad basado en variables como sexo, edad, estado civil, mes de la boleta y área geográfica.\n")
cat("El modelo de Área Geográfica predice el área geográfica basada en variables como nivel de escolaridad, estado civil, grupo étnico y edad.\n")
cat("El modelo de Condición de Alfabetismo predice la condición de alfabetismo basada en variables como estado civil, nivel de escolaridad, área geográfica, sexo y estado de ebriedad.\n")
cat("El modelo de Estado Civil predice el estado civil basado en variables como edad, sexo, nivel de escolaridad, área geográfica y grupo étnico.\n")

# Función para entrenar y evaluar modelos con Random Forest
train_and_evaluate_rf <- function(data, target_var) {
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
  model <- randomForest(formula, data = train, ntree = 100, mtry = 4)
  
  # Predicciones
  predictions <- predict(model, test)
  
  # Matriz de confusión
  conf_matrix <- table(test[[target_var]], predictions)
  
  # Calcular precisión
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  return(list(
    model = model,
    confusion_matrix = conf_matrix,
    accuracy = accuracy
  ))
}

# Entrenar y evaluar los modelos con Random Forest
results_ebriedad_rf <- train_and_evaluate_rf(multas_ebriedad, "Est_ebriedad_inf")
results_area_rf <- train_and_evaluate_rf(multas_area, "Área_geo_inf")

# Imprimir resultados de Random Forest
cat("\nPrecisión de los modelos con Random Forest:\n")
cat("Modelo de Estado de Ebriedad:", round(results_ebriedad_rf$accuracy * 100, 2), "%\n")
cat("Modelo de Área Geográfica:", round(results_area_rf$accuracy * 100, 2), "%\n")

# Predecir en un nuevo dato
dato_nuevo <- data.frame(Sexo_inf = "Mujer", Edad_inf = 30, Est_conyugal_inf = "Soltero", Mes_boleta = 5, Área_geo_inf = "Urbano")

# Asegurar que los niveles de los factores en el nuevo dato coincidan con los niveles en los datos de entrenamiento
for (var in names(dato_nuevo)) {
  if (var %in% names(multas_ebriedad)) {
    if (is.factor(multas_ebriedad[[var]])) {
      dato_nuevo[[var]] <- factor(dato_nuevo[[var]], levels = levels(multas_ebriedad[[var]]))
    } else {
      dato_nuevo[[var]] <- as.numeric(dato_nuevo[[var]])
    }
  }
}

prediccion_nueva <- predict(results_ebriedad_rf$model, dato_nuevo)

cat("\nPredicción para un nuevo caso de Estado de Ebriedad:\n")
cat("Nuevo caso:", prediccion_nueva, "\n")

# Mostrar las tablas de datos
View(multas)
View(multas_ebriedad)
View(multas_area)