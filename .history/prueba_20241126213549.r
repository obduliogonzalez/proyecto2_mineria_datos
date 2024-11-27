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
file_path <- "C:/Users/nestor.gonzalez/Documents/GitHub/proyecto2_mineria_datos/20190718224829QlKHEHYjsnuXyJ0R0nU16Fo9SHmLER0z.sav"

if (file.exists(file_path)) {
  # Leer datos y realizar limpieza inicial
  multas <- read_sav(file_path)
  multas <- as.data.frame(multas)
  
  # Normalizar nombres de columnas (manejo de caracteres especiales)
  colnames(multas) <- make.names(colnames(multas))
  
  # Verificar las columnas disponibles
  print("Columnas disponibles en el DataFrame:")
  print(colnames(multas))
  
  # Función para validar columnas necesarias
  validate_columns <- function(data, required_cols) {
    missing_cols <- setdiff(required_cols, colnames(data))
    if (length(missing_cols) > 0) {
      stop(paste("Faltan las siguientes columnas:", paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Seleccionar variables relevantes para cada predicción
  # 1. Predicción de Estado de Ebriedad
  cols_ebriedad <- c("Est_ebriedad_inf", "Sexo_inf", "Edad_inf", 
                     "Est_conyugal_inf", "Mes_boleta", "Área_geo_inf")
  validate_columns(multas, cols_ebriedad)
  multas_ebriedad <- multas %>%
    select(all_of(cols_ebriedad)) %>%
    na.omit()
  print("Datos de ebriedad seleccionados y limpiados.")
  
  # 2. Predicción de Área Geográfica
  cols_area <- c("Área_geo_inf", "Niv_escolaridad_inf", "Est_conyugal_inf", 
                 "Grupo_étnico_inf", "Edad_inf")
  validate_columns(multas, cols_area)
  multas_area <- multas %>%
    select(all_of(cols_area)) %>%
    na.omit()
  print("Datos de área geográfica seleccionados y limpiados.")
  
  # Generar gráficos
  print("Generando gráficos...")

  # Gráfico 1: Distribución de Edad para Estado de Ebriedad
  ggplot(multas_ebriedad, aes(x = Edad_inf, fill = Est_ebriedad_inf)) +
    geom_histogram(binwidth = 5, position = "dodge", color = "black") +
    labs(title = "Distribución de Edad por Estado de Ebriedad", 
         x = "Edad", 
         y = "Frecuencia") +
    theme_minimal()

  # Gráfico 2: Estado Civil vs. Área Geográfica
  ggplot(multas_area, aes(x = Est_conyugal_inf, fill = Área_geo_inf)) +
    geom_bar(position = "dodge") +
    labs(title = "Estado Civil por Área Geográfica", 
         x = "Estado Civil", 
         y = "Frecuencia") +
    theme_minimal()

  # Gráfico 3: Niveles de Escolaridad
  ggplot(multas_area, aes(x = Niv_escolaridad_inf)) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(title = "Distribución de Niveles de Escolaridad", 
         x = "Nivel de Escolaridad", 
         y = "Frecuencia") +
    theme_minimal()

  # Gráfico 4: Condición de Alfabetismo
  cols_alfabetismo <- c("Cond_alfabetismo_inf", "Est_conyugal_inf", 
                        "Niv_escolaridad_inf", "Área_geo_inf", "Sexo_inf", "Est_ebriedad_inf")
  validate_columns(multas, cols_alfabetismo)
  multas_alfabetismo <- multas %>%
    select(all_of(cols_alfabetismo)) %>%
    na.omit()

  ggplot(multas_alfabetismo, aes(x = Cond_alfabetismo_inf, fill = Sexo_inf)) +
    geom_bar(position = "dodge") +
    labs(title = "Condición de Alfabetismo por Sexo", 
         x = "Condición de Alfabetismo", 
         y = "Frecuencia") +
    theme_minimal()
} else {
  stop("El archivo no se encuentra en la ruta especificada.")
}
