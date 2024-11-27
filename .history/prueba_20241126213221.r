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
  
  # 3. Predicción de Condición de Alfabetismo
  cols_alfabetismo <- c("Cond_alfabetismo_inf", "Est_conyugal_inf", "Niv_escolaridad_inf", 
                        "Área_geo_inf", "Sexo_inf", "Est_ebriedad_inf")
  validate_columns(multas, cols_alfabetismo)
  multas_alfabetismo <- multas %>%
    select(all_of(cols_alfabetismo)) %>%
    na.omit()
  print("Datos de alfabetismo seleccionados y limpiados.")
  
  # 4. Predicción de Estado Civil
  cols_civil <- c("Est_conyugal_inf", "Edad_inf", "Sexo_inf", "Área_geo_inf")
  validate_columns(multas, cols_civil)
  multas_civil <- multas %>%
    select(all_of(cols_civil)) %>%
    na.omit()
  print("Datos de estado civil seleccionados y limpiados.")
  
} else {
  stop("El archivo no se encuentra en la ruta especificada.")
}

# Verificación final
print("Preparación de datos completada.")
print("Datos de ebriedad:")
print(head(multas_ebriedad))
print("Datos de área geográfica:")
print(head(multas_area))
print("Datos de alfabetismo:")
print(head(multas_alfabetismo))
print("Datos de estado civil:")
print(head(multas_civil))
