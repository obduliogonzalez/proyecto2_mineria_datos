# Limpiar la consola
cat("\014")

# Configurar el espejo de CRAN
options(repos = c(CRAN = "https://cran.r-project.org"))

# Instalar y cargar paquetes necesarios
required_packages <- c("randomForest", "haven", "rpart", "rpart.plot", "ggplot2", "dplyr", "farver")
for (package in required_packages) {
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
  
  # Seleccionar variables relevantes
  cols_ebriedad <- c("Est_ebriedad_inf", "Sexo_inf", "Edad_inf", "Est_conyugal_inf", "Mes_boleta", "Área_geo_inf")
  validate_columns(multas, cols_ebriedad)
  multas_ebriedad <- multas %>%
    select(all_of(cols_ebriedad)) %>%
    na.omit()
  print("Datos de ebriedad seleccionados y limpiados.")
  
  # Gráfico 1: Distribución de edad por estado de ebriedad
  plot1 <- ggplot(multas_ebriedad, aes(x = Edad_inf, fill = Est_ebriedad_inf)) +
    geom_histogram(binwidth = 5, position = "dodge") +
    labs(title = "Distribución de Edad por Estado de Ebriedad", x = "Edad", y = "Frecuencia") +
    theme_minimal()
  print(plot1)
  
  # Gráfico 2: Distribución por sexo
  plot2 <- ggplot(multas_ebriedad, aes(x = Sexo_inf, fill = Est_ebriedad_inf)) +
    geom_bar(position = "dodge") +
    labs(title = "Distribución por Sexo y Estado de Ebriedad", x = "Sexo", y = "Conteo") +
    theme_minimal()
  print(plot2)
  
  # Gráfico 3: Relación entre estado conyugal y estado de ebriedad
  plot3 <- ggplot(multas_ebriedad, aes(x = Est_conyugal_inf, fill = Est_ebriedad_inf)) +
    geom_bar(position = "dodge") +
    labs(title = "Estado Conyugal y Estado de Ebriedad", x = "Estado Conyugal", y = "Conteo") +
    theme_minimal()
  print(plot3)
  
} else {
  stop("El archivo no existe en la ruta especificada.")
}
