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
  
  # Generar gráficos con correcciones
# Gráfico 1: Distribución de Estado de Ebriedad
multas_ebriedad$Est_ebriedad_inf <- as.factor(multas_ebriedad$Est_ebriedad_inf)
g1 <- ggplot(multas_ebriedad, aes(x = Est_ebriedad_inf, fill = Est_ebriedad_inf)) +
  geom_bar() +
  labs(title = "Distribución de Estado de Ebriedad", x = "Estado de Ebriedad", y = "Frecuencia") +
  theme_minimal()
print(g1)

# Gráfico 2: Distribución de Sexo por Estado de Ebriedad
multas_ebriedad$Sexo_inf <- as.factor(multas_ebriedad$Sexo_inf)
g2 <- ggplot(multas_ebriedad, aes(x = Sexo_inf, fill = Est_ebriedad_inf)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de Sexo por Estado de Ebriedad", x = "Sexo", y = "Frecuencia") +
  theme_minimal()
print(g2)

# Gráfico 3: Distribución de Edad
g3 <- ggplot(multas_ebriedad, aes(x = Edad_inf)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "black") +
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()
print(g3)

# Gráfico 4: Área Geográfica
multas_area$Área_geo_inf <- as.factor(multas_area$Área_geo_inf)
g4 <- ggplot(multas_area, aes(x = Área_geo_inf, fill = Área_geo_inf)) +
  geom_bar() +
  labs(title = "Distribución por Área Geográfica", x = "Área Geográfica", y = "Frecuencia") +
  theme_minimal()
print(g4)

  
} else {
  stop("El archivo no existe en la ruta especificada.")
}
