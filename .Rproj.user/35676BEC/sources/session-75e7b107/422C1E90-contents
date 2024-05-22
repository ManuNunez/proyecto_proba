# Cargar los datos desde los archivos CSV
checo_results <- read.csv("checo_ps.csv", stringsAsFactors = FALSE)
countries <- read.csv("countries_in_f1.csv", stringsAsFactors = FALSE)

# Ajustar nombres de columnas en checo_results
colnames(checo_results) <- c("País", "Gran Premio", "Año", "Equipo", "Posición")

# Ajustar nombres de columnas en countries
colnames(countries) <- c("País", "Gran Premio", "Índice_de_Libertad_Humana")

# Verificar la estructura de los datos
head(checo_results)
head(countries)

# Combinar los datos por el país y el gran premio (asegurarse de que ambos nombres coincidan)
merged_data <- merge(checo_results, countries, by = c("País", "Gran Premio"), all.x = TRUE)

# Verificar la combinación de datos
head(merged_data)


# Calcular la correlación entre la posición de Checo y el índice de libertad humana
correlation <- cor(merged_data$Posición, merged_data$Índice_de_Libertad_Humana, use = "complete.obs")

# Mostrar el valor de correlación
print(paste("Correlación entre posición de Checo y índice de libertad humana:", correlation))

# Graficar la relación entre las variables
plot(merged_data$Índice_de_Libertad_Humana, merged_data$Posición,
     xlab = "Índice de Libertad Humana",
     ylab = "Posición de Checo",
     main = "Relación entre Índice de Libertad Humana y Posición de Checo")

# Realizar una regresión lineal
model <- lm(Posición ~ Índice_de_Libertad_Humana, data = merged_data)

# Mostrar un resumen del modelo
summary(model)

# Graficar la recta de regresión
plot(merged_data$Índice_de_Libertad_Humana, merged_data$Posición,
     xlab = "Índice de Libertad Humana",
     ylab = "Posición de Checo",
     main = "Regresión Lineal: Posición de Checo vs Índice de Libertad Humana")
abline(model, col = "red")
