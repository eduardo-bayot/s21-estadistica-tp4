# Configuración inicial
set.seed(21242021) # Fijar semilla para reproducibilidad

# Generación de datos simulados
Mes_t <- 1:12
Sigma_t <- runif(12, min = -1, max = 1)
Venta_ft <- 3 * Mes_t + 2 * Sigma_t

# Crear un data frame para organizar los datos
datos_venta <- data.frame(Mes_t, Venta_ft)
print(datos_venta)

# Ajustar la regresión lineal
modelo_regresion <- lm(Venta_ft ~ Mes_t, data = datos_venta)
resumen_modelo <- summary(modelo_regresion) # Resumen del modelo
print(resumen_modelo)

# Predicción para t = 13
t13_prediccion <- predict(modelo_regresion, newdata = data.frame(Mes_t = 13))
cat("Predicción para t = 13:", t13_prediccion, "\n")

# Generar valor simulado para t = 13
Sigma_t13 <- runif(1, min = -1, max = 1)
f_t13_simulado <- 3 * 13 + 2 * Sigma_t13
cat("Valor simulado para t = 13:", f_t13_simulado, "\n")

# Error entre predicción y valor simulado
error <- f_t13_simulado - t13_prediccion
cat("Error entre predicción y valor simulado:", error, "\n")

# Crear un data frame con los resultados clave
resultados <- data.frame(
  Mes_t = c(datos_venta$Mes_t, 13),
  Venta_ft = c(datos_venta$Venta_ft, NA),
  Prediccion = c(rep(NA, 12), t13_prediccion),
  Valor_Simulado = c(rep(NA, 12), f_t13_simulado),
  Error = c(rep(NA, 12), error)
)

# Guardar datos y resultados en un archivo CSV
write.csv(resultados, "resultados_tp4.csv", row.names = FALSE)

# Mostrar los resultados en consola
print("Resumen del modelo de regresión:")
print(resumen_modelo)
print("Resultados exportados a 'resultados_tp4.csv'")

# Cargar ggplot2
library(ggplot2)

# Gráfico de datos simulados y recta de ajuste
ggplot(data = datos_venta, aes(x = Mes_t, y = Venta_ft)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Datos simulados y recta de ajuste",
    x = expression(Mes~(italic(t))),
    y = expression(Ventas~(italic(f(t))))
  ) +
  theme_minimal()

# Guardar el gráfico
ggsave("resultados_regresion.png", width = 8, height = 6)

# Generar residuales
residuales <- resid(modelo_regresion)

# Gráfico de residuales
ggplot(data = datos_venta, aes(x = Mes_t, y = residuales)) +
  geom_point(color = "purple", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Análisis de residuales",
    x = expression(Mes~(italic(t))),
    y = "Residuales"
  ) +
  theme_minimal()

# Guardar el gráfico de residuales
ggsave("residuales_regresion.png", width = 8, height = 6)
