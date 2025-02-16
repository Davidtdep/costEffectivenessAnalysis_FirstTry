############################################################
#        ANÁLISIS DE COSTO-EFECTIVIDAD SACUBITRIL/VALSARTÁN vs LOSARTÁN
#        -------------------------------------------------
#        Secciones:
#          1. Carga de librerías y datos
#          2. Características de los pacientes
#          3. Evaluación de Costos Totales
#          4. Evaluación de QALYs
#          5. Cálculo del ICER
#          6. Evaluación de Costo-Efectividad con Umbrales
#          7. Análisis de Bootstrap (Plano de Costo-Efectividad)
#          8. Curva de Aceptabilidad Costo-Efectiva (CEAC)
#          9. Análisis de Sensibilidad por Tamaño de Muestra
############################################################

# 1. CARGA DE LIBRERÍAS Y DATOS =========================================

# ---- 1.1 Librerías ----
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tableone)
library(gtsummary)
library(flextable)
library(officer)
library(tidyr)
library(scales)

# ---- 1.2 Carga de datos ----
data <- read.csv("~/Desktop/Cost_Effectiveness_Analysis/data/data.csv")


############################################################
# 2. CARACTERÍSTICAS DE LOS PACIENTES ======================
############################################################

# ---- 2.1 Tabla de resumen por tratamiento (edad, sexo, comorbilidades) ----
tabla_resumen <- data %>%
  select(Edad, Sexo, Comorbilidades, Tratamiento) %>%
  tbl_summary(
    by = Tratamiento, 
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_p()  # Agregar pruebas de significancia

# Mostrar tabla en R
tabla_resumen

# ---- 2.2 Exportar tabla a Word ----
tabla_word <- as_flex_table(tabla_resumen)

doc <- read_docx()
doc <- body_add_flextable(doc, tabla_word)
print(doc, target = "Tabla_Caracteristicas_Pacientes.docx")

# ---- 2.3 Visualizaciones: histogramas y gráficos de barras ----
# Histograma de la distribución de edad por tratamiento
ggplot(data, aes(x = Edad, fill = Tratamiento)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(title = "Distribución de la Edad por Tratamiento", x = "Edad", y = "Frecuencia") +
  theme_minimal()

# Gráfico de barras para distribución de comorbilidades por tratamiento
ggplot(data, aes(x = Comorbilidades, fill = Tratamiento)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribución de Comorbilidades por Tratamiento", x = "Comorbilidades", y = "Frecuencia") +
  theme_minimal() +
  coord_flip()


############################################################
# 3. EVALUACIÓN DE COSTOS TOTALES =========================
############################################################

# ---- 3.1 Estadísticas descriptivas de costos por tratamiento ----
data %>%
  group_by(Tratamiento) %>%
  summarise(
    Costo_Total_Mean = mean(Costo_Total),
    Costo_Total_SD = sd(Costo_Total),
    QALY_Mean = mean(QALY),
    QALY_SD = sd(QALY)
  )

# ---- 3.2 Visualización: boxplot e histograma de costos totales ----
ggplot(data, aes(x = Tratamiento, y = Costo_Total, fill = Tratamiento)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(title = "Distribución de Costos Totales por Tratamiento", 
       x = "Tratamiento", y = "Costo Total (COP)") +
  theme_minimal()

ggplot(data, aes(x = QALY, fill = Tratamiento)) +
  geom_histogram(binwidth = 0.1, alpha = 0.6, position = "identity") +
  labs(title = "Distribución de QALYs por Tratamiento", x = "QALY", y = "Frecuencia") +
  theme_minimal()

# ---- 3.3 Comparar tasas de control de PA, eventos CV y mortalidad ----
proporciones <- data %>%
  group_by(Tratamiento) %>%
  summarise(
    Control_PA = mean(Control_PA),
    Evento_CV = mean(Evento_CV),
    Mortalidad = mean(Mortalidad)
  )

# Tabla de proporciones
proporciones

# Pruebas de chi-cuadrado para proporciones
chisq.test(table(data$Tratamiento, data$Control_PA))
chisq.test(table(data$Tratamiento, data$Evento_CV))
chisq.test(table(data$Tratamiento, data$Mortalidad))

# ---- 3.4 Visualizaciones: control de PA, eventos CV, mortalidad ----
ggplot(data, aes(x = Tratamiento, fill = factor(Control_PA))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("red", "green"), labels = c("No Control", "Control")) +
  labs(title = "Porcentaje de Pacientes con Control de PA por Tratamiento", 
       x = "Tratamiento", y = "Proporción") +
  theme_minimal()

p1 <- ggplot(data, aes(x = Tratamiento, fill = factor(Evento_CV))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("gray", "blue"), labels = c("Sin Evento CV", "Con Evento CV")) +
  labs(title = "Porcentaje de Pacientes con Evento CV", x = "Tratamiento", y = "Proporción") +
  theme_minimal()

p2 <- ggplot(data, aes(x = Tratamiento, fill = factor(Mortalidad))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("gray", "black"), labels = c("Vivo", "Muerto")) +
  labs(title = "Porcentaje de Pacientes Fallecidos", x = "Tratamiento", y = "Proporción") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)


############################################################
# 4. ANÁLISIS DETALLADO DE COSTOS =========================
############################################################

# ---- 4.1 Costo total promedio por tratamiento ----
costo_promedio <- data %>%
  group_by(Tratamiento) %>%
  summarise(
    Costo_Medicamento_Mean = mean(Costo_Medicamento),
    Costo_Consultas_Mean = mean(Costo_Consultas),
    Costo_Examenes_Mean = mean(Costo_Examenes),
    Costo_Hospitalizacion_Mean = mean(Costo_Hospitalizacion),
    Costo_Total_Mean = mean(Costo_Total),
    Costo_Total_SD = sd(Costo_Total),
    n = n()
  )

print(costo_promedio)

# Invertir dataframe para una vista más clara
costo_promedio_invertido <- costo_promedio %>%
  pivot_longer(cols = -Tratamiento, names_to = "Variable", values_to = "Valor") %>%
  pivot_wider(names_from = Tratamiento, values_from = Valor)

print(costo_promedio_invertido)

# ---- 4.2 Prueba de significancia: t-test para cada componente de costo ----
t.test(Costo_Total ~ Tratamiento, data = data, var.equal = FALSE)
t.test(Costo_Medicamento ~ Tratamiento, data = data, var.equal = FALSE)
t.test(Costo_Consultas ~ Tratamiento, data = data, var.equal = FALSE)
t.test(Costo_Examenes ~ Tratamiento, data = data, var.equal = FALSE)
t.test(Costo_Hospitalizacion ~ Tratamiento, data = data, var.equal = FALSE)

# ---- 4.3 Visualizaciones de costos por tratamiento ----
ggplot(data, aes(x = Tratamiento, y = Costo_Total, fill = Tratamiento)) +
  geom_boxplot(alpha = 0.6) +
  scale_y_continuous(labels = comma) +
  labs(title = "Distribución de Costos Totales por Tratamiento", 
       x = "Tratamiento", y = "Costo Total (COP)") +
  theme_minimal()

ggplot(data, aes(x = Costo_Total, fill = Tratamiento)) +
  geom_histogram(binwidth = 500000, alpha = 0.6, position = "identity") +
  scale_x_continuous(labels = comma) +
  labs(title = "Distribución de Costos Totales", x = "Costo Total (COP)", y = "Frecuencia") +
  theme_minimal()

# ---- 4.4 Exportar tabla de costos a Word ----
tabla_costos <- costo_promedio_invertido %>%
  flextable() %>%
  theme_vanilla() %>%
  autofit()

doc <- read_docx()
doc <- body_add_flextable(doc, tabla_costos)
print(doc, target = "Tabla_Costos_Promedio.docx")


############################################################
# 5. EVALUACIÓN DE QALYs ==================================
############################################################

# ---- 5.1 Cálculo de QALYs promedio ----
qalys_promedio <- data %>%
  group_by(Tratamiento) %>%
  summarise(
    QALY_Mean = mean(QALY),
    QALY_SD = sd(QALY),
    n = n()
  )

print(qalys_promedio)

# ---- 5.2 Comparación de QALYs (t-test o Wilcoxon) ----
# Prueba de normalidad
shapiro.test(data$QALY[data$Tratamiento == "Losartán"])
shapiro.test(data$QALY[data$Tratamiento == "Sacubitril/Valsartán"])

# Dependiendo del resultado, se elige Wilcoxon o t-test
if (shapiro.test(data$QALY)$p.value < 0.05) {
  wilcox.test(QALY ~ Tratamiento, data = data)
} else {
  t.test(QALY ~ Tratamiento, data = data, var.equal = FALSE)
}

# ---- 5.3 Ajuste por eventos adversos (Evento_CV, Mortalidad) ----
qalys_ajustados <- data %>%
  group_by(Tratamiento, Evento_CV, Mortalidad) %>%
  summarise(
    QALY_Mean = mean(QALY),
    QALY_SD = sd(QALY),
    n = n()
  )

print(qalys_ajustados)

# ---- 5.4 Visualizaciones de QALYs ----
ggplot(data, aes(x = Tratamiento, y = QALY, fill = Tratamiento)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Distribución de QALYs por Tratamiento", 
       x = "Tratamiento", y = "QALY") +
  theme_minimal()

ggplot(data, aes(x = QALY, fill = Tratamiento)) +
  geom_histogram(binwidth = 0.1, alpha = 0.6, position = "identity") +
  labs(title = "Distribución de QALYs", x = "QALY", y = "Frecuencia") +
  theme_minimal()


############################################################
# 6. CÁLCULO DEL ICER ======================================
############################################################

# ---- 6.1 ICER basado en QALYs ----
icer_qaly <- data %>%
  group_by(Tratamiento) %>%
  summarise(
    Costo_Total = mean(Costo_Total),
    QALY = mean(QALY)
  ) %>%
  arrange(Costo_Total)

icer_qaly_valor <- (icer_qaly$Costo_Total[2] - icer_qaly$Costo_Total[1]) / 
  (icer_qaly$QALY[2] - icer_qaly$QALY[1])

print(paste("ICER basado en QALYs:", round(icer_qaly_valor, 2), "COP por QALY ganado"))

# ---- 6.2 ICER basado en eventos clínicos ----
calcular_icer <- function(variable) {
  icer_evento <- data %>%
    group_by(Tratamiento) %>%
    summarise(
      Costo_Total = mean(Costo_Total),
      Tasa_Evento = mean(!!sym(variable))
    ) %>%
    arrange(Costo_Total)
  
  # Diferencia en costos / diferencia en tasa de evento
  icer_valor <- (icer_evento$Costo_Total[2] - icer_evento$Costo_Total[1]) /
    (icer_evento$Tasa_Evento[1] - icer_evento$Tasa_Evento[2])
  
  return(paste("ICER basado en", variable, ":", 
               round(icer_valor, 2), "COP por evento evitado"))
}

icer_pa <- calcular_icer("Control_PA")
icer_cv <- calcular_icer("Evento_CV")
icer_mort <- calcular_icer("Mortalidad")

print(icer_pa)
print(icer_cv)
print(icer_mort)


############################################################
# 7. EVALUACIÓN DE COSTO-EFECTIVIDAD CON UMBRALES ==========
############################################################

# ---- 7.1 Definir umbrales de WTP (PIB per cápita en Colombia) ----
wtp_min <- 28000000  # ~28M COP/QALY
wtp_max <- 84000000  # ~84M COP/QALY

# ---- 7.2 Evaluación del ICER (dominancia o costo-efectividad) ----
if (icer_qaly_valor < 0) {
  print("🔴 El tratamiento es estrictamente dominante (más efectivo y más barato). Se debería adoptar.")
} else if (icer_qaly_valor > wtp_max) {
  print("🟠 El tratamiento NO es costo-efectivo (ICER por encima del umbral de WTP).")
} else {
  print("🟢 El tratamiento es costo-efectivo (ICER dentro del rango de WTP en Colombia).")
}

# ---- 7.3 Evaluar dominancia para otros eventos clínicos ----
evaluar_icer <- function(icer, evento) {
  if (icer < 0) {
    return(paste("🔴", evento, ": El nuevo tratamiento es dominante (más efectivo y más barato)."))
  } else if (icer > wtp_max) {
    return(paste("🟠", evento, ": NO es costo-efectivo (ICER por encima de WTP)."))
  } else {
    return(paste("🟢", evento, ": Es costo-efectivo (ICER dentro del rango de WTP)."))
  }
}

# Extraer valores numéricos de los ICERs
icer_pa_valor <- as.numeric(gsub("[^0-9.-]", "", icer_pa))
icer_cv_valor <- as.numeric(gsub("[^0-9.-]", "", icer_cv))
icer_mort_valor <- as.numeric(gsub("[^0-9.-]", "", icer_mort))

print(evaluar_icer(icer_pa_valor, "Presión arterial controlada"))
print(evaluar_icer(icer_cv_valor, "Evento cardiovascular evitado"))
print(evaluar_icer(icer_mort_valor, "Mortalidad evitada"))


############################################################
# 8. ANÁLISIS DE BOOTSTRAP (PLANO DE CE) ==================
############################################################

# ---- 8.1 Generar replicaciones bootstrap de los costos y QALYs en paralelo ----

# Número de iteraciones del bootstrap
n_bootstrap <- 5000

# Semilla para reproducibilidad
set.seed(42)

# Cargar librería para paralelismo
library(parallel)

# Detectar número de núcleos y reservar 1 para el S.O.
n_cores <- detectCores() - 1

# Realizar replicaciones en paralelo
bootstrap_list <- mclapply(
  X = 1:n_bootstrap,
  FUN = function(i) {
    # Muestras con reemplazo para cada tratamiento
    sample_intervencion <- data %>%
      filter(Tratamiento == "Sacubitril/Valsartán") %>%
      sample_frac(1, replace = TRUE)
    
    sample_control <- data %>%
      filter(Tratamiento == "Losartán") %>%
      sample_frac(1, replace = TRUE)
    
    # Diferencias en costo y QALY
    delta_costo <- mean(sample_intervencion$Costo_Total) - mean(sample_control$Costo_Total)
    delta_qaly <- mean(sample_intervencion$QALY) - mean(sample_control$QALY)
    
    # Devolvemos un vector con (QALY, Costo)
    c(delta_qaly, delta_costo)
  },
  mc.cores = n_cores
)

# Convertir la lista a un data frame
icer_df <- do.call(rbind, bootstrap_list)
colnames(icer_df) <- c("Incremental_QALY", "Incremental_Cost")

# ---- 8.2 Graficar el plano de costo-efectividad ----
# Cargar librería de escalas si no está instalada
library(scales)

# Graficar el plano de costo-efectividad con eje Y en miles de COP
ggplot(icer_df, aes(x = Incremental_QALY, y = Incremental_Cost / 1e3)) +  # Convertir a miles
  geom_point(alpha = 0.4, color = "navy", size = 0.6) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  geom_vline(xintercept = 0, linetype = "solid", color = "red", size = 0.3) +
  # Líneas de umbrales de WTP en miles de COP
  geom_abline(slope = 20000, intercept = 0, linetype = "dashed", color = "green", size = 0.7) +  # 20K COP/QALY
  geom_abline(slope = 30000, intercept = 0, linetype = "dashed", color = "black", size = 0.7) +  # 30K COP/QALY
  scale_x_continuous(limits = c(-0.02, NA)) +  # Limitar el eje X desde -0.02
  scale_y_continuous(limits = c(NA, 100), labels = label_number(scale = 1, big.mark = ",", prefix = "$", suffix = "K")) +  # Mostrar en miles con prefijo y sufijo
  labs(
    title = element_blank(),
    x = "QALY Incremental",
    y = "Costos Incrementales (Miles de COP)"
  ) +
  theme_classic()








# Calcular el ICER para cada repetición del bootstrap
icer_bootstrap <- icer_df$Incremental_Cost / icer_df$Incremental_QALY

# Calcular el intervalo de confianza del 95% (percentiles 2.5% y 97.5%)
ci_95 <- quantile(icer_bootstrap, probs = c(0.025, 0.975))

# Mostrar el resultado
print(paste("ICER 95% CI:", round(ci_95[1], 2), "a", round(ci_95[2], 2), "COP/QALY"))





############################################################
# 9. CEAC (CURVA DE ACEPTABILIDAD) =========================
############################################################

icer_df <- do.call(rbind, bootstrap_list)

# Convertir a data.frame
icer_df <- as.data.frame(icer_df)

# Asignar nombres de columna
colnames(icer_df) <- c("Incremental_QALY", "Incremental_Cost")



# ---- 9.1 Calcular probabilidad de ser costo-efectivo para cada WTP ----
wtp_values <- seq(0, 50000000, length.out = 100)

prob_costo_efectivo <- sapply(wtp_values, function(wtp) {
  mean(icer_df$Incremental_Cost / icer_df$Incremental_QALY < wtp)
})

ceac_df <- data.frame(WTP = wtp_values, Prob_CostoEfectivo = prob_costo_efectivo)

# ---- 9.2 Graficar la CEAC ----

# Graficar la CEAC con WTP en millones de COP, corregido el formato del eje X
# Cargar librería de escalas si no está instalada
library(scales)

# Graficar la CEAC con WTP en millones de COP, corregido el formato del eje X
ggplot(ceac_df, aes(x = WTP, y = Prob_CostoEfectivo)) +  # Mantener valores originales en WTP
  geom_line(color = "navy", size = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 20000000, linetype = "dashed", color = "green") +  # 20M COP
  geom_vline(xintercept = 30000000, linetype = "dashed", color = "black") +  # 30M COP
  scale_x_continuous(labels = label_number(scale = 1/1e6, suffix = "M COP")) +  # Formato correcto
  labs(
    title = element_blank(), #"Curva de Aceptabilidad Costo-Efectiva (CEAC)"
    x = "Disposición a Pagar (Millones de COP por QALY ganado)",
    y = "Probabilidad de ser costo-efectivo"
  ) +
  theme_classic()







############################################################
# 10. ANÁLISIS DE SENSIBILIDAD POR TAMAÑO DE MUESTRA =======
############################################################

# ---- 10.1 Evaluar el ICER cambiando el número de usuarios ----
grupo_intervencion <- data %>% filter(Tratamiento == "Sacubitril/Valsartán")
grupo_control <- data %>% filter(Tratamiento == "Losartán")

# Definir un rango de tamaños de muestra
num_users <- round(seq(50, min(nrow(grupo_intervencion), nrow(grupo_control)), length.out = 20))

# Calcular ICER para cada tamaño
icer_values <- sapply(num_users, function(n) {
  set.seed(42)
  sample_intervencion <- grupo_intervencion %>% sample_n(n, replace = FALSE)
  sample_control <- grupo_control %>% sample_n(n, replace = FALSE)
  
  delta_costo <- mean(sample_intervencion$Costo_Total) - mean(sample_control$Costo_Total)
  delta_qaly <- mean(sample_intervencion$QALY) - mean(sample_control$QALY)
  
  delta_costo / delta_qaly
})

icer_df <- data.frame(N_Usuarios = num_users, ICER = icer_values)

# ---- 10.2 Graficar el análisis de sensibilidad ----
puntos_destacados <- icer_df[c(3, 4), ]  # Ejemplo de puntos a resaltar
puntos_destacados$N_Usuarios <- round(puntos_destacados$N_Usuarios, 0)

# Cargar librería de escalas si no está instalada
library(scales)

# Graficar el análisis de sensibilidad del ICER en función del número de usuarios
ggplot(icer_df, aes(x = N_Usuarios, y = ICER / 1e6)) +  # Convertimos ICER a millones
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", size = 0.3) +  # Línea discontinua en Y = 0
  geom_line(color = "navy", size = 0.6) +
  geom_point(data = puntos_destacados, aes(x = N_Usuarios, y = ICER / 1e6), color = "red", size = 1.5) +
  scale_y_continuous(labels = comma_format(suffix = " M COP/QALY")) +  # Formato corregido
  labs(
    title = element_blank(), #"Análisis de sensibilidad del ICER en función del número de usuarios"
    x = "Número de usuarios",
    y = "ICER (Millones de COP/QALY)"
  ) +
  theme_classic()






















modelo <- lm(Costo_Total ~ QALY + Edad + Sexo + Comorbilidades, data = data)
summary(modelo)

icer_ajustado <- lm((Costo_Total ~ Tratamiento), data = data)
summary(icer_ajustado)

