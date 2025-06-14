
# ============================================================
# Proyecto: Análisis de los Hábitos Digitales y la Salud Mental
# Autor: Aldair Yauri Altamirano
# ============================================================

# ---------------------------
# DEFINICIÓN DEL CONJUNTO DE DATOS
# ---------------------------
# Este conjunto de datos sintéticos está diseñado para simular 
# el comportamiento digital de jóvenes pertenecientes a la Generación Z 
# y explorar sus posibles efectos sobre la salud mental.
# Se basa en la generación artificial de patrones estadísticos 
# que imitan comportamientos reales sin comprometer la privacidad.

# ---------------------------
# OBJETIVO GENERAL
# ---------------------------
# Analizar la relación entre los hábitos digitales y la salud mental 
# en jóvenes de la Generación Z, mediante la identificación de patrones, 
# correlaciones y predictores significativos que permitan generar conciencia 
# y orientar futuras intervenciones en el ámbito de la salud pública.

# ---------------------------
# DICCIONARIO DE VARIABLES
# ---------------------------
# screen_time_hours              → Tiempo total diario frente a pantallas digitales (en horas).
# social_media_platforms_used    → Número de plataformas de redes sociales utilizadas al día.
# hours_on_TikTok                → Tiempo promedio diario dedicado exclusivamente a TikTok (en horas).
# sleep_hours                    → Número promedio de horas de sueño por día.
# stress_level                   → Nivel de estrés percibido (escala de 1 a 10).
# mood_score                     → Nivel de estado de ánimo (escala de 1 a 10, donde un valor mayor indica mejor estado de ánimo).

# ---------------------------
# FUENTE DE LOS DATOS
# ---------------------------
# Plataforma: Kaggle
# URL: https://www.kaggle.com/datasets/abhishekdave9/digital-habits-vs-mental-health-dataset/data

# ---------------------------
# 1. CARGA DE PAQUETES
# ---------------------------
# Caso de necesitar instalar un paquete usa:
# ejemplo -> install.packages("tidyverse")

library(tidyverse) # Manipulación y visualización de datos
library(readr)     # Lectura de archivos CSV
library(ggplot2)   # Gráficos

# ---------------------------------
# 2. CARGA DE DATOS Y EXPLORACION 
# ---------------------------------

datos <- read_csv("digital_habits_vs_mental_health.csv")

# Cambiar el nombre las columnas 
names(datos) <- c("tiempo_pantalla", "numero_redes", 
                  "horas_tiktok", "promedio_sueño",
                  "nivel_estres","estado_animo")
View(datos)

glimpse(datos) # filas, columnas y tipo de dato
summary(datos) # resumen estadistico

# ---------------------------------
# 3. LIMPIEZA
# ---------------------------------
colSums(is.na(datos)) # Verificar valores nulos

ggplot(stack(datos), aes(x = ind, y = values)) + 
  geom_boxplot() # outliers

# Todos las variables estan completas (no hay nulos)

# Los outliers fueron conservados debido 
# a que no presentan valores extremos 
# ni inconsistencias, siendo moderados y plausibles 
# dentro del contexto del estudio.

# ---------------------------------------
# 4. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# ---------------------------------------

# a) Correlaciones entre variables

correlacion <- cor(datos)
print(round(correlacion, 2))


# b) Grafico de Dispersion del Estado de Animo 

# Convertir en columna
dispersion <- datos %>%
  select(estado_animo, tiempo_pantalla, horas_tiktok, promedio_sueño, nivel_estres) %>%
  pivot_longer(cols = -estado_animo, names_to = "variable", values_to = "valor")

# %>% encadenar
# select -> seleccionar todas las variables
# pivot_longer -> transforma en columnas 
# excepto -> estado_animo 

# Ejemplo:
# ANTES:
# estado_animo|tiempo_pantalla|horas_tiktok|promedio_sueño|nivel_estres|
#   3               5.2              2.1            6.5         7            

# DESPUES:
# estado_animo | variable         | valor |
#   3          | tiempo\_pantalla | 5.2   |
#   3         | horas\_tiktok    | 2.1   |
#   3          | promedio\_sueño  | 6.5   |
#   3          | nivel\_estres    | 7     |
  
# Grafico:
ggplot(dispersion, aes(x = valor, y = estado_animo)) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1.3) +
  facet_wrap(~variable, scales = "free_x") +
  labs(title = "Líneas de regresión entre variables y estado de ánimo",
       x = "Variables explicativas",
       y = "Estado de ánimo") +
  theme_minimal()


# c) Histogramas por variable

# Convertir en columna
histograma <- datos %>% 
  pivot_longer(cols = everything()) 

# everything() -> Todas las variables se vuelven columnas

# Grafico:
ggplot(histograma, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "Distribución de variables del estudio",
       x = "Valor",
       y = "Frecuencia")


# ---------------------------
# 5. MODELO DE REGRESIÓN MÚLTIPLE
# ---------------------------

modelo_lineal <- lm(estado_animo ~ tiempo_pantalla + numero_redes + 
                      horas_tiktok + promedio_sueño + nivel_estres, 
                    data = datos)
summary(modelo_lineal)

# Interpretacion:
# intercepto: 6.07 *** -> Constante 

# tiempo_pantalla: -0.141 *** 
# Cada hora adicional Cada hora adicional 
# frente a pantallas se asocia con una 
# reducción de 0.141 puntos en el estado de ánimo

# numero_redes: 0.0017 (no significativo)
# horas_tiktok: -0.236 ***
# Cada hora adicional en TikTok se relaciona 
# con una disminución de 0.236 puntos en el 
# estado de ánimo.

# promedio_sueño: 0.604 ***
# Cada hora adicional de sueño mejora el 
# estado de ánimo en 0.604 puntos.

# nivel_estres: 0.029 ***
# Un mayor nivel de estrés se asocia con 
# una leve mejora en el estado de ánimo
# --> Debe ser negativo 

# Estadisticas:
# F-statistic => significancia en conjunto de las variables
# R-squared => El estado de ánimo está siendo 
# predicho en un 57% por las variables independientes

# ---------------------------
# 6. MODELO CUADRATICO
# ---------------------------

# Visualizar el grafico del estres y estado animo
ggplot(datos, aes(x = nivel_estres, y = estado_animo)) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1.2) +
  labs(title = "Relación entre nivel de estrés y estado de ánimo",
       x = "Nivel de estrés", y = "Estado de ánimo") +
  theme_minimal()

# Se evidencia una relacion no lineal (cuadratica)


modelo_cuad <- lm(estado_animo ~ tiempo_pantalla + numero_redes +
                    horas_tiktok + promedio_sueño + nivel_estres + 
                    I(nivel_estres^2), data = datos)
summary(modelo_cuad)

# nivel_estres: 0.67 ***
# nivel_estres^2: -0.05 ***

# Niveles bajos estres el estado de animo aumenta ligeramente.
# Niveles altos estres el estado de animo disminuye 

# Mejorar en los estadisticos -> R-squared

## ---------------------------
# 7. CONCLUSIONES
# ---------------------------

# a) El tiempo frente a pantallas y el uso excesivo de TikTok 
# tienen un efecto negativo en el estado de ánimo de los jóvenes, 
# por lo tanto, se recomienda reducir su uso.

# b) Mantener un nivel de sueño adecuado mejora significativamente 
# el estado de ánimo y la productividad de los jóvenes.

# c) Un nivel moderado de estrés puede elevar el estado de ánimo, 
# pero a mayor intensidad, el estrés afecta negativamente la salud mental.

# d) El modelo presenta un poder explicativo del 62%, 
# mostrando estadísticos sólidos y signos coherentes con la teoría.

# e) El uso excesivo de tecnología y redes sociales impacta negativamente 
# en la salud mental de los jóvenes, evidenciando la necesidad de un consumo digital equilibrado.







