#-------------------Base de Datos----------------------#
#-----------------Materia Optativa I-------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

# ----------------Ruta del archivo --------------------#
# Cambiar de acuerdo a la ubicación de los archivos de datos
# setwd("C:/Users/Usuario/Documents/UNSL/Cuarto Año/Optativa I")

# Scripts auxiliares
#------------------------------------------------------
source("./db.R")
source("./common.R")

#-----------------------------------------------------
# Bibliotecas
#------------------------------------------------------
library(tidyverse)
library(glmmTMB)
library(lubridate)
library(car)
library(performance)
library(sjPlot)

model_solact_hibrido <- glmmTMB(
  SOL_ACT_hibrido ~ Treatment + StudyPeriodWeekFactor +SOL_ACT_AVG_BASE +SET1_ACT_AVG_BASE + (1 | ParticipantID),
  data = melatonine,
  # family = Gamma(link = log) #R2 condicional 0.096
  # family = gaussian()
  # family = tweedie(link = "lg") R2 condicional 0.229 mismo que normal
  family = ziGamma(link = "log"),
  ziformula = ~StudyPeriodWeekFactor
)

summary(model_solact_hibrido)
Anova(model_solact_hibrido)
# No significativo la condición de trabajo (como demostraba el summary), y el SET1_ACT es mayor a 0.05 (Prueba de Wald)

print("--- VIF: Modelo de Latencia (SOL_ACT) ---")
vif_solact <- check_collinearity(model_solact_hibrido)
print(vif_solact)
# El análisis de inflación de la Varianza (VIF) muestra una baja correlación (poca multicolinealidad) entre las variables 
# predictoras en el modelo de latencia (SOL_ACT). Valores entre 1 y 2 (menores a 5). 


model_set1 <- glmmTMB(
  SET1_ACT ~ Treatment + StudyPeriodWeekFactor + SOL_ACT_AVG_BASE +SET1_ACT_AVG_BASE  + (1 | ParticipantID),
  data = melatonine,
  family = beta_family()
)
summary(model_set1)
Anova(model_set1)
# No significativa la semana de estudio y SOL_ACT_AVG_BASE. En este caso si dio significativo el trabajo.

print("--- VIF: Modelo de Eficiencia (SET1_ACT) ---")
vif_set1 <- check_collinearity(model_set1)
print(vif_set1)

# R2 marginal: Es la varianza explicada exclusivamente por los efectos fijos 
# (las variables predictoras principales).
# R2 condicional: Es la varianza explicada por el modelo completo 
# (efectos fijos + efectos aleatorios combinados)
r2_nakagawa(model_solact_hibrido, ci = NULL)
r2_nakagawa(model_set1, ci = NULL)

# El análisis de inflación de la Varianza (VIF) muestra una baja correlación (poca multicolinealidad) entre las variables 
# predictoras en el modelo de latencia (SET1_ACT). Valores entre 1 y 2 (menores a 5). 

# Grafico
# Quitamos work_status para simplificar el gráfico, siendo no significativo para SOL y si para SET1.

#if(!require(sjPlot)) install.packages("sjPlot")

ventana(width = 12, height = 8)
# png("img/model_solact_hibrido.png", width=800, height = 600)
plot_model(model_solact_hibrido, 
           type = "pred", 
           terms = c("StudyPeriodWeekFactor", "Treatment"),
           ci.lvl = 0.95) +
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  labs(title = "Predicciones del Modelo Mixto: Efecto del Tratamiento",
       subtitle = "Controlado por Estacionalidad y Situación Laboral",
       x = "Semana de Estudio", 
       y = "Latencia de sueño",
       color = "Grupo") +
  theme_minimal()
# dev.off()
ventana(width = 12, height = 8)
# png("img/model_set1.png", width=800, height = 600)
plot_model(model_set1, 
           type = "pred", 
           terms = c("StudyPeriodWeekFactor", "Treatment"),
           ci.lvl = 0.95) +
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  labs(title = "Predicciones del Modelo Mixto: Efecto del Tratamiento",
       subtitle = "Controlado por Estacionalidad y Situación Laboral",
       x = "Semana de Estudio", 
       y = "Eficiencia de sueño en el primer tercio",
       color = "Grupo") +
  theme_minimal()
# dev.off()
# Grafico similar donde sumamos las observaciones reales al final SOL_ACT.

predicciones_sol <- get_model_data(model_solact_hibrido, type = "pred", terms = c("StudyPeriodWeekFactor", "Treatment"))

ventana(width = 11, height = 7)
# png("img/model_solact_jitter.png", width=800, height = 600)
ggplot() +
  geom_jitter(data = melatonine, 
              aes(x = StudyPeriodWeek, y = SOL_ACT, color = Treatment), 
              alpha = 0.18, width = 0.15, height = 0) +
  geom_line(data = predicciones_sol, 
            aes(x = x, y = predicted, color = group, group = group), 
            linewidth = 1.3) +
  geom_ribbon(data = predicciones_sol, 
              aes(x = x, ymin = conf.low, ymax = conf.high, fill = group, group = group), 
              alpha = 0.15, show.legend = FALSE) +
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  scale_fill_manual(values = c("1" = "firebrick", "2" = "dodgerblue3")) +
  labs(title = "Efecto del Tratamiento en la Latencia de Sueño",
       subtitle = "Líneas: Predicciones del Modelo (ziGamma) | Puntos: Valores Reales de Actigrafía",
       x = "Semana de Estudio", 
       y = "Latencia de sueño (SOL_ACT)",
       color = "Grupo de Estudio") +
  theme_minimal()
# dev.off()
# Grafico similar donde sumamos las observaciones reales al final SOL_ACT.
predicciones_set1 <- get_model_data(model_set1, type = "pred", terms = c("StudyPeriodWeekFactor", "Treatment"))

ventana(width = 11, height = 7)
# png("img/model_set1_jitter.png", width=800, height = 600)
ggplot() +
  # Capa de puntos: Datos reales comprimidos de fondo
  geom_jitter(data = melatonine, 
              aes(x = StudyPeriodWeek, y = SET1_ACT, color = Treatment), 
              alpha = 0.18, width = 0.15, height = 0) +
  # Capa de líneas: Predicciones estimadas por el modelo Beta
  geom_line(data = predicciones_set1, 
            aes(x = x, y = predicted, color = group, group = group), 
            linewidth = 1.3) +
  # Capa de áreas: Bandas de Intervalos de Confianza al 95%
  geom_ribbon(data = predicciones_set1, 
              aes(x = x, ymin = conf.low, ymax = conf.high, fill = group, group = group), 
              alpha = 0.15, show.legend = FALSE) +
  # Estética visual del gráfico
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  scale_fill_manual(values = c("1" = "firebrick", "2" = "dodgerblue3")) +
  labs(title = "Efecto del Tratamiento en la Eficiencia del Primer Tercio",
       subtitle = "Líneas: Predicciones del Modelo (Beta) | Puntos: Valores Reales de Actigrafía",
       x = "Semana de Estudio", 
       y = "Eficiencia de sueño (SET1_ACT)",
       color = "Grupo de Estudio") +
  theme_minimal()
# dev.off()

