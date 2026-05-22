#-------------------Base de Datos----------------------#
#-----------------Materia Optativa I-------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

# ----------------Ruta del archivo --------------------#
# Cambiar de acuerdo a la ubicación de los archivos de datos
 setwd("C:/Users/Usuario/Documents/UNSL/Cuarto Año/Optativa I")
#setwd("~/unsl/bio/scripts")

# Scripts auxiliares
#------------------------------------------------------
# Determinación de estación en base a la fecha
 library(lubridate)
 library(dplyr)
 
 estacion <- function(fecha) {
   if (is.na(fecha)) {
     e <- NA    
   } else {
     m <- month(fecha)  
     d <- day(fecha)
     
     if (d >= 21 && m == 3 || m %in% c(4, 5) || d < 21 && m == 6) {
       e <- "Otoño"
     } else if (d >= 21 && m == 6 || m %in% c(7, 8) || d < 21 && m == 9) {
       e <- "Invierno"
     } else if (d >= 21 && m == 9 || m %in% c(10, 11) || d < 21 && m == 12) {
       e <- "Primavera"
     } else {
       e <- "Verano"
     }
     
   }
   e
 }
#-----------------------------------------------------
# Bibliotecas
#------------------------------------------------------
library(readxl)
library(tidyverse)
# ----------------- Creación de la base de datos --------------------------#
dataset_name <- "C:/Users/Usuario/Documents/UNSL/Cuarto Año/Optativa I/pmed.1002587.s005.xlsx"

melatonine_participante <- read_excel(dataset_name, sheet = "Combined", range="A1:H3735")
melatonine_fecha_actigrafo <- read_excel(dataset_name, sheet = "Combined", range="AG1:AG3735")
melatonine_actigrafo <- read_excel(dataset_name, sheet = "Combined", range="AM1:AT3735")
melatonine <- cbind(melatonine_participante, melatonine_fecha_actigrafo, melatonine_actigrafo)
melatonine$anio_mes <- format(melatonine$Date_Onset_ACT, "%Y%m")
melatonine$anio_mes <- factor(format(melatonine$Date_Onset_ACT, "%Y%m"))
melatonine$Mes <- factor(month(melatonine$Date_Onset_ACT))
melatonine$estacion <- factor(sapply(melatonine$Date_Onset_ACT, estacion))

# ------------------------- Variables descriptivas auxiliares --------------------------#
melatonine$TratamientoDesc <- factor(ifelse(melatonine$Treatment == 1, "Placebo", "Melatonina 0.5 mg"))
melatonine$TrabajaDesc <- factor(ifelse(melatonine$`Work/Non-work` == 1, "Obligaciones", "Descanso"))

#--------------------------- Cambio de nombre de variables --------------------------#
colnames(melatonine)[colnames(melatonine) == "Work/Non-work"] <- "Work_status"
colnames(melatonine)[colnames(melatonine) == "Delayed/Not Delayed"] <- "Delayed_status"

#--------------------------- Promedios Semana 0 ---------------------------#
promedios_base <- melatonine %>%
  filter(StudyPeriodWeek == 0) %>%
  group_by(ParticipantID) %>% 
  summarise(
    SOL_ACT_AVG_BASE = mean(SOL_ACT),
    SET1_ACT_AVG_BASE = mean(SET1_ACT)
  )

melatonine <- inner_join(melatonine, promedios_base, by = "ParticipantID")
melatonine <- melatonine %>%
  filter(StudyPeriodWeek != 0)
melatonine$StudyPeriodWeek <- factor(melatonine$StudyPeriodWeek)
#------------------------Convertir a factor a ParticipantID--------------------#
melatonine$ParticipantID <- as.factor(melatonine$ParticipantID)
melatonine$Treatment <- as.factor(melatonine$Treatment)
melatonine$Work_status <- as.factor(melatonine$Work_status)
str(melatonine)
#==============================================================================
#===============================================================================
library(tidyverse)
library(glmmTMB)
library(lubridate)

# Variables del actígrafo que vamos a analizar juntas
vars_act <- c("SOL_ACT", "SET1_ACT", "TST_ACT", "TIB_ACT", "SE_ACT", "SET2_ACT", "SET3_ACT")

# El Modelo Mixto Multivariado (GLMM)
# Incluye interacción Tratamiento*Tiempo y efecto aleatorio por Participante

melatonine$SET1_ACT<- melatonine$SET1_ACT/100

data_model <- melatonine[, c("SOL_ACT","SET1_ACT","Work_status","Treatment", "StudyPeriodWeek","SOL_ACT_AVG_BASE","SET1_ACT_AVG_BASE", "ParticipantID")]
x <- na.omit(data_model)
borrados <- na.action(x)
data_nona <- data_model[-borrados,]
data_nona <-data_nona %>% 
  filter(SET1_ACT>0)

# data_nona <- melatonine

model_solact <- glmmTMB(
  SOL_ACT ~ Work_status + Treatment + StudyPeriodWeek +SOL_ACT_AVG_BASE +SET1_ACT_AVG_BASE + (1 | ParticipantID),
  data = data_nona,
  # family = Gamma(link = log) #R2 condicional 0.096
  # family = gaussian()
  # family = tweedie(link = "log") R2 condicional 0.229 mismo que normal
  family = ziGamma(link = "log"), 
  ziformula=~1
)

summary(model_solact)
Anova(model_solact)
# No significativo la condición de trabajo (como demostraba el summary), y el SET1_ACT es mayor a 0.05 (Prueba de Wald)

print("--- VIF: Modelo de Latencia (SOL_ACT) ---")
vif_solact <- check_collinearity(model_solact)
print(vif_solact)
# El análisis de inflación de la Varianza (VIF) muestra una baja correlación (poca multicolinealidad) entre las variables 
# predictoras en el modelo de latencia (SOL_ACT). Valores entre 1 y 2 (menores a 5). 


model_set1 <- glmmTMB(
  SET1_ACT ~ Work_status + Treatment + StudyPeriodWeek +SOL_ACT_AVG_BASE +SET1_ACT_AVG_BASE  + (1 | ParticipantID),
  # Valor_Z ~ Metrica + Work_status + Treatment * Semana_Num + (1 | ParticipantID | Mes | estacion), hacer con sol_act y se_act
  # interacciones entre tratamiento y semana de tratamiento
  data = data_nona,
  family = beta_family()
)
summary(model_set1)
Anova(model_set1)
# No significativa la semana de estudio y SOL_ACT_AVG_BASE. En este caso si dio significativo el trabajo.

print("--- VIF: Modelo de Eficiencia (SET1_ACT) ---")
vif_set1 <- check_collinearity(model_set1)
print(vif_set1)

# El análisis de inflación de la Varianza (VIF) muestra una baja correlación (poca multicolinealidad) entre las variables 
# predictoras en el modelo de latencia (SET1_ACT). Valores entre 1 y 2 (menores a 5). 

#summary(data_nona$SET1_ACT)
#is.na(data_nona)
#data_nona[is.na(data_nona)]
#summary(data_nona)

# Grafico

# Quitamos work_status para simplificar el gráfico, siendo no significativo para SOL y si para SET1.

#if(!require(sjPlot)) install.packages("sjPlot")
library(sjPlot)

# windows(width = 12, height = 8)
x11(width = 12, height = 8)
plot_model(model_solact, 
           type = "pred", 
           terms = c("StudyPeriodWeek", "Treatment"),,
           ci.lvl = 0.95) +
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  labs(title = "Predicciones del Modelo Mixto: Efecto del Tratamiento",
       subtitle = "Controlado por Estacionalidad y Situación Laboral",
       x = "Semana de Estudio", 
       y = "Latencia de sueño",
       color = "Grupo") +
  theme_minimal()

x11(width = 12, height = 8)
plot_model(model_set1, 
           type = "pred", 
           terms = c("StudyPeriodWeek", "Treatment"),
           ci.lvl = 0.95) +
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  labs(title = "Predicciones del Modelo Mixto: Efecto del Tratamiento",
       subtitle = "Controlado por Estacionalidad y Situación Laboral",
       x = "Semana de Estudio", 
       y = "Eficiencia de sueño en el primer tercio",
       color = "Grupo") +
  theme_minimal()

# Grafico similar donde sumamos las observaciones reales al final SOL_ACT.

predicciones_sol <- get_model_data(model_solact, type = "pred", terms = c("StudyPeriodWeek", "Treatment"))

x11(width = 11, height = 7)
ggplot() +
  geom_jitter(data = data_nona, 
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

# Grafico similar donde sumamos las observaciones reales al final SOL_ACT.
predicciones_set1 <- get_model_data(model_set1, type = "pred", terms = c("StudyPeriodWeek", "Treatment"))

x11(width = 11, height = 7)
ggplot() +
  # Capa de puntos: Datos reales comprimidos de fondo
  geom_jitter(data = data_nona, 
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

library(lme4)
library(performance)
# R2 marginal: Es la varianza explicada exclusivamente por los efectos fijos 
# (las variables predictoras principales).
# R2 condicional: Es la varianza explicada por el modelo completo 
# (efectos fijos + efectos aleatorios combinados)
r2_nakagawa(model_solact, ci = NULL)
r2_nakagawa(model_set1, ci = NULL)

