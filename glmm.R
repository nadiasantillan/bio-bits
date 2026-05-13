#------------------------GLMM--------------------------#
#-----------------Materia Optativa I-------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

# ----------------------- GLMM Estandarizado ----------------------------------------#  
setwd("~/unsl/bio/scripts")
# Scripts auxiliares
#------------------------------------------------------
source("./db.R")
# Bibliotecas
#------------------------------------------------------
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("glmmTMB")
library(tidyverse)
library(glmmTMB)
library(lubridate)

# Variables del actígrafo que vamos a analizar juntas
vars_act <- c("SOL_ACT", "SET1_ACT", "TST_ACT", "TIB_ACT", "SE_ACT", "SET2_ACT", "SET3_ACT")

# El Modelo Mixto Multivariado (GLMM)
# Incluye interacción Tratamiento*Tiempo y efecto aleatorio por Participante
model_solact <- glmmTMB(
  SOL_ACT ~ Work_status + Treatment * StudyPeriodWeek + (1 | ParticipantID/estacion),
  data = melatonine,
  family = beta_family()
)
summary(model_solact)

model_set1 <- glmmTMB(
  SET1_ACT ~ Work_status + Treatment * StudyPeriodWeek + (1 | ParticipantID/estacion),
  # Valor_Z ~ Metrica + Work_status + Treatment * Semana_Num + (1 | ParticipantID | Mes | estacion), hacer con sol_act y se_act
  # interacciones entre tratamiento y semana de tratamiento
  data = melatonine,
  family = gaussian()
)
summary(model_set1)

# Grafico
if(!require(sjPlot)) install.packages("sjPlot")
library(sjPlot)

# windows(width = 12, height = 8)
x11(width = 12, height = 8)
plot_model(model_solact, 
           type = "pred", 
           terms = c("StudyPeriodWeek", "Treatment", "Work_status", "estacion"),
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
           terms = c("StudyPeriodWeek", "Treatment", "Work_status", "estacion"),
           ci.lvl = 0.95) +
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  labs(title = "Predicciones del Modelo Mixto: Efecto del Tratamiento",
       subtitle = "Controlado por Estacionalidad y Situación Laboral",
       x = "Semana de Estudio", 
       y = "Eficiencia de sueño en el primer tercio",
       color = "Grupo") +
  theme_minimal()


