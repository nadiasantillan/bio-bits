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
melatonine$Work_status <- factor(melatonine$Work_status)
str(melatonine)
# Trmelatonine# Transformacion de dia de semana y trabaja o no trabaja.
data_preparada <- melatonine %>%
  mutate(
    # Work_status = ifelse(Work_status == 0, NA, Work_status),
    Semana_Num = as.numeric(StudyPeriodWeek),
    Mes = factor(month(as.Date(Date_Onset_ACT)))
  )

# Transformación a Formato Largo y Centrado Z 
data_z <- data_preparada %>%
  pivot_longer(cols = all_of(vars_act), 
               names_to = "Metrica", 
               values_to = "Valor") %>%
  drop_na(Valor) %>% 
  group_by(Metrica) %>%
  mutate(Valor_Z = as.vector(scale(Valor))) %>%
  ungroup()

vars_act <- c("SOL_ACT", "SET1_ACT", "TST_ACT", "TIB_ACT", "SE_ACT", "SET2_ACT", "SET3_ACT")
# El Modelo Mixto Multivariado (GLMM)
# Incluye interacción Tratamiento*Tiempo y efecto aleatorio por Participante
fit_final <- glmmTMB(
  Valor_Z ~ Metrica + Work_status + Treatment * StudyPeriodWeek + (1 | ParticipantID/estacion),
  # Valor_Z ~ Metrica + Work_status + Treatment * Semana_Num + (1 | ParticipantID | Mes | estacion), hacer con sol_act y se_act
  # interacciones entre tratamiento y semana de tratamiento
  data = data_z,
  family = gaussian()
)

summary(fit_final)

# Grafico
if(!require(sjPlot)) install.packages("sjPlot")
library(sjPlot)

# windows(width = 12, height = 8)
x11(width = 12, height = 8)
plot_model(fit_final, 
           type = "pred", 
           terms = c("StudyPeriodWeek", "Treatment", "estacion", "Metrica", "Work_status"),
           ci.lvl = 0.95) +
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  labs(title = "Predicciones del Modelo Mixto: Efecto del Tratamiento",
       subtitle = "Controlado por Estacionalidad y Situación Laboral",
       x = "Semana de Estudio", 
       y = "Valor Z (Desviaciones Estándar)",
       color = "Grupo") +
  theme_minimal()

unique(data_z$Metrica)
