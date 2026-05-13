#-------------------Base de Datos----------------------#
#-----------------Materia Optativa I-------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

# ----------------Ruta del archivo --------------------#
# Cambiar de acuerdo a la ubicación de los archivos de datos
# setwd("C:/Users/Usuario/Documents/UNSL/Cuarto Año/Optativa I")
setwd("~/unsl/bio/scripts")

# Scripts auxiliares
#------------------------------------------------------
# Determinación de estación en base a la fecha
source("./fechas.R")

# Bibliotecas
#------------------------------------------------------
library(readxl)
library(tidyverse)
# ----------------- Creación de la base de datos --------------------------#
dataset_name <- "data/pmed.1002587.s005.xlsx"

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
