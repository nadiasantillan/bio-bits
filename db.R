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
# Determinación de estación en base a la fecha
source("./fechas.R")

# Bibliotecas
#------------------------------------------------------
library(readxl)
library(tidyverse)
# ----------------- Creación de la base de datos --------------------------#
dataset_name <- "data/pmed.1002587.s005.xlsx"

melatonine_participante <- read_excel(dataset_name, sheet = "Combined", range="A1:H3735")
melatonine_lat_subj <- read_excel(dataset_name, sheet = "Combined", range="W1:W3735")
melatonine_fecha_actigrafo <- read_excel(dataset_name, sheet = "Combined", range="AG1:AG3735")
melatonine_actigrafo <- read_excel(dataset_name, sheet = "Combined", range="AM1:AT3735")
melatonine <- cbind(
  melatonine_participante, 
  melatonine_lat_subj, 
  melatonine_fecha_actigrafo, 
  melatonine_actigrafo)
melatonine$anio_mes <- format(melatonine$Date_Onset_ACT, "%Y%m")
melatonine$anio_mes <- factor(format(melatonine$Date_Onset_ACT, "%Y%m"))
melatonine$Mes <- factor(month(melatonine$Date_Onset_ACT))
melatonine$estacion <- factor(sapply(melatonine$Date_Onset_ACT, estacion))

# Remoción de columnas no relevantes
melatonine[, c("Matching Diary with Actigraphy")] <- NULL

# ------------------------- Variables descriptivas auxiliares --------------------------#
melatonine$TratamientoDesc <- factor(ifelse(melatonine$Treatment == 1, "Placebo", "Melatonina 0.5 mg"))
melatonine$TrabajaDesc <- factor(ifelse(melatonine$`Work/Non-work` == 1, "Obligaciones", "Descanso"))

#--------------------------- Cambio de nombre de variables --------------------------#
colnames(melatonine)[colnames(melatonine) == "Work/Non-work"] <- "Work_status"
colnames(melatonine)[colnames(melatonine) == "Delayed/Not Delayed"] <- "Delayed_status"
#------------------ Conservacion del data frame original ----------------------#
melatonine_orig <- data.frame(melatonine)
#--------------------------- Eliminación de NAs ---------------------------#
variables_modelos <- c('SET1_ACT', 'Work_status', 'Treatment', 'StudyPeriodWeek', 
                        'ParticipantID',"TIB_ACT", "TST_ACT", "SOL_ACT", 
                        "WASO_ACT", "SET2_ACT", "SET3_ACT")
data_modelos <- melatonine[, variables_modelos]
melatonine <- melatonine[complete.cases(data_modelos),]
#------------------------- Corrección SET1_ACT=0 --------------------------#
melatonine <- melatonine %>%
  mutate(SET1_ACT = if_else(SET1_ACT == 0, SE_ACT, SET1_ACT))
#------------------------- Conversion a porcentaje-------------------------#
melatonine$SET1_ACT<- melatonine$SET1_ACT/100
#------------ Eliminar registros sin SOL_ACT ni SOL_SD --------------------
# melatonine <- melatonine %>%
#   filter(SOL_ACT > 10 & !is.na(SOL_SD) & as.numeric(SOL_SD) > 10)

#--------------------------- Promedios Semana 0 ---------------------------#
promedios_base <- melatonine %>%
  filter(StudyPeriodWeek == 0) %>%
  group_by(ParticipantID) %>% 
  summarise(
    SOL_ACT_AVG_BASE = mean(SOL_ACT),
    SET1_ACT_AVG_BASE = mean(SET1_ACT)
  )

melatonine <- inner_join(melatonine, promedios_base, by = "ParticipantID")
melatonine_base <- melatonine %>%
  filter(StudyPeriodWeek == 0)
melatonine <- melatonine %>%
  filter(StudyPeriodWeek != 0)

#------------------------Convertir a factores--------------------#
convertir_a_factores <- function(melatonine) {
  melatonine$StudyPeriod <- factor(melatonine$StudyPeriod)
  melatonine$StudyPeriodWeekFactor <- factor(melatonine$StudyPeriodWeek)
  melatonine$ParticipantID <- factor(melatonine$ParticipantID)
  melatonine$Treatment <- factor(melatonine$Treatment)
  melatonine$Work_status <- factor(melatonine$Work_status)
  return(melatonine)
}

melatonine<-convertir_a_factores(melatonine)
melatonine_base<-convertir_a_factores(melatonine_base)

melatonine <- melatonine %>%
  mutate(
    # Primero aseguramos que SOL_SD sea numérica
    SOL_SD_num = as.numeric(SOL_SD), 
    # Ahora sí hacemos el reemplazo
    SOL_ACT_hibrido = if_else(SOL_ACT < 10, SOL_SD_num, SOL_ACT)
  )

melatonine_base <- melatonine_base %>%
  mutate(
    # Primero aseguramos que SOL_SD sea numérica
    SOL_SD_num = as.numeric(SOL_SD), 
    # Ahora sí hacemos el reemplazo
    SOL_ACT_hibrido = if_else(SOL_ACT < 10, SOL_SD_num, SOL_ACT)
  )
