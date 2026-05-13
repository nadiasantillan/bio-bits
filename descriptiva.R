#----------------Análisis descriptivo------------------#
#-----------------Materia Optativa I-------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

setwd("~/unsl/bio/scripts")
# Scripts auxiliares
#------------------------------------------------------
source("./db.R")

# Bibliotecas
#------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(dplyr)
# ----------------- Escrutinio de Datos Faltantes (NAs) -------------------#
# El artículo menciona que no hubo imputación; se verifica cuántos nulls hay por variable (sobre un total 3734)
nas <- colSums(is.na(melatonine[, c("Delayed_status", "SOL_ACT", "SET1_ACT", "Date_Onset_ACT")])) %>%
                 as.data.frame() %>%
                 rownames_to_column("Variable") %>%
                 rename(Value = 2)
ggplot(nas, aes(x = Variable, y = Value)) +
  geom_col(fill = "steelblue") +
  labs(title="Cantidad de datos faltantes", y = "Cantidad faltantes")
  theme_minimal()               
               
# ------------------ Distribuciones de variables categóricas --------------------------#
x11(50,30); # Sistema Operativo Linux
# windows(50,30) # Sistema operativo Windows
par(mfrow=c(1,4))
barplot(table(melatonine$TratamientoDesc), main="Tratamiento", xlab="Tratamiento", ylab="Frecuencia")
barplot(table(melatonine$Delayed_status), main="Diagnóstico retraso sueño", xlab="Retraso sueño", ylab="Frecuencia")
barplot(table(melatonine$TrabajaDesc), main="Trabaja día registro", xlab="Trabaja", ylab="Frecuencia")
barplot(table(melatonine$StudyPeriodWeek), main="Semana de estudio", xlab="Semana", ylab="Frecuencia")
  
# ------------------- Diagramas de caja para las variables continuas ------------------#
# windows(50, 30); # Sistema operativo Windows
x11(50,30); # Sistema Operativo Linux
par(mfrow=c(1,2))
boxplot(melatonine$SOL_ACT, col="paleturquoise4", ylab="Latencia de inicio de sueño - SOL (minutos)")
boxplot(melatonine$SET1_ACT, col="paleturquoise4", ylab="Eficiencia del sueño 1er tercio - SET1 (%)")

# -------------------- Histogramas variables continuas --------------------------------#
x11(50,30); # Sistema Operativo Linux
# windows(50, 30); # Sistema operativo Windows
par(mfrow=c(1,2))
hist(melatonine$SOL_ACT, breaks=30, main="Latencia de Sueño - Base", xlab="SOL (minutos)", ylab="Frecuencia")
hist(melatonine$SET1_ACT, breaks=30, main="Eficiencia sueño - 1er tercil - Base", xlab="SET1 (%)", ylab="Frecuencia")

# ------------------- Varianzas por tratamiento y semana -------------------------------#
# windows()
x11();
ggplot(melatonine, aes(x = factor(TratamientoDesc), fill=StudyPeriodWeek, y = SOL_ACT)) +
  geom_boxplot() +
  labs(title = "Latencia de Sueño",
       x = "Tratamiento",
       y = "Latencia de Sueño (min)") +
  theme_minimal()
x11();
# windows()
ggplot(melatonine, aes(x = factor(TratamientoDesc), fill = StudyPeriodWeek, y = SET1_ACT)) +
  geom_boxplot() +
  labs(title = "Eficiencia sueño - 1er tercio",
       x = "Tratamiento",
       y = "Eficiencia sueño (%)") +
  theme_minimal()

# ---------------- Independencia: Observaciones por Participante ------------------------#
obs_por_sujeto <- table(melatonine$ParticipantID)
hist(obs_por_sujeto, main="Nro de noches registradas por participante")

# ------------------------------ Análisis de fechas -------------------------------------#
meses <- as.data.frame(table(melatonine$anio_mes))

x11(20,10);
# windows(20,10);
ggplot(meses, aes(x=Var1, y=Freq)) + 
  geom_col(color="white", fill="paleturquoise4") +
  labs(x = "Mes", y = "Observaciones", title = "Meses abarcados por el estudio")

# ----------------------------- Lapso entre semana 0 y 1 -------------------------------------#
ultimo_dia_semana_0 <-melatonine |>
  select(ParticipantID, StudyPeriodWeek, Date_Onset_ACT) |>
  filter(StudyPeriodWeek == 0) |>
  na.omit() |>
  group_by(ParticipantID) |> 
  summarise(
    fin_semana_0 = max(Date_Onset_ACT)
  )
primer_dia_semana_1 <-melatonine |>
  select(ParticipantID, StudyPeriodWeek, Date_Onset_ACT) |>
  filter(StudyPeriodWeek == 1) |>
  na.omit() |>
  group_by(ParticipantID) |> 
  summarise(
    comienzo_semana_1 = min(Date_Onset_ACT)
  )

diff_semanas_0_1 <- merge(ultimo_dia_semana_0, primer_dia_semana_1, by = "ParticipantID")
diff_semanas_0_1$dias <- as.numeric(diff_semanas_0_1$comienzo_semana_1 - diff_semanas_0_1$fin_semana_0)
x11()
ggplot(as.data.frame(table(diff_semanas_0_1$dias)), aes(x=Var1, y=Freq)) + 
  geom_col(color="white", fill="paleturquoise4") +
  labs(x = "Lapso entre semanas 0 - 1 (días)", y = "Cantidad", title = "Lapso de tiempo entre la semana base y el tratamiento")

# ----------------------------- Lapso entre semana 1 y 5 -------------------------------------#
ultimo_dia_semana_4 <-melatonine |>
  select(ParticipantID, StudyPeriodWeek, Date_Onset_ACT) |>
  filter(StudyPeriodWeek == 4) |>
  na.omit() |>
  group_by(ParticipantID) |> 
  summarise(
    fin_semana_4 = max(Date_Onset_ACT)
  )

fechas_por_participante <- merge(ultimo_dia_semana_4, primer_dia_semana_1, by = "ParticipantID")
fechas_por_participante$dias <- as.numeric(fechas_por_participante$fin_semana_4 - fechas_por_participante$comienzo_semana_1)

x11()
ggplot(as.data.frame(table(fechas_por_participante$dias)), aes(x=Var1, y=Freq)) + 
  geom_col(color="white", fill="paleturquoise4") +
  labs(x = "Duración tratamiento (días)", y = "Cantidad", title = "Lapso de tiempo entre el comienzo y fin del tratamiento")

scatter_data <- melatonine[, c("ParticipantID", "estacion")]
scatter_data$ParticipantID <- as.character(melatonine$ParticipantID)
str(scatter_data)
x11()
ggplot(melatonine, aes(y=ParticipantID, x=estacion)) + 
  geom_point()


