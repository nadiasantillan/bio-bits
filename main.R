#-------------Análisis Exploratorio--------------------#
#-------------Materia Optativa I ----------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

# ----------------Ruta del archivo --------------------#
# Cambiar de acuerdo a la ubicación de los archivos de datos
# setwd("C:/Users/Usuario/Documents/UNSL/Cuarto Año/Optativa I")
setwd("/home/nadia/unsl/bio/code/unsl-bio/data")

# ------------------Carga de Librerias----------------#
# install.packages("ggplot2")
# install.packages("gridExtra")
library(readxl)
library(ggplot2)
library(gridExtra)
library(ggbiplot)
library(dplyr)
source("/home/nadia/unsl/bio/code/unsl-bio/fechas.R")
# ----------------- Creación de la base de datos --------------------------#
dataset_name <- "pmed.1002587.s005.xlsx"

melatonine_participante <- read_excel(dataset_name, sheet = "Combined", range="A1:H3735")
melatonine_fecha_actigrafo <- read_excel(dataset_name, sheet = "Combined", range="AG1:AG3735")
melatonine_actigrafo <- read_excel(dataset_name, sheet = "Combined", range="AM1:AT3735")
melatonine <- cbind(melatonine_participante, melatonine_fecha_actigrafo, melatonine_actigrafo)
melatonine$anio_mes <- format(melatonine$Date_Onset_ACT, "%Y%m")
melatonine$StudyPeriodWeek <- factor(melatonine$StudyPeriodWeek)
melatonine$anio_mes <- format(melatonine$Date_Onset_ACT, "%Y%m")
melatonine$Mes <- factor(month(melatonine$Date_Onset_ACT))
melatonine$estacion <- sapply(melatonine$Date_Onset_ACT, estacion)
melatonine$estacion <- factor(melatonine$estacion)
str(melatonine)
# ----------------- Escrutinio de Datos Faltantes (NAs) -------------------#
# El artículo menciona que no hubo imputación; se verifica cuántos nulls hay por variable (sobre un total 3734)
colSums(is.na(melatonine[, c("SOL_ACT", "SET1_ACT", "Date_Onset_ACT")]))

# ------------------------- Variables auxiliares --------------------------#
melatonine$TratamientoDesc <- ifelse(melatonine$Treatment == 1, "Placebo", "Melatonina 0.5 mg")
melatonine$TrabajaDesc <- ifelse(melatonine$`Work/Non-work` == 1, "Obligaciones", "Descanso")

# ------------------ Distribuciones de variables categóricas --------------------------#
x11(50,30); # Sistema Operativo Linux
# windows(50,30) # Sistema operativo Windows
par(mfrow=c(1,4))
barplot(table(melatonine$TratamientoDesc), main="Tratamiento", xlab="Tratamiento", ylab="Frecuencia")
barplot(table(melatonine$`Delayed/Not Delayed`), main="Diagnóstico retraso sueño", xlab="Retraso sueño", ylab="Frecuencia")
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

fechas_por_participante <-melatonine |>
  select(ParticipantID, StudyPeriodWeek, Date_Onset_ACT) |>
  na.omit() |>
  group_by(ParticipantID) |> 
  summarise(
    maximum = max(Date_Onset_ACT),
    minimum = min(Date_Onset_ACT),
    n = length(ParticipantID)
  )

fechas_por_participante$dias <- as.numeric(fechas_por_participante$maximum - fechas_por_participante$minimum)
# Se observan varios días de diferencia entre inicio y final del estudio por 
# participante si no se filta la primera semana
head(fechas_por_participante[order(-fechas_por_participante$dias),])


fechas_por_participante <-melatonine |>
  filter(StudyPeriodWeek != 0) |>
  select(ParticipantID, Date_Onset_ACT) |>
  na.omit() |>
  group_by(ParticipantID) |> 
  summarise(
    maximum = max(Date_Onset_ACT),
    minimum = min(Date_Onset_ACT),
    n = length(ParticipantID)
  )
fechas_por_participante$dias <- as.numeric(fechas_por_participante$maximum - fechas_por_participante$minimum)

# Si se consideran las semanas del estudio propiamente dichas, hay como máximo 28 
# días entre comienzo y final del estudio
head(fechas_por_participante[order(-fechas_por_participante$dias),])
######################################################################################
# Modelos
######################################################################################
variables_num <- c("TIB_ACT", "TST_ACT", "SOL_ACT", "SE_ACT", "WASO_ACT", "SET1_ACT", "SET2_ACT", "SET3_ACT")
#--------------------------- Cambio de nombre de variables --------------------------#
colnames(melatonine)[colnames(melatonine) == "Work/Non-work"] <- "Work_status"
colnames(melatonine)[colnames(melatonine) == "Delayed/Not Delayed"] <- "Delayed_status"

# ----------------------- GLMM Estandarizado ----------------------------------------#  
# install.packages("tidyverse")
# install.packages("lubridate")
library(tidyverse)
library(glmmTMB)
library(lubridate)

# Variables del actígrafo que vamos a analizar juntas
vars_act <- c("SOL_ACT", "SET1_ACT", "TST_ACT", "TIB_ACT", "SE_ACT", "SET2_ACT", "SET3_ACT")

# Transformacion de dia de semana y trabaja o no trabaja.
data_preparada <- melatonine %>%
  mutate(
    Work_status = ifelse(Work_status == 0, NA, Work_status),
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

# El Modelo Mixto Multivariado (GLMM)
# Incluye interacción Tratamiento*Tiempo y efecto aleatorio por Participante
fit_final <- glmmTMB(
  Valor_Z ~ Metrica + Mes + Work_status + Treatment * Semana_Num + (1 | ParticipantID),
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
           terms = c("Semana_Num", "Treatment", "Metrica"),
           ci.lvl = 0.95) +
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  labs(title = "Predicciones del Modelo Mixto: Efecto del Tratamiento",
       subtitle = "Controlado por Estacionalidad (Mes) y Situación Laboral",
       x = "Semana de Estudio", 
       y = "Valor Z (Desviaciones Estándar)",
       color = "Grupo") +
  theme_minimal()

unique(data_z$Metrica)
# -------------------------------------- PCA ----------------------------------------#  
library(rgl)
library('glmmTMB')
library('ggeffects')
library(patchwork)
variables_pca <- c("TIB_ACT", "TST_ACT", "SOL_ACT", "SET1_ACT", "WASO_ACT", "SET2_ACT", "SET3_ACT")
variables_pca <- c("TIB_ACT", "TST_ACT", "SOL_ACT", "SET1_ACT", "WASO_ACT")
# variables_pca <- c("SOL_ACT", "SE_ACT", "TST_ACT")

melatonine_pca <- melatonine[, variables_pca] # menos variables en la entrada del PCA
summary(melatonine_pca)

# Original data melatonine_num, cleaned data x
x <- na.omit(melatonine_pca)
borrados <- na.action(x)
str(melatonine)
melatonine_nona <- melatonine[-borrados,]
open3d();plot3d(x[,"SOL_ACT"],x[,"SE_ACT"],x[,"TST_ACT"], type = "s", col = "red", size = 1)

model <- lm(x[,3] ~ x[,1] + x[,2])
coefs <- coef(model)
a <- coefs[2]
b <- coefs[3]
c <- -1
d <- coefs[1]

planes3d(a, b, c, d, alpha = 0.5)

pca <- prcomp(x, scale = T, center = T)

summary(pca)
var_prop <- round((pca$sdev^2)*100/sum(pca$sdev^2),2)

options(rgl.printRglwidget = T)
open3d();plot3d(pca$x[,1],pca$x[,2],pca$x[,3], type = "s", col = "red", size = 1)

x11(50,20)
par(mfrow=c(1,3))
h1 <- ggplot(pca$x, aes(x = PC1)) +
  geom_histogram(color="gray", fill="red4") +
  labs(title = "Componente Principal 1", x = paste("PC1", var_prop[1], "%"), y = "Frecuencia")
h2 <- ggplot(pca$x, aes(x = PC2)) +
  geom_histogram(color="gray", fill="green4") +
  labs(title = "Componente Principal 2", x = paste("PC2", var_prop[2], "%"), y = "Frecuencia")
h3 <- ggplot(pca$x, aes(x = PC3)) +
  geom_histogram(color="gray", fill="blue4") +
  labs(title = "Componente Principal 3", x = paste("PC3", var_prop[3], "%"), y = "Frecuencia")
wrap_plots(h1, h2, h3)

# windows(20, 20);
x11(10,10);
ggbiplot(pca, obs.scale = 1, var.scale = 1,
                     groups=factor(melatonine_nona$TratamientoDesc),
                     point.size=1,
                     varname.size = 4, 
                     varname.color = "firebrick",
                     varname.adjust = 1.2,
                     ellipse = F, 
                     circle = F) +
  theme_minimal() 


pca_for_model <- cbind(melatonine_nona,pca$x)
pca_for_model$TratamientoDesc <- factor(pca_for_model$TratamientoDesc)
dim(pca_for_model)


# Valor_Z ~ Metrica + Work_status + Treatment * Semana_Num + (1 | ParticipantID | Mes | estacion), hacer con sol_act y se_act
# interacciones entre tratamiento y semana de tratamiento

fit_pc1 <- glmmTMB(
  PC1 ~ TratamientoDesc * StudyPeriodWeek + Work_status + Mes + (1| Mes) + (1 | ParticipantID ),
  data = pca_for_model,
  family = gaussian()
)
summary(fit_pc1)

fit_pc2 <- glmmTMB(
  PC2 ~ TratamientoDesc * StudyPeriodWeek + Work_status + Mes + (1| Mes) + (1 | ParticipantID ),
  data = pca_for_model,
  family = gaussian()
)
summary(fit_pc2)

fit_pc3 <- glmmTMB(
  PC3 ~ TratamientoDesc * StudyPeriodWeek + Work_status + Mes + (1| Mes) + (1 | ParticipantID ),
  data = pca_for_model,
  family = gaussian()
)
summary(fit_pc3)

x11(20, 10);plot(ggpredict(fit_pc1, terms = c("TratamientoDesc", "StudyPeriodWeek", "Work_status", "Mes")))
x11(20, 10);plot(ggpredict(fit_pc2, terms = c("TratamientoDesc", "StudyPeriodWeek", "Work_status", "Mes")))
x11(20, 10);plot(ggpredict(fit_pc3, terms = c("TratamientoDesc", "StudyPeriodWeek", "Work_status", "Mes")))


#-----------------------Limpieza de valores NA -------------------------------#
columnas_uso <- c("SOL_ACT", "SET1_ACT", "Treatment", "StudyPeriodWeek",
                  "Work_status", "TIB_ACT", "TST_ACT", "SE_ACT",
                  "SET2_ACT", "SET3_ACT", "Date_Onset_ACT")

datos_clean <- na.omit(melatonine[, columnas_uso])

#--------------------MODELO BAYESIANO-----------------------------#
# install.packages("brms")
library(brms)
# usar mismo modelo que en glmm
formula_multi <- mvbf(
  SOL_ACT ~ Treatment + StudyPeriodWeek + Work_status +
    TIB_ACT + TST_ACT + SE_ACT,
  # SOL_ACT ~ Treatment * StudyPeriodWeek + Work_status ver como da este modelo
  SET1_ACT ~ Treatment + StudyPeriodWeek + Work_status +
    TIB_ACT + TST_ACT + SE_ACT
)
fit_multi <- brm(
  formula = formula_multi,
  data = datos_clean,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 2000
)

summary(fit_multi)
