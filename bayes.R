#----------------- Modelo Bayesiano--------------------#
#-----------------Materia Optativa I-------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#
#--------------------MODELO BAYESIANO-----------------------------#
# Scripts auxiliares
#------------------------------------------------------
source("./db.R")
# Bibliotecas
#------------------------------------------------------
# install.packages("brms")
# setwd("C:/Users/Usuario/Documents/UNSL/Cuarto Año/Optativa I")

# ------------------Carga de Librerias----------------------------------------#
library(readxl)
library(ggplot2)
library(gridExtra)
library(brms)
library(tidyverse)
library(tidybayes)
library(performance)
library(ggplot2)
library(rstan)
library(dplyr)
# Detección de núcleos para BRMS------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# ------------------Creacion de la base de datos --------------------------#
# melatonine_all <- read_excel("data/pmed.1002587.s005.xlsx", sheet = "Combined")
# melatonine <- melatonine_all[,c("ParticipantID", "Treatment", "Delayed/Not Delayed",
#                                 "StudyPeriodWeek", "Work/Non-work","Date_offset_ACT", "TIB_ACT", "TST_ACT", "SOL_ACT",	"SE_ACT",
#                                 "WASO_ACT",	"SET1_ACT",	"SET2_ACT",	"SET3_ACT")]

#----------------- Cambio de nombre de varuables -----------------------------#
# colnames(melatonine)[colnames(melatonine) == "Work/Non-work"] <- "Work_status"
# colnames(melatonine)[colnames(melatonine) == "Delayed/Not Delayed"] <- "Delayed_status"
str(melatonine)
#---------------------------- Creacion de la variable Mes ---------------------#

#Convertir a formato de fecha real (R Base)

# melatonine$Fecha_Real <- as.Date(melatonine$Date_offset_ACT, format = "%d/%m/%Y")

#Extraer el mes como un factor
# melatonine$Mes <- as.factor(format(melatonine$Fecha_Real, "%m"))

#------------------------Convertir a factor a ParticipantID--------------------#

# melatonine$ParticipantID <- as.factor(melatonine$ParticipantID)

#------------------------- Limpieza de la base de datos -----------------------#
columnas_uso <- c("ParticipantID","SOL_SD_num", "SET1_ACT", "Treatment", "StudyPeriodWeek",
                  "Work_status", "TIB_ACT", "TST_ACT", "SE_ACT", "Mes", "StudyPeriodWeekFactor")

datos_clean <- na.omit(melatonine[, columnas_uso])

sol_mean <- mean(datos_clean$SOL_SD_num)
sol_sd <- sd(datos_clean$SOL_SD_num)

set1_mean <- mean(datos_clean$SET1_ACT)
set1_sd <- sd(datos_clean$SET1_ACT)

datos_est <- data.frame(
  SOL_SD_num=melatonine$SOL_SD_num,
  SET1_ACT=melatonine$SET1_ACT,
  SE_ACT=melatonine$SE_ACT,
  SET1_ACT_AVG_BASE=melatonine$SET1_ACT_AVG_BASE,
  SOL_ACT_AVG_BASE=melatonine$SOL_ACT_AVG_BASE)
#--------------------Estandarizacion de los datos -----------------------------#
X <- scale(datos_est, center = T, scale = T)
X <- as.data.frame(X)

X$ParticipantID <- melatonine$ParticipantID
X$Treatment <- melatonine$Treatment
X$StudyPeriodWeek <- melatonine$StudyPeriodWeek
X$StudyPeriodWeekFactor <- melatonine$StudyPeriodWeekFactor
X$Work_status <- melatonine$Work_status
X$Mes <- melatonine$Mes
X
X_clean <- na.omit(X)

X_clean$Work_status <- factor(X_clean$Work_status)
str(X_clean)
unique(X_clean$Work_status)
#--------------Función para convertir mes a estación--------------------------#
#
# estacion <- function(mes_num) {
#   ifelse(mes_num %in% c(12, 1, 2), "Verano",
#          ifelse(mes_num %in% c(3, 4, 5), "Otoño",
#                 ifelse(mes_num %in% c(6, 7, 8), "Invierno",
#                        ifelse(mes_num %in% c(9, 10, 11), "Primavera", NA))))
# }

# Aplicar la función (asegurarse de que 'Mes' sea numérico para esto)
# X_clean$Estacion <- as.factor(estacion(as.numeric(as.character(X_clean$Mes))))

#---------------------- Convertir las categoricas a factor---------------------#
# X_clean$Treatment <- as.factor(X_clean$Treatment)
# X_clean$StudyPeriodWeek <- as.factor(X_clean$StudyPeriodWeek)
# X_clean$Work_status <- as.factor(X_clean$Work_status)

#----------------------------- Modelo BRMS ------------------------------------#
formula_multi <- mvbf(
  SOL_SD_num ~ Treatment + StudyPeriodWeekFactor + Work_status +SOL_ACT_AVG_BASE +SET1_ACT_AVG_BASE + (1 | ParticipantID/StudyPeriodWeekFactor),
  SET1_ACT ~ Treatment + StudyPeriodWeekFactor + Work_status +SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE+ (1|ParticipantID/StudyPeriodWeekFactor),
  rescor = T
)
fit_multi <- brm(
  formula_multi,
  data = X_clean,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 2000
)

summary(fit_multi)
get_variables(fit_multi)
#-----------------Gráficos----------------------------------------------------#
x11();pp_check(fit_multi, resp = "SOLSDnum")
x11();pp_check(fit_multi, resp = "SET1ACT")

#----------------R Cuadrado --------------------------------------------------~#
bayes_R2(fit_multi)

str(X_clean)

# Use el modelo de bayes fit_multi
# Calculo de las media posterior a partir del modelo

pred_MVR <- fit_multi %>% 
  epred_draws(newdata = expand_grid(
    Treatment = levels(X_clean$Treatment),
    StudyPeriodWeekFactor = levels(X_clean$StudyPeriodWeekFactor),
    Work_status = levels(X_clean$Work_status),
    SOL_ACT_AVG_BASE= seq( min( X_clean$SOL_ACT_AVG_BASE), max(X_clean$SOL_ACT_AVG_BASE), by=1),
    SET1_ACT_AVG_BASE= seq( min( X_clean$SET1_ACT_AVG_BASE), max(X_clean$SET1_ACT_AVG_BASE), by=1),
  ), 
  re_formula = NA)
# Escala de los datos
mutated_pred <- pred_MVR %>%
  mutate(.epred = if_else(.category == "SOLSDnum", .epred*sol_sd+sol_mean, .epred*set1_sd+set1_mean))

# Grafico separado por latencia y eficiencia

windows()
# png("img/bayes_sol.png", width = 1000, height = 800)
ggplot(mutated_pred[mutated_pred$.category=="SOLSDnum",], aes(y = StudyPeriodWeekFactor, x = .epred, color = Treatment)) +
  # stat_pointinterval(position = position_dodge(width = 0.3)) + 
  stat_slab(position = position_dodge(width = 0.5)) +
  scale_color_hue(labels=c("1" = "Placebo", "2"="Melatonina 0.5 mg")) +
  # theme_minimal() +
  labs(
    title = "Efecto esperado de la Melatonina",
    subtitle = "Predicciones de la media posterior de la latencia del sueño",
    x = "Valor esperado SOL_SD",
    y = "Semana del Periodo de Estudio",
    color = "Tratamiento"
  )
# dev.off()
windows()
# png("img/bayes_set.png", width = 1000, height = 800)
ggplot(mutated_pred[mutated_pred$.category=="SET1ACT",], aes(y = StudyPeriodWeekFactor, x = .epred, color = Treatment)) +
  # stat_pointinterval(position = position_dodge(width = 0.3)) + 
  stat_slab(position = position_dodge(width = 0.5)) +
  scale_color_hue(labels=c("1" = "Placebo", "2"="Melatonina 0.5 mg")) +
  # theme_minimal() +
  labs(
    title = "Efecto esperado de la Melatonina ",
    subtitle = "Predicciones de la media posterior de la eficiencia del sueño",
    x = "Valor esperado SET1_ACT",
    y = "Semana del Periodo de Estudio",
    color = "Tratamiento"
  )
# dev.off()
# Ajuste del Modelo Bayesiano para Latencia con inflación de 0 Gamma
fit_bayes_sol <- brm(
  bf(SOL_ACT ~ Treatment + StudyPeriodWeek + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID),
     hu ~ 1), 
  data = data_nona,
  family = hurdle_gamma(link = "log"), 
  chains = 4, 
  iter = 2000, 
  cores = 4
)

summary(fit_bayes_sol)

# Ajuste del Modelo Bayesiano para Eficiencia (Regresión Beta)
fit_bayes_set1 <- brm(
  SET1_ACT ~ Treatment + StudyPeriodWeek + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID),
  data = data_nona,
  family = Beta(link = "logit"),
  chains = 4, iter = 2000, cores = 4
)

summary(fit_bayes_set1)

# Gráfico de entre simulado y real
x11()
pp_check(fit_bayes_sol, nsamples = 50) 
 x11()
pp_check(fit_bayes_set1, nsamples = 50)

# Para guardar el modelo bayesiano de SOL_ACT
#saveRDS(fit_bayes_sol, file = "fit_bayes_sol.rds")

# Guardar el modelo bayesiano de SET_ACT
#saveRDS(fit_bayes_set1, file = "fit_bayes_set1.rds")

# Para leer:
fit_bayes_sol <- readRDS("fit_bayes_sol.rds")
fit_bayes_set1 <- readRDS("fit_bayes_set1.rds")

# R2
bayes_R2(fit_bayes_sol)
bayes_R2(fit_bayes_set1)

pred_MVR_sol <- fit_bayes_sol %>% 
  epred_draws(newdata = expand_grid(
    Treatment = levels(X_clean$Treatment),
    StudyPeriodWeek = levels(X_clean$StudyPeriodWeek),
    Work_status = levels(X_clean$Work_status),
    SOL_ACT_AVG_BASE= seq( min( X_clean$SOL_ACT_AVG_BASE), max(X_clean$SOL_ACT_AVG_BASE), by=1),
    SET1_ACT_AVG_BASE= seq( min( X_clean$SET1_ACT_AVG_BASE), max(X_clean$SET1_ACT_AVG_BASE), by=1),
  ), 
  re_formula = NA)

summary(pred_MVR_sol)

pred_MVR_set <- fit_bayes_set1 %>% 
  epred_draws(newdata = expand_grid(
    Treatment = levels(X_clean$Treatment),
    StudyPeriodWeek = levels(X_clean$StudyPeriodWeek),
    Work_status = levels(X_clean$Work_status),
    SOL_ACT_AVG_BASE= seq( min( X_clean$SOL_ACT_AVG_BASE), max(X_clean$SOL_ACT_AVG_BASE), by=1),
    SET1_ACT_AVG_BASE= seq( min( X_clean$SET1_ACT_AVG_BASE), max(X_clean$SET1_ACT_AVG_BASE), by=1),
  ), 
  re_formula = NA)
summary(pred_MVR_set)


x11()
ggplot(pred_MVR_sol, aes(x = StudyPeriodWeek, y = .epred, color = Treatment)) +
  stat_pointinterval(position = position_dodge(width = 0.3)) + 
  theme_minimal() +
  labs(
    title = "Efecto esperado de la Melatonina",
    subtitle = "Predicciones de la media posterior (Modelo Hurdle Gamma)",
    y = "Valor esperado de SOL_ACT",
    x = "Semana del Periodo de Estudio",
    color = "Tratamiento"
  )

x11()
ggplot(pred_MVR_set, aes(x = StudyPeriodWeek, y = .epred, color = Treatment)) +
  stat_pointinterval(position = position_dodge(width = 0.3)) + 
  theme_minimal() +
  labs(
    title = "Efecto esperado de la Melatonina",
    subtitle = "Predicciones de la media posterior (Beta)",
    y = "Valor esperado de SET1_ACT",
    x = "Semana del Periodo de Estudio",
    color = "Tratamiento"
  )