#----------------- Modelo Bayesiano--------------------#
#-----------------Materia Optativa I-------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#
setwd("~/unsl/bio/scripts")
#--------------------MODELO BAYESIANO-----------------------------#
# Scripts auxiliares
#------------------------------------------------------
source("./db.R")
# Bibliotecas
#------------------------------------------------------
# install.packages("brms")
setwd("C:/Users/Usuario/Documents/UNSL/Cuarto Año/Optativa I")

# ------------------Carga de Librerias----------------------------------------#
library(readxl)
library(ggplot2)
library(gridExtra)
library(brms)

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
columnas_uso <- c("ParticipantID","SOL_ACT", "SET1_ACT", "Treatment", "StudyPeriodWeek",
                  "Work_status", "TIB_ACT", "TST_ACT", "SE_ACT", "Mes")

datos_clean <- na.omit(melatonine[, columnas_uso])

datos_est <- data.frame(SOL_ACT=melatonine$SOL_ACT,SET1_ACT=melatonine$SET1_ACT,SE_ACT=melatonine$SE_ACT)
#--------------------Estandarizacion de los datos -----------------------------#
X <- scale(datos_est, center = T, scale = T)
X <- as.data.frame(X)

X$ParticipantID <- melatonine$ParticipantID
X$Treatment <- melatonine$Treatment
X$StudyPeriodWeek <- melatonine$StudyPeriodWeek
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
  SOL_ACT ~ Treatment * StudyPeriodWeek + Work_status + (1|ParticipantID),
  SET1_ACT ~ Treatment * StudyPeriodWeek + Work_status + (1|ParticipantID),
  rescor = T
)
fit_multi <- brm(
  SET1_ACT ~ Treatment * StudyPeriodWeek + Work_status + (1|ParticipantID),
  data = X_clean,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 2000
)

unique(X_clean$StudyPeriodWeek)

summary(fit_multi)
get_variables(fit_multi)
#-----------------Gráficos----------------------------------------------------#
x11();pp_check(fit_multi, resp = "SOLACT")
x11();pp_check(fit_multi, resp = "SET1ACT")

#----------------R Cuadrado --------------------------------------------------~#
bayes_R2(fit_multi)

library(tidyverse)
library(tidybayes)
str(X_clean)
X_clean %>%
  data_grid(ParticipantID,Treatment, StudyPeriodWeek, Work_status) %>%
  add_epred_draws(fit_multi, allow_new_levels=T) %>%
  head(10, allow_new_levels=T)


# pred_MVR <- fit_multi %>% #el nombre del objeto con el modelo
#   spread_draws(newdata = expand_grid(
#     #en este caso se construye una grilla con la secuencia de valores del predictor1 (cambiar segun tu caso)
#     #la combincion con los niveles de tu predictor2 que en este caso es una var catergorica,
#     # y los niveles del factor aleatorio como Est.aletoria (utilizar el nombre de la variable que pusiste).
#     #ajustar este ejemplo a tu caso.
#     pred1= levels( X_clean$Treatment),
#     pred2= levels( X_clean$StudyPeriodWeek),
#     pred3= levels( X_clean$Work_status),
#     pred4= levels( X_clean$ParticipantID),
#     Estr.aleatorio= c( "1", "2", "3", "4", "5", "6", "7"),
#     re_formula = NA))
# 

# Use el modelo de bayes fit_multi

# Calculo de las media posterior a partir del modelo

pred_MVR <- fit_multi %>% 
  epred_draws(newdata = expand_grid(
    Treatment = levels(X_clean$Treatment),
    StudyPeriodWeek = levels(X_clean$StudyPeriodWeek),
    Work_status = levels(X_clean$Work_status)
  ), 
  re_formula = NA)

summary(pred_MVR)

library(ggplot2)

# Grafico separado por latencia y eficiencia

#windows()
ggplot(pred_MVR, aes(x = StudyPeriodWeek, y = .epred, color = Treatment)) +
  stat_pointinterval(position = position_dodge(width = 0.3)) + 
  facet_wrap(~.category, scales = "free_y") + 
  theme_minimal() +
  labs(
    title = "Efecto esperado de la Melatonina",
    subtitle = "Predicciones de la media posterior por grupo",
    y = "Valor esperado (escala del modelo)",
    x = "Semana del Periodo de Estudio",
    color = "Tratamiento"
  )

