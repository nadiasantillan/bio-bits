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

ajuste <- function(formula_sol, formula_set, data) {
  model_sol <- glmmTMB(
    formula_sol,
    data = data,
    family = Gamma(link = "log")
  )
  model_set <- glmmTMB(
    formula_set,
    data = data,
    family = beta_family()
  )
  
  fit_sol <- list(
    model=model_sol, 
    anova=Anova(model_sol), 
    vif=check_collinearity(model_sol), 
    r2=r2_nakagawa(model_sol, ci = NULL))
  fit_set <- list(
    model=model_set, 
    anova=Anova(model_set), 
    vif=check_collinearity(model_set), 
    r2=r2_nakagawa(model_set, ci = NULL))
  
  return(list(fit_sol=fit_sol, fit_set=fit_set))
}

melatonine_sol <- melatonine %>%
  filter(!is.na(SOL_ACT) & SOL_ACT > 0 )
melatonine_solse <- melatonine %>%
  filter(!is.na(SOL_SD_num) & SOL_SD_num > 0 )

dim(melatonine)
dim(melatonine_sol)
dim(melatonine_solse)
summary(melatonine_sol$SOL_ACT)
summary(melatonine_solse$SET1_ACT)
summary(melatonine$SET1_ACT)

# Set1~ trat*semananumerica +……(1|Participante)
ajuste1 <- ajuste(
  SOL_SD_num ~ Treatment * StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID),
  SET1_ACT ~ Treatment * StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID),
  melatonine_solse)

# Set1~ trat*semananumerica +……(1 +semananumerica | Participante)
ajuste2 <- ajuste(
  SOL_SD_num ~ Treatment * StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID),
  SET1_ACT ~ Treatment * StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID),
  melatonine_solse)
# Set1~ trat +……(1 +semananumerica | Parcipante)
ajuste3 <- ajuste(
  SOL_SD_num ~ Treatment + StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID),
  SET1_ACT ~ Treatment + StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID),
  melatonine_solse)
# Set1~ trat+……(1  | Parcipante/semanaFactor)
ajuste4 <- ajuste(
  SOL_SD_num ~ Treatment + StudyPeriodWeekFactor + Work_status +SOL_ACT_AVG_BASE +SET1_ACT_AVG_BASE + (1 | ParticipantID/StudyPeriodWeekFactor),
  SET1_ACT ~ Treatment + StudyPeriodWeekFactor + Work_status +SOL_ACT_AVG_BASE +SET1_ACT_AVG_BASE + (1 | ParticipantID/StudyPeriodWeekFactor),
  melatonine_solse)


comp <- data.frame(Formula=c(
  
  "Treatment * StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID)",
  "Treatment * StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID)",
  "Treatment + StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID)",
  "Treatment + StudyPeriodWeekFactor + Work_status +SOL_ACT_AVG_BASE +SET1_ACT_AVG_BASE + (1 | ParticipantID/StudyPeriodWeekFactor)"), 
  SOL_R2_Condicional=c(ajuste1$fit_sol$r2$R2_conditional,
                       ajuste2$fit_sol$r2$R2_conditional,
                       ajuste3$fit_sol$r2$R2_conditional,
                       ajuste4$fit_sol$r2$R2_conditional),
  SOL_R2_Marginal=c(ajuste1$fit_sol$r2$R2_marginal,
                    ajuste2$fit_sol$r2$R2_marginal,
                    ajuste3$fit_sol$r2$R2_marginal,
                    ajuste4$fit_sol$r2$R2_marginal),
  SOL_BIC=c(
    BIC(ajuste1$fit_sol$model),
    BIC(ajuste2$fit_sol$model),
    BIC(ajuste3$fit_sol$model),
    BIC(ajuste4$fit_sol$model)),
  SET_R2_Condicional=c(ajuste1$fit_set$r2$R2_conditional,
                       ajuste2$fit_set$r2$R2_conditional,
                       ajuste3$fit_set$r2$R2_conditional,
                       ajuste4$fit_set$r2$R2_conditional),
  PC2_R2_Marginal=c(ajuste1$fit_set$r2$R2_marginal,
                    ajuste2$fit_set$r2$R2_marginal,
                    ajuste3$fit_set$r2$R2_marginal,
                    ajuste4$fit_set$r2$R2_marginal),
  PC2_BIC=c(
    BIC(ajuste1$fit_set$model),
    BIC(ajuste2$fit_set$model),
    BIC(ajuste3$fit_set$model),
    BIC(ajuste4$fit_set$model))
)

comp


# Resumen mejor modelo ---------------------------------------------------------
summary(ajuste4$fit_sol$model)
summary(ajuste4$fit_set$model)
# ANOVA mejor modelo -----------------------------------------------------------
print(ajuste4$fit_sol$anova)
print(ajuste4$fit_set$anova)
# No significativo la condición de trabajo (como demostraba el summary), y el SET1_ACT es mayor a 0.05 (Prueba de Wald)
# VIF mejor modelo -------------------------------------------------------------
print("--- VIF: Modelo de Latencia (SOL_ACT) ---")
print(ajuste4$fit_sol$vif)
print("--- VIF: Modelo de Eficiencia (SET1_ACT) ---")
print(ajuste4$fit_set$vif)

# El análisis de inflación de la Varianza (VIF) muestra una baja correlación (poca multicolinealidad) entre las variables 
# predictoras en el modelo de latencia (SOL_ACT). Valores entre 1 y 2 (menores a 5). 
# R2 mejor modelo -------------------------------------------------------------
# R2 marginal: Es la varianza explicada exclusivamente por los efectos fijos 
# (las variables predictoras principales).
# R2 condicional: Es la varianza explicada por el modelo completo 
# (efectos fijos + efectos aleatorios combinados)
print(ajuste4$fit_sol$r2)
print(ajuste4$fit_set$r2)
# El análisis de inflación de la Varianza (VIF) muestra una baja correlación (poca multicolinealidad) entre las variables 
# predictoras en el modelo de latencia (SET1_ACT). Valores entre 1 y 2 (menores a 5). 

# Grafico
# Quitamos work_status para simplificar el gráfico, siendo no significativo para SOL y si para SET1.

#if(!require(sjPlot)) install.packages("sjPlot")

ventana(width = 12, height = 8)
# png("img/model_solact_hibrido.png", width=800, height = 600)
plot_model(ajuste4$fit_sol$model, 
           type = "pred", 
           terms = c("StudyPeriodWeekFactor", "Treatment", "Work_status"),
           ci.lvl = 0.95) +
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  labs(title = "Predicciones del Modelo Mixto: Efecto del Tratamiento",
       subtitle = "Controlado por Semana de Tratamiento y Situación Laboral",
       x = "Semana de Estudio", 
       y = "Latencia de sueño",
       color = "Grupo") +
  theme_minimal()
# dev.off()
ventana(width = 12, height = 8)
# png("img/model_set1.png", width=800, height = 600)
plot_model(ajuste4$fit_set$model, 
           type = "pred", 
           terms = c("StudyPeriodWeekFactor", "Treatment", "Work_status"),
           ci.lvl = 0.95) +
  scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
                     labels = c("Placebo", "Melatonina 0.5mg")) +
  labs(title = "Predicciones del Modelo Mixto: Efecto del Tratamiento",
       subtitle = "Controlado por Semana de Tratamiento y Situación Laboral",
       x = "Semana de Estudio", 
       y = "Eficiencia de sueño en el primer tercio",
       color = "Grupo") +
  theme_minimal()
# dev.off()
# Grafico similar donde sumamos las observaciones reales al final SOL_ACT.

predicciones_sol <- get_model_data(ajuste4$fit_sol$model, type = "pred", terms = c("StudyPeriodWeekFactor", "Treatment"))

ventana(width = 11, height = 7)
# png("img/model_solact_jitter.png", width=800, height = 600)
ggplot() +
  geom_jitter(data = melatonine_solse, 
              aes(x = StudyPeriodWeekFactor, y = SOL_SD_num, color = Treatment), 
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
       subtitle = "Líneas: Predicciones del Modelo (Gamma) | Puntos: Valores Informados",
       x = "Semana de Estudio", 
       y = "Latencia de sueño informada(SOL_SD)",
       color = "Grupo de Estudio") +
  theme_minimal()
# dev.off()
# Grafico similar donde sumamos las observaciones reales al final SOL_ACT.
predicciones_set1 <- get_model_data(ajuste4$fit_set$model, type = "pred", terms = c("StudyPeriodWeekFactor", "Treatment"))

ventana(width = 11, height = 7)
# png("img/model_set1_jitter.png", width=800, height = 600)
ggplot() +
  # Capa de puntos: Datos reales comprimidos de fondo
  geom_jitter(data = melatonine_solse, 
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

