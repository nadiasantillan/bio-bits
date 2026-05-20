#-------Análisis de componentes principales------------#
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
library(patchwork)
library(ggbiplot)
library(emmeans)
library(glmmTMB)
library(ggeffects)
# -------------------------------------- PCA ----------------------------------------#  
melatonine_pca <- function(data, variables_pca, titulo) {
  data_pca <- data[, variables_pca] # menos variables en la entrada del PCA
  
  # Original data melatonine_num, cleaned data x
  x <- na.omit(data_pca)
  borrados <- na.action(x)

  data_nona <- data[-borrados,]
  
  pca <- prcomp(x, scale = T, center = T)
  
  plot <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
           groups=factor(data_nona$TratamientoDesc),
           point.size=1,
           varname.size = 4, 
           varname.color = "firebrick",
           varname.adjust = 1.2,
           ellipse = T, 
           circle = F) +
    labs(title = titulo)
    theme_minimal() 
  
  return(list(pca=pca, plot=plot, cor=cor(x, pca$x), data=data_nona))
}

variables_pca_todas <- c("TIB_ACT", "TST_ACT", "SOL_ACT", "SET1_ACT", "WASO_ACT", "SET2_ACT", "SET3_ACT")
variables_pca_reducido <- c("TIB_ACT", "TST_ACT", "SOL_ACT", "SET1_ACT", "WASO_ACT")

melatonine_all <- rbind(melatonine_base, melatonine)

initial_pca <- melatonine_pca(
  melatonine_all, variables_pca_todas, "Todas las semanas - Variables Actígrafo")
base_pca <- melatonine_pca(melatonine_base, variables_pca_reducido, "Semana Base - Variables no colineales")
all_pca <- melatonine_pca(melatonine_all, variables_pca_reducido, "Todas las semanas - Variables no colineales")
final_pca <- melatonine_pca(
  melatonine, 
  variables_pca_reducido,
  "Semanas de tratamiento - Variables no colineales")

x11();par(mfrow=c(2, 2))
wrap_plots(initial_pca$plot,base_pca$plot, all_pca$plot, final_pca$plot)

# Correlación entre componentes y variables reales------------------------------
final_pca$cor
# Proporciones varianza explicada-----------------------------------------------
var_prop <- round((final_pca$pca$sdev^2)*100/sum(final_pca$pca$sdev^2),2)

# Distribucion componentes------------------------------------------------------
x11(50,20)
par(mfrow=c(1,2))
h1 <- ggplot(final_pca$pca$x, aes(x = PC1)) +
  geom_histogram(color="gray", fill="red4") +
  labs(title = "Componente Principal 1", x = paste("PC1", var_prop[1], "%"), y = "Frecuencia")
h2 <- ggplot(final_pca$pca$x, aes(x = PC2)) +
  geom_histogram(color="gray", fill="green4") +
  labs(title = "Componente Principal 2", x = paste("PC2", var_prop[2], "%"), y = "Frecuencia")
wrap_plots(h1, h2)

# Tests de normalidad-----------------------------------------------------------
shapiro.test(pca$x[,"PC1"])
shapiro.test(pca$x[,"PC2"])


pca_for_model <- cbind(final_pca$data, final_pca$pca$x)
pca_for_model$TratamientoDesc <- factor(pca_for_model$TratamientoDesc)

library(car)

fit_pc1_glmm <- glmmTMB(
  PC1 ~ TratamientoDesc + StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE+ (1 | ParticipantID),
  data = pca_for_model,
  family = gaussian()
)
Anova(fit_pc1_glmm) # devianza
summary(fit_pc1_glmm)

fit_pc2_glmm <- glmmTMB(
  PC2 ~ TratamientoDesc + StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE+ (1 | ParticipantID),
  data = pca_for_model,
  family = gaussian()
)
Anova(fit_pc2_glmm) # devianza

summary(fit_pc2_glmm)


library(sjPlot)
x11(width = 24, height = 8)
par(mfrow=c(1,2))
plot_model(fit_pc2_glmm, 
           type = "pred", 
           terms = c("StudyPeriodWeek", "TratamientoDesc", "SET1_ACT_AVG_BASE", "Work_status"),
           ci.lvl = 0.95) +
  # scale_color_manual(values = c("1" = "firebrick", "2" = "dodgerblue3"), 
  #                    labels = c("Placebo", "Melatonina 0.5mg")) +
  labs(title = "Predicciones del Modelo Mixto: Efecto del Tratamiento",
       subtitle = "Controlado por Tratamiento, semana, eficiencia del sueño y situación laboral",
       x = "Semana de Estudio", 
       y = "PC2",
       color = "Grupo") +
  theme_minimal()
# VER !!! extraer predicciones de los efectos
# efecto promedio del uso de la melatonina sobre la latencia y eficiencia del sueño
library(ggeffects)
# 
# r2 marginal y condicional
r2_nakagawa(fit_pc2_glmm)


predicciones <- ggpredict(fit_pc2_glmm, terms = c("StudyPeriodWeek", "TratamientoDesc", "SET1_ACT_AVG_BASE", "Work_status"))

summary(predicciones)

