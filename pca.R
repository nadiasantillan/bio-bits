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
library(performance)
library(car)
# -------------------------------------- PCA ----------------------------------------#  
melatonine_pca <- function(data, variables_pca, titulo) {
  data_pca <- data[, variables_pca] # menos variables en la entrada del PCA
  
  pca <- prcomp(data_pca, scale = T, center = T)
  
  plot <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
           groups=factor(data$TratamientoDesc),
           point.size=1,
           varname.size = 4, 
           varname.color = "firebrick",
           varname.adjust = 1.2,
           ellipse = T, 
           circle = F) +
    labs(title = titulo)
    theme_minimal() 
  
  return(list(pca=pca, plot=plot, cor=cor(data_pca, pca$x)))
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
ventana();wrap_plots(ncol=2, nrow=2, initial_pca$plot, base_pca$plot, all_pca$plot, final_pca$plot)
semana2_pca <- melatonine_pca(melatonine %>% filter(StudyPeriodWeek == 1), variables_pca_reducido, "Semana 2 - Variables no colineales")
semana3_pca <- melatonine_pca(melatonine %>% filter(StudyPeriodWeek == 2), variables_pca_reducido, "Semana 3 - Variables no colineales")
semana4_pca <- melatonine_pca(melatonine %>% filter(StudyPeriodWeek == 3), variables_pca_reducido, "Semana 4 - Variables no colineales")
semana5_pca <- melatonine_pca(melatonine %>% filter(StudyPeriodWeek == 4), variables_pca_reducido, "Semana 5 - Variables no colineales")
ventana()
png(filename="pca_semana_1.png")
print(base_pca$plot)
dev.off()
ventana()
png(filename="pca_semana_2.png")
print(semana2_pca$plot)
dev.off()
ventana()
png(filename="pca_semana_3.png")
print(semana3_pca$plot)
dev.off()
ventana()
png(filename="pca_semana_4.png")
print(semana4_pca$plot)
dev.off()
ventana()
png(filename="pca_semana_5.png")
print(semana5_pca$plot)
dev.off()
ventana()
png(filename="pca_tratamiento.png")
print(final_pca$plot)
dev.off()
# Correlación entre componentes y variables reales------------------------------
final_pca$cor
# Proporciones varianza explicada-----------------------------------------------
var_prop <- round((final_pca$pca$sdev^2)*100/sum(final_pca$pca$sdev^2),2)
#vif base
# Distribucion componentes------------------------------------------------------
ventana(50,20)
par(mfrow=c(1,2))
h1 <- ggplot(final_pca$pca$x, aes(x = PC1)) +
  geom_histogram(color="gray", fill="red4") +
  labs(title = "Componente Principal 1", x = paste("PC1", var_prop[1], "%"), y = "Frecuencia")
h2 <- ggplot(final_pca$pca$x, aes(x = PC2)) +
  geom_histogram(color="gray", fill="green4") +
  labs(title = "Componente Principal 2", x = paste("PC2", var_prop[2], "%"), y = "Frecuencia")
wrap_plots(h1, h2)

# Tests de normalidad-----------------------------------------------------------
shapiro.test(final_pca$pca$x[,"PC1"])
shapiro.test(final_pca$pca$x[,"PC2"])

# Ajuste PC1 y PC2--------------------------------------------------------------
pca_for_model <- cbind(melatonine, final_pca$pca$x)
pca_for_model$TratamientoDesc <- factor(pca_for_model$TratamientoDesc)

fit_pc1_glmm <- glmmTMB(
  PC1 ~ TratamientoDesc + StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE+ (1 | ParticipantID),
  data = pca_for_model,
  family = gaussian()
)
Anova(fit_pc1_glmm)
summary(fit_pc1_glmm)

fit_pc2_glmm <- glmmTMB(
  PC2 ~ TratamientoDesc + StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE+ (1 | ParticipantID),
  data = pca_for_model,
  family = gaussian()
)
Anova(fit_pc2_glmm) # devianza

summary(fit_pc2_glmm)

# Inflación de la varianza------------------------------------------------------
check_collinearity(
  fit_pc1_glmm,
  component = c('all') # 'all' shows both conditional and zi components
)

check_collinearity(
  fit_pc2_glmm,
  component = c('all') # 'all' shows both conditional and zi components
)

# R2 ---------------------------------------------------------------------------
r2_nakagawa(fit_pc1_glmm)
r2_nakagawa(fit_pc2_glmm)

predicciones_1 <- ggpredict(fit_pc1_glmm, terms = c("StudyPeriodWeek", "TratamientoDesc", "SET1_ACT_AVG_BASE", "Work_status"))
predicciones_2 <- ggpredict(fit_pc2_glmm, terms = c("StudyPeriodWeek", "TratamientoDesc", "SET1_ACT_AVG_BASE", "Work_status"))

summary(predicciones_1)
summary(predicciones_2)
ventana();plot(predicciones_1)
ventana();plot(predicciones_2)

