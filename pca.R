#-------Análisis de componentes principales------------#
#-----------------Materia Optativa I-------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

setwd("~/unsl/bio/scripts")
# Scripts auxiliares
#------------------------------------------------------
source("./db.R")
source("./common.R")

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
#install.packages("gt")
library(gt)
# -------------------------------------- PCA ----------------------------------------#  
melatonine_pca <- function(data, variables_pca, titulo) {
  data_pca <- data[, variables_pca]
  
  pca <- prcomp(data_pca, scale = T, center = T)
  
  biplot <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
           groups=factor(data$TratamientoDesc),
           point.size=1,
           varname.size = 4, 
           varname.color = "firebrick",
           varname.adjust = 1.2,
           ellipse = T, 
           circle = F) +
    labs(title = titulo)
    theme_minimal() 
    
    # Proporciones varianza explicada-------------------------------------------
    var_prop <- round((pca$sdev^2)*100/sum(pca$sdev^2),2)
    
    # Histogramas PC1 y PC2 ----------------------------------------------------
    h1 <- ggplot(pca$x, aes(x = PC1)) +
      geom_histogram(color="gray", fill="red4") +
      labs(title = "Componente Principal 1", x = paste("PC1", var_prop[1], "%"), y = "Frecuencia")
    h2 <- ggplot(pca$x, aes(x = PC2)) +
      geom_histogram(color="gray", fill="green4") +
      labs(title = "Componente Principal 2", x = paste("PC2", var_prop[2], "%"), y = "Frecuencia")
    
      
  return(list(pca=pca, biplot=biplot, hist1=h1, hist2=h2, cor=cor(data_pca, pca$x)))
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
ventana();wrap_plots(ncol=2, nrow=2, initial_pca$biplot, base_pca$biplot, all_pca$biplot, final_pca$biplot)
semana2_pca <- melatonine_pca(melatonine %>% filter(StudyPeriodWeekFactor == 1), variables_pca_reducido, "Semana 2 - Variables no colineales")
semana3_pca <- melatonine_pca(melatonine %>% filter(StudyPeriodWeekFactor == 2), variables_pca_reducido, "Semana 3 - Variables no colineales")
semana4_pca <- melatonine_pca(melatonine %>% filter(StudyPeriodWeekFactor == 3), variables_pca_reducido, "Semana 4 - Variables no colineales")
semana5_pca <- melatonine_pca(melatonine %>% filter(StudyPeriodWeekFactor == 4), variables_pca_reducido, "Semana 5 - Variables no colineales")
ventana()
# png(filename="img/pca_semana_1.png")
print(base_pca$biplot)
# dev.off()
ventana()
# png(filename="img/pca_semana_2.png")
print(semana2_pca$biplot)
# dev.off()
ventana()
# png(filename="img/pca_semana_3.png")
print(semana3_pca$biplot)
# dev.off()
ventana()
# png(filename="img/pca_semana_4.png")
print(semana4_pca$biplot)
# dev.off()
ventana()
# png(filename="img/pca_semana_5.png")
print(semana5_pca$biplot)
# dev.off()
ventana()
# png(filename="img/pca_tratamiento.png")
print(final_pca$biplot)
# dev.off()
# Correlación entre componentes y variables reales------------------------------
final_pca$cor

# Distribucion componentes------------------------------------------------------
ventana(50,20)
par(mfrow=c(1,2))
wrap_plots(final_pca$hist1, final_pca$hist2)

# Tests de normalidad-----------------------------------------------------------
shapiro.test(final_pca$pca$x[,"PC1"])
shapiro.test(final_pca$pca$x[,"PC2"])

# Ajuste PC1 y PC2--------------------------------------------------------------
ajuste <- function(formula1, formula2, pca, original) {
  pca_for_model <- cbind(original, pca$x)
  pca_for_model$WorkDesc <- factor(pca_for_model$TratamientoDesc)
  pca_for_model$WorkDesc <- factor(if_else(pca_for_model$Work_status == 1, "Obligaciones", "Descanso"))
  model_pc1 <- glmmTMB(formula1, data = pca_for_model, family = gaussian())

  model_pc2 <- glmmTMB(formula2, data = pca_for_model, family = gaussian())

  fit1 <- list(
    model=model_pc1, 
    anova=Anova(model_pc1), 
    vif=check_collinearity(model_pc1, component = c('all')),
    r2=r2_nakagawa(model_pc1))

  fit2 <- list(
    model=model_pc2, 
    anova=Anova(model_pc2), 
    vif=check_collinearity(model_pc2, component = c('all')),
    r2=r2_nakagawa(model_pc2))
  
  return(list(fit1=fit1, fit2=fit2))
}

# Set1~ trat*semananumerica +……(1|Parcipante)
ajuste_interaccion_numerica <- ajuste(
  PC1 ~ TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID),
  PC2 ~ TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID),
  final_pca$pca, 
  melatonine
)

# Set1~ trat*semananumerica +……(1 +semananumerica | Parcipante)
ajuste_interaccion_numerica_pendiente <- ajuste(
  PC1 ~ TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID),
  PC2 ~ TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek  | ParticipantID),
  final_pca$pca, 
  melatonine
)

# Set1~ trat +……(1 +semananumerica | Parcipante)
ajuste_pendiente <- ajuste(
  PC1 ~ TratamientoDesc + StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID),
  PC2 ~ TratamientoDesc + StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek  | ParticipantID),
  final_pca$pca, 
  melatonine
)

# Set1~ trat+……(1  | Parcipante/semanaFactor)
ajuste_anidado_factor <- ajuste(
  PC1 ~ TratamientoDesc + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID/StudyPeriodWeekFactor),
  PC2 ~ TratamientoDesc + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID/StudyPeriodWeekFactor),
  final_pca$pca, 
  melatonine
)

comp <- data.frame(Formula=c(
  "TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID)",
  "TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID)",
  "TratamientoDesc + StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID)",
  "TratamientoDesc + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID/StudyPeriodWeekFactor)"), 
  PC1_R2_Condicional=c(ajuste_interaccion_numerica$fit1$r2$R2_conditional,
        ajuste_interaccion_numerica_pendiente$fit1$r2$R2_conditional,
        ajuste_pendiente$fit1$r2$R2_conditional,
        ajuste_anidado_factor$fit1$r2$R2_conditional),
  PC1_R2_Marginal=c(ajuste_interaccion_numerica$fit1$r2$R2_marginal,
                       ajuste_interaccion_numerica_pendiente$fit1$r2$R2_marginal,
                       ajuste_pendiente$fit1$r2$R2_marginal,
                       ajuste_anidado_factor$fit1$r2$R2_marginal),
  PC1_BIC=c(
    BIC(ajuste_interaccion_numerica$fit1$model),
    BIC(ajuste_interaccion_numerica_pendiente$fit1$model),
    BIC(ajuste_pendiente$fit1$model),
    BIC(ajuste_anidado_factor$fit1$model)),
  PC2_R2_Condicional=c(ajuste_interaccion_numerica$fit2$r2$R2_conditional,
                       ajuste_interaccion_numerica_pendiente$fit2$r2$R2_conditional,
                       ajuste_pendiente$fit2$r2$R2_conditional,
                       ajuste_anidado_factor$fit2$r2$R2_conditional),
  PC2_R2_Marginal=c(ajuste_interaccion_numerica$fit2$r2$R2_marginal,
                    ajuste_interaccion_numerica_pendiente$fit2$r2$R2_marginal,
                    ajuste_pendiente$fit2$r2$R2_marginal,
                    ajuste_anidado_factor$fit2$r2$R2_marginal),
  PC2_BIC=c(
    BIC(ajuste_interaccion_numerica$fit2$model),
    BIC(ajuste_interaccion_numerica_pendiente$fit2$model),
    BIC(ajuste_pendiente$fit2$model),
    BIC(ajuste_anidado_factor$fit2$model))
)

# Comparación de modelos--------------------------------------------------------
comp

# Gŕaficos----------------------------------------------------------------------
predicciones_1 <- ggpredict(
  ajuste_interaccion_numerica$fit1$model, 
  terms = c("StudyPeriodWeek", "TratamientoDesc", "SET1_ACT_AVG_BASE", "WorkDesc"))
predicciones_2 <- ggpredict(
  ajuste_interaccion_numerica$fit2$model, 
  terms = c("StudyPeriodWeek", "TratamientoDesc", "WorkDesc", "SET1_ACT_AVG_BASE"))

summary(predicciones_1)
summary(predicciones_2)
ventana();plot(predicciones_1)
ventana();plot(predicciones_2)

