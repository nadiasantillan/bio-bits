#-------Análisis de componentes principales------------#
#-----------------Materia Optativa I-------------------#
#--------------Autores: -------------------------------#
#-----FERRAGUTTI - SANTILLAN - VILLARREAL--------------#
#-------------------Año: 2026 -------------------------#

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
library(lme4)
library(ggeffects)
library(performance)
library(car)
# install.packages("DHARMa")
# library(DHARMa)
# install.packages("vegan")
library(vegan)
# -------------------------------------- PCA ----------------------------------------#  
melatonine_pca <- function(data, variables_pca, titulo) {
  data_pca <- data[, variables_pca]
  
  pca <- prcomp(data_pca, scale = T, center = T)
  
  biplot <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
           groups=factor(data$TratamientoDesc),
           point.size=1,
           varname.size = 4, 
           varname.color = "black",
           varname.adjust = 1.2,
           ellipse = T, 
           circle = F) +
    labs(title = titulo, colour = "Tratamiento", fill="Tratamiento") +
    coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) +
    scale_color_manual(values = c("Melatonina 0.5 mg" = "darkorange3", "Placebo" = "darkorchid3"))
    theme_minimal() 
    
    # Proporciones varianza explicada-------------------------------------------
    var_prop <- round((pca$sdev^2)*100/sum(pca$sdev^2),2)
    
    # Histogramas PC1 y PC2 ----------------------------------------------------
    h1 <- ggplot(pca$x, aes(x = PC1)) +
      geom_histogram(color="gray", fill="darkorange3") +
      labs(title = "Componente Principal 1", x = paste("PC1", var_prop[1], "%"), y = "Frecuencia")
    h2 <- ggplot(pca$x, aes(x = PC2)) +
      geom_histogram(color="gray", fill="darkorchid3") +
      labs(title = "Componente Principal 2", x = paste("PC2", var_prop[2], "%"), y = "Frecuencia")
    
      
  return(list(pca=pca, biplot=biplot, hist1=h1, hist2=h2, cor=cor(data_pca, pca$x)))
}

variables_pca_todas <- c("TIB_ACT", "TST_ACT", "SET1_ACT", "WASO_ACT", "SET2_ACT", "SET3_ACT", "SOL_SD_num")
variables_pca_reducido <- c("TIB_ACT", "TST_ACT", "SOL_SD_num", "SET1_ACT", "WASO_ACT")

melatonine_all <- rbind(melatonine_base, melatonine)

initial_pca <- melatonine_pca(
  melatonine_all, variables_pca_todas, "Todas las semanas - Variables Actígrafo")
base_pca <- melatonine_pca(melatonine_base, variables_pca_reducido, "Semana Base - Variables no colineales")
all_pca <- melatonine_pca(melatonine_all, variables_pca_reducido, "Todas las semanas - Variables no colineales")
final_pca <- melatonine_pca(
  melatonine, 
  variables_pca_reducido,
  "Semanas de tratamiento - Variables no colineales")
semana5_pca <- melatonine_pca(melatonine %>% filter(StudyPeriodWeekFactor == 4), variables_pca_reducido, "Semana 5 - Variables no colineales")

png(filename="img/pca_inicial_colineales.png", width = 1000, height = 400)
wrap_plots(ncol=2, initial_pca$biplot, all_pca$biplot)
dev.off()

png(filename="img/pca_base_vs_tratamiento.png", width = 1500, height = 400)
wrap_plots(ncol=3, base_pca$biplot, final_pca$biplot, semana5_pca$biplot)
dev.off()

ventana();wrap_plots(ncol=2, nrow=2, initial_pca$biplot, base_pca$biplot, all_pca$biplot, final_pca$biplot)

# Correlación entre componentes y variables reales------------------------------
final_pca$cor

# Análisis de Procrustes -------------------------------------------------------
# procrustes(base_pca$pca, final_pca$pca)
# No funciona como está: Matrices have different number of rows: 745 and 2345

# Distribución componentes------------------------------------------------------
ventana(50,20)
par(mfrow=c(1,2))
# png(filename="img/pca_distribucion.png", width = 800, height = 400)
wrap_plots(final_pca$hist1, final_pca$hist2)
# dev.off()

# Tests de normalidad-----------------------------------------------------------
shapiro.test(final_pca$pca$x[,"PC1"])
shapiro.test(final_pca$pca$x[,"PC2"])

# Ajuste PC1 y PC2--------------------------------------------------------------
ajuste <- function(formula1, formula2, pca, original) {
  pca_for_model <- cbind(original, pca$x)
  pca_for_model$WorkDesc <- factor(pca_for_model$TratamientoDesc)
  pca_for_model$WorkDesc <- factor(if_else(pca_for_model$Work_status == 1, "Obligaciones", "Descanso"))
  # model_pc1 <- glmmTMB(formula1, data = pca_for_model, family = gaussian())
  model_pc1 <- lmer(formula1, data = pca_for_model)

  # model_pc2 <- glmmTMB(formula2, data = pca_for_model, family = gaussian())
  model_pc2 <- lmer(formula2, data = pca_for_model)

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
ajuste1 <- ajuste(
  PC1 ~ TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID),
  PC2 ~ TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID),
  final_pca$pca, 
  melatonine
)

# Set1~ trat*semananumerica +……(1 +semananumerica | Parcipante)
ajuste2 <- ajuste(
  PC1 ~ TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID),
  PC2 ~ TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek  | ParticipantID),
  final_pca$pca, 
  melatonine
)

# Set1~ trat +……(1 +semananumerica | Parcipante)
ajuste3 <- ajuste(
  PC1 ~ TratamientoDesc + StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID),
  PC2 ~ TratamientoDesc + StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek  | ParticipantID),
  final_pca$pca, 
  melatonine
)

# Set1~ trat+……(1  | Parcipante/semanaFactor)
ajuste4 <- ajuste(
  PC1 ~ TratamientoDesc + StudyPeriodWeekFactor + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID/StudyPeriodWeekFactor),
  PC2 ~ TratamientoDesc + StudyPeriodWeekFactor + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID/StudyPeriodWeekFactor),
  final_pca$pca, 
  melatonine
)

# ajuste_articulo <- ajuste(
#   PC1 ~ TratamientoDesc * StudyPeriodWeek  + (1 | ParticipantID),
#   PC2 ~ TratamientoDesc * StudyPeriodWeek  + (1 | ParticipantID),
#   all_pca$pca, 
#   melatonine_all
# )


comp <- data.frame(Formula=c(
  "TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID)",
  "TratamientoDesc * StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID)",
  "TratamientoDesc + StudyPeriodWeek + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 + StudyPeriodWeek | ParticipantID)",
  "TratamientoDesc + WorkDesc + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID/StudyPeriodWeekFactor)"), 
  PC1_R2_Condicional=c(ajuste1$fit1$r2$R2_conditional,
                       ajuste2$fit1$r2$R2_conditional,
                       ajuste3$fit1$r2$R2_conditional,
                       ajuste4$fit1$r2$R2_conditional),
  PC1_R2_Marginal=c(ajuste1$fit1$r2$R2_marginal,
                       ajuste2$fit1$r2$R2_marginal,
                       ajuste3$fit1$r2$R2_marginal,
                       ajuste4$fit1$r2$R2_marginal),
  PC1_BIC=c(
    BIC(ajuste1$fit1$model),
    BIC(ajuste2$fit1$model),
    BIC(ajuste3$fit1$model),
    BIC(ajuste4$fit1$model)),
  PC2_R2_Condicional=c(ajuste1$fit2$r2$R2_conditional,
                       ajuste2$fit2$r2$R2_conditional,
                       ajuste3$fit2$r2$R2_conditional,
                       ajuste4$fit2$r2$R2_conditional),
  PC2_R2_Marginal=c(ajuste1$fit2$r2$R2_marginal,
                    ajuste2$fit2$r2$R2_marginal,
                    ajuste3$fit2$r2$R2_marginal,
                    ajuste4$fit2$r2$R2_marginal),
  PC2_BIC=c(
    BIC(ajuste1$fit2$model),
    BIC(ajuste2$fit2$model),
    BIC(ajuste3$fit2$model),
    BIC(ajuste4$fit2$model))
)

# Comparación de modelos--------------------------------------------------------
comp

# Anova mejor modelo -----------------------------------------------------------
print(ajuste4$fit2$anova)
# Inflación de la varianza - Mejor modelo --------------------------------------
print(ajuste4$fit1$vif)
print(ajuste4$fit2$vif)
# Residuos ---------------------------------------------------------------------
res_pc1 <- residuals(ajuste4$fit1$model)
res_pc2 <- residuals(ajuste4$fit2$model)
ventana(50,20)
png("img/residuos.png", width = 1000, height = 800)
par(mfrow=c(2,3))
plot(res_pc1, main =)
qqnorm(res_pc1)
qqline(res_pc1)
hist(res_pc1)
plot(res_pc2)
qqnorm(res_pc2)
qqline(res_pc2)
hist(res_pc2)
dev.off()
# Gŕaficos----------------------------------------------------------------------
predicciones_1 <- ggpredict(
  ajuste4$fit1$model, 
  terms = c("StudyPeriodWeekFactor", "TratamientoDesc", "WorkDesc"))
predicciones_2 <- ggpredict(
  ajuste4$fit2$model, 
  terms = c("StudyPeriodWeekFactor", "TratamientoDesc", "WorkDesc"))

summary(predicciones_1)
summary(predicciones_2)
ventana();
png("img/ajuste_pc1.png", width = 1000, height = 800)
plot(predicciones_1)
dev.off()
ventana();
png("img/ajuste_pc2.png", width = 1000, height = 800)
plot(predicciones_2)
dev.off()

