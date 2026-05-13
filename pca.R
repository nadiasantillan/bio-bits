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
library(lme4)
library(emmeans)
library(glmmTMB)
# -------------------------------------- PCA ----------------------------------------#  
# library(rgl)
# library('glmmTMB')
# library('ggeffects')
# variables_pca <- c("TIB_ACT", "TST_ACT", "SOL_ACT", "SET1_ACT", "WASO_ACT", "SET2_ACT", "SET3_ACT")
variables_pca <- c("TIB_ACT", "TST_ACT", "SOL_ACT", "SET1_ACT", "WASO_ACT")
# variables_pca <- c("SOL_ACT", "SE_ACT", "TST_ACT")

melatonine_pca <- melatonine[, variables_pca] # menos variables en la entrada del PCA
summary(melatonine_pca)

# Original data melatonine_num, cleaned data x
x <- na.omit(melatonine_pca)
borrados <- na.action(x)
str(melatonine)
melatonine_nona <- melatonine[-borrados,]

pca <- prcomp(x, scale = T, center = T)

cor(x, pca$x)
summary(pca)
var_prop <- round((pca$sdev^2)*100/sum(pca$sdev^2),2)


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
str(melatonine)

pca_for_model <- cbind(melatonine_nona,pca$x)
pca_for_model$TratamientoDesc <- factor(pca_for_model$TratamientoDesc)

library(car)
fit_pc2_glmm <- glmmTMB(
  PC2 ~ TratamientoDesc * StudyPeriodWeek + Work_status + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE+ (1 | ParticipantID),
  # Valor_Z ~ Metrica + Work_status + Treatment * Semana_Num + (1 | ParticipantID | Mes | estacion), hacer con sol_act y se_act
  # interacciones entre tratamiento y semana de tratamiento
  data = pca_for_model,
  family = gaussian()
)
Anova(fit_pc2_glmm) # devianza
# sacar semana base del tratamiento
# agregar promedios de la semana base como un predictor

summary(fit_pc2_glmm)

# fit_pc1 <- lmer(
#   PC1 ~ TratamientoDesc * StudyPeriodWeek + Work_status +  (1 | ParticipantID/estacion ),
#   data = pca_for_model
# )
# summary(fit_pc1)

fit_pc2 <- lmer(
  PC2 ~ TratamientoDesc * StudyPeriodWeek + Work_status+ SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID ),
  data = pca_for_model,
)
summary(fit_pc2)

fit_pc2_sin_estacion <- lmer(
  PC2 ~ TratamientoDesc * StudyPeriodWeek + Work_status+ SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + (1 | ParticipantID ),
  data = pca_for_model,
)
summary(fit_pc2_sin_estacion)

model_pc1 <- glmmTMB(
  PC1 ~ Work_status + TratamientoDesc * StudyPeriodWeek + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE+ (1 | ParticipantID/estacion),
  data = pca_for_model,
  family = gaussian()
)
summary(model_pc1)


library(sjPlot)
x11(width = 12, height = 8)
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
