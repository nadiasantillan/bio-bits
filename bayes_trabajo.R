formula_sol <- bf(
  SOL_ACT_hibrido ~ Treatment + StudyPeriodWeekFactor + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + 
    (1 | p1 | ParticipantID) + (1 | p2 | ParticipantID:StudyPeriodWeekFactor),
  hu ~ 1
) + hurdle_gamma(link = "log")

# 2. Definimos la FÓRMULA de Eficiencia (Beta con enlace logit)
formula_set <- bf(
  SET1_ACT ~ Treatment + StudyPeriodWeekFactor + SOL_ACT_AVG_BASE + SET1_ACT_AVG_BASE + 
    (1 | p1 | ParticipantID) + (1 | p2 | ParticipantID:StudyPeriodWeekFactor)
) + Beta(link = "logit")

saveRDS(fit_multi_real, file = "fit_multi_real.rds")


# 3. Ahora sí, sumamos las FÓRMULAS dentro de brm()
fit_multi_real <- brm(
  formula_sol + formula_set,
  data = melatonine, 
  chains = 4, cores = 4, iter = 2000
)

summary(fit_multi_real)