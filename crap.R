
# cfa means ---------------------------------------------------------------


model <- "
    classclimate =~ feess_si_mean + feess_kk_mean + LM_in_mean + LL_in_mean + Friend_in_mean + sar_perpetrator_mean + sar_victim_mean
"

fit <- sem(model, dat_mean, std.lv = TRUE)

#tidy(fit)
#glance(fit)

summary(fit, fit.measures = TRUE)


