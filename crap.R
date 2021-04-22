
# cfa means ---------------------------------------------------------------


model <- "
    classclimate =~ feess_si_mean + feess_kk_mean + LM_in_mean + LL_in_mean + Friend_in_mean + sar_perpetrator_mean + sar_victim_mean
"

fit <- sem(model, dat_mean, std.lv = TRUE)

#tidy(fit)
#glance(fit)

summary(fit, fit.measures = TRUE)

# Exploring the problems with sociometric data

```{r}
dat %>% group_by(grade) %>%
  summarise(
    across(c("LL_in", "LM_in", "Friend_in", "soc_relatedness", "soc_positivity"), list(mean = ~mean(.x, na.rm = TRUE))),
    n = n()) %>% round(2) %>%
  html_table()


```
