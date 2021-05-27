
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



# Q3a

```{r}
dat2 <- dat %>%
  select(!!var_cc_l1) %>%
  rename_items() %>%
  na.omit() %>%
  mutate(across(everything(), scale))

# fit_bic <- mclustBIC(dat2)
# saveRDS(fit_bic, "fit_bic.rds")
fit_bic <- readRDS("fit_bic.rds")

#plot(fit_bic)
#summary(fit_bic)

# fit_icl <- mclustICL(dat2)
# saveRDS(fit_icl, "fit_icl.rds")
# fit_icl <- readRDS("fit_icl.rds")

#plot(fit_icl)
#summary(fit_icl)

# bootstrap <- mclustBootstrapLRT(dat2, modelName = "VEV", maxG = 9)
# saveRDS(bootstrap, "bootstrap.rds")
# bootstrap <- readRDS("bootstrap.rds")
```


```{r}
mod1 <- Mclust(dat2, modelNames = "VEV", G = 3, x = fit_bic)
#summary(mod1)

means <- as.data.frame(mod1$parameters$mean) %>%
  rownames_to_column("Variable") %>%
  pivot_longer(cols = -1) %>%
  mutate(name = recode(name,
                       V1 = "Bully and bullied; socially rejected 17%",
                       V2 = "Socially included 60%",
                       V3 = "Bullying victim; socially rejected 23%"
  ))

p <- means %>%
  ggplot(aes(Variable, value, group = name, color = name)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p


mod1 <- Mclust(dat2, modelNames = "VEV", G = 5, x = fit_bic)
#summary(mod1)

means <- as.data.frame(mod1$parameters$mean) %>%
  rownames_to_column("Variable") %>%
  pivot_longer(cols = -1) %>%
  mutate(name = recode(name,
                       V1 = "Bullying victim; socially rejected 19%",
                       V2 = "Socially included; few relations 31%",
                       V3 = "Bully and bullied; socially rejected 14%",
                       V4 = "Slight bully; slight socially rejected 14%",
                       V5 = "Socially included; many relations 22%"
                       
  ))



p <- means %>%
  ggplot(aes(Variable, value, group = name, color = name)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p

```

Add on questions(probably they will be dropped later :-) )

> Q3a: Can we find profiles of components of social-emotional wellbeing?
  > Q3b: Can we find profiles of components of social-emotional classclimate?
  > Q3c: What is the nexus between the profiles of social-emotional wellbeing and behavioral problems?
  > Q3c: What is the nexus between the profiles of social-emotional classclimate and behavioral problems?
  
  


# Q3b

```{r}
dat2 <- dat_agg %>%
  select(!!var_cc_l2) %>%
  rename_items() %>%
  na.omit() %>%
  mutate(across(everything(), scale))

# fit_bic <- mclustBIC(dat2)
# saveRDS(fit_bic, "fit_bic_l2.rds")
fit_bic <- readRDS("fit_bic_l2.rds")

#plot(fit_bic)
#summary(fit_bic)

#bootstrap <- mclustBootstrapLRT(dat2, modelName = "EVE", maxG = 4)
#saveRDS(bootstrap, "bootstrap_l2.rds")
#bootstrap <- readRDS("bootstrap_l2.rds")
```



```{r}
mod1 <- Mclust(dat2, modelNames = "EVE", G = 3, x = fit_bic)
#summary(mod1)

means <- as.data.frame(mod1$parameters$mean) %>%
  rownames_to_column("Variable") %>%
  pivot_longer(cols = -1) %>%
  mutate(name = recode(name,
                       V1 = "Average inclusion, lower bullying 70%",
                       V2 = "Intense bullying 20%",
                       V3 = "High social inclusion 10%"
  ))

p <- means %>%
  ggplot(aes(Variable, value, group = name, color = name)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p

```

# Q3c

```{r}
dat2 <- dat %>%
  select(!!var_cc_l1) %>% 
  rename_items() %>%
  na.omit() %>%
  mutate(across(everything(), scale))

fit_bic <- readRDS("fit_bic.rds")

mod1 <- Mclust(dat2, modelNames = "VEV", G = 5, x = fit_bic)

dat2 <- dat
dat2$filter <- apply(dat2[,var_cc_l1], 1, function(x) any(is.na(x)))
dat2 <- dat2 %>% filter(!filter)
dat2$profile <- factor(mod1$classification, labels = c(
  "Bullying victim; socially rejected 19%",
  "Socially included; few relations 31%",
  "Bully and bullied; socially rejected 14%",
  "Slight bully; slight socially rejected 14%",
  "Socially included; many relations 22%"))

dat2 <- dat2 %>% mutate(across(!!c(var_bp_l1, var_cc_l1), scale))

dat2 %>% group_by(profile) %>% 
  summarise(across(!!var_bp_l1, mean, na.rm = TRUE)) %>%
  pivot_longer(col = -1) %>%
  ggplot(aes(x = profile, y = value, fill = name)) +
  geom_bar(stat = "identity",position = "dodge" ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




```

# Q3d

```{r}
dat2 <- dat_agg %>%
  select(!!var_cc_l2) %>%
  rename_items() %>%
  na.omit() %>%
  mutate(across(everything(), scale))

fit_bic <- readRDS("fit_bic_l2.rds")

mod1 <- Mclust(dat2, modelNames = "EVE", G = 3, x = fit_bic)

dat2 <- dat_agg
dat2$filter <- apply(dat2[,var_cc_l2], 1, function(x) any(is.na(x)))

dat2 <- dat2 %>% filter(!filter)
dat2$profile <- factor(mod1$classification, labels = c("Average inclusion, lower bullying 70%","Intense bullying 20%", "High social inclusion 10%"))


dat2 <- dat2 %>% mutate(across(!!c(var_bp_l2, var_cc_l2), scale))

dat2 %>% group_by(profile) %>% 
  summarise(across(!!var_bp_l2, mean, na.rm = TRUE)) %>%
  pivot_longer(col = -1) %>%
  ggplot(aes(x = profile, y = value, fill = name)) +
  geom_bar(stat = "identity",position = "dodge" )  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

#fit <- lme(itrf_sw ~ feess_si + feess_ga + soc_positivity + soc_relatedness + sar_victim + sar_perpetrator + age + migration_background + sex, random =~ 1 | id_class_teacher, data = dat, na.action = na.omit)

#html_regression(fit, title = "Social withdrawal regressed on wellbing variables")

```{r}
fit <- lme(itrf_sw ~ feess_si + feess_ga + soc_relatedness + soc_positivity + sar_victim + sar_perpetrator + feess_si_mean + feess_ga_mean + soc_relatedness_mean + soc_positivity_mean + sar_victim_mean + sar_perpetrator_mean + age + migration_background + age_mean + sex + migration_background_numeric_mean + sex_male_mean, random =~ 1 | id_class_teacher, data = dat_l12, na.action = na.omit)
html_regression(fit, title = "Social withdrawal regressed on wellbing and classclimate variables")
```

