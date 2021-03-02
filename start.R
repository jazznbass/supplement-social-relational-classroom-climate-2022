# load packages -----------------------------------------------------------

if (!("wmisc" %in% installed.packages())) 
  devtools::install_github("jazznbass/wmisc")
if (!("scaledic" %in% installed.packages())) 
  devtools::install_github("jazznbass/scaledic")

packages <- c('sjPlot', 'knitr', 'semTable', 'kableExtra', 'bookdown', 
              'scaledic', 'Amelia', 'wmisc', 'psych', 'lavaan', 'nlme', 'lme4',
              'tidyverse', 'readxl', 'gridExtra', 'broom')

invisible(sapply(packages, library, character.only = TRUE))

dat <- readRDS("data.rds")

dat$sar_victim <- score_scale(dat, scale == "sar" & subscale == "v")
dat$sar_perpetrator <- score_scale(dat, scale == "sar" & subscale == "o")

dat$feess_kk  <- dat$FEESS34_KK / 3
dat$feess_kk[is.na(dat$feess_kk)] <- dat$FEESS12_KK[is.na(dat$feess_kk)]

dat$feess_si  <- dat$FEESS34_SI / 3
dat$feess_si[is.na(dat$feess_si)] <- dat$FEESS12_SI[is.na(dat$feess_si)]



.var <- c("feess_kk", "feess_si", "LM_in", "LL_in","Friend_in", "sar_victim", "sar_perpetrator")

dat_mean <- dat %>% group_by(id_class_teacher) %>%
  summarise(
    across(all_of(.var), ~mean(.x, na.rm = TRUE),.names = "mean_{.col}")
  )





