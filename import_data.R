###################################
# Project:  class_climate         #
# Function: import /prepare data  #
# Author: Juergen Wilbert         #
# Date:Sun Mar  7 11:15:39 2021   #
###################################

## This code contains the import and preparation of the data 
## used for the analyses in this project.
## This code is for documentation and transparency and 
## can only be executed by the author.

dir <- getwd()
source(file.path(DIR$PROJECT$MM20, "mm20_control_file.R"))
setwd(dir_mm20$data_2019_students_clean)
dat <- readRDS("2019_students.rds")
dat <- dat %>% filter(school_type == "GL")

dat$sar_victim <- score_scale(dat, scale == "sar" & subscale == "v")
dat$sar_perpetrator <- score_scale(dat, scale == "sar" & subscale == "o")

dat$feess_kk  <- dat$FEESS34_KK / 3
dat$feess_kk[is.na(dat$feess_kk)] <- dat$FEESS12_KK[is.na(dat$feess_kk)]

dat$feess_si  <- dat$FEESS34_SI / 3
dat$feess_si[is.na(dat$feess_si)] <- dat$FEESS12_SI[is.na(dat$feess_si)]

dat <- dat %>% mutate(
  LM_in = LM_in / (n_valid_like -1),
  LL_in = LL_in / (n_valid_like -1),
  Friend_in = Friend_in / (n_valid_like -1),
  soc_relatedness = (LL_in + LM_in) / 2,
  soc_positivity = ((LM_in - LL_in) + 1) / 2
) %>%
  filter(!is.na(grade))

class(dat$age) <- c("dic", "numeric")

.var <- c(
  "feess_kk", "feess_si", "LM_in", "LL_in","Friend_in", "sar_victim", 
  "sar_perpetrator", "itrf_int", "itrf_ext", "itrf_sw", "itrf_ad", "itrf_apd", 
  "itrf_opp", "itrf_int", "itrf_ext", "n_valid_like", "soc_relatedness", "soc_positivity", "density_ll", "density_lm", "density_friend", "density_impact"
)

dat_mean <- dat %>% group_by(id_class_teacher) %>%
  summarise(
    across(all_of(.var), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE)
    )
    )
  )

dat <- full_join(dat, dat_mean, by = "id_class_teacher")

.var <- paste0(.var, rep(c("", "_mean", "_sd"), each = length(.var)))
.var <- c("id_class_teacher", "age", "migration_background", "sex", "grade", .var)

dat <- select(dat, !!.var)

setwd(dir)

saveRDS(dat, "data.rds")
