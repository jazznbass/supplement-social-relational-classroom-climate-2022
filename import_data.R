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

## function that turns ids into numbers

anonymise <- function(x) {
  factor(x, labels = paste0("ID_", seq_along(unique(x))))
}

## remember current id
dir <- getwd()

## import dataset
source(file.path(DIR$PROJECT$MM20, "mm20_control_file.R"))
setwd(dir_mm20$data_2019_students_clean)
dat <- readRDS("2019_students.rds")

## filter and prepare data

dat <- dat %>% filter(school_type == "GL") # only primary schools

# score bullying scales

dat$sar_victim <- score_scale(dat, scale == "sar" & subscale == "v", name = "Bullying victim")
dat$sar_perpetrator <- score_scale(dat, scale == "sar" & subscale == "o", name = "Bullying perpetrator")

# scale feess 1-2 and fees 3-4 on the same scale
dat$feess_kk  <- dat$FEESS34_KK / 3
dat$feess_kk[is.na(dat$feess_kk)] <- dat$FEESS12_KK[is.na(dat$feess_kk)]

dat$feess_si  <- dat$FEESS34_SI / 3
dat$feess_si[is.na(dat$feess_si)] <- dat$FEESS12_SI[is.na(dat$feess_si)]

# further preparations
dat <- dat %>% mutate(
  id_class_teacher = anonymise(id_class_teacher), # turn ids to numbers
  LM_in = LM_in / (n_valid_like -1), # turn sociometric data to proportions
  LL_in = LL_in / (n_valid_like -1), # turn sociometric data to proportions
  Friend_in = Friend_in / (n_valid_like -1), # turn sociometric data to proportions
  soc_relatedness = (LL_in + LM_in) / 2, # create soc_relatedness
  soc_positivity = ((LM_in - LL_in) + 1) / 2 # create soc_positivity
) %>%
  filter(!is.na(grade))

class(dat$age) <- c("dic", "numeric")


## add level 2 variables (means and sd)

.var <- c(
  "feess_kk", "feess_si", "LM_in", "LL_in","Friend_in", "sar_victim", 
  "sar_perpetrator", "itrf_int", "itrf_ext", "itrf_sw", "itrf_ad", "itrf_apd", 
  "itrf_opp", "n_valid_like", "soc_relatedness", "soc_positivity", "density_ll", "density_lm", "density_friend", "density_impact"
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

## define variables of interest

.var <- paste0(.var, rep(c("", "_mean", "_sd"), each = length(.var)))
.var <- c("id_class_teacher", "age", "migration_background", "sex", "grade", .var)

dat <- select(dat, !!.var)

setwd(dir)

saveRDS(dat, "data.rds")
