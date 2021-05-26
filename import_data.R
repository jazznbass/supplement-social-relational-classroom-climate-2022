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

##

library(tidyverse)
library(scaledic)

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

dat$sar_victim <- score_scale(dat, scale == "sar" & subscale == "v", label = "Bullying victim")
dat$sar_perpetrator <- score_scale(dat, scale == "sar" & subscale == "o", label = "Bullying perpetrator")

# scale feess 1-2 and feess 3-4 on a 0 to 1 scale (FEESS 34 now is from 0 to 3)

## dat$feess_kk  <- dat$FEESS34_KK / 3
## dat$feess_kk[is.na(dat$feess_kk)] <- dat$FEESS12_KK[is.na(dat$feess_kk)]

dat$feess_si  <- dat$FEESS34_SI / 3
dat$feess_si[is.na(dat$feess_si)] <- dat$FEESS12_SI[is.na(dat$feess_si)]

dat$feess_ga  <- dat$FEESS34_GA / 3
dat$feess_ga[is.na(dat$feess_ga)] <- dat$FEESS12_SI[is.na(dat$feess_ga)]

# further preparations
dat <- dat %>% mutate(
  id_class_teacher = anonymise(id_class_teacher), # turn ids to numbers
  LM_in = LM_in / (n_valid_like -1), # turn frequencies to proportions
  LL_in = LL_in / (n_valid_like -1), # turn frequencies to proportions
  #Friend_in = Friend_in / (n_valid_like -1), # turn frequencies to proportions
  soc_relatedness = (LL_in + LM_in) / 2, # create soc_relatedness
  soc_positivity = ((LM_in - LL_in) + 1) / 2, # create soc_positivity,
  sex_male = as.numeric(sex) - 1,
  migration_background_numeric = as.numeric(migration_background) - 1,
  sex = C(sex, contr.helmert),
  migration_background = C(migration_background, contr.helmert)
) %>%
  filter(!is.na(grade))


dic_attr(dat$soc_relatedness, "item_label") <- "Social relatedness"
dic_attr(dat$soc_positivity, "item_label") <- "Social positivity"
dic_attr(dat$migration_background_numeric, "item_label") <- "Migration background"
dic_attr(dat$migration_background, "item_label") <- "Migration background"
dic_attr(dat$sex_male, "item_label") <- "Sex male"
dic_attr(dat$sex, "item_label") <- "Sex"
dic_attr(dat$age, "item_label") <- "Age"

dic_attr(dat$feess_si, "item_label") <- "Social integration"
dic_attr(dat$feess_ga, "item_label") <- "Teacher acceptance"

class(dat$age) <- c("dic", "numeric")


## add level 2 variables (means and sd)

.var <- c(
  "feess_ga", "feess_si", "sar_victim", #"LM_in", "LL_in","Friend_in", 
  "sar_perpetrator", "itrf_sw", "itrf_ad", "itrf_apd", 
  "itrf_opp", "n_valid_like", "soc_relatedness",     
  "soc_positivity", "age", "sex_male", "migration_background_numeric"
)


dat_mean <- dat %>% group_by(id_class_teacher) %>%
  summarise(
    across(all_of(.var), list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE)
    )
    )
  )

#dat <- full_join(dat, dat_mean, by = "id_class_teacher")

## select variables of interest

var_control <- c("id_class_teacher", "age", "migration_background", "sex", "sex_male", "migration_background_numeric", "grade")
var <- c(
  "feess_ga", "feess_si", "LM_in", "LL_in","Friend_in", "sar_victim", 
  "sar_perpetrator", "itrf_sw", "itrf_ad", "itrf_apd", "itrf_opp", "n_valid_like",
  "soc_relatedness", "soc_positivity"
)

#var_l2 <- paste0(var, rep(c("", "_mean", "_sd"), each = length(var)))

dat <- dat %>% select(!!c(var, var_control))

dat <- dic_haven(dat)

dat_mean <- dat_mean %>%
  mutate(
    soc_relatedness_mean = dic(soc_relatedness_mean, item_label = "Average social relatedness"),
    soc_positivity_mean = dic(soc_positivity_mean, item_label = "Average social positivity"),
    migration_background_numeric_mean = dic(migration_background_numeric_mean, item_label = "Proportion migration background"),
    sex_male_mean = dic(sex_male_mean, item_label = "Proportion male"),
    sar_perpetrator_mean = dic(sar_perpetrator_mean, item_label = "Average bullying perpetrator"),
    sar_victim_mean = dic(sar_victim_mean, item_label = "Average bullying victim"),
    feess_si_mean = dic(feess_si_mean, item_label = "Average social inclusion"),
    feess_ga_mean = dic(feess_ga_mean, item_label = "Average tacher acceptance"),
    age_mean = dic(age_mean, item_label = "Average age"),
    itrf_sw_mean = dic(itrf_sw_mean, item_label = "Average social withdrawal"),
    itrf_apd_mean = dic(itrf_apd_mean, item_label = "Average academically disorganized"),
    itrf_ad_mean = dic(itrf_ad_mean, item_label = "Average anxiety/depression"),
    itrf_opp_mean = dic(itrf_opp_mean, item_label = "Average oppositional behavior")
  )

dat_mean <- dic_haven(dat_mean)

setwd(dir)

saveRDS(dat, "data_l1.rds")
saveRDS(dat_mean, "data_l2.rds")

