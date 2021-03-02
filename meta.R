

# render ------------------------------------------------------------------

rmarkdown::render("report.Rmd", output_file = "index.html", output_dir = "docs")


# import original datafile ------------------------------------------------

dir <- getwd()
source(file.path(DIR$PROJECT$MM20, "mm20_control_file.R"))
setwd(dir_mm20$data_2019_students_clean)
dat <- readRDS("2019_students.rds")
dat <- dat %>% filter(school_type == "GL")
setwd(dir)
saveRDS(dat, "data.rds")
