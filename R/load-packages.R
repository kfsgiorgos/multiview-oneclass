# install & load the required packages
pkg <- c("ggplot2", "lubridate", "data.table", "purrr", "reticulate", "foreign",
        "pROC", "caret", "dplyr", "esquisse", "stringr", "readtext", "esquisse",
        "dbscan", "philentropy", "glue", "readtext", "config")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
  lapply(new.pkg, require, character.only = TRUE)
} else{
  lapply(pkg, require, character.only = TRUE)
}
options(scipen = 999L)


source("R/Functional/OCSVM-multiview.R")
source("R/Functional/construct-tabular-outlier.R")
reticulate::source_python("Python/sklearn-outlier-algos.py")
