# install & load the required packages
pkg <- c("ggplot2", "lubridate", "data.table", "purrr", "reticulate", "foreign",
        "pROC", "caret", "dplyr", "esquisse", "stringr", "readtext", "esquisse",
        "philentropy", "glue", "readtext", "config", "doParallel",
        "foreach", "devtools", "scmamp", "hmeasure", "rlist", "fst", "rsample",
        "magrittr")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)
  lapply(new.pkg, require, character.only = TRUE)
  # devtools::install_github("b0rxa/scmamp")
} else{
  lapply(pkg, require, character.only = TRUE)
}
options(scipen = 999L)


source("R/Functional/OCSVM-multiview.R")
source("R/Functional/OCSVM_CrossV.R")
source("R/Functional/OCSVM_CrossV-1.R")
source("R/Functional/augmented_ensemble_functions.R")
source("R/Functional/construct-tabular-outlier.R")
source("R/Functional/supporting_functions.R")
reticulate::source_python("Python/sklearn-outlier-algos.py")
