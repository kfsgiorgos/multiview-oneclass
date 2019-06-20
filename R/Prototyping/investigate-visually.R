# load packages & source scripts ------------------------------------------
setwd("~/GitHub_projects/multiview-oneclass/")
source("R/Functional/OCSVM-multiview.R")
source("R/Functional/construct-tabular-outlier.R")

set.seed(2239)

# Read CSV results --------------------------------------------------------
Original_DT_worse <- fread("~/Downloads/DAMI_datasets/derived_data/Stamps_withoutdupl_norm_02_v10.csv")
setnames(Original_DT_worse, "outlier", "Label")
id_selected1 <- sample(Original_DT_worse$id, 0.05 * dim(Original_DT_worse)[1])

Original_DT_worse[id %in% id_selected1, Class:= "True Normal & OCSVM Train"]
Original_DT_worse[!(id %in% id_selected1) & Label == "no", Class:= "True Normal & OCSVM Predict"]
Original_DT_worse[is.na(Class), Class:= "True Outlier"]


p1 <- ggplot(data = Original_DT_worse) +
  aes(x = `att3`, y = `att1`, color = Class) +
  geom_point(aes(shape = Class)) +
  theme_minimal() +
  labs(title = "Stamps_withoutdupl_norm_02_v10 - Original Space")
cols <- c("True Normal & OCSVM Train" = "#005ae0", "True Normal & OCSVM Predict" = "#f5917b", "True Outlier" = "black")
p1 + scale_colour_manual(values = cols)


Outliers_DT_worse <- create_unsupervised_view(datasetname = "Stamps_withoutdupl_norm_02_v10", 
                                              percentage_OD = 0.2, mixed_view_features = 1)[["mixed_arthur"]]
Outliers_DT_worse[, Label:=Original_DT_worse$Label]
Outliers_DT_worse[, id:= Original_DT_worse$id]
id_selected <- sample(Outliers_DT_worse$id, 0.05 * dim(Outliers_DT_worse)[1])

Outliers_DT_worse[id %in% id_selected, Class:= "True Normal & OCSVM Train"]
Outliers_DT_worse[!(id %in% id_selected) & Label == "no", Class:= "True Normal & OCSVM Predict"]
Outliers_DT_worse[is.na(Class), Class:= "True Outlier"]



p <- ggplot(data = Outliers_DT_worse) +
  aes(x = `LDF-081`, y = `COF-091`, color = Class) +
  geom_point(aes(shape = Class)) +
  theme_minimal() +
  labs(title = "Stamps_withoutdupl_norm_02_v10 - Outlier Space")
cols <- c("True Normal & OCSVM Train" = "#005ae0", "True Normal & OCSVM Predict" = "#f5917b", "True Outlier" = "black")
p + scale_colour_manual(values = cols)






Outliers_DT_better <- create_unsupervised_view(datasetname = "Annthyroid_withoutdupl_norm_02_v01", 
                                              percentage_OD = 0.2, mixed_view_features = 1)[["mixed_arthur"]]

Outliers_DT_better[, Label:=Original_DT_better$Label]
Outliers_DT_better[, id:= Original_DT_better$id]
id_selected <- sample(Outliers_DT_better$id, 0.05 * dim(Outliers_DT_better)[1])

Outliers_DT_better[id %in% id_selected, Class:= "True Normal & OCSVM Train"]
Outliers_DT_better[!(id %in% id_selected) & Label == "no", Class:= "True Normal & OCSVM Predict"]
Outliers_DT_better[is.na(Class), Class:= "True Outlier"]



p <- ggplot(data = Outliers_DT_better) +
  aes(x = `KNN-071`, y = `COF-016`, color = Class) +
  geom_point(aes(shape = Class)) +
  theme_minimal() +
  labs(title = "Annthyroid_withoutdupl_norm_02_v01 - Outlier Space")
cols <- c("True Normal & OCSVM Train" = "#005ae0", "True Normal & OCSVM Predict" = "#f5917b", "True Outlier" = "black")
p + scale_colour_manual(values = cols)




Original_DT_better <- fread("~/Downloads/DAMI_datasets/derived_data/Annthyroid_withoutdupl_norm_02_v01.csv")
setnames(Original_DT_better, "outlier", "Label")
id_selected1 <- sample(Original_DT_better$id, 0.05 * dim(Original_DT_better)[1])

Original_DT_better[id %in% id_selected1, Class:= "True Normal & OCSVM Train"]
Original_DT_better[!(id %in% id_selected1) & Label == "no", Class:= "True Normal & OCSVM Predict"]
Original_DT_better[is.na(Class), Class:= "True Outlier"]


p11 <- ggplot(data = Original_DT_better) +
  aes(x = `att18`, y = `att19`, color = Class) +
  geom_point(aes(shape = Class)) +
  theme_minimal() +
  labs(title = "Annthyroid_withoutdupl_norm_02_v01 - Original Space")
cols <- c("True Normal & OCSVM Train" = "#005ae0", "True Normal & OCSVM Predict" = "#f5917b", "True Outlier" = "black")
p11 + scale_colour_manual(values = cols)
