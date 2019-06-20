# load packages & source scripts ------------------------------------------
setwd("~/GitHub_projects/multiview-oneclass/")
source("R/Functional/OCSVM-multiview.R")
source("R/Functional/construct-tabular-outlier.R")



fnames <- list.files("~/Downloads/DAMI_datasets/derived_data/", pattern = "results")
fnames[20]

system.time({t <- run_unsupervised_multiviem_1random(datasetname = "InternetAds_withoutdupl_norm_02_v10",
                                                     mixed_view_features = 1,
                                                     Iter_outlier_features = 10,
                                                     normal_size = 0.2,
                                                     Iters_normal_class = 50, percentage_OD = 1)})
temp22 <- t[[2]]
temp22[, Features_Iteration:= as.factor(Features_Iteration)]

esquisse::esquisser()
p <- ggplot(data = temp22) +
  aes(x = Features_Iteration, y = V1, fill = Representation) +
  geom_boxplot() +
  theme_minimal() +
  theme_minimal() + scale_y_continuous(breaks = seq(0.3, 1.0,0.05))
p
