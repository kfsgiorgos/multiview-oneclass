# ------- Run just once the following two functions

given_datasetname <- "Wilt_withoutdupl_norm_02_v02"
given_datasetname <- "Wilt_withoutdupl_norm_02_v05"
given_datasetname <- "Wilt_withoutdupl_norm_02_v08"

given_datasetname <- "Hepatitis_withoutdupl_norm_05_v03"
given_datasetname <- "Hepatitis_withoutdupl_norm_05_v06"
given_datasetname <- "Hepatitis_withoutdupl_norm_05_v09"


given_datasetname <- "PageBlocks_withoutdupl_norm_02_v02"
given_datasetname <- "PageBlocks_withoutdupl_norm_02_v05"
given_datasetname <- "PageBlocks_withoutdupl_norm_02_v10"
# 1. consrtuct the csv with all the outliers for each method i.e. KNN, LOF, etc
GetTabularOutlierScore(datasetname = paste0(given_datasetname, ".results"))
# 2. convert arff datasets to csv
GetCsvFromArff(datasetname = given_datasetname)

# compare the OCSVM on the original-View VS the the feature space composed of
# 1 random outlier vector selected from each method. 
# example: (KNN(3), LOF(98), KDEOS(1), .... last method)

# comparison_results <- run_unsupervised_multiview_per_dataset(datasetname = "SpamBase_withoutdupl_norm_02_v01")
# temp <- comparison_results[[5]][[3]][[23]]
# summary(temp)
# DT <- fread("data/derived-data/SpamBase_withoutdupl_norm_02_v01.csv")
# temp[, Label:= DT$Label]
# esquisse::esquisser()
# 
# # read the Scores results
# datasetname <- "SpamBase_withoutdupl_norm_02_v01"
# normal_size <- c(0.01, 0.05, 0.1, 0.2)
# # first normal_size
# DTscores <- fread(paste0("data/derived-data/OCSVM-multiview/", datasetname, 
#                          "_OCSVM_scores_1random_normal_class_", 100*normal_size[1], ".csv"), 
#                   nThread = 5)
# DTauc <- fread(paste0("data/derived-data/OCSVM-multiview/", datasetname, 
#                       "_OCSVM_auc_1random_normal_class_", 100*normal_size[1], ".csv"), nThread = 5)





