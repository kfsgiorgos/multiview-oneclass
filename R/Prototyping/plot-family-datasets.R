


get_dataset_performanceDT  <- function(subfolder, datasetname, dataset_col) {
  
random1_DT <- fread(paste0("data/derived-data/OC_Combined_CV/figures/", subfolder, "/", datasetname, "_1random.csv"))
random1_max_hyper <- random1_DT[, .SD[which.max(V1)], by = c("Cross-Validation", "features_Iteration")]
random1_max_hyper[, features_Iteration:=as.factor(features_Iteration)]
random1_max_hyper[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
random1_max_hyper1 <- random1_max_hyper[, mean(V2), by = "features_Iteration"]
random1_max_hyper1[, Representation:= "Outlier-Space"]
random1_max_hyper1[, Dataset:= dataset_col]


combined_DT <-  fread(paste0("data/derived-data/OC_Combined_CV/figures/", 
                             subfolder, "/", datasetname, "_Combined.csv"))
combined_max_hyper <- combined_DT[, .SD[which.max(V1)], by = c("Cross-Validation", 
                                                               "features_Iteration")]
combined_max_hyper[, features_Iteration:=as.factor(features_Iteration)]
combined_max_hyper[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
combined_max_hyper1 <- combined_max_hyper[, mean(V2), by = "features_Iteration"]
combined_max_hyper1[, Representation:= "Combined-Space"]
combined_max_hyper1[, Dataset:= dataset_col]

original_DT <- fread(paste0("data/derived-data/OC_Combined_CV/figures/", 
                            subfolder, "/", datasetname, "_Original.csv"))
original_DT[, Representation:= "Original-Space"]
original_max_hyper <- original_DT[, .SD[which.max(V1)], by = c("Cross-Validation")]
original_max_hyper[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
original_max_hyper[, `:=` (V1 = NULL, gamma = NULL, nu = NULL,kernel = NULL)]
original_max_hyper[, Dataset:= dataset_col]



combined_max_hyper1[, V3:= (V1 - original_max_hyper[, mean(V2)])/original_max_hyper[, sd(V2)]]
random1_max_hyper1[, V3:= (V1 - original_max_hyper[, mean(V2)])/original_max_hyper[, sd(V2)]]

DT <- rbindlist(list(combined_max_hyper1, random1_max_hyper1))
return(DT)
}

Annthyroid_v01 <- get_dataset_performanceDT(subfolder = "Annthyroid", 
                                            datasetname = "Annthyroid_withoutdupl_norm_02_v01",
                                            dataset_col = "Annthyroid_v01")
Annthyroid_v08 <- get_dataset_performanceDT(subfolder = "Annthyroid", 
                                            datasetname = "Annthyroid_withoutdupl_norm_02_v08",
                                            dataset_col = "Annthyroid_v08")
Annthyroid_v10 <- get_dataset_performanceDT(subfolder = "Annthyroid", 
                                            datasetname = "Annthyroid_withoutdupl_norm_02_v10",
                                            dataset_col = "Annthyroid_v10")

Annthyroid <- rbindlist(list(Annthyroid_v01, Annthyroid_v08, Annthyroid_v10))


PenDigits_v01 <- get_dataset_performanceDT(subfolder = "PenDigits", 
                                           datasetname = "PenDigits_withoutdupl_norm_v01", 
                                           dataset_col = "PenDigits_v01")
PenDigits_v02 <- get_dataset_performanceDT(subfolder = "PenDigits", 
                                           datasetname = "PenDigits_withoutdupl_norm_v02", 
                                           dataset_col = "PenDigits_v02")
PenDigits_v03 <- get_dataset_performanceDT(subfolder = "PenDigits", 
                                           datasetname = "PenDigits_withoutdupl_norm_v01", 
                                           dataset_col = "PenDigits_v03")

PenDigits <- rbindlist(list(PenDigits_v01, PenDigits_v02, PenDigits_v03))


Stamps_v01 <- get_dataset_performanceDT(subfolder = "Stamps", 
                                           datasetname = "Stamps_withoutdupl_norm_02_v01", 
                                           dataset_col = "Stamps_v01")
Stamps_v02 <- get_dataset_performanceDT(subfolder = "Stamps", 
                                           datasetname = "Stamps_withoutdupl_norm_02_v02", 
                                           dataset_col = "Stamps_v02")
Stamps_v03 <- get_dataset_performanceDT(subfolder = "Stamps", 
                                           datasetname = "Stamps_withoutdupl_norm_02_v01", 
                                           dataset_col = "Stamps_v03")

Stamps <- rbindlist(list(Stamps_v01, Stamps_v02, Stamps_v03))



Shuttle_v01 <- get_dataset_performanceDT(subfolder = "Shuttle", 
                                           datasetname = "Shuttle_withoutdupl_norm_v01", 
                                           dataset_col = "Shuttle_v01")
Shuttle_v02 <- get_dataset_performanceDT(subfolder = "Shuttle", 
                                           datasetname = "Shuttle_withoutdupl_norm_v05", 
                                           dataset_col = "Shuttle_v05")
Shuttle_v03 <- get_dataset_performanceDT(subfolder = "Shuttle", 
                                           datasetname = "Shuttle_withoutdupl_norm_v01", 
                                           dataset_col = "Shuttle_v03")

Shuttle <- rbindlist(list(Shuttle_v01, Shuttle_v02, Shuttle_v03))



Pima_v01 <- get_dataset_performanceDT(subfolder = "Pima", 
                                        datasetname = "Pima_withoutdupl_norm_02_v01", 
                                        dataset_col = "Pima_v01")
Pima_v02 <- get_dataset_performanceDT(subfolder = "Pima", 
                                        datasetname = "Pima_withoutdupl_norm_02_v02", 
                                        dataset_col = "Pima_v02")
Pima_v03 <- get_dataset_performanceDT(subfolder = "Pima", 
                                        datasetname = "Pima_withoutdupl_norm_02_v03", 
                                        dataset_col = "Pima_v03")

Pima <- rbindlist(list(Pima_v01, Pima_v02, Pima_v03))


Ionosphere <- get_dataset_performanceDT(subfolder = "Ionosphere", 
                                        datasetname = "Ionosphere_withoutdupl_norm", 
                                        dataset_col = "Ionosphere")



DT <- rbindlist(list(Annthyroid, PenDigits, Waveform, Stamps, Shuttle,
                     Ionosphere, Pima))

p <- ggplot(data = DT) +
  aes(x = Dataset, y = V3, fill = Representation) +
  geom_boxplot() +
  theme_bw()+
  coord_flip()+
  geom_hline(yintercept = 0, color = "red")
p
