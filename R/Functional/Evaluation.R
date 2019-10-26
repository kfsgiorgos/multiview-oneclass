experiments <- "OC_combined_CV"
path_to_read <- config::get("path_to_read_datasets", 
                            file = config_file_path,
                            config = loaded_config_type)
path_to_save <- config::get("path_to_save_derived_datasets", 
                            file = config_file_path,
                            config = loaded_config_type)

if(experiments == "OC_combined"){
  folder_to_save <- config::get("OC_combined_experiments", 
                                file = config_file_path,
                                config = loaded_config_type)
  final_path_to_save <- paste0(paste0(path_to_save, folder_to_save))
}
if(experiments == "OC_combined_CV"){
  folder_to_save <- config::get("OC_CV_combined_experiments", 
                                file = config_file_path,
                                config = loaded_config_type)
  final_path_to_save <- paste0(paste0(path_to_save, folder_to_save))
}

read_extended_metricsDT <- function(subfolder, datasetname, algorithm, repeatedCV, augmented_boolean, norm_boolean) {
  
  
  if(augmented_boolean == "yes"){
    DT <- fread(paste0(final_path_to_save, "figures/", 
                       subfolder, "/", 
                       datasetname, "_", algorithm, "_Multiple_Repres_allMetrics_Augmented", repeatedCV,"_iters.csv"))
    
    
  }
  if(augmented_boolean == "no" & norm_boolean == "no"){
    DT <- fread(paste0(final_path_to_save, "figures/", 
                       subfolder, "/", 
                       datasetname, "_", algorithm, "_Multiple_Repres_allMetrics", repeatedCV,"_iters.csv"))
  }
  if(augmented_boolean == "no" & norm_boolean == "yes"){
    DT <- fread(paste0(final_path_to_save, "figures/", 
                       subfolder, "/", 
                       datasetname, "_", algorithm, "_Norm_Multiple_Repres_allMetrics", repeatedCV,"_iters.csv"))
  }
  
  
  return(DT)
}

read_correct_AOMUR_moreMUR  <- function(subfolder, datasetname, algorithm, repeatedCV) {
  
  DT <- fread(paste0(final_path_to_save, "figures/", 
                     subfolder, "/", 
                     datasetname, "_", algorithm, "_Multiple_Repres_5_10_15_15_30", 
                     repeatedCV,"_iters.csv"))
  return(DT)
  
  
}

get_evaluation_5_21_DT <- function(givenDT_ensemble, given_metric) {
  
  #givenDT_ensemble <- givenDT_ensemble[!(is.na(R_Prec))]
  
  DT_21 <- data.table(Original = givenDT_ensemble[MUR == "21" & Representation == "Original", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                      MUR_21 = givenDT_ensemble[MUR == "21" & Representation == "Multiple_Representations" & Ensemble == "Average Representations", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                      Original_MUR_21 = givenDT_ensemble[MUR == "21" & Representation == "Multiple_Representations" & Ensemble == "Average Ensemble", ..given_metric][, lapply(.SD,median), .SDcols = 1])
  setnames(DT_21, names(DT_21), c("Original", "MUR_21", "AOMUR_21"))
  # Average_DT_21 <- data.table(Original = median(DT_21$Original,na.rm=TRUE), 
  #                             MUR = median(DT_21$MUR_21,na.rm=TRUE), 
  #                             AOMUR = median(DT_21$Original_MUR_21,na.rm=TRUE), 
  #                             Representations = "21")
  
  DT_5 <- data.table(MUR_5 = givenDT_ensemble[MUR == "5" & Representation == "Multiple_Representations" & Ensemble == "Average Representations", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                     Original_MUR_5 = givenDT_ensemble[MUR == "5" & Representation == "Multiple_Representations" & Ensemble == "Average Ensemble", ..given_metric][, lapply(.SD,median), .SDcols = 1])
  setnames(DT_5, names(DT_5), c("MUR_5", "AOMUR_5"))
  
  # Average_DT_5 <- data.table(Original = median(DT_5$Original,na.rm=TRUE), 
  #                             MUR = median(DT_5$MUR_5,na.rm=TRUE), 
  #                             AOMUR = median(DT_5$Original_MUR_5,na.rm=TRUE), 
  #                             Representations = "5")
  
  DT_10 <- data.table(MUR_10 = givenDT_ensemble[MUR == "10" & Representation == "Multiple_Representations" & Ensemble == "Average Representations", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                      Original_MUR_10 = givenDT_ensemble[MUR == "10" & Representation == "Multiple_Representations" & Ensemble == "Average Ensemble", ..given_metric][, lapply(.SD,median), .SDcols = 1])
  setnames(DT_10, names(DT_10), c("MUR_10", "AOMUR_10"))
  # Average_DT_10 <- data.table(Original = median(DT_10$Original,na.rm=TRUE), 
  #                             MUR = median(DT_10$MUR_10,na.rm=TRUE), 
  #                             AOMUR = median(DT_10$Original_MUR_10,na.rm=TRUE), 
  #                             Representations = "10")
  # 
  DT_15 <- data.table(MUR_15 = givenDT_ensemble[MUR == "15" & Representation == "Multiple_Representations" & Ensemble == "Average Representations", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                      Original_MUR_15 = givenDT_ensemble[MUR == "15" & Representation == "Multiple_Representations" & Ensemble == "Average Ensemble", ..given_metric][, lapply(.SD,median), .SDcols = 1])
  setnames(DT_15, names(DT_15), c("MUR_15", "AOMUR_15"))
  # Average_DT_15 <- data.table(Original = median(DT_15$Original,na.rm=TRUE), 
  #                            MUR = median(DT_15$MUR_15,na.rm=TRUE), 
  #                            AOMUR = median(DT_15$Original_MUR_15,na.rm=TRUE), 
  #                            Representations = "15")
  # 
  # allDT <- as.data.table(dplyr::bind_cols(list(DT_21$Original, DT_21$MUR_21, 
  #                                         DT_5$MUR_5, DT_10$MUR_10, DT_15$MUR_15,
  #                                         DT_21$Original_MUR_21, DT_5$Original_MUR_5, 
  #                                         DT_10$Original_MUR_10, DT_15$Original_MUR_15)))
  
  # DT_26 <- data.table(MUR_26 = givenDT_ensemble[MUR == "26" & Representation == "Multiple_Representations" & Ensemble == "Average Representations", ..given_metric][, lapply(.SD,median), .SDcols = 1],
  #                     Original_MUR_26 = givenDT_ensemble[MUR == "26" & Representation == "Multiple_Representations" & Ensemble == "Average Ensemble", ..given_metric][, lapply(.SD,median), .SDcols = 1])
  # setnames(DT_26, names(DT_26), c("MUR_26", "AOMUR_26"))
  
  # all_averageDT <- as.data.table(dplyr::bind_cols(list(DT_26, DT_21, DT_15, 
  #                                 DT_10, DT_5)))
  
  all_averageDT <- as.data.table(dplyr::bind_cols(list(DT_21, DT_15, 
                                                       DT_10, DT_5)))
  
  return(all_averageDT)
}

get_evaluation_5_31_DT <- function(givenDT_ensemble, given_metric) {
  
  #givenDT_ensemble <- givenDT_ensemble[(is.na(R_Prec))]
  
  DT_31 <- data.table(Original = givenDT_ensemble[MUR == "31" & Representation == "Original", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                      MUR_31 = givenDT_ensemble[MUR == "31" & Representation == "Multiple_Representations" & Ensemble == "Average Representations", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                      Original_MUR_31 = givenDT_ensemble[MUR == "31" & Representation == "Multiple_Representations" & Ensemble == "Average Ensemble", ..given_metric][, lapply(.SD,median), .SDcols = 1])
  setnames(DT_31, names(DT_31), c("Original", "MUR_31", "AOMUR_31"))
  
  DT_26 <- data.table(MUR_26 = givenDT_ensemble[MUR == "26" & Representation == "Multiple_Representations" & Ensemble == "Average Representations", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                      Original_MUR_26 = givenDT_ensemble[MUR == "26" & Representation == "Multiple_Representations" & Ensemble == "Average Ensemble", ..given_metric][, lapply(.SD,median), .SDcols = 1])
  setnames(DT_26, names(DT_26), c("MUR_26", "AOMUR_26"))
  
  DT_15 <- data.table(MUR_15 = givenDT_ensemble[MUR == "15" & Representation == "Multiple_Representations" & Ensemble == "Average Representations", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                      Original_MUR_15 = givenDT_ensemble[MUR == "15" & Representation == "Multiple_Representations" & Ensemble == "Average Ensemble", ..given_metric][, lapply(.SD,median), .SDcols = 1])
  setnames(DT_15, names(DT_15), c("MUR_15", "AOMUR_15"))
  
  DT_10 <- data.table(MUR_10 = givenDT_ensemble[MUR == "10" & Representation == "Multiple_Representations" & Ensemble == "Average Representations", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                      Original_MUR_10 = givenDT_ensemble[MUR == "10" & Representation == "Multiple_Representations" & Ensemble == "Average Ensemble", ..given_metric][, lapply(.SD,median), .SDcols = 1])
  setnames(DT_10, names(DT_10), c("MUR_10", "AOMUR_10"))
  
  DT_5 <- data.table(MUR_5 = givenDT_ensemble[MUR == "5" & Representation == "Multiple_Representations" & Ensemble == "Average Representations", ..given_metric][, lapply(.SD,median), .SDcols = 1],
                     Original_MUR_5 = givenDT_ensemble[MUR == "5" & Representation == "Multiple_Representations" & Ensemble == "Average Ensemble", ..given_metric][, lapply(.SD,median), .SDcols = 1])
  setnames(DT_5, names(DT_5), c("MUR_5", "AOMUR_5"))
  
  
  all_averageDT <- as.data.table(dplyr::bind_cols(list(DT_31, DT_26, DT_15, 
                                                       DT_10, DT_5)))
  
  return(all_averageDT)
}


get_augmented_evaluation_5_21DT <- function(givenDT_ensemble, given_metric) {
  
  temp21 <- givenDT_ensemble[MUR == "21", ..given_metric][, lapply(.SD,median), .SDcols = 1]
  temp15 <- givenDT_ensemble[MUR == "15", ..given_metric][, lapply(.SD,mean), .SDcols = 1]
  temp10 <- givenDT_ensemble[MUR == "10", ..given_metric][, lapply(.SD,mean), .SDcols = 1]
  temp5 <- givenDT_ensemble[MUR == "5", ..given_metric][, lapply(.SD,mean), .SDcols = 1]
  
  resDT <- data.table(Augm_21 = temp21$AUC,
                      Augm_15 = temp15$AUC,
                      Augm_10 = temp10$AUC,
                      Augm_5 = temp5$AUC)
  return(resDT)
  
}


# Cardio ------------------------------------------------------------------

Cardio_ensemble_21MUR <- read_extended_metricsDT(subfolder = "Cardio", 
                                           datasetname = "Cardiotocography_withoutdupl_norm_02_v08",
                                           algorithm = "OCSVM",
                                           repeatedCV = 30, augmented_boolean = "no", norm_boolean = "no")
Cardio_ensemble_aug <- read_extended_metricsDT(subfolder = "Cardio",
                                               datasetname = "Cardiotocography_withoutdupl_norm_02_v08",
                                               algorithm = "OCSVM",
                                               repeatedCV = 20, augmented_boolean = "yes")

Cardio_ensemble_MUR_5_30_1 <- read_correct_AOMUR_moreMUR(subfolder = "Cardio",
                                               datasetname = "Cardiotocography_withoutdupl_norm_02_v08",
                                               algorithm = "OCSVM",
                                               repeatedCV = 9)
Cardio_ensemble_MUR_5_30_2 <- read_correct_AOMUR_moreMUR(subfolder = "Cardio",
                                                       datasetname = "Cardiotocography_withoutdupl_norm_02_v08",
                                                       algorithm = "OCSVM",
                                                       repeatedCV = 10)
Cardio_ensemble_MUR_5_30 <- rbindlist(list(Cardio_ensemble_MUR_5_30_1, Cardio_ensemble_MUR_5_30_2))

tempDT <- get_evaluation_5_21_DT(givenDT_ensemble = Cardio_ensemble_21MUR, given_metric = "AUC")
tempDT_21 <- tempDT[, .(Original, MUR_21, AOMUR_21)]
tempDT_2 <- get_evaluation_5_31_DT(givenDT_ensemble = Cardio_ensemble_MUR_5_30, given_metric = "AUC")
tempDT_3 <- get_augmented_evaluation_5_21DT(givenDT_ensemble = Cardio_ensemble_aug, given_metric = "AUC")

DT_final <- dplyr::bind_cols(tempDT_21, tempDT_2, tempDT_3)
# Annthyroid ------------------------------------------------------------------

Annthyroid_ensemble_21MUR <- read_extended_metricsDT(subfolder = "Annthyroid", 
                                               datasetname = "Annthyroid_withoutdupl_norm_02_v05",
                                               algorithm = "OCSVM",
                                               repeatedCV = 30, augmented_boolean = "no", norm_boolean = "no")
Annthyroid_ensemble_aug1 <- read_extended_metricsDT(subfolder = "Annthyroid", 
                                                    datasetname = "Annthyroid_withoutdupl_norm_02_v05",
                                                    algorithm = "OCSVM",
                                                    repeatedCV = 14, augmented_boolean = "yes")
Annthyroid_ensemble_aug2 <- read_extended_metricsDT(subfolder = "Annthyroid", 
                                                    datasetname = "Annthyroid_withoutdupl_norm_02_v05",
                                                    algorithm = "OCSVM",
                                                    repeatedCV = 15, augmented_boolean = "yes")
Annthyroid_ensemble_aug <- rbindlist(list(Annthyroid_ensemble_aug1, Annthyroid_ensemble_aug2))


Annthyroid_ensemble_MUR_5_30 <- read_correct_AOMUR_moreMUR(subfolder = "Annthyroid",
                                                       datasetname = "Annthyroid_withoutdupl_norm_02_v09",
                                                       algorithm = "OCSVM",
                                                       repeatedCV = 4)

tempDT <- get_evaluation_5_21_DT(givenDT_ensemble = Annthyroid_ensemble_21MUR, given_metric = "AUC")
tempDT_21 <- tempDT[, .(Original, MUR_21, AOMUR_21)]
tempDT_2 <- get_evaluation_5_31_DT(givenDT_ensemble = Annthyroid_ensemble_MUR_5_30, given_metric = "AUC")
tempDT_3 <- get_augmented_evaluation_5_21DT(givenDT_ensemble = Annthyroid_ensemble_aug, given_metric = "AUC")

DT_final <- dplyr::bind_cols(tempDT_21, tempDT_2, tempDT_3)
# Pima --------------------------------------------------------------------
Pima_ensemble_21MUR <- read_extended_metricsDT(subfolder = "Pima", 
                                         datasetname = "Pima_withoutdupl_norm_02_v02",
                                         algorithm = "OCSVM",
                                         repeatedCV = 30, augmented_boolean = "no", norm_boolean = "no")
Pima_ensemble_21MUR_norm1 <- read_extended_metricsDT(subfolder = "Pima", 
                                               datasetname = "Pima_withoutdupl_norm_02_v02",
                                               algorithm = "OCSVM",
                                               repeatedCV = 14, augmented_boolean = "no", norm_boolean = "yes")
Pima_ensemble_21MUR_norm2 <- read_extended_metricsDT(subfolder = "Pima", 
                                                    datasetname = "Pima_withoutdupl_norm_02_v02",
                                                    algorithm = "OCSVM",
                                                    repeatedCV = 15, augmented_boolean = "no", norm_boolean = "yes")
Pima_ensemble_21MUR_norm <- rbindlist(list(Pima_ensemble_21MUR_norm1, Pima_ensemble_21MUR_norm2))

Pima_ensemble_aug <- read_extended_metricsDT(subfolder = "Pima",
                                             datasetname = "Pima_withoutdupl_norm_02_v02",
                                             algorithm = "OCSVM",
                                             repeatedCV = 20, augmented_boolean = "yes")
Pima_ensemble_MUR_5_30 <- read_correct_AOMUR_moreMUR(subfolder = "Pima",
                                                           datasetname = "Pima_withoutdupl_norm_02_v02",
                                                           algorithm = "OCSVM",
                                                           repeatedCV = 10)

tempDT <- get_evaluation_5_21_DT(givenDT_ensemble = Pima_ensemble_21MUR, given_metric = "AUC")
tempDT_21 <- tempDT[, .(Original, MUR_21, AOMUR_21)]
tempDT_2 <- get_evaluation_5_31_DT(givenDT_ensemble = Pima_ensemble_MUR_5_30, given_metric = "AUC")
tempDT_3 <- get_augmented_evaluation_5_21DT(givenDT_ensemble = Pima_ensemble_aug, given_metric = "AUC")
tempDT_4 <- get_evaluation_5_21_DT(givenDT_ensemble = Pima_ensemble_21MUR_norm, given_metric = "AUC")
DT_final <- dplyr::bind_cols(tempDT_21, tempDT_2, tempDT_3)


# Heart -------------------------------------------------------------------

Heart_ensemble_21MUR_norm_1 <- read_extended_metricsDT(subfolder = "HeartDisease", 
                                          datasetname = "HeartDisease_withoutdupl_norm_02_v02",
                                          algorithm = "OCSVM",
                                          repeatedCV = 15, augmented_boolean = "no", norm_boolean = "yes")
Heart_ensemble_21MUR_norm_2 <- read_extended_metricsDT(subfolder = "HeartDisease", 
                                                     datasetname = "HeartDisease_withoutdupl_norm_02_v02",
                                                     algorithm = "OCSVM",
                                                     repeatedCV = 5, augmented_boolean = "no", norm_boolean = "yes")
Heart_ensemble_21MUR_norm_3 <- read_extended_metricsDT(subfolder = "HeartDisease", 
                                                     datasetname = "HeartDisease_withoutdupl_norm_02_v02",
                                                     algorithm = "OCSVM",
                                                     repeatedCV = 6, augmented_boolean = "no", norm_boolean = "yes")
Heart_ensemble_21MUR_norm <- rbindlist(list(Heart_ensemble_21MUR_norm_1, Heart_ensemble_21MUR_norm_2, Heart_ensemble_21MUR_norm_3))

tempDT_4 <- get_evaluation_5_21_DT(givenDT_ensemble = Heart_ensemble_21MUR_norm, given_metric = "AUC")



