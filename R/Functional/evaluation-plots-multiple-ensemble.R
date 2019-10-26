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


read_metricsDT <- function(subfolder, datasetname, repeatedCV) {
  
  DT <- fread(paste0(final_path_to_save, "figures/", 
                     subfolder, "/", 
                     datasetname, "_OCSVM_21_Multiple_Repres_allMetrics", repeatedCV,"_iters.csv"))
  return(DT)
}

read_extended_metricsDT <- function(subfolder, datasetname, algorithm, repeatedCV, augmented_boolean) {
  
  
  if(augmented_boolean == "yes"){
    DT <- fread(paste0(final_path_to_save, "figures/", 
                       subfolder, "/", 
                       datasetname, "_", algorithm, "_Multiple_Repres_allMetrics_Augmented", repeatedCV,"_iters.csv"))
    
    
  }else{
    DT <- fread(paste0(final_path_to_save, "figures/", 
                       subfolder, "/", 
                       datasetname, "_", algorithm, "_Multiple_Repres_allMetrics", repeatedCV,"_iters.csv"))
  }
  
  
  return(DT)
}


read_extended_metricsDT_26 <- function(subfolder, datasetname, algorithm, repeatedCV, augmented_boolean) {
  
  
  if(augmented_boolean == "yes"){
    DT <- fread(paste0(final_path_to_save, "figures/", 
                       subfolder, "/", 
                       datasetname, "_", algorithm, "_Multiple_Repres_26_allMetrics_Augmented", repeatedCV,"_iters.csv"))
    
    
  }else{
    DT <- fread(paste0(final_path_to_save, "figures/", 
                       subfolder, "/", 
                       datasetname, "_", algorithm, "_Multiple_Repres_26_allMetrics", repeatedCV,"_iters.csv"))
  }
  
  
  return(DT)
}


ALOI_1 <- read_metricsDT(subfolder = "ALOI", 
                               datasetname = "ALOI_withoutdupl_norm",
                               repeatedCV = 5)


Annthyroid_1 <- read_metricsDT(subfolder = "Annthyroid", 
                             datasetname = "Annthyroid_withoutdupl_norm_02_v01",
                             repeatedCV = 30)
Annthyroid_2 <- read_metricsDT(subfolder = "Annthyroid", 
                             datasetname = "Annthyroid_withoutdupl_norm_02_v05",
                             repeatedCV = 30)
Annthyroid_3 <- read_metricsDT(subfolder = "Annthyroid", 
                             datasetname = "Annthyroid_withoutdupl_norm_02_v09",
                             repeatedCV = 30)


Wilt_1 <- read_metricsDT(subfolder = "Wilt", 
                       datasetname = "Wilt_withoutdupl_norm_02_v02",
                       repeatedCV = 30)
Wilt_2 <- read_metricsDT(subfolder = "Wilt", 
                       datasetname = "Wilt_withoutdupl_norm_02_v05",
                       repeatedCV = 30)
Wilt_3 <- read_metricsDT(subfolder = "Wilt", 
                             datasetname = "Wilt_withoutdupl_norm_02_v08",
                             repeatedCV = 30)


PageBlocks_1 <- read_metricsDT(subfolder = "PageBlocks", 
                       datasetname = "PageBlocks_withoutdupl_norm_02_v02",
                       repeatedCV = 30)
PageBlocks_2 <- read_metricsDT(subfolder = "PageBlocks", 
                             datasetname = "PageBlocks_withoutdupl_norm_02_v05",
                             repeatedCV = 30)
PageBlocks_3 <- read_metricsDT(subfolder = "PageBlocks", 
                             datasetname = "PageBlocks_withoutdupl_norm_02_v10",
                             repeatedCV = 30)


Shuttle_1 <- read_metricsDT(subfolder = "Shuttle", 
                             datasetname = "Shuttle_withoutdupl_norm_v04",
                             repeatedCV = 30)
Shuttle_2 <- read_metricsDT(subfolder = "Shuttle", 
                            datasetname = "Shuttle_withoutdupl_norm_v01",
                            repeatedCV = 10)



SpamBase_1 <- read_metricsDT(subfolder = "SpamBase", 
                           datasetname = "SpamBase_withoutdupl_norm_02_v01",
                           repeatedCV = 30)
SpamBase_2 <- read_metricsDT(subfolder = "SpamBase", 
                           datasetname = "SpamBase_withoutdupl_norm_02_v02",
                           repeatedCV = 30)
SpamBase_3 <- read_metricsDT(subfolder = "SpamBase", 
                          datasetname = "SpamBase_withoutdupl_norm_02_v03",
                          repeatedCV = 30)



Cardio_1 <- read_metricsDT(subfolder = "Cardio", 
                         datasetname = "Cardiotocography_withoutdupl_norm_02_v05",
                         repeatedCV = 30)
Cardio_2 <- read_metricsDT(subfolder = "Cardio", 
                         datasetname = "Cardiotocography_withoutdupl_norm_02_v08",
                         repeatedCV = 30)


Wave_1 <- read_metricsDT(subfolder = "Waveform", 
                       datasetname = "Waveform_withoutdupl_norm_v03",
                       repeatedCV = 30)
Wave_2 <- read_metricsDT(subfolder = "Waveform", 
                       datasetname = "Waveform_withoutdupl_norm_v05",
                       repeatedCV = 30)
Wave_3 <- read_metricsDT(subfolder = "Waveform", 
                          datasetname = "Waveform_withoutdupl_norm_v09",
                          repeatedCV = 30)

Internet_1 <- read_metricsDT(subfolder = "InternetAds", 
                       datasetname = "InternetAds_withoutdupl_norm_02_v01",
                       repeatedCV = 5)
Internet_2 <- read_metricsDT(subfolder = "InternetAds", 
                             datasetname = "InternetAds_withoutdupl_norm_02_v01",
                             repeatedCV = 30)


PenDigits_1 <- read_metricsDT(subfolder = "PenDigits", 
                            datasetname = "PenDigits_withoutdupl_norm_v01",
                            repeatedCV = 10)



get_metric_data <- function(index_DT_list, index_metric_list, column_name) {
  metric_to_plot <- c("H", "Gini", "AUC", "Spec.Sens95", "Sens.Spec95")
  list_DTs <- list(Annthyroid_1, Annthyroid_2, Annthyroid_3,
                   Cardio_1, Cardio_2,
                   Shuttle_1,
                   SpamBase_1, SpamBase_2,SpamBase_3, 
                   PageBlocks_1, PageBlocks_2, PageBlocks_3, 
                   Wave_1, Wave_2, Wave_3, 
                   Wilt_1, Wilt_2, Wilt_3, 
                   Internet_2,
                   PenDigits_1, ALOI_1)
  DT <- list_DTs[[index_DT_list]][variable == metric_to_plot[index_metric_list]]
  
  metric_DT_mean <- DT[, median(value), by = Ensemble]
  setnames(metric_DT_mean, "V1", column_name)
  
  return(list(metric_DT_mean, DT))
}


Metric_index <- 3
Annthyroid_Gini_1 <- get_metric_data(index_DT_list = 1, index_metric_list = Metric_index, column_name = "Annthyroid")[[1]]
Annthyroid_Gini_2 <- get_metric_data(index_DT_list = 2, index_metric_list = Metric_index, column_name = "Annthyroid")[[1]]
Annthyroid_Gini_3 <- get_metric_data(index_DT_list = 3, index_metric_list = Metric_index, column_name = "Annthyroid")[[1]]
DT1 <- as.data.table(Annthyroid_Gini_1$Annthyroid)
DT2 <- as.data.table(Annthyroid_Gini_2$Annthyroid)
DT3 <- as.data.table(Annthyroid_Gini_3$Annthyroid)

Cardio_Gini_1 <- get_metric_data(index_DT_list = 4, index_metric_list = Metric_index, column_name = "Cardio")[[1]]
Cardio_Gini_2 <- get_metric_data(index_DT_list = 5, index_metric_list = Metric_index, column_name = "Cardio")[[1]]
DT4 <- as.data.table(Cardio_Gini_1$Cardio)
DT5 <- as.data.table(Cardio_Gini_2$Cardio)

Shuttle_Gini_1 <- get_metric_data(index_DT_list = 6, index_metric_list = Metric_index, column_name = "Shuttle")[[1]]
DT6 <- as.data.table(Shuttle_Gini_1$Shuttle)

SpamBase_Gini_7 <- get_metric_data(index_DT_list = 7, index_metric_list = Metric_index, column_name = "SpamBase")[[1]]
SpamBase_Gini_8 <- get_metric_data(index_DT_list = 8, index_metric_list = Metric_index, column_name = "SpamBase")[[1]]
SpamBase_Gini_9 <- get_metric_data(index_DT_list = 9, index_metric_list = Metric_index, column_name = "SpamBase")[[1]]
DT7 <- as.data.table(SpamBase_Gini_7$SpamBase)
DT8 <- as.data.table(SpamBase_Gini_8$SpamBase)
DT9 <- as.data.table(SpamBase_Gini_9$SpamBase)

PageBlocks_Gini_1 <- get_metric_data(index_DT_list = 10, index_metric_list = Metric_index, column_name = "PageBlocks")[[1]]
PageBlocks_Gini_2 <- get_metric_data(index_DT_list = 11, index_metric_list = Metric_index, column_name = "PageBlocks")[[1]]
PageBlocks_Gini_3 <- get_metric_data(index_DT_list = 12, index_metric_list = Metric_index, column_name = "PageBlocks")[[1]]

DT10 <- as.data.table(PageBlocks_Gini_1$PageBlocks)
DT11 <- as.data.table(PageBlocks_Gini_2$PageBlocks)
DT12 <- as.data.table(PageBlocks_Gini_3$PageBlocks)

Wave_Gini_1 <- get_metric_data(index_DT_list = 13, index_metric_list = Metric_index, column_name = "Waveform")[[1]]
Wave_Gini_2 <- get_metric_data(index_DT_list = 14, index_metric_list = Metric_index, column_name = "Waveform")[[1]]
Wave_Gini_3 <- get_metric_data(index_DT_list = 15, index_metric_list = Metric_index, column_name = "Waveform")[[1]]
DT13 <- as.data.table(Wave_Gini_1$Waveform)
DT14 <- as.data.table(Wave_Gini_2$Waveform)
DT15 <- as.data.table(Wave_Gini_3$Waveform)

Wilt_Gini_1 <- get_metric_data(index_DT_list = 16, index_metric_list = Metric_index, column_name = "Wilt")[[1]]
Wilt_Gini_2 <- get_metric_data(index_DT_list = 17, index_metric_list = Metric_index, column_name = "Wilt")[[1]]
Wilt_Gini_3 <- get_metric_data(index_DT_list = 18, index_metric_list = Metric_index, column_name = "Wilt")[[1]]
DT16 <- as.data.table(Wilt_Gini_1$Wilt)
DT17 <- as.data.table(Wilt_Gini_2$Wilt)
DT18 <- as.data.table(Wilt_Gini_3$Wilt)

Internet_Gini_1 <- get_metric_data(index_DT_list = 19, index_metric_list = Metric_index, column_name = "Internet")[[1]]
DT19 <- as.data.table(Internet_Gini_1$Internet)

PenDigits_Gini_1 <- get_metric_data(index_DT_list = 20, index_metric_list = Metric_index, column_name = "PenDigits")[[1]]
DT20 <- as.data.table(PenDigits_Gini_1$PenDigits)

ALOI_Gini_1 <- get_metric_data(index_DT_list = 21, index_metric_list = Metric_index, column_name = "ALOI")[[1]]
DT21 <- as.data.table(ALOI_Gini_1$ALOI)


# Metric 3
Gini_DT <- bind_cols(DT2, 
                     #DT5, 
                     DT6, 
                     DT8, 
                     DT11, 
                     DT14, 
                     DT17, 
                     DT19, DT20, DT21
                     )
Gini_DT1 <- data.table::transpose(Gini_DT)

setnames(Gini_DT1, old = names(Gini_DT1), new = c("Weighted_50%","Weighted_60%","Weighted_70%","Average Representations","Average Ensemble","None"))
Gini_DT1[, `Weighted_50%`:=NULL]
Gini_DT1[, `Weighted_60%`:=NULL]
Gini_DT1[, `Weighted_70%`:=NULL]


library(Rgraphviz)
pv.matrix <- friedmanAlignedRanksPost(data=Gini_DT1, control=NULL)
pv.adj <- adjustBergmannHommel(pv.matrix)
pv.adj
r.means1 <- colMeans(rankMatrix(Gini_DT1))

r.means <- as.data.table(colMeans(rankMatrix(Gini_DT1)))
r.means[, Ensembles:= c("Average Representations", "Average Ensemble", "None")]
r.means[order(V1, decreasing = F)]

drawAlgorithmGraph(pvalue.matrix=pv.adj, mean.value=r.means1, alpha=0.05,
                   font.size=10, node.width=3, node.height=1)




# Evaluation of Ensembles - OCSVM -------------------------------------------------

Glass_ensemble <- read_extended_metricsDT(subfolder = "Glass", 
                                          datasetname = "Glass_withoutdupl_norm",
                                          algorithm = "OCSVM",
                                          repeatedCV = 30, augmented_boolean = "no")

SpamBase_ensemble <- read_extended_metricsDT(subfolder = "SpamBase", 
                                          datasetname = "SpamBase_withoutdupl_norm_02_v02",
                                          algorithm = "OCSVM",
                                          repeatedCV = 30, augmented_boolean = "no")

Cardio_ensemble <- read_extended_metricsDT(subfolder = "Cardio", 
                                           datasetname = "Cardiotocography_withoutdupl_norm_02_v08",
                                           algorithm = "OCSVM",
                                           repeatedCV = 30, augmented_boolean = "no")

Ionosphere_ensemble <- read_extended_metricsDT(subfolder = "Ionosphere",
                                              datasetname = "Ionosphere_withoutdupl_norm",
                                              algorithm = "OCSVM",
                                              repeatedCV = 30, augmented_boolean = "no")
# KDD_ensemble <- read_extended_metricsDT(subfolder = "KDD", 
#                                          datasetname = "KDDCup99_withoutdupl_catremoved",
#                                          algorithm = "iForest",
#                                          repeatedCV = 5)


Pima_ensemble <- read_extended_metricsDT(subfolder = "Pima", 
                                         datasetname = "Pima_withoutdupl_norm_02_v02",
                                         algorithm = "OCSVM",
                                         repeatedCV = 30, augmented_boolean = "no")

Shuttle_ensemble <- read_extended_metricsDT(subfolder = "Shuttle", 
                                            datasetname = "Shuttle_withoutdupl_norm_v01",
                                            algorithm = "OCSVM",
                                            repeatedCV = 30, augmented_boolean = "no")
Wave_ensemble <- read_extended_metricsDT(subfolder = "Waveform", 
                                         datasetname = "Waveform_withoutdupl_norm_v03",
                                         algorithm = "OCSVM",
                                         repeatedCV = 30, augmented_boolean = "no")
Internet_ensemble1 <- read_extended_metricsDT(subfolder = "InternetAds", 
                                             datasetname = "InternetAds_withoutdupl_norm_02_v01",
                                             algorithm = "OCSVM",
                                             repeatedCV = 10, augmented_boolean = "no")
Internet_ensemble2 <- read_extended_metricsDT(subfolder = "InternetAds", 
                                              datasetname = "InternetAds_withoutdupl_norm_02_v01",
                                              algorithm = "OCSVM",
                                              repeatedCV = 11, augmented_boolean = "no")
Internet_ensemble <- rbindlist(list(Internet_ensemble2, Internet_ensemble1))

Stamps_ensemble <- read_extended_metricsDT(subfolder = "Stamps", 
                                           datasetname = "Stamps_withoutdupl_norm_05_v04",
                                           algorithm = "OCSVM",
                                           repeatedCV = 30, augmented_boolean = "no")
Heart_ensemble <- read_extended_metricsDT(subfolder = "HeartDisease", 
                                           datasetname = "HeartDisease_withoutdupl_norm_02_v02",
                                           algorithm = "OCSVM",
                                           repeatedCV = 30, augmented_boolean = "no")
Wilt_ensemble <- read_extended_metricsDT(subfolder = "Wilt", 
                                          datasetname = "Wilt_withoutdupl_norm_02_v02",
                                          algorithm = "OCSVM",
                                          repeatedCV = 30, augmented_boolean = "no")
Page_ensemble <- read_extended_metricsDT(subfolder = "PageBlocks", 
                                         datasetname = "PageBlocks_withoutdupl_norm_02_v02",
                                         algorithm = "OCSVM",
                                         repeatedCV = 30, augmented_boolean = "no")


WBC_ensemble <- read_extended_metricsDT(subfolder = "WBC", 
                                         datasetname = "WBC_withoutdupl_norm_v05",
                                         algorithm = "OCSVM",
                                         repeatedCV = 30, augmented_boolean = "no")

WDBC_ensemble <- read_extended_metricsDT(subfolder = "WDBC", 
                                         datasetname = "WDBC_withoutdupl_norm_v09",
                                         algorithm = "OCSVM",
                                         repeatedCV = 30, augmented_boolean = "no")

WPBC_ensemble <- read_extended_metricsDT(subfolder = "WPBC", 
                                         datasetname = "WPBC_withoutdupl_norm",
                                         algorithm = "OCSVM",
                                         repeatedCV = 30, augmented_boolean = "no")
Annthyroid_ensemble <- read_extended_metricsDT(subfolder = "Annthyroid", 
                                         datasetname = "Annthyroid_withoutdupl_norm_02_v05",
                                         algorithm = "OCSVM",
                                         repeatedCV = 30, augmented_boolean = "no")

Parkinson_ensemble <- read_extended_metricsDT(subfolder = "Parkinson", 
                                              datasetname = "Parkinson_withoutdupl_norm_05_v01",
                                              algorithm = "OCSVM",
                                              repeatedCV = 30, augmented_boolean = "no")

Arrhythmia_ensemble <- read_extended_metricsDT(subfolder = "Arrhythmia", 
                                              datasetname = "Arrhythmia_withoutdupl_norm_02_v02",
                                              algorithm = "OCSVM",
                                              repeatedCV = 30, augmented_boolean = "no")
PenDigits_ensemble <- read_extended_metricsDT(subfolder = "PenDigits", 
                                               datasetname = "PenDigits_withoutdupl_norm_v03",
                                               algorithm = "OCSVM",
                                               repeatedCV = 30, augmented_boolean = "no")





get_evaluation_DT <- function(givenDT_ensemble, given_metric) {

  givenDT_ensemble <- givenDT_ensemble[!(is.na(R_Prec))]
  
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


  
list_ensembles <- list(cardio = Cardio_ensemble, 
                       glass = Glass_ensemble, 
                       heart = Heart_ensemble, 
                       internet = Internet_ensemble, 
                       page = Page_ensemble, 
                       pima = Pima_ensemble, 
                       shuttle = Shuttle_ensemble, 
                       spam = SpamBase_ensemble, 
                       stamps = Stamps_ensemble, 
                       wave = Wave_ensemble,
                       wilt = Wilt_ensemble,
                       wbc = WBC_ensemble,
                       wdbc = WDBC_ensemble, 
                       ann = Annthyroid_ensemble,
                       park = Parkinson_ensemble, 
                       arr = Arrhythmia_ensemble,
                       iono = Ionosphere_ensemble,
                       wpbc = WPBC_ensemble
                       ,pen = PenDigits_ensemble
                       )


metric <- "AUC"
list_names <- list("cardio", 
                   "glass", 
                   "heart", 
                   "internet",
                   "page",
                   "pima", 
                   "shuttle",
                   "spam",
                   "stamps", 
                   "wave", 
                   "wilt", 
                   "wbc", 
                   "wdbc", 
                   "annthyroid",
                   "parkinson", 
                   "arrhythmia",
                   "iono",
                   "wpbc"
                   ,"pen"
                   )


list_res <- list()
for( i in 1:length(list_ensembles)){
  
  tempDT <- get_evaluation_DT(givenDT_ensemble = list_ensembles[[i]], given_metric = given_metric)
  tempDT[, Dataset:= list_names[[i]]]
  tempDT[, Metric:= metric]
  list_res[[i]] <- tempDT
  }
DT <- rbindlist(list_res)
DT
DT[, Dataset:= NULL]  
DT[, Metric:= NULL] 
#DT[, Original:= NULL] 
#DT[, AOMUR_21:= NULL]
DT[, AOMUR_15:= NULL]
DT[, MUR_15:= NULL]
DT[, AOMUR_10:= NULL]
DT[, MUR_10:= NULL]
DT[, AOMUR_5:= NULL]
DT[, MUR_5:= NULL]

friedmanTest(DT)
friedmanAlignedRanksTest(DT)
imanDavenportTest(DT)
quadeTest(DT)
plotCD(DT, alpha = 0.1)
colMeans(rankMatrix(DT))
test <- nemenyiTest(DT, alpha = 0.1)
test
test$diff.matrix
abs(test$diff.matrix) > test$statistic

pv.matrix <- friedmanAlignedRanksPost(data=DT, control=NULL)
pv.matrix
friedmanAlignedRanksPost(data=DT, control="Original")
colMeans(rankMatrix(DT))
# pv.adj <- adjustBergmannHommel(pv.matrix)
# pv.adj


# pv <- quadePost(DT, control = "Original")
# pv
# adjustHolland(pvalues=pv)
# adjustFinner(pv)
# adjustRom(pvalues=pv, alpha=0.1)

# pv.adj <- adjustBergmannHommel(pv.matrix)
# r.means <- colMeans(rankMatrix(DT))
# drawAlgorithmGraph(pvalue.matrix=pv.adj, mean.value=r.means, alpha=0.05,
#                    font.size=10, node.width=3, node.height=1)



# # Evaluation of Augmented Ensembles - OCSVM -----------------------------------------------------------------


Cardio_ensemble_aug <- read_extended_metricsDT(subfolder = "Cardio",
                                              datasetname = "Cardiotocography_withoutdupl_norm_02_v08",
                                              algorithm = "OCSVM",
                                              repeatedCV = 20, augmented_boolean = "yes", norm_boolean = "no")

Glass_ensemble_aug1 <- read_extended_metricsDT(subfolder = "Glass",
                                             datasetname = "Glass_withoutdupl_norm",
                                             algorithm = "OCSVM",
                                             repeatedCV = 20, augmented_boolean = "yes", norm_boolean = "no")
Glass_ensemble_aug2 <- read_extended_metricsDT(subfolder = "Glass",
                                              datasetname = "Glass_withoutdupl_norm",
                                              algorithm = "OCSVM",
                                              repeatedCV = 10, augmented_boolean = "yes", norm_boolean = "no")
Glass_ensemble_aug <- rbindlist(list(Glass_ensemble_aug1, Glass_ensemble_aug2))

WPBC_ensemble_aug <- read_extended_metricsDT(subfolder = "WPBC", 
                                         datasetname = "WPBC_withoutdupl_norm",
                                         algorithm = "OCSVM",
                                         repeatedCV = 30, augmented_boolean = "yes", norm_boolean = "no")

Wilt_ensemble_aug_1 <- read_extended_metricsDT(subfolder = "Wilt",
                                            datasetname = "Wilt_withoutdupl_norm_02_v02",
                                            algorithm = "OCSVM",
                                            repeatedCV = 19, augmented_boolean = "yes", norm_boolean = "no")
Wilt_ensemble_aug_2 <- read_extended_metricsDT(subfolder = "Wilt",
                                             datasetname = "Wilt_withoutdupl_norm_02_v02",
                                             algorithm = "OCSVM",
                                             repeatedCV = 11, augmented_boolean = "yes", norm_boolean = "no")
Wilt_ensemble_aug <- rbindlist(list(Wilt_ensemble_aug_1, Wilt_ensemble_aug_2))

WDBC_ensemble_aug <- read_extended_metricsDT(subfolder = "WDBC", 
                                         datasetname = "WDBC_withoutdupl_norm_v09",
                                         algorithm = "OCSVM",
                                         repeatedCV = 30, augmented_boolean = "yes", norm_boolean = "no")

Shuttle_ensemble_aug_1 <- read_extended_metricsDT(subfolder = "Shuttle",
                                               datasetname = "Shuttle_withoutdupl_norm_v01",
                                               algorithm = "OCSVM",
                                               repeatedCV = 20, augmented_boolean = "yes", norm_boolean = "no")
Shuttle_ensemble_aug_2 <- read_extended_metricsDT(subfolder = "Shuttle",
                                                  datasetname = "Shuttle_withoutdupl_norm_v01",
                                                  algorithm = "OCSVM",
                                                  repeatedCV = 10, augmented_boolean = "yes", norm_boolean = "no")
Shuttle_ensemble_aug <- rbindlist(list(Shuttle_ensemble_aug_1, Shuttle_ensemble_aug_2))

Pima_ensemble_aug <- read_extended_metricsDT(subfolder = "Pima",
                                            datasetname = "Pima_withoutdupl_norm_02_v02",
                                            algorithm = "OCSVM",
                                            repeatedCV = 20, augmented_boolean = "yes", norm_boolean = "no")

Page_ensemble_aug_1 <- read_extended_metricsDT(subfolder = "PageBlocks",
                                            datasetname = "PageBlocks_withoutdupl_norm_02_v02",
                                            algorithm = "OCSVM",
                                            repeatedCV = 20, augmented_boolean = "yes", norm_boolean = "no")
Page_ensemble_aug_2 <- read_extended_metricsDT(subfolder = "PageBlocks",
                                            datasetname = "PageBlocks_withoutdupl_norm_02_v02",
                                            algorithm = "OCSVM",
                                            repeatedCV = 10, augmented_boolean = "yes", norm_boolean =  "no")
Page_ensemble_aug <- rbindlist(list(Page_ensemble_aug_1, Page_ensemble_aug_2))


Stamps_ensemble_aug1 <- read_extended_metricsDT(subfolder = "Stamps", 
                                           datasetname = "Stamps_withoutdupl_norm_05_v04",
                                           algorithm = "OCSVM",
                                           repeatedCV = 10, augmented_boolean = "yes", norm_boolean = "no")
Stamps_ensemble_aug2 <- read_extended_metricsDT(subfolder = "Stamps", 
                                               datasetname = "Stamps_withoutdupl_norm_05_v04",
                                               algorithm = "OCSVM",
                                               repeatedCV = 20, augmented_boolean = "yes", norm_boolean = "no")
Stamps_ensemble_aug <- rbindlist(list(Stamps_ensemble_aug1, Stamps_ensemble_aug2))

Wave_ensemble_aug <- read_extended_metricsDT(subfolder = "Waveform",
                                            datasetname = "Waveform_withoutdupl_norm_v03",
                                            algorithm = "OCSVM",
                                            repeatedCV = 10, augmented_boolean = "yes", norm_boolean = "no")

SpamBase_ensemble_aug <- read_extended_metricsDT(subfolder = "SpamBase", 
                                             datasetname = "SpamBase_withoutdupl_norm_02_v02",
                                             algorithm = "OCSVM",
                                             repeatedCV = 15, augmented_boolean = "yes", norm_boolean = "no")
Annthyroid_ensemble_aug1 <- read_extended_metricsDT(subfolder = "Annthyroid", 
                                               datasetname = "Annthyroid_withoutdupl_norm_02_v05",
                                               algorithm = "OCSVM",
                                               repeatedCV = 14, augmented_boolean = "yes", norm_boolean = "no")
Annthyroid_ensemble_aug2 <- read_extended_metricsDT(subfolder = "Annthyroid", 
                                                   datasetname = "Annthyroid_withoutdupl_norm_02_v05",
                                                   algorithm = "OCSVM",
                                                   repeatedCV = 15, augmented_boolean = "yes", norm_boolean = "no")
Annthyroid_ensemble_aug <- rbindlist(list(Annthyroid_ensemble_aug1, Annthyroid_ensemble_aug2))

Ionosphere_ensemble_aug1 <- read_extended_metricsDT(subfolder = "Ionosphere",
                                               datasetname = "Ionosphere_withoutdupl_norm",
                                               algorithm = "OCSVM",
                                               repeatedCV = 20, augmented_boolean = "yes", norm_boolean = "no")
Ionosphere_ensemble_aug2 <- read_extended_metricsDT(subfolder = "Ionosphere",
                                                   datasetname = "Ionosphere_withoutdupl_norm",
                                                   algorithm = "OCSVM",
                                                   repeatedCV = 10, augmented_boolean = "yes", norm_boolean = "no")
Ionosphere_ensemble_aug <- rbindlist(list(Ionosphere_ensemble_aug1, Ionosphere_ensemble_aug2))

PenDigits_ensemble_aug1 <- read_extended_metricsDT(subfolder = "PenDigits", 
                                              datasetname = "PenDigits_withoutdupl_norm_v01",
                                              algorithm = "OCSVM",
                                              repeatedCV = 4, augmented_boolean = "yes", norm_boolean = "no")

PenDigits_ensemble_aug2 <- read_extended_metricsDT(subfolder = "PenDigits", 
                                               datasetname = "PenDigits_withoutdupl_norm_v01",
                                               algorithm = "OCSVM",
                                               repeatedCV = 5, augmented_boolean = "yes", norm_boolean = "no")
PenDigits_ensemble_aug <- rbindlist(list(PenDigits_ensemble_aug1, PenDigits_ensemble_aug2))

Heart_ensemble_aug <- read_extended_metricsDT(subfolder = "HeartDisease", 
                                          datasetname = "HeartDisease_withoutdupl_norm_02_v02",
                                          algorithm = "OCSVM",
                                          repeatedCV = 20, augmented_boolean = "yes", norm_boolean = "no")

WBC_ensemble_aug <- read_extended_metricsDT(subfolder = "WBC", 
                                        datasetname = "WBC_withoutdupl_norm_v05",
                                        algorithm = "OCSVM",
                                        repeatedCV = 30, augmented_boolean = "yes", norm_boolean = "no")


# WDBC_ensemble_iF <- read_extended_metricsDT(subfolder = "WDBC", 
#                                             datasetname = "WDBC_withoutdupl_norm_v07",
#                                             algorithm = "iForest",
#                                             repeatedCV = 30)
# Arrhythmia_ensemble_iF <- read_extended_metricsDT(subfolder = "Arrhythmia",
#                                                   datasetname = "Arrhythmia_withoutdupl_norm_02_v02",
#                                                   algorithm = "iForest",
#                                                   repeatedCV = 30)

# Annthyroid_ensemble_iF <- read_extended_metricsDT(subfolder = "Annthyroid",
#                                                   datasetname = "Annthyroid_withoutdupl_norm_02_v09",
#                                                   algorithm = "OCSVM",
#                                                   repeatedCV = 30)

# Internet_ensemble1 <- read_extended_metricsDT(subfolder = "InternetAds", 
#                                               datasetname = "InternetAds_withoutdupl_norm_02_v01",
#                                               algorithm = "OCSVM",
#                                               repeatedCV = 10)
# Internet_ensemble2 <- read_extended_metricsDT(subfolder = "InternetAds", 
#                                               datasetname = "InternetAds_withoutdupl_norm_02_v01",
#                                               algorithm = "OCSVM",
#                                               repeatedCV = 11)
# Internet_ensemble <- rbindlist(list(Internet_ensemble2, Internet_ensemble1))
# 


# 

# 
# Parkinson_ensemble <- read_extended_metricsDT(subfolder = "Parkinson", 
#                                               datasetname = "Parkinson_withoutdupl_norm_05_v01",
#                                               algorithm = "OCSVM",
#                                               repeatedCV = 30)
# 




list_ensembles_aug <- list(cardio = Cardio_ensemble_aug, 
                       glass = Glass_ensemble_aug, 
                       heart = Heart_ensemble_aug, 
                       #internet = Internet_ensemble, 
                       page = Page_ensemble_aug, 
                       pima = Pima_ensemble_aug, 
                       shuttle = Shuttle_ensemble_aug, 
                       spam = SpamBase_ensemble_aug, 
                       stamps = Stamps_ensemble_aug, 
                       wave = Wave_ensemble,
                       wilt = Wilt_ensemble_aug,
                       wbc = WBC_ensemble,
                       #wdbc = WDBC_ensemble_aug, 
                       ann = Annthyroid_ensemble,
                       #park = Parkinson_ensemble, 
                       #arr = Arrhythmia_ensemble,
                       iono = Ionosphere_ensemble,
                       wpbc = WPBC_ensemble_aug
                      # ,pen = PenDigits_ensemble_aug
)


metric <- "AUC"
list_names_aug <- list("cardio", 
                   "glass", 
                   "heart", 
                   #"internet",
                   "page",
                   "pima", 
                   "shuttle",
                   "spam",
                   "stamps", 
                   "wave", 
                   "wilt", 
                   "wbc", 
                   #"wdbc", 
                   "annthyroid",
                   #"parkinson", 
                   #"arrhythmia",
                   "iono",
                   "wpbc"
                   #,"pen"
)



get_augmented_evaluationDT <- function(givenDT_ensemble, given_metric) {
 
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


list_res_aug <- list()
for( i in 1:length(list_ensembles_aug)){
  
  tempDT <- get_augmented_evaluation_5_21DT(givenDT_ensemble = list_ensembles_aug[[i]], given_metric = metric)
  tempDT[, Dataset:= list_names_aug[[i]]]
  #tempDT[, Metric:= metric]
  list_res_aug[[i]] <- tempDT
}
DT_aug <- rbindlist(list_res_aug)
DT_aug
DT_aug[, Dataset:= NULL]  
#DT_aug[, Metric:= NULL]
#DT_aug[, AOMUR_21:= NULL]
#DT_aug[, AOMUR_21:= NULL]
DT_aug[, AOMUR_15:= NULL]
DT_aug[, MUR_15:= NULL]
DT_aug[, AOMUR_10:= NULL]
DT_aug[, MUR_10:= NULL]
DT_aug[, AOMUR_5:= NULL]
DT_aug[, MUR_5:= NULL]

friedmanTest(DT_aug)
plotCD(DT_aug, alpha = 0.1)
colMeans(rankMatrix(DT_aug))
test_aug <- nemenyiTest(DT_aug, alpha = 0.1)
test_aug
test_aug$daugf.matrix
abs(test_aug$daugf.matrix) > test_aug$statistic

pv.matrix_aug <- friedmanAlignedRanksPost(data=DT_aug, control="Original")
pv.matrix_aug
colMeans(rankMatrix(DT_aug))
# pv.adj <- adjustBergmannHommel(pv.matrix)
# pv.adj


# pv.adj <- adjustBergmannHommel(pv.matrix)
# r.means <- colMeans(rankMatrix(DT))
# drawAlgorithmGraph(pvalue.matrix=pv.adj, mean.value=r.means, alpha=0.05,
#                    font.size=10, node.width=3, node.height=1)



# Evaluation augmented & URL  ---------------------------------------------

existingDT <- DT[Dataset %in% DT_aug$Dataset]
finalDT <- dplyr::bind_cols(DT_aug, existingDT) 
#sub <- finalDT[c(2,8)]
#finalDT1 <- rbindlist(list(sub, finalDT))
#finalDT <- finalDT[c(-1, -9)]
finalDT[, Dataset:= NULL]  
finalDT[, Dataset1:= NULL]  
finalDT[, Metric:= NULL] 
#finalDT[, Original:= NULL] 
#finalDT[, Augm_10:=NULL]
# finalDT[, MUR_5:=NULL]
finalDT[, AOMUR_21:=NULL]
finalDT[, AOMUR_15:=NULL]
finalDT[, AOMUR_10:=NULL]
finalDT[, AOMUR_5:=NULL]

friedmanTest(finalDT)
friedmanAlignedRanksTest(finalDT)
imanDavenportTest(finalDT)
quadeTest(finalDT)

plotCD(finalDT, alpha = 0.05)
colMeans(rankMatrix(finalDT))
test_f <- nemenyiTest(finalDT, alpha = 0.1)
test_f
test_f$diff.matrix
abs(test_f$diff.matrix) > test_f$statistic

pv.matrix_all <- friedmanAlignedRanksPost(data=finalDT, control="Original")
pv.matrix_all
which.min(colMeans(rankMatrix(finalDT)))


# # Evaluation of Ensembles - iForest -----------------------------------------------------------------


Cardio_ensemble_iF <- read_extended_metricsDT(subfolder = "Cardio", 
                                              datasetname = "Cardiotocography_withoutdupl_norm_02_v05",
                                              algorithm = "iForest",
                                              repeatedCV = 30)
Glass_ensemble_iF <- read_extended_metricsDT(subfolder = "Glass", 
                                          datasetname = "Glass_withoutdupl_norm",
                                          algorithm = "iForest",
                                          repeatedCV = 30)
Internet_ensemble_iF <- read_extended_metricsDT(subfolder = "InternetAds", 
                                              datasetname = "InternetAds_withoutdupl_norm_02_v01",
                                              algorithm = "iForest",
                                              repeatedCV = 11)
KDD_ensemble_iF1 <- read_extended_metricsDT(subfolder = "KDD", 
                                        datasetname = "KDDCup99_withoutdupl_catremoved",
                                        algorithm = "iForest",
                                        repeatedCV = 5)
KDD_ensemble_iF2 <- read_extended_metricsDT(subfolder = "KDD", 
                                           datasetname = "KDDCup99_withoutdupl_catremoved",
                                           algorithm = "iForest",
                                           repeatedCV = 6)
KDD_ensemble_iF3 <- read_extended_metricsDT(subfolder = "KDD", 
                                            datasetname = "KDDCup99_withoutdupl_catremoved",
                                            algorithm = "iForest",
                                            repeatedCV = 7)
KDD_ensemble_iF <- rbindlist(list(KDD_ensemble_iF1, KDD_ensemble_iF2, KDD_ensemble_iF3))
ALOI_ensemble_iF <- read_extended_metricsDT(subfolder = "ALOI", 
                                           datasetname = "ALOI_withoutdupl_norm",
                                           algorithm = "iForest",
                                           repeatedCV = 5)
Pima_ensemble_iF <- read_extended_metricsDT(subfolder = "Pima", 
                                         datasetname = "Pima_withoutdupl_norm_02_v01",
                                         algorithm = "iForest",
                                         repeatedCV = 30)
Shuttle_ensemble_iF <- read_extended_metricsDT(subfolder = "Shuttle", 
                                            datasetname = "Shuttle_withoutdupl_norm_v01",
                                            algorithm = "iForest",
                                            repeatedCV = 30)
SpamBase_ensemble_iF <- read_extended_metricsDT(subfolder = "SpamBase", 
                                             datasetname = "SpamBase_withoutdupl_norm_02_v02",
                                             algorithm = "iForest",
                                             repeatedCV = 30)
Stamps_ensemble_iF <- read_extended_metricsDT(subfolder = "Stamps", 
                                           datasetname = "Stamps_withoutdupl_norm_02_v02",
                                           algorithm = "iForest",
                                           repeatedCV = 30)
WBC_ensemble_iF <- read_extended_metricsDT(subfolder = "WBC", 
                                        datasetname = "WBC_withoutdupl_norm_v05",
                                        algorithm = "iForest",
                                        repeatedCV = 30)
WDBC_ensemble_iF <- read_extended_metricsDT(subfolder = "WDBC", 
                                         datasetname = "WDBC_withoutdupl_norm_v07",
                                         algorithm = "iForest",
                                         repeatedCV = 30)
Heart_ensemble_iF <- read_extended_metricsDT(subfolder = "HeartDisease",
                                          datasetname = "HeartDisease_withoutdupl_norm_02_v02",
                                          algorithm = "iForest",
                                          repeatedCV = 30)
Arrhythmia_ensemble_iF <- read_extended_metricsDT(subfolder = "Arrhythmia",
                                               datasetname = "Arrhythmia_withoutdupl_norm_02_v02",
                                               algorithm = "iForest",
                                               repeatedCV = 30)
Wilt_ensemble_iF <- read_extended_metricsDT(subfolder = "Wilt",
                                         datasetname = "Wilt_withoutdupl_norm_02_v08",
                                         algorithm = "iForest",
                                         repeatedCV = 30)
Wave_ensemble_iF <- read_extended_metricsDT(subfolder = "Waveform",
                                         datasetname = "Waveform_withoutdupl_norm_v03",
                                         algorithm = "iForest",
                                         repeatedCV = 30)
Page_ensemble_iF <- read_extended_metricsDT(subfolder = "PageBlocks",
                                         datasetname = "PageBlocks_withoutdupl_norm_02_v02",
                                         algorithm = "iForest",
                                         repeatedCV = 30)
Annthyroid_ensemble_iF <- read_extended_metricsDT(subfolder = "Annthyroid",
                                               datasetname = "Annthyroid_withoutdupl_norm_02_v09",
                                               algorithm = "OCSVM",
                                               repeatedCV = 30)

# Internet_ensemble1 <- read_extended_metricsDT(subfolder = "InternetAds", 
#                                               datasetname = "InternetAds_withoutdupl_norm_02_v01",
#                                               algorithm = "OCSVM",
#                                               repeatedCV = 10)
# Internet_ensemble2 <- read_extended_metricsDT(subfolder = "InternetAds", 
#                                               datasetname = "InternetAds_withoutdupl_norm_02_v01",
#                                               algorithm = "OCSVM",
#                                               repeatedCV = 11)
# Internet_ensemble <- rbindlist(list(Internet_ensemble2, Internet_ensemble1))
# 


# 

# 
# Parkinson_ensemble <- read_extended_metricsDT(subfolder = "Parkinson", 
#                                               datasetname = "Parkinson_withoutdupl_norm_05_v01",
#                                               algorithm = "OCSVM",
#                                               repeatedCV = 30)
# 




list_ensembles_iF <- list(cardio = Cardio_ensemble_iF, 
                       glass = Glass_ensemble_iF, 
                       internet = Internet_ensemble_iF, 
                       pima = Pima_ensemble_iF, 
                       shuttle = Shuttle_ensemble_iF, 
                       spam = SpamBase_ensemble_iF, 
                       stamps = Stamps_ensemble_iF, 
                       wbc = WBC_ensemble_iF,
                       wdbc = WDBC_ensemble_iF, 
                       kdd = KDD_ensemble_iF, 
                       heart = Heart_ensemble_iF,
                       arr = Arrhythmia_ensemble_iF,
                       aloi = ALOI_ensemble_iF,
                       wilt = Wilt_ensemble_iF,
                       wave = Wave_ensemble_iF,
                       page = Page_ensemble_iF,
                       ann = Annthyroid_ensemble_iF)


metric <- "AUC"
list_names_iF <- list("cardio", 
                   "glass", 
                   "internet",
                   "pima", 
                   "shuttle",
                   "spam",
                   "stamps", 
                   "wbc", 
                   "wdbc", 
                   "kdd", 
                   "heart",
                   "arr",
                   "aloi",
                   "wilt",
                   "wave",
                   "page",
                   "ann")


list_typeII <- list(Glass_ensemble, Shuttle_ensemble, Wave_ensemble)
list_namesII <- list("Glass", "Shuttle", "Wave")

list_res_iF <- list()
for( i in 1:length(list_ensembles_iF)){
  
  tempDT <- get_evaluation_DT(givenDT_ensemble = list_ensembles_iF[[i]], given_metric = metric)
  tempDT[, Dataset:= list_names_iF[[i]]]
  tempDT[, Metric:= metric]
  list_res_iF[[i]] <- tempDT
}
DT_iF <- rbindlist(list_res_iF)
DT_iF
DT_iF[, Dataset:= NULL]  
DT_iF[, Metric:= NULL] 
#DT_iF[, AOMUR_21:= NULL]
#DT_iF[, AOMUR_21:= NULL]
DT_iF[, AOMUR_15:= NULL]
DT_iF[, MUR_15:= NULL]
DT_iF[, AOMUR_10:= NULL]
DT_iF[, MUR_10:= NULL]
DT_iF[, AOMUR_5:= NULL]
DT_iF[, MUR_5:= NULL]

friedmanTest(DT_iF)
plotCD(DT_iF, alpha = 0.1)
colMeans(rankMatrix(DT_iF))
test_iF <- nemenyiTest(DT_iF, alpha = 0.1)
test_iF
test_iF$diff.matrix
abs(test_iF$diff.matrix) > test_iF$statistic

pv.matrix_iF <- friedmanAlignedRanksPost(data=DT_iF, control="Original")
pv.matrix_iF
colMeans(rankMatrix(DT_iF))
# pv.adj <- adjustBergmannHommel(pv.matrix)
# pv.adj


# pv.adj <- adjustBergmannHommel(pv.matrix)
# r.means <- colMeans(rankMatrix(DT))
# drawAlgorithmGraph(pvalue.matrix=pv.adj, mean.value=r.means, alpha=0.05,
#                    font.size=10, node.width=3, node.height=1)


