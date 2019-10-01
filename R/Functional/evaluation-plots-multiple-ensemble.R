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
                   PenDigits_1)
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

# Metric 3
Gini_DT <- bind_cols(DT2, 
                     #DT5, 
                     DT6, 
                     DT8, 
                     DT11, 
                     DT14, 
                     DT17, 
                     DT19, DT20
                     )
Gini_DT1 <- data.table::transpose(Gini_DT)

setnames(Gini_DT1, old = names(Gini_DT1), new = c("Weighted_50%","Weighted_60%","Weighted_70%","Average Representations","Average Ensemble","None"))
Gini_DT1[, `Weighted_50%`:=NULL]
Gini_DT1[, `Weighted_60%`:=NULL]
Gini_DT1[, `Weighted_70%`:=NULL]
#setnames(Gini_DT1, old = names(Gini_DT1), new = c("AMUR", "AOMUR", "Original"))

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







