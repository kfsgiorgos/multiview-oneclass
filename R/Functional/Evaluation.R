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


# Read results functions --------------------------------------------------
read_metricsDT_augm26_31 <- function(subfolder, datasetname, algorithm, repeatedCV) {
  
  DTaugmented <- fread(paste0(final_path_to_save, "figures/", 
                       subfolder, "/", 
                       datasetname, "_", algorithm, 
                       "_Multiple_Repres_allMetrics_Augmented_26_31_", 
                       repeatedCV,"_iters.csv"))
  return(DTaugmented)
}

read_metricsDT_augm <- function(subfolder, datasetname, algorithm, repeatedCV) {
  
  DTaugmented <- fread(paste0(final_path_to_save, "figures/",
                       subfolder, "/",
                       datasetname, "_", algorithm, 
                       "_Multiple_Repres_allMetrics_Augmented", 
                       repeatedCV,"_iters.csv"))
  return(DTaugmented)
}

read_metricsDT_AOMUR_5_31  <- function(subfolder, datasetname, algorithm, repeatedCV) {
  
  DT <- fread(paste0(final_path_to_save, "figures/", 
                     subfolder, "/", 
                     datasetname, "_", algorithm, "_Multiple_Repres_5_10_15_15_30_", 
                     repeatedCV,"_iters.csv"))
  return(DT)
  }

read_metricsDT_AOMUR_21 <- function(subfolder, datasetname, algorithm, repeatedCV) {
  
  DTaugmented <- fread(paste0(final_path_to_save, "figures/",
                              subfolder, "/",
                              datasetname, "_", algorithm, 
                              "_Multiple_Repres_allMetrics", 
                              repeatedCV,"_iters.csv"))
  return(DTaugmented)
}


# Evaluation functions ------------------------------------------------------
get_evaluation_DT_21MUR <- function(givenDT_ensemble, given_metric) {
  
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

get_evaluation_DT_5_31MUR <- function(givenDT_ensemble, given_metric) {
  
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

get_augmented_evaluation_26_31DT <- function(givenDT_ensemble, given_metric) {
  
  temp31 <- givenDT_ensemble[MUR == "31", ..given_metric][, lapply(.SD,median), .SDcols = 1]
  temp26 <- givenDT_ensemble[MUR == "26", ..given_metric][, lapply(.SD,mean), .SDcols = 1]

  resDT <- data.table(Augm_31 = temp31$AUC,
                      Augm_26 = temp26$AUC)
  return(resDT)
  
}





get_DT_21MUR <- function(given_subfolder, given_datasetname, given_algorithm, list_given_repeatedCV) {
  
  k <- 1
  temp_list <- list()
  for( i in list_given_repeatedCV){
    temp_list[[k]] <- read_metricsDT_AOMUR_21(subfolder = given_subfolder,
                                                datasetname = given_datasetname,
                                                algorithm = given_algorithm,
                                                repeatedCV = i)
    k <- k +1
  }
  
  
  Wilt_ensemble_21MUR <- rbindlist(temp_list)
  
  Wilt_MUR_21 <- get_evaluation_DT_21MUR(givenDT_ensemble = Wilt_ensemble_21MUR, given_metric = "AUC")
  Wilt_MUR21_DT <- Wilt_MUR_21[, .(Original, MUR_21, MUR_15, MUR_10, MUR_5, AOMUR_21)]
  return(Wilt_MUR21_DT)
}
get_DT_5_31MUR <- function(given_subfolder, given_datasetname, given_algorithm, list_given_repeatedCV) {
  
  k <- 1
  temp_list <- list()
  for( i in list_given_repeatedCV){
    temp_list[[k]] <- read_metricsDT_AOMUR_5_31(subfolder = given_subfolder,
                                                datasetname = given_datasetname,
                                                algorithm = given_algorithm,
                                                repeatedCV = i)
    k <- k +1
  }
  Wilt_ensemble_5_31MUR <- rbindlist(temp_list)
  Wilt_MUR5_31DT <- get_evaluation_DT_5_31MUR(givenDT_ensemble = Wilt_ensemble_5_31MUR, given_metric = "AUC")
  return(Wilt_MUR5_31DT)
}
get_augmDT_26_31MUR <- function(given_subfolder, given_datasetname, given_algorithm, list_given_repeatedCV){
  
  k <- 1
  temp_list <- list()
  for( i in list_given_repeatedCV){
    temp_list[[k]] <- read_metricsDT_augm26_31(subfolder = given_subfolder,
                                               datasetname = given_datasetname,
                                               algorithm = given_algorithm,
                                               repeatedCV = i)
    k <- k +1
  }
  Wilt_ensemble_26_31aug <- rbindlist(temp_list)
  Wilt_aug_26_31DT <- get_augmented_evaluation_26_31DT(givenDT_ensemble = Wilt_ensemble_26_31aug, given_metric = "AUC")
  return(Wilt_aug_26_31DT)
  }
get_augmDT_21MUR <- function(given_subfolder, given_datasetname, given_algorithm, list_given_repeatedCV){
  
  k <- 1
  temp_list <- list()
  for( i in list_given_repeatedCV){
    temp_list[[k]] <- read_metricsDT_augm(subfolder = given_subfolder,
                                          datasetname = given_datasetname,
                                          algorithm = given_algorithm,
                                          repeatedCV = i)
    k <- k +1
  }
  
  Wilt_ensemble_21aug <- rbindlist(temp_list)
  Wilt_aug_21DT <- get_augmented_evaluation_5_21DT(givenDT_ensemble = Wilt_ensemble_21aug, given_metric = "AUC")
  return(Wilt_aug_21DT)
  
  }


# 21 Augmented: Read CSV and automate  --------------------------------------------------

# augmented_21DT <- fread("/Users/georgios.kaiafas/Desktop/Augmented_21MUR.csv")
augmented_21DT <- fread("/home/giorgoslux/Downloads/Augmented_21MUR.csv")
dim(augmented_21DT)[2]

list_my_datasetname <- list()
list_my_subfolder <- list()
my_list_repeatedCV <- list()
for(i in 1:dim(augmented_21DT)[1]){
  list_my_datasetname[[i]] <- augmented_21DT[i, datasetname]
  list_my_subfolder[[i]] <- augmented_21DT[i, subfolder]
  my_list_repeatedCV[[i]] <- data.table::transpose(augmented_21DT[i, 3:(dim(augmented_21DT)[2]-1)])[V1>0, V1]
}

repeatedCV_remove1 <- which(my_list_repeatedCV %>% 
                             map_lgl(function(x) length(x) == 0))

x1 <- 1:length(my_list_repeatedCV)
temp_list <- list()
for(i in x1[-repeatedCV_remove1]){
  tempDT <- get_augmDT_21MUR(given_subfolder = list_my_subfolder[[i]], 
                             given_datasetname = list_my_datasetname[[i]], 
                             given_algorithm = "OCSVM", list_given_repeatedCV = my_list_repeatedCV[[i]])
  tempDT[, dataset:= list_my_subfolder[[i]]]
  temp_list[[i]] <- tempDT
  }
Aug21DT_eval <- rbindlist(temp_list)

fwrite(Aug21DT_eval, paste0(final_path_to_save, "figures/OCSVM_AllResults_Augmented21.csv"))
# 26-31 Augmented: Read CSV and automate  --------------------------------------------------

# augmented_26_31DT <- fread("/Users/georgios.kaiafas/Desktop/Augmented_26_31.csv")
augmented_26_31DT <- fread("/home/giorgoslux/Downloads/Augmented_26_31.csv")
dim(augmented_26_31DT)[2]

list_my_datasetname <- list()
list_my_subfolder <- list()
my_list_repeatedCV <- list()
for(i in 1:dim(augmented_26_31DT)[1]){
  list_my_datasetname[[i]] <- augmented_26_31DT[i, datasetname]
  list_my_subfolder[[i]] <- augmented_26_31DT[i, subfolder]
  my_list_repeatedCV[[i]] <- data.table::transpose(augmented_26_31DT[i, 3:(dim(augmented_26_31DT)[2]-1)])[V1>0, V1]
}

repeatedCV_remove2 <- which(my_list_repeatedCV %>% 
                             map_lgl(function(x) length(x) == 0))

x2 <- 1:length(my_list_repeatedCV)
temp_list <- list()
for(i in x2[-repeatedCV_remove2]){
  tempDT <- get_augmDT_26_31MUR(given_subfolder = list_my_subfolder[[i]], 
                                given_datasetname = list_my_datasetname[[i]], 
                                given_algorithm = "OCSVM", list_given_repeatedCV = my_list_repeatedCV[[i]])
  tempDT[, dataset:= list_my_subfolder[[i]]]
  temp_list[[i]] <- tempDT
}
Aug26_31DT_eval <- rbindlist(temp_list)
fwrite(Aug26_31DT_eval, paste0(final_path_to_save, "figures/OCSVM_AllResults_Augmented26_31.csv"))
# 21 MUR: Read CSV and automate  --------------------------------------------------

#MUR_21DT <- fread("/Users/georgios.kaiafas/Desktop/MUR21.csv")
MUR_21DT <- fread("/home/giorgoslux/Downloads/MUR21.csv")
dim(MUR_21DT)[2]

list_my_datasetname <- list()
list_my_subfolder <- list()
my_list_repeatedCV <- list()
for(i in 1:dim(MUR_21DT)[1]){
  list_my_datasetname[[i]] <- MUR_21DT[i, datasetname]
  list_my_subfolder[[i]] <- MUR_21DT[i, subfolder]
  my_list_repeatedCV[[i]] <- data.table::transpose(MUR_21DT[i, 3:(dim(MUR_21DT)[2]-1)])[V1>0, V1]
}

repeatedCV_remove3 <- which(my_list_repeatedCV %>% 
                              map_lgl(function(x) length(x) == 0))

x3 <- 1:length(my_list_repeatedCV)
temp_list <- list()
for(i in x3){
  tempDT <- get_DT_21MUR(given_subfolder = list_my_subfolder[[i]], 
                         given_datasetname = list_my_datasetname[[i]], 
                         given_algorithm = "OCSVM", list_given_repeatedCV = my_list_repeatedCV[[i]])
  tempDT[, dataset:= list_my_subfolder[[i]]]
  temp_list[[i]] <- tempDT
}
MUR21_DT_eval <- rbindlist(temp_list)
fwrite(MUR21_DT_eval, paste0(final_path_to_save, "figures/OCSVM_AllResults_MUR21.csv"))

# MUR 5-31: Read CSV and automate  --------------------------------------------------

# MUR_5_31DT <- fread("/Users/georgios.kaiafas/Desktop/MUR5_31.csv")
MUR_5_31DT <- fread("/home/giorgoslux/Downloads/MUR5_31.csv")
dim(MUR_5_31DT)[2]

list_my_datasetname <- list()
list_my_subfolder <- list()
my_list_repeatedCV <- list()
for(i in 1:dim(MUR_5_31DT)[1]){
  list_my_datasetname[[i]] <- MUR_5_31DT[i, datasetname]
  list_my_subfolder[[i]] <- MUR_5_31DT[i, subfolder]
  my_list_repeatedCV[[i]] <- data.table::transpose(MUR_5_31DT[i, 3:(dim(MUR_5_31DT)[2]-1)])[V1>0, V1]
}

repeatedCV_remove4 <- which(my_list_repeatedCV %>% 
                              map_lgl(function(x) length(x) == 0))

x4 <- 1:length(my_list_repeatedCV)
temp_list <- list()
for(i in x4){
  tempDT <- get_DT_5_31MUR(given_subfolder = list_my_subfolder[[i]], 
                           given_datasetname = list_my_datasetname[[i]], 
                           given_algorithm = "OCSVM", list_given_repeatedCV = my_list_repeatedCV[[i]])
  tempDT[, dataset:= list_my_subfolder[[i]]]
  temp_list[[i]] <- tempDT
}
MUR5_31DT_eval <- rbindlist(temp_list)
fwrite(MUR5_31DT_eval, paste0(final_path_to_save, "figures/OCSVM_AllResults_MUR5_31.csv"))




# MUR21_DT <- get_DT_21MUR(given_subfolder = my_subfolder, 
#                               given_datasetname = my_datasetname, 
#                               given_algorithm = "OCSVM", list_given_repeatedCV = )
# MUR5_31DT <- get_DT_5_31MUR(given_subfolder = my_subfolder, 
#                             given_datasetname = my_datasetname, 
#                             given_algorithm = "OCSVM", list_given_repeatedCV = )
# Aug26_31DT_eval <- get_augmDT_26_31MUR(given_subfolder = my_subfolder, 
#                                   given_datasetname = my_datasetname, 
#                                   given_algorithm = "OCSVM", list_given_repeatedCV = )
# Aug21DT_eval <- get_augmDT_21MUR(given_subfolder = my_subfolder, 
#                                   given_datasetname = my_datasetname, 
#                                   given_algorithm = "OCSVM", list_given_repeatedCV = )

# DT <- dplyr::bind_cols(Aug26_31DT_eval, Aug21DT_eval,  
#                        MUR5_31DT_eval[, .(Original, MUR_31, MUR_26)], 
#                        MUR21_DT_eval[, 1:5], MUR5_31DT_eval[, .(AOMUR_31, AOMUR_26)],
#                        MUR21_DT_eval[, .(AOMUR_21)], MUR5_31DT_eval[, .(AOMUR_15, AOMUR_10, AOMUR_5, dataset)])
#avg_original <- mean(x = c(DT$Original, DT$Original1))
#DT[, `:=` (Original = NULL, Original1 = NULL)]
#DT[, Original:= avg_original]
#DT[, Dataset:= my_subfolder]

# fwrite(DT, paste0(final_path_to_save, "figures/",
#                        arg2, "/", arg1, "_OCSVM_AllResults.csv"))

fwrite(DT, paste0(final_path_to_save, "figures/OCSVM_AllResults.csv"))



