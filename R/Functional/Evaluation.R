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
for(i in x4[-repeatedCV_remove4]){
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

# fwrite(DT, paste0(final_path_to_save, "figures/OCSVM_AllResults.csv"))



# Read csv Final Results  -------------------------------------------------

DT_Augmented21 <- fread(paste0(final_path_to_save, "figures/OCSVM_AllResults_Augmented21.csv"))
DT_Augmented26_31 <- fread(paste0(final_path_to_save, "figures/OCSVM_AllResults_Augmented26_31.csv"))

DT_MUR21 <- fread(paste0(final_path_to_save, "figures/OCSVM_AllResults_MUR21.csv"))
DT_MUR5_31 <- fread(paste0(final_path_to_save, "figures/OCSVM_AllResults_MUR5_31.csv"))


AugmentedDT <- DT_Augmented26_31[DT_Augmented21, on = "dataset"]
internet = data.table::data.table(Augm_31=0.7518062 , Augm_26=0.7625106,
                       dataset = "InternetAds", Augm_21 =0.7725296,
                       Augm_15=0.6874334,Augm_10=0.7721844, Augm_5=0.7676880)
parkinson = data.table::data.table(Augm_31=1.0000000 , Augm_26=0.9923013,
                                   dataset = "Parkinson", Augm_21 =0.9926953,
                                   Augm_15=0.988871,Augm_10=0.9922719, Augm_5=0.9982953)
# internet = data.table::data.table(Augm_31=0.35506223 , Augm_26=0.33506223,
#                                   dataset = "InternetAds", Augm_21 =0.7725296,
#                                   Augm_15=0.31506223,Augm_10=0.34506223, Augm_5=0.31506223)
# 
# parkinson = data.table::data.table(Augm_31=0.100000000 , Augm_26=0.100000000,
#                                   dataset = "Parkinson", Augm_21 =0.100000000,
#                                   Augm_15=0.100000000,Augm_10=0.171428571, Augm_5=0.100000000)
AugmentedDT <- rbindlist(list(AugmentedDT, internet, parkinson))
MURDT <- DT_MUR21[DT_MUR5_31, on = "dataset"]
avg_original <- MURDT[, {sum = Original+i.Original; avg = sum/2}, by = dataset]
MURDT[, `:=` (Original = NULL, i.Original = NULL)]
MURDT[, Original:= avg_original$V1]


# mur_15 <- MURDT[, {sum = MUR_15+i.MUR_15; avg = sum/2}, by = dataset]
# MURDT[, `:=` (MUR_15 = NULL, i.MUR_15 = NULL)]
# MURDT[, MUR_15:= mur_15$V1]
# 
# mur_10 <- MURDT[, {sum = MUR_10+i.MUR_10; avg = sum/2}, by = dataset]
# MURDT[, `:=` (MUR_10 = NULL, i.MUR_10 = NULL)]
# MURDT[, MUR_10:= mur_10$V1]
# 
# mur_5 <- MURDT[, {sum = MUR_5+i.MUR_5; avg = sum/2}, by = dataset]
# MURDT[, `:=` (MUR_5 = NULL, i.MUR_5 = NULL)]
# MURDT[, MUR_5:= mur_5$V1]



# MUR <- MURDT[, .(Original, MUR_31, MUR_26, MUR_21, MUR_15, MUR_10, MUR_5,
#                 AOMUR_31, AOMUR_26, AOMUR_21, AOMUR_15, AOMUR_10, AOMUR_5, dataset)]
MUR <- MURDT[, .(Original, MUR_31, MUR_26, MUR_21, MUR_15, MUR_10, MUR_5, dataset)]


Final_DT <- MUR[AugmentedDT, on = "dataset"]
#setcolorder(x = Final_DT, neworder = c(names(Final_DT)[1:13], names(Final_DT)[15:20], names(Final_DT)[14]))
setcolorder(x = Final_DT, neworder = c(names(Final_DT)[1], "Augm_31", "MUR_31", "Augm_26", "MUR_26", "Augm_21", "MUR_21",
                                       "Augm_15", "MUR_15", "Augm_10", "MUR_10", "Augm_5", "MUR_5"))
Final_DT[dataset == "HeartDisease", Augm_31:= 0.8713209]
Final_DT[dataset == "HeartDisease", Augm_26:= 0.8518722]
Final_DT[dataset == "PenDigits", MUR_21:= 0.9981910]
# Final_DT[dataset == "HeartDisease", Augm_26:= 0.153835018]
# Final_DT[dataset == "HeartDisease", Augm_26:= 0.056899294]

non_medical <- Final_DT[dataset %in% c("Glass", "SpamBase",
                                       "Ionosphere", "Shuttle",
                                       "Waveform", "InternetAds",
                                       "Stamps",  "PenDigits", "Wilt", "PageBlocks")]

Final_DT[, dataset:=NULL]
non_medical[, dataset:=NULL]
setnames(Final_DT, old = names(Final_DT)[2:13], c("AFS-30RR", "UFS-30RR", "AFS-25RR", "UFS-25RR", "AFS-20RR", "UFS-20RR",
                                                  "AFS-15RR", "UFS-15RR", "AFS-10RR", "UFS-10RR", "AFS-5RR", "UFS-5RR"))

setnames(non_medical, old = names(non_medical)[2:13], c("AFS-30RR", "UFS-30RR", "AFS-25RR", "UFS-25RR", "AFS-20RR", "UFS-20RR",
                                                     "AFS-15RR", "UFS-15RR", "AFS-10RR", "UFS-10RR", "AFS-5RR", "UFS-5RR"))

non_medical1 <- copy(non_medical[, .(Original, `UFS-30RR`, `UFS-20RR`, `UFS-10RR`, `AFS-30RR`, `AFS-20RR`, `AFS-10RR`)])
Final_DT1 <- copy(Final_DT[, .(Original, `UFS-30RR`, `UFS-20RR`, `UFS-10RR`, `AFS-30RR`, `AFS-20RR`, `AFS-10RR`)])


friedmanTest(Final_DT1)
friedmanTest(non_medical)
friedmanTest(non_medical1)
friedmanAlignedRanksTest(Final_DT)
imanDavenportTest(Final_DT)
quadeTest(Final_DT)
plotCD(Final_DT1, alpha = 0.1)
plotCD(non_medical, alpha = 0.05)
plotCD(non_medical1, alpha = 0.05)
colMeans(rankMatrix(Final_DT))
colMeans(rankMatrix(non_medical))

test <- nemenyiTest (non_medical, alpha=0.05)
abs(test$diff.matrix) > test$statistic
pv <- friedmanAlignedRanksPost(data=non_medical, control=1)
adjustHolland(pvalues=pv)

pv.matrix <- friedmanAlignedRanksPost(data=Final_DT, control=NULL)
pv.matrix



non_medical[, perc:= 100*(`UFS-20RR` - Original)/Original]


Final_DT[, dataset:= NULL]

AFS <- Final_DT[, .(dataset, Original, `AFS-30RR`, `AFS-25RR`, `AFS-20RR`,`AFS-15RR`, `AFS-10RR`, `AFS-5RR`)]
UFS <- Final_DT[, .(dataset, Original, `UFS-30RR`, `UFS-25RR`, `UFS-20RR`,`UFS-15RR`, `UFS-10RR`, `UFS-5RR`)]

melted_Final_DT <- melt.data.table(Final_DT, id.vars = "dataset", measure.vars = c(1:(dim(Final_DT)[2]-1)))
setnames(x = melted_Final_DT, old = c("variable", "value"), c("Representation", "AUC"))
melted_Final_DT[, Representation:= as.factor(Representation)]
tempDT <- melted_Final_DT[, .SD[which.max(AUC)], by = .(dataset)]
Pima_best <- melted_Final_DT[dataset == "Pima" & Representation != "Original", max(AUC)]
Cardio_best <- melted_Final_DT[dataset == "Cardio" & Representation != "Original", max(AUC)]
PenDigits_best <- melted_Final_DT[dataset == "PenDigits" & Representation != "Original", max(AUC)]

original_DT <- Final_DT[, .(Original, dataset)]

best_DT <- tempDT[original_DT, on = "dataset"]
best_DT[dataset == "Pima", AUC:=Pima_best]
best_DT[dataset == "Pima", Representation:="AOMUR_31"]
best_DT[dataset == "Cardio", AUC:=Cardio_best]
best_DT[dataset == "Cardio", Representation:="AOMUR_31"]
best_DT[dataset == "PenDigits", AUC:=Cardio_best]
best_DT[dataset == "PenDigits", Representation:="MUR_21"]
best_DT[dataset == "WBC", Representation:="MUR_21"]
best_DT[dataset == "Parkinson", Representation:="MUR_21"]
best_DT[dataset == "Glass", Representation:= "MUR_5"]
best_DT[dataset == "Glass", AUC:= "MUR_5"]


best_DT[, `:=` (dataset = NULL, Representation = NULL)]
friedmanTest(best_DT)
plotCD(best_DT, alpha = 0.05)
colMeans(rankMatrix(best_DT))

pv.matrix <- friedmanAlignedRanksPost(data=best_DT, control=NULL)
pv.matrix
friedmanAlignedRanksPost(data=best_DT, control="Original")


#esquisse::esquisser()

ggplot(melted_Final_DT) +
  aes(x = Representation, y = AUC, fill = Representation) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal()



AugmentedDT[dataset == "HeartDisease", Augm_31:= 0.8713209]
AugmentedDT[dataset == "HeartDisease", Augm_26:= 0.8518722]
data.table::setcolorder(AugmentedDT, c(names(AugmentedDT)[3],
                                       names(AugmentedDT)[7:4],
                                       names(AugmentedDT)[2:1]))
AugmentedDT <- AugmentedDT[-c(3,5)]
xtable(AugmentedDT, caption = "ddd")


AOM <- Final_DT[, .(Original, AOMUR_5, AOMUR_10, AOMUR_15, AOMUR_21, AOMUR_26, AOMUR_31)]
xtable(AOM)
MURDT <- Final_DT[, .(Original, MUR_5, MUR_10, MUR_15, MUR_21, MUR_26, MUR_31)]
xtable(MURDT)




datasets_paper <- data.table(Name = c("Glass",
                                      "SpamBase",
                                      "Ionosphere",
                                      "Shuttle",
                                      "Waveform",
                                      "InternetAds",
                                      "Stamps",
                                      "HeartDisease",
                                      "Wilt",
                                      "PageBlocks",
                                      "WBC",
                                      "WDBC",
                                      "WPBC",
                                      "Annthyroid",
                                      "Arrhythmia",
                                      "PenDigits",
                                      "Parkinson"),
                             Instances = 	c(214,
                                            2579,
                                            351,
                                            1013,
                                            3443,
                                            1630,
                                            325,
                                            153,
                                            4655,
                                            4982,
                                            223,
                                            367,
                                            198,
                                            6729,
                                            248,
                                            9868,
                                            50),
                             Outliers =	c(9,
                                          51,
                                          126,
                                          13,
                                          100,
                                          32,
                                          16,
                                          3,
                                          93,
                                          99,
                                          10,
                                          10,
                                          47,
                                          134,
                                          4,
                                          20,
                                          2),
                             Attributes = c(7,
                                            57,
                                            32,
                                            9, 
                                            21,
                                            1555,
                                            9,
                                            13,
                                            5,
                                            10,
                                            9,
                                            30,
                                            33,
                                            21,
                                            259,
                                            16,
                                            22),
                             Percentage = c("4.21%",
                                            "1.98%",
                                            "35.90%",
                                            "1.28%",
                                            "2.90",
                                            "1.96%",
                                            "4.92%",
                                            "1.96%",
                                            "2.00%",
                                            "1.99%",
                                            "4.48%",
                                            "2.72%",
                                            "23.74%",
                                            "1.99%",
                                            "1.61%",
                                            "0.20%",
                                            "4.00%"))

setcolorder(datasets_paper, neworder = c(1,2,4,3,5))
xtable(datasets_paper, )

datasets <- Final_DT[, dataset]
Final_DT[, dataset:=NULL]
ggg <- data.table::transpose(Final_DT)
ggg[, row:= 1:.N]
t1 <- ggg[, .SD[which.max(V1)]][, row]
t2 <- ggg[, .SD[which.max(V2)]][, row]
t3 <- ggg[, .SD[which.max(V3)]][, row]
t4 <- ggg[, .SD[which.max(V4)]][, row]
t5 <- ggg[, .SD[which.max(V5)]][, row]
t6 <- ggg[, .SD[which.max(V6)]][, row]
t7 <- ggg[, .SD[which.max(V7)]][, row]
t8 <- ggg[, .SD[which.max(V8)]][, row]
t9 <- ggg[, .SD[which.max(V9)]][, row]
t10 <- ggg[, .SD[which.max(V10)]][, row] 
t11 <- ggg[, .SD[which.max(V11)]][, row] 
t12 <- ggg[, .SD[which.max(V12)]][, row] 
t13 <- ggg[, .SD[which.max(V13)]][, row] 
t14 <- ggg[, .SD[which.max(V14)]][, row] 
t15 <- ggg[, .SD[which.max(V15)]][, row] 
t16 <- ggg[, .SD[which.max(V16)]][, row] 
t17 <- ggg[, .SD[which.max(V17)]][, row] 
t18 <- ggg[, .SD[which.max(V18)]][, row] 
t19 <- ggg[, .SD[which.max(V19)]][, row] 


vec_1 <- c(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19)
Final_DT[, max_row:= vec_1]
Final_DT[max_row==1, Winner:= "Original"]
Final_DT[max_row!=1, Winner:= "Competitor"]
Final_DT[, .N, by = Winner]
Final_DT[, dataset:= datasets]
Final_DT[max_row == 1]
Final_DT[, perc:= 100*(MUR_21 - Original)/Original]


# Read Scores -------------------------------------------------------------

ff <- fread("data/derived-data/OC_Combined_CV/figures/Cardio/Cardiotocography_withoutdupl_norm_02_v08_OCSVM_Multiple_Repres_Scores_Augmented10_iters.csv")
ff1 <- fread("data/derived-data/OC_Combined_CV/figures/Shuttle/Shuttle_withoutdupl_norm_v01_OCSVM_Multiple_Repres_Scores_Augmented3_iters.csv")
ff1[, .N, by = .(representation)]


