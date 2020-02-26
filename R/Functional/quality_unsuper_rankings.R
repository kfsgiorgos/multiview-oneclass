# source("~/R Language Default Dir/Github-projects/multiview-oneclass/src.R")
source("~/GitHub_projects/multiview-oneclass/src.R")
# source("src.R")
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


create_unsupervised_scoresDT_temp <- function(datasetname, percentage_OD, mixed_view_features) {
  
  
  #DToutliers1 <- fread(paste0("data/derived-data/", datasetname, ".results.csv"))
  DToutliers1 <- fread(paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".results.csv"))
  
  
  outlier_algorithms <- names(DToutliers1)[2:length(names(DToutliers1))] %>%
    purrr::map(function(x) stringi::stri_split(str = x, fixed = "-")[[1]][1])
  
  range_outlier_algorithms <- names(DToutliers1)[2:length(names(DToutliers1))] %>%
    purrr::map(function(x) stringi::stri_split(str = x, fixed = "-")[[1]][2])  
  
  #datasetname <- "Stamps_withoutdupl_norm_02_v01"
  #datasetname <- "Parkinson_withoutdupl_norm_05_v01"
  #datasetname <- "Pima_withoutdupl_norm_02_v09"
  DT <- as.data.table(unlist(outlier_algorithms))
  setnames(DT, "V1", "OD")
  DT[, range:=unlist(range_outlier_algorithms)]
  DT[, nchar1:= nchar(range)]
  DT[nchar1==1, range:= paste0("00",range)]
  DT[nchar1==2, range:= paste0("0",range)]
  DT[, selected_columns:= paste0(OD, "-", range)]
  setnames(DToutliers1, old = 2:length(names(DToutliers1)), new = DT[, selected_columns])
  
  if("KNN" %in% DT[, unique(OD)]){
    KNNs <- DT[OD == "KNN", selected_columns]
    sampleKNNs <- sample(x = KNNs, size = percentage_OD * length(KNNs), replace = F)
    DToutliersKNNs <- DToutliers1[, .SD, .SDcols = sampleKNNs]
    sampleKNN <- sample(x = KNNs, size = mixed_view_features, replace = F)
    
    # Rank Normalization: https://people.revoledu.com/kardi/tutorial/Similarity/Normalized-Rank.html 
    KNN_DT <- DToutliers1[, .SD, .SDcols = c(sampleKNN)]
    KNN_DT[, id:= 1:.N]
    setnames(KNN_DT, 1, "KNN")
    KNN_DT1 <- copy(KNN_DT[order(KNN, decreasing = T)])
    KNN_DT1[, rank:= 1:.N]
    KNN_DT1[, KNN_normalized_rank:= round((rank-1)/(.N-1), digits = 9)]
    setkey(x = KNN_DT1, "id")
    KNN_DT2 <- as.data.table(KNN_DT1$KNN_normalized_rank)
    setnames(KNN_DT2, 1, "KNN")
    
  } else{
    KNN_DT2 <- NULL
  }
  
  
  if("KNNW" %in% DT[, unique(OD)]){
    KNNWs <- DT[OD == "KNNW", selected_columns]
    sampleKNNWs <- sample(x = KNNWs, size = percentage_OD * length(KNNWs), replace = F)
    DToutliersKNNWs <- DToutliers1[, .SD, .SDcols = sampleKNNWs]
    sampleKNNW <- sample(x = KNNWs, size = mixed_view_features, replace = F)
    
    # Rank Normalization
    KNNW_DT <- DToutliers1[, .SD, .SDcols = c(sampleKNNW)]
    KNNW_DT[, id:= 1:.N]
    setnames(KNNW_DT, 1, "KNNW")
    KNNW_DT1 <- copy(KNNW_DT[order(KNNW, decreasing = T)])
    KNNW_DT1[, rank:= 1:.N]
    KNNW_DT1[, KNNW_normalized_rank:= round((rank-1)/(.N-1), digits = 9)]
    setkey(KNNW_DT1, "id")
    KNNW_DT2 <- as.data.table(KNNW_DT1$KNNW_normalized_rank)
    setnames(KNNW_DT2, 1, "KNNW")
  } else{
    KNNW_DT2 <- NULL
  }
  
  
  
  if("LOF" %in% DT[, unique(OD)]){
    LOFs <- DT[OD == "LOF", selected_columns]
    sampleLOFs <- sample(x = LOFs, size = percentage_OD * length(LOFs), replace = F)
    DToutliersLOFs <- DToutliers1[, .SD, .SDcols = sampleLOFs]
    sampleLOF <- sample(x = LOFs, size = mixed_view_features, replace = F)
    
    # Rank Normalization
    LOF_DT <- DToutliers1[, .SD, .SDcols = c(sampleLOF)]
    LOF_DT[, id:= 1:.N]
    setnames(LOF_DT, 1, "LOF")
    LOF_DT1 <- copy(LOF_DT[order(LOF, decreasing = T)])
    LOF_DT1[, rank:= 1:.N]
    LOF_DT1[, LOF_normalized_rank:= round((rank-1)/(.N-1), digits = 9)]
    setkey(LOF_DT1, "id")
    LOF_DT2 <- as.data.table(LOF_DT1$LOF_normalized_rank)
    setnames(LOF_DT2, 1, "LOF")
  } else{
    LOF_DT2 <- NULL
  }
  
  
  if("SimplifiedLOF" %in% DT[, unique(OD)]){
    SimplifiedLOFs <- DT[OD == "SimplifiedLOF", selected_columns]
    sampleSimplifiedLOFs <- sample(x = SimplifiedLOFs, size = percentage_OD * length(SimplifiedLOFs), replace = F)
    DToutliersSimplifiedLOFs <- DToutliers1[, .SD, .SDcols = sampleSimplifiedLOFs]
    sampleSimplifiedLOF <- sample(x = SimplifiedLOFs, size = mixed_view_features, replace = F)
    
    # Rank Normalization
    SimplifiedLOF_DT <- DToutliers1[, .SD, .SDcols = c(sampleSimplifiedLOF)]
    SimplifiedLOF_DT[, id:= 1:.N]
    setnames(SimplifiedLOF_DT, 1, "SimplifiedLOF")
    SimplifiedLOF_DT1 <- copy(SimplifiedLOF_DT[order(SimplifiedLOF, decreasing = T)])
    SimplifiedLOF_DT1[, rank:= 1:.N]
    SimplifiedLOF_DT1[, SimplifiedLOF_normalized_rank:= round((rank-1)/(.N-1), digits = 9)]
    setkey(SimplifiedLOF_DT1, "id")
    SimplifiedLOF_DT2 <- as.data.table(SimplifiedLOF_DT1$SimplifiedLOF_normalized_rank)
    setnames(SimplifiedLOF_DT2, 1, "SimplifiedLOF")
  } else{
    SimplifiedLOF_DT2 <- NULL
  }
  
  
  
  if("LoOP" %in% DT[, unique(OD)]){
    LoOPs <- DT[OD == "LoOP", selected_columns]
    sampleLoOPs <- sample(x = LoOPs, size = percentage_OD * length(LoOPs), replace = F)
    DToutliersLoOPs <- DToutliers1[, .SD, .SDcols = sampleLoOPs]
    sampleLoOP <- sample(x = LoOPs, size = mixed_view_features, replace = F)
    
    # Normalized by default
    LoOP_DT2 <- DToutliers1[, .SD, .SDcols = c(sampleLoOP)]
    setnames(LoOP_DT2, 1, "LoOP")
    
  } else{
    LoOP_DT2 <- NULL
  }
  
  
  if("LDOF" %in% DT[, unique(OD)]){
    LDOFs <- DT[OD == "LDOF", selected_columns]
    sampleLDOFs <- sample(x = LDOFs, size = percentage_OD * length(LDOFs), replace = F)
    DToutliersLDOFs <- DToutliers1[, .SD, .SDcols = sampleLDOFs]
    sampleLDOF <- sample(x = LDOFs, size = mixed_view_features, replace = F)
    # Rank Normalization
    LDOF_DT <- DToutliers1[, .SD, .SDcols = c(sampleLDOF)]
    LDOF_DT[, id:= 1:.N]
    setnames(LDOF_DT, 1, "LDOF")
    LDOF_DT1 <- copy(LDOF_DT[order(LDOF, decreasing = T)])
    LDOF_DT1[, rank:= 1:.N]
    LDOF_DT1[, LDOF_normalized_rank:= round((rank-1)/(.N-1), digits = 9)]
    setkey(LDOF_DT1, "id")
    LDOF_DT2 <- as.data.table(LDOF_DT1$LDOF_normalized_rank)
    setnames(LDOF_DT2, 1, "LDOF")
  } else{
    LDOF_DT2 <- NULL
  }
  
  
  if("ODIN" %in% DT[, unique(OD)]){
    ODINs <- DT[OD == "ODIN", selected_columns]
    sampleODINs <- sample(x = ODINs, size = percentage_OD * length(ODINs), replace = F)
    DToutliersODINs <- DToutliers1[, .SD, .SDcols = sampleODINs]
    sampleODIN <- sample(x = ODINs, size = mixed_view_features, replace = F)
    
    # Rank Normalization
    ODIN_DT <- DToutliers1[, .SD, .SDcols = c(sampleODIN)]
    ODIN_DT[, id:= 1:.N]
    setnames(ODIN_DT, 1, "ODIN")
    # The lower the scores the higher the chance to be an outlier. 
    # That is why we order with decreasing = F
    ODIN_DT1 <- copy(ODIN_DT[order(ODIN, decreasing = F)])
    ODIN_DT1[, rank:= 1:.N]
    ODIN_DT1[, ODIN_normalized_rank:= round((rank-1)/(.N-1), digits = 9)]
    setkey(ODIN_DT1, "id")
    ODIN_DT2 <- as.data.table(ODIN_DT1$ODIN_normalized_rank)
    setnames(ODIN_DT2, 1, "ODIN")
  } else{
    ODIN_DT2 <- NULL
  }
  
  if("FastABOD" %in% DT[, unique(OD)]){
    FastABODs <- DT[OD == "FastABOD", selected_columns]
    sampleFastABODs <- sample(x = FastABODs, size = percentage_OD * length(FastABODs), replace = F)
    DToutliersFastABODs <- DToutliers1[, .SD, .SDcols = sampleFastABODs]
    sampleFastABOD <- sample(x = FastABODs, size = mixed_view_features, replace = F)
    
    # Rank Normalization
    FastABOD_DT <- DToutliers1[, .SD, .SDcols = c(sampleFastABOD)]
    FastABOD_DT[, id:= 1:.N]
    setnames(FastABOD_DT, 1, "FastABOD")
    
    FastABOD_DT[, inv:= -log(FastABOD/max(FastABOD))]
    
    FastABOD_DT2 <- as.data.table(FastABOD_DT[, round((inv-min(inv))/ (max(inv) - min(inv)), digits = 9)])
    setnames(FastABOD_DT2, 1, "FastABOD") 
    
  } else{
    FastABOD_DT2 <- NULL
  }
  
  if("KDEOS" %in% DT[, unique(OD)]){
    KDEOSs <- DT[OD == "KDEOS", selected_columns]
    sampleKDEOSs <- sample(x = KDEOSs, size = percentage_OD * length(KDEOSs), replace = F)
    DToutliersKDEOSs <- DToutliers1[, .SD, .SDcols = sampleKDEOSs]
    sampleKDEOS <- sample(x = KDEOSs, size = mixed_view_features, replace = F)
    
    # Normalized by default
    KDEOS_DT2 <- DToutliers1[, .SD, .SDcols = c(sampleKDEOS)]
    setnames(KDEOS_DT2, 1, "KDEOS")
    
  } else{
    KDEOS_DT2 <- NULL
  }
  
  
  if("LDF" %in% DT[, unique(OD)]){
    LDFs <- DT[OD == "LDF", selected_columns]
    sampleLDFs <- sample(x = LDFs, size = percentage_OD * length(LDFs), replace = F)
    DToutliersLDFs <- DToutliers1[, .SD, .SDcols = sampleLDFs]
    sampleLDF <- sample(x = LDFs, size = mixed_view_features, replace = F)
    
    # Rank Normalization: https://people.revoledu.com/kardi/tutorial/Similarity/Normalized-Rank.html
    LDF_DT <- DToutliers1[, .SD, .SDcols = c(sampleLDF)]
    LDF_DT[, id:= 1:.N]
    setnames(LDF_DT, 1, "LDF")
    LDF_DT1 <- copy(LDF_DT[order(LDF, decreasing = T)])
    LDF_DT1[, rank:= 1:.N]
    LDF_DT1[, LDF_normalized_rank:= round((rank-1)/(.N-1), digits = 9)]
    setkey(LDF_DT1, "id")
    LDF_DT2 <- as.data.table(LDF_DT1$LDF_normalized_rank)
    setnames(LDF_DT2, 1, "LDF")
    
  } else{
    LDF_DT2 <- NULL
  }
  
  if("INFLO" %in% DT[, unique(OD)]){
    INFLOs <- DT[OD == "INFLO", selected_columns]
    sampleINFLOs <- sample(x = INFLOs, size = percentage_OD * length(INFLOs), replace = F)
    DToutliersINFLOs <- DToutliers1[, .SD, .SDcols = sampleINFLOs]
    sampleINFLO <- sample(x = INFLOs, size = mixed_view_features, replace = F)
    
    # Rank Normalization: https://people.revoledu.com/kardi/tutorial/Similarity/Normalized-Rank.html
    INFLO_DT <- DToutliers1[, .SD, .SDcols = c(sampleINFLO)]
    INFLO_DT[, id:= 1:.N]
    setnames(INFLO_DT, 1, "INFLO")
    INFLO_DT1 <- copy(INFLO_DT[order(INFLO, decreasing = T)])
    INFLO_DT1[, rank:= 1:.N]
    INFLO_DT1[, INFLO_normalized_rank:= round((rank-1)/(.N-1), digits = 9)]
    setkey(INFLO_DT1, "id")
    INFLO_DT2 <- as.data.table(INFLO_DT1$INFLO_normalized_rank)
    setnames(INFLO_DT2, 1, "INFLO")
  } else{
    INFLO_DT2 <- NULL
  }
  
  
  
  if("COF" %in% DT[, unique(OD)]){
    COFs <- DT[OD == "COF", selected_columns]
    sampleCOFs <- sample(x = COFs, size = percentage_OD * length(COFs), replace = F)
    DToutliersCOFs <- DToutliers1[, .SD, .SDcols = sampleCOFs]
    sampleCOF <- sample(x = COFs, size = mixed_view_features, replace = F)
    # Rank Normalization: https://people.revoledu.com/kardi/tutorial/Similarity/Normalized-Rank.html
    COF_DT <- DToutliers1[, .SD, .SDcols = c(sampleCOF)]
    COF_DT[, id:= 1:.N]
    setnames(COF_DT, 1, "COF")
    COF_DT1 <- copy(COF_DT[order(COF, decreasing = T)])
    COF_DT1[, rank:= 1:.N]
    COF_DT1[, COF_normalized_rank:= round((rank-1)/(.N-1), digits = 9)]
    setkey(COF_DT1, "id")
    COF_DT2 <- as.data.table(COF_DT1$COF_normalized_rank)
    setnames(COF_DT2, 1, "COF")
  } else{
    COF_DT2 <- NULL
  }
  
  DToutliers_all <- dplyr::bind_cols(KNN_DT2, KNNW_DT2, LOF_DT2, SimplifiedLOF_DT2, 
                                     LoOP_DT2, LDOF_DT2, ODIN_DT2, FastABOD_DT2,
                                     KDEOS_DT2, LDF_DT2, INFLO_DT2, COF_DT2)
  
  
  
  
  return(mixed_arthur = DToutliers_all)
}


#DToutliers1 <- fread(paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".results.csv"))

# This function creates the data for the augmented, unsupervised and original feature space. 
# at each of the above we are going to perform 10fold CV at the One-Class framework using the OCSVM algorithm.
get_augmented_and_unsupervisedDT_temp <- function(given_datasetname, experiments = "OC_combined_CV") {
  
  # start2 <- Sys.time()
  # path_to_read <- config::get("path_to_read_datasets", 
  #                             file = config_file_path,
  #                             config = loaded_config_type)
  # path_to_save <- config::get("path_to_save_derived_datasets", 
  #                             file = config_file_path,
  #                             config = loaded_config_type)
  # 
  # if(experiments == "OC_combined"){
  #   folder_to_save <- config::get("OC_combined_experiments", 
  #                                 file = config_file_path,
  #                                 config = loaded_config_type)
  #   final_path_to_save <- paste0(paste0(path_to_save, folder_to_save))
  # }
  # if(experiments == "OC_combined_CV"){
  #   folder_to_save <- config::get("OC_CV_combined_experiments", 
  #                                 file = config_file_path,
  #                                 config = loaded_config_type)
  #   final_path_to_save <- paste0(paste0(path_to_save, folder_to_save))
  # }
  
  #DToriginal <- fread(paste0("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/", given_datasetname, ".csv"))
  DToriginal <- fread(paste0(path_to_read, "/", given_datasetname,".csv"))
  setnames(DToriginal, "outlier", "Label", skip_absent = T)
  DToriginal[, .N, by = Label]
  
  one_randomOD <- create_unsupervised_scoresDT_temp(given_datasetname, percentage_OD=1, mixed_view_features=1)
  dimension <- dim(one_randomOD)[2]
  
  if(length(which(one_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension] != 0))){
    tempDT <- data.table::transpose(as.data.table(one_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension]))
    tempDT[, cols1:=names(one_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension])]
    cols_to_delete <- tempDT[V1!=0, cols1]
    print(cols_to_delete)
    cols_to_keep <- setdiff(names(one_randomOD), cols_to_delete)
    
    DT <- one_randomOD[, .SD, .SDcols = cols_to_keep]
  }else{
    DT <- one_randomOD
  }
  combinedDT_1 <- dplyr::bind_cols(DToriginal, DT)
  
  if ("id1" %in% names(combinedDT_1)){
    combinedDT_1[, id1:= NULL]
  }
    
  
  one_randomOD[, `:=` (id = combinedDT_1$id, Label = combinedDT_1$Label)]
  return(list(augmented = combinedDT_1, 
              unsupervised = one_randomOD, 
              original = DToriginal))
}


get_res_dataset <- function(given_datasetname1, col_name) {
  
  list_auc <- list()
  for(i in 1:30){
    print(paste0("Iteration-", i))
    Stamps_temp <- get_augmented_and_unsupervisedDT_temp(given_datasetname = given_datasetname1)[["unsupervised"]]
    dim1 <- (dim(Stamps_temp)[2])-2
    auc_res <- copy(data.table::transpose(Stamps_temp[, lapply(.SD, function(x) pROC::auc(Label, x, quiet = T)), .SDcols = 1:dim1]))
    auc_res[, Iteration:= i]
    auc_res[, Algorithm:= names(Stamps_temp)[1:dim1]]
    list_auc[[i]] <- copy(auc_res)
  }
  
  resDT <- rbindlist(list_auc)
  resDT[, Iteration:= as.factor(Iteration)]
  mean_resDT <- resDT[, mean(V1), by = .(Algorithm)]
  mean_resDT[, dataset:= col_name]
  return(resDT)
}

get_results_DT_average <- function(given_datasetname_1, version_string, col_name) {
  
  if(version_string == "none"){
    version_string1 <- ""
    list_names <- as.list(c(paste0(given_datasetname_1, "_withoutdupl_norm_", version_string1, "v0", 1:9),
                            paste0(given_datasetname_1, "_withoutdupl_norm_", version_string1, "v10")))
    list_names_short <- as.list(c(paste0(given_datasetname_1, "_v0", 1:9), 
                                  paste0(given_datasetname_1, "_v10" )))
  } else if(version_string == "no_versions"){
    version_string1 <- ""
    list_names <- as.list(paste0(given_datasetname_1, "_withoutdupl_norm"))
    list_names_short <- as.list(given_datasetname_1)
  }  else{
    version_string1 <-  paste0(version_string, "_")
    list_names <- as.list(c(paste0(given_datasetname_1, "_withoutdupl_norm_", version_string1, "v0", 1:9),
                            paste0(given_datasetname_1, "_withoutdupl_norm_", version_string1, "v10")))
    list_names_short <- as.list(c(paste0(given_datasetname_1, "_v0", 1:9), 
                                  paste0(given_datasetname_1, "_v10" )))
    }
  

  res_auc <- rbindlist(pmap(list(list_names, list_names_short), get_res_dataset))
  res_auc[, col:= col_name]
  return(res_auc)
}



# DT_res <- get_results_DT_average(given_datasetname_1 = "PenDigits", version_string = "none", col_name = "PenDigits")
#DT_res <- get_results_DT_average(given_datasetname_1 = "Annthyroid", version_string = "02", col_name = "Annthyroid")

args <- commandArgs(TRUE)
arg1 <- args[1]
arg2 <- args[2]



DT_res <- get_results_DT_average(given_datasetname_1 = arg1, version_string = arg2, col_name = arg1)

fst::write.fst(DT_res, paste0(final_path_to_save, "ECML_exp/", 
                          arg1,"normalized_ranking_AUC.fst"), 100)









