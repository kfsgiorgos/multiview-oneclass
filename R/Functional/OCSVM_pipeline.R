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


# This function read a XXXXXresults.csv from the DAMI repository and 
# transforms it to a proper tabular csv file. Before we apply thid function, 
# we have to unzip the XXXX.results.csv file

# Examples:
# GetTabularOutlierScore(datasetname = "Ionosphere_withoutdupl_norm")
# GetTabularOutlierScore(datasetname = "Parkinson_withoutdupl_norm_10_v03")
GetTabularOutlierScore <- function(datasetname) {

  # fnames <- list.files(paste0("~/Downloads/", datasetname,  "/"))
  # text.loaded <- readtext::readtext(paste0("data/downloaded-data/", datasetname, ".results.csv"))
  text.loaded <- readtext::readtext(paste0("~/Downloads/", datasetname, ".results.csv"))
  
  
  
  list.columns <- list()
  for (i in 1:dim(text.loaded)[1]) {
    DTtext <- as.data.table(text.loaded$text[i])
    DTtext1 <- as.data.table(str_split(DTtext, " "))
    if(i == 1){
      setnames(DTtext1, "V1", "Label")
      DT <- DTtext1[-1] 
      list.columns[[i]] <- DT
    } else{
      setnames(DTtext1, "V1", DTtext1$V1[1])
      DT <- DTtext1[-1] 
      list.columns[[i]] <- DT
    }
    DTtabular <- dplyr::bind_cols(list.columns)
    
    
    # fwrite(DTtabular, paste0("data/derived-data/", datasetname, ".results.csv"), nThread = 2)
    fwrite(DTtabular, paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".results.csv"), nThread = 20)
    
  }
}


# This function creates .csv from .arff when you have downloaded already the.arff 
# dataset that you want to analyze from specific directory

# Examples
# GetCsvFromArff(datasetname = "Ionosphere_withoutdupl_norm")
# GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v01")
GetCsvFromArff <- function(datasetname) {
  
  # DT <- as.data.table(foreign::read.arff(paste0("data/downloaded-data/", datasetname, ".arff")))
  DT<- as.data.table(foreign::read.arff(paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".arff")))
  setnames(DT, old = "outlier", "Label")
  # fwrite(DT, paste0("data/derived-data/", datasetname, ".csv"), nThread = 2)
  fwrite(DT, paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".csv"), nThread = 20)
  
}



# This function read the csv file of the corresponding datasetname and outputs 
# a dataset with 12 columns for 12 outlier detection algorithms. 
create_unsupervised_scoresDT <- function(datasetname, percentage_OD, mixed_view_features) {
  
  
  # DToutliers1 <- fread(paste0("data/derived-data/", datasetname, ".results.csv"))
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
    KNN_DT1[, KNN_normalized_rank:= (rank-1)/(.N-1)]
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
    KNNW_DT1[, KNNW_normalized_rank:= (rank-1)/(.N-1)]
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
    LOF_DT1[, LOF_normalized_rank:= (rank-1)/(.N-1)]
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
    SimplifiedLOF_DT1[, SimplifiedLOF_normalized_rank:= (rank-1)/(.N-1)]
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
    LDOF_DT1[, LDOF_normalized_rank:= (rank-1)/(.N-1)]
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
    ODIN_DT1[, ODIN_normalized_rank:= (rank-1)/(.N-1)]
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
    
    FastABOD_DT2 <- as.data.table(FastABOD_DT[, (inv-min(inv))/ (max(inv) - min(inv))])
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
    LDF_DT1[, LDF_normalized_rank:= (rank-1)/(.N-1)]
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
    INFLO_DT1[, INFLO_normalized_rank:= (rank-1)/(.N-1)]
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
    COF_DT1[, COF_normalized_rank:= (rank-1)/(.N-1)]
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

# This function creates the training and testing dataset for a 10fold 
# cross-validation in the One-Class framework. We split the datset to 80%-20% 
# and inside the 80% we do 10fold CV. 

# When we run this function N times we get N different splits of 80%-20%
get_10folds_id_positive_scenario <- function(given_datasetname, experiments = "OC_combined_CV", iterations) {
  
  
  start2 <- Sys.time()
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
  
  
  DToriginal <- fread(paste0(path_to_read, "/", given_datasetname,".csv"))
  setnames(DToriginal, "outlier", "Label", skip_absent = T)
  DToriginal[, .N, by = Label]
  
  
  # split 80% - 20% & Cross Validation
  
  # DT_split <- rsample::initial_split(data = DToriginal, prop = 0.8, strata = "Label") 
  # traini1 <- training(DT_split)
  # test1 <- testing(DT_split)
  # traini1[, .N, by = Label]
  # test1[, .N, by = Label]
  # DToriginal[, .N, by = Label]
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:iterations){
    # We repeat the following process iterations times to split the datasets to 
    # training & test but we demend the test set to have at least 2 outlier examples
    DT_split <- rsample::initial_split(data = DToriginal, prop = 0.8, strata = "Label") 
    trainDT <- rsample::training(DT_split)
    testDT <- rsample::testing(DT_split)
    
    if(testDT[Label == "yes", .N] < 2){
      list_train_id[[i]] <- NULL
      list_test_id[[i]] <- NULL
    } else{
      list_train_id[[i]] <- trainDT[, id]
      list_test_id[[i]] <- testDT[, id]
    }
  }
  
  
  list_train_id <- list_train_id[!sapply(list_train_id, is.null)]
  list_test_id <- list_test_id[!sapply(list_test_id, is.null)]
  
  
  train_data <- DToriginal[id %in% list_train_id[[1]]]
  print("train data")
  print(train_data)
  train_data[, .N, by = Label]
  
  test_data <- DToriginal[id %in% list_test_id[[1]]]
  test_data[, .N, by = Label]
  
  
  # We divide the 80% training data to 10folds. For the training folds 1:9 we have to exclude all the outliers. 
  # Inside the testing fold 10 we must have all the outliers that exist to folds 1:9. 
  # We do the same for the training folds c(1:8 & 10) and testing fold 9....
  Folds <- rsample::vfold_cv(train_data, v = 10, repeats = 1, strata = 'Label')
  
  Fold1 <- as.data.table(Folds$splits$`1`, data = "assessment")
  Fold2 <- as.data.table(Folds$splits$`2`, data = "assessment")
  Fold3 <- as.data.table(Folds$splits$`3`, data = "assessment")
  Fold4 <- as.data.table(Folds$splits$`4`, data = "assessment")
  Fold5 <- as.data.table(Folds$splits$`5`, data = "assessment")
  Fold6 <- as.data.table(Folds$splits$`6`, data = "assessment")
  Fold7 <- as.data.table(Folds$splits$`7`, data = "assessment")
  Fold8 <- as.data.table(Folds$splits$`8`, data = "assessment")
  Fold9 <- as.data.table(Folds$splits$`9`, data = "assessment")
  Fold10 <- as.data.table(Folds$splits$`10`, data = "assessment")
  
  chunk1 <- Fold1[, id]
  chunk2 <- Fold2[, id]
  chunk3 <- Fold3[, id]
  chunk4 <- Fold4[, id]
  chunk5 <- Fold5[, id]
  chunk6 <- Fold6[, id]
  chunk7 <- Fold7[, id]
  chunk8 <- Fold8[, id]
  chunk9 <- Fold9[, id]
  chunk10 <- Fold10[, id]
  
  list_chunks <- list(chunk1, chunk2, chunk3, chunk4, chunk5, chunk6,
                      chunk7, chunk8, chunk9, chunk10)
  
  train_chunk1 <- unlist(list_chunks[-1])
  train_chunk1_1 <- train_data[id %in% train_chunk1][Label == "no", id]
  test_chunk1 <- list_chunks[[1]]
  add_outliers_id_1 <- setdiff(train_data[Label == "yes", id], 
                               train_data[id %in% test_chunk1][Label == "yes", id])
  test_chunk1_1 <- c(test_chunk1, add_outliers_id_1)
  
  
  train_chunk2 <- unlist(list_chunks[-2])
  train_chunk2_1 <- train_data[id %in% train_chunk2][Label == "no", id]
  test_chunk2 <- list_chunks[[2]]
  add_outliers_id_2 <- setdiff(train_data[Label == "yes", id], 
                               train_data[id %in% test_chunk2][Label == "yes", id])
  test_chunk2_1 <- c(test_chunk2, add_outliers_id_2)
  
  train_chunk3 <- unlist(list_chunks[-3])
  train_chunk3_1 <- train_data[id %in% train_chunk3][Label == "no", id]
  test_chunk3 <- list_chunks[[3]]
  add_outliers_id_3 <- setdiff(train_data[Label == "yes", id], 
                               train_data[id %in% test_chunk3][Label == "yes", id])
  test_chunk3_1 <- c(test_chunk3, add_outliers_id_3)
  
  
  train_chunk4 <- unlist(list_chunks[-4])
  train_chunk4_1 <- train_data[id %in% train_chunk4][Label == "no", id]
  test_chunk4 <- list_chunks[[4]]
  add_outliers_id_4 <- setdiff(train_data[Label == "yes", id], 
                               train_data[id %in% test_chunk4][Label == "yes", id])
  test_chunk4_1 <- c(test_chunk4, add_outliers_id_4)
  
  
  train_chunk5 <- unlist(list_chunks[-5])
  train_chunk5_1 <- train_data[id %in% train_chunk5][Label == "no", id]
  test_chunk5 <- list_chunks[[5]]
  add_outliers_id_5 <- setdiff(train_data[Label == "yes", id], 
                               train_data[id %in% test_chunk5][Label == "yes", id])
  test_chunk5_1 <- c(test_chunk5, add_outliers_id_5)
  
  
  train_chunk6 <- unlist(list_chunks[-6])
  train_chunk6_1 <- train_data[id %in% train_chunk6][Label == "no", id]
  test_chunk6 <- list_chunks[[6]]
  add_outliers_id_6 <- setdiff(train_data[Label == "yes", id], 
                               train_data[id %in% test_chunk6][Label == "yes", id])
  test_chunk6_1 <- c(test_chunk6, add_outliers_id_6)
  
  
  train_chunk7 <- unlist(list_chunks[-7])
  train_chunk7_1 <- train_data[id %in% train_chunk7][Label == "no", id]
  test_chunk7 <- list_chunks[[7]]
  add_outliers_id_7 <- setdiff(train_data[Label == "yes", id], 
                               train_data[id %in% test_chunk7][Label == "yes", id])
  test_chunk7_1 <- c(test_chunk7, add_outliers_id_7)
  
  
  train_chunk8 <- unlist(list_chunks[-8])
  train_chunk8_1 <- train_data[id %in% train_chunk8][Label == "no", id]
  test_chunk8 <- list_chunks[[8]]
  add_outliers_id_8 <- setdiff(train_data[Label == "yes", id], 
                               train_data[id %in% test_chunk8][Label == "yes", id])
  test_chunk8_1 <- c(test_chunk8, add_outliers_id_8)
  
  
  train_chunk9 <- unlist(list_chunks[-9])
  train_chunk9_1 <- train_data[id %in% train_chunk9][Label == "no", id]
  test_chunk9 <- list_chunks[[9]]
  add_outliers_id_9 <- setdiff(train_data[Label == "yes", id], 
                               train_data[id %in% test_chunk9][Label == "yes", id])
  test_chunk9_1 <- c(test_chunk9, add_outliers_id_9)
  
  
  train_chunk10 <- unlist(list_chunks[-10])
  train_chunk10_1 <- train_data[id %in% train_chunk10][Label == "no", id]
  test_chunk10 <- list_chunks[[10]]
  # These are the outliers that do not belong to test_chunk10. We want to add to 
  # the test_chunk10 all the outliers that exist in the Folds 1-9. 
  add_outliers_id_10 <- setdiff(train_data[Label == "yes", id], 
                                train_data[id %in% test_chunk10][Label == "yes", id])
  test_chunk10_1 <- c(test_chunk10, add_outliers_id_10)
  
  
  list_train_chunks <- list(train_chunk1_1, train_chunk2_1,train_chunk3_1,train_chunk4_1,
                            train_chunk5_1,train_chunk6_1,train_chunk7_1,train_chunk8_1,
                            train_chunk9_1,train_chunk10_1)
  
  list_test_chunks <- list(test_chunk1_1, test_chunk2_1,test_chunk3_1,test_chunk4_1,
                           test_chunk5_1,test_chunk6_1,test_chunk7_1,test_chunk8_1,
                           test_chunk9_1,test_chunk10_1)  
  
  #just to evaluate that training folds do not contain outliers but the testing folds contain. 
  print("Training folds")
  for(i in 1:10){
    print(DToriginal[id %in% list_train_chunks[[i]] & Label == "yes", .N])
  }
  print("Testing folds")
  for(i in 1:10){
    print(DToriginal[id %in% list_test_chunks[[i]] & Label == "yes", .N])
  }
  
  
  # get_train_representations <- function(representationDT) {
  #   
  #   randomOD_train <- list()
  #   for(i in 1:10){
  #     randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
  #   }
  #   return(randomOD_train)
  # }
  # 
  # get_testCV_representations <- function(representationDT) {
  #   
  #   randomOD_testCV <- list()
  #   for(i in 1:10){
  #     randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
  #   }
  #   return(randomOD_testCV)
  # }
  return(list(list_train_folds = list_train_chunks, 
              list_test_folds = list_test_chunks, 
              hold_out_data = test_data, training_data = train_data))
}

# This function creates the data for the augmented, unsupervised and original feature space. 
# at each of the above we are going to perform 10fold CV at the One-Class framework using the OCSVM algorithm.
get_augmented_and_unsupervisedDT <- function(given_datasetname, experiments = "OC_combined_CV") {
  
  start2 <- Sys.time()
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
  
  
  DToriginal <- fread(paste0(path_to_read, "/", given_datasetname,".csv"))
  setnames(DToriginal, "outlier", "Label", skip_absent = T)
  DToriginal[, .N, by = Label]
  
  one_randomOD <- create_unsupervised_scoresDT(given_datasetname, percentage_OD=1, mixed_view_features=1)
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
  one_randomOD[, `:=` (id = combinedDT_1$id, Label = combinedDT_1$Label)]
  return(list(augmented = combinedDT_1, 
              unsupervised = one_randomOD, 
              original = DToriginal))
}

# This function trains the OCSVM on augmented spaces. 
# "number_of_representations" is the total number of augmented representations we want to apply the OCSVM. 
# for instance if number_of_representations = 8 it means that:
# We create 8 different augmented spaces(each one calls the function random create_unsupervised_scoresDT to augment) 
# and on each of them we train the OCSVM. 
# The output is a data.table with the OCSVM scores on each of the 8 augmented saces. 

run_OCSVM_augmented_representation <- function(datasetname, given_folds, number_of_representations) {
  
  
  folds_id <- given_folds
  
  res_holdout_representations <- list()
  for(ii in 1:number_of_representations){
    augmentedDT <- data.table::copy(get_augmented_and_unsupervisedDT(given_datasetname  = datasetname)[["augmented"]])
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <-  data.table::copy(augmentedDT[id %in% folds_id$list_train_folds[[ij]] ])
      CVtest_DT <- data.table::copy(augmentedDT[id %in% folds_id$list_test_folds[[ij]] ])
      
      CVtest_id <- CVtest_DT$id
      CVtest_Label <- CVtest_DT$Label
      
      trainDT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      
      for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
        for(gammas in c("scale", "auto")){
          iters <- iters+1
          
          scores_CV <- calculate_OCSVM_params(DTtrain = trainDT, DTtest = CVtest_DT, 
                                              given_nu = nus, given_kernel = "rbf", given_gamma = gammas)
          DT_CV <- data.table(scores = scores_CV, 
                              id = CVtest_id,
                              Label = CVtest_Label)
          DT_CV[, Kfold:=ij]
          DT_CV[, gamma := gammas]
          DT_CV[, nu := nus]
          res_CV[[iters]] <- DT_CV
        }
      }
    }
    augmented_resDT <- rbindlist(res_CV)
    augmented_resDT[, Model:= paste0(gamma, "_", nu)]
    
    aucCV <- augmented_resDT[, pROC::auc(Label, scores, quiet = T), by = .(Kfold, Model)]
    
    mean_aucCV <- aucCV[, mean(V1), by = .(Model)]
    best_hyper_CV <- mean_aucCV[, .SD[which.max(V1)]]
    
    # Train with the best hyperparameter all the 80% data
    best_hyper <- best_hyper_CV[, stringr::str_split(Model, pattern = "_")[[1]]]
    
    holdoutDT <- data.table::copy(folds_id$hold_out_data)
    holdout_ids_labels <- holdoutDT[, .(id, Label)]
    
    trainingDT <- data.table::copy(folds_id$training_data)
    
    trainingDT[, `:=` (id = NULL, Label = NULL)]
    holdoutDT[, `:=` (id = NULL, Label = NULL)]
    
    scores_holdout <- calculate_OCSVM_params(DTtrain = trainingDT, DTtest = holdoutDT, 
                                             given_kernel = "rbf", 
                                             given_gamma = best_hyper[[1]],
                                             given_nu = as.numeric(best_hyper[[2]]))
    
    holdoutDT_1 <- data.table(scores = scores_holdout, 
                              Label = holdout_ids_labels$Label, 
                              id = holdout_ids_labels$id)
    holdoutDT_1[scores >=0, Outlier:=0]
    holdoutDT_1[scores <0, Outlier:=1]
    holdoutDT_1[, Representation:= paste0("Augmented_",ii)]
    res_holdout_representations[[ii]] <- holdoutDT_1
    
  }
  all_representationsDT_1 <- rbindlist(res_holdout_representations)
  gg2 <- all_representationsDT_1[, sum(Outlier), by=.(id)]
  gg2[, Label:= all_representationsDT_1[Representation=="Augmented_1", Label]]
  gg2[V1>0, Outlier:= 1]
  gg2[V1==0, Outlier:= 0]
  gg2[, .N, by = .(Label, Outlier)]
  return(all_representationsDT = all_representationsDT_1)
}

# The same as the above function but this function os for the Unsupervised representations
run_OCSVM_unsupervised_representation <- function(datasetname, given_folds, number_of_representations) {
  
  
  folds_id <- given_folds
  
  res_holdout_representations <- list()
  for(ii in 1:number_of_representations){
    unsupervisedDT <- data.table::copy(get_augmented_and_unsupervisedDT(given_datasetname  = datasetname)[["unsupervised"]])
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <-  data.table::copy(unsupervisedDT[id %in% folds_id$list_train_folds[[ij]] ])
      CVtest_DT <- data.table::copy(unsupervisedDT[id %in% folds_id$list_test_folds[[ij]] ])
      
      CVtest_id <- CVtest_DT$id
      CVtest_Label <- CVtest_DT$Label
      
      trainDT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      
      for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
        for(gammas in c("scale", "auto")){
          iters <- iters+1
          
          scores_CV <- calculate_OCSVM_params(DTtrain = trainDT, DTtest = CVtest_DT, 
                                              given_nu = nus, given_kernel = "rbf", given_gamma = gammas)
          DT_CV <- data.table(scores = scores_CV, 
                              id = CVtest_id,
                              Label = CVtest_Label)
          DT_CV[, Kfold:=ij]
          DT_CV[, gamma := gammas]
          DT_CV[, nu := nus]
          res_CV[[iters]] <- DT_CV
        }
      }
    }
    unsupervised_resDT <- rbindlist(res_CV)
    unsupervised_resDT[, Model:= paste0(gamma, "_", nu)]
    
    aucCV <- unsupervised_resDT[, pROC::auc(Label, scores, quiet = T), by = .(Kfold, Model)]
    
    mean_aucCV <- aucCV[, mean(V1), by = .(Model)]
    best_hyper_CV <- mean_aucCV[, .SD[which.max(V1)]]
    
    # Train with the best hyperparameter all the 80% data
    best_hyper <- best_hyper_CV[, stringr::str_split(Model, pattern = "_")[[1]]]
    
    holdoutDT <- data.table::copy(folds_id$hold_out_data)
    holdout_ids_labels <- holdoutDT[, .(id, Label)]
    
    trainingDT <- data.table::copy(folds_id$training_data)
    
    trainingDT[, `:=` (id = NULL, Label = NULL)]
    holdoutDT[, `:=` (id = NULL, Label = NULL)]
    
    scores_holdout <- calculate_OCSVM_params(DTtrain = trainingDT, DTtest = holdoutDT, 
                                             given_kernel = "rbf", 
                                             given_gamma = best_hyper[[1]],
                                             given_nu = as.numeric(best_hyper[[2]]))
    
    holdoutDT_1 <- data.table(scores = scores_holdout, 
                              Label = holdout_ids_labels$Label, 
                              id = holdout_ids_labels$id)
    holdoutDT_1[scores >=0, Outlier:=0]
    holdoutDT_1[scores <0, Outlier:=1]
    holdoutDT_1[, Representation:= paste0("Unsupervised_", ii)]
    res_holdout_representations[[ii]] <- holdoutDT_1
    
  }
  all_representationsDT <- rbindlist(res_holdout_representations)
  gg1 <- all_representationsDT[, sum(Outlier), by=.(id)]
  gg1[, Label:= all_representationsDT[Representation=="Unsupervised_1", Label]]
  gg1[V1>0, Outlier:= 1]
  gg1[V1==0, Outlier:= 0]
  gg1[, .N, by = .(Label, Outlier)]
  return(all_representationsDT = all_representationsDT)
}

# This function trains the OCSVM on the the original space. 
# Keep in mind that we do not need the input "number_of_representations"
run_OCSVM_original_representation <- function(datasetname, given_folds) {
  
  folds_id <- given_folds
  
  
  originalDT <- data.table::copy(get_augmented_and_unsupervisedDT(given_datasetname  = datasetname)[["original"]])
  iters <- 0
  res_CV <- list()
  for(ij in 1:10){
    
    trainDT <-  data.table::copy(originalDT[id %in% folds_id$list_train_folds[[ij]] ])
    CVtest_DT <- data.table::copy(originalDT[id %in% folds_id$list_test_folds[[ij]] ])
    
    CVtest_id <- CVtest_DT$id
    CVtest_Label <- CVtest_DT$Label
    
    trainDT[, `:=` (id = NULL, Label = NULL)]
    CVtest_DT[, `:=` (id = NULL, Label = NULL)]
    
    
    for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
      for(gammas in c("scale", "auto")){
        iters <- iters+1
        
        scores_CV <- calculate_OCSVM_params(DTtrain = trainDT, DTtest = CVtest_DT, 
                                            given_nu = nus, given_kernel = "rbf", given_gamma = gammas)
        DT_CV <- data.table(scores = scores_CV, 
                            id = CVtest_id,
                            Label = CVtest_Label)
        DT_CV[, Kfold:=ij]
        DT_CV[, gamma := gammas]
        DT_CV[, nu := nus]
        res_CV[[iters]] <- DT_CV
      }
    }
  }
  original_resDT <- rbindlist(res_CV)
  original_resDT[, Model:= paste0(gamma, "_", nu)]
  
  aucCV <- original_resDT[, pROC::auc(Label, scores, quiet = T), by = .(Kfold, Model)]
  
  mean_aucCV <- aucCV[, mean(V1), by = .(Model)]
  best_hyper_CV <- mean_aucCV[, .SD[which.max(V1)]]
  
  # Train with the best hyperparameter all the 80% data
  best_hyper <- best_hyper_CV[, stringr::str_split(Model, pattern = "_")[[1]]]
  
  holdoutDT <- data.table::copy(folds_id$hold_out_data)
  holdout_ids_labels <- holdoutDT[, .(id, Label)]
  
  trainingDT <- data.table::copy(folds_id$training_data)
  
  trainingDT[, `:=` (id = NULL, Label = NULL)]
  holdoutDT[, `:=` (id = NULL, Label = NULL)]
  
  scores_holdout <- calculate_OCSVM_params(DTtrain = trainingDT, DTtest = holdoutDT, 
                                           given_kernel = "rbf", 
                                           given_gamma = best_hyper[[1]],
                                           given_nu = as.numeric(best_hyper[[2]]))
  
  holdoutDT_1 <- data.table(scores = scores_holdout, 
                            Label = holdout_ids_labels$Label, 
                            id = holdout_ids_labels$id)
  holdoutDT_1[scores >=0, Outlier:=0]
  holdoutDT_1[scores <0, Outlier:=1]
  holdoutDT_1[, Representation:= "Original"]
  
  
  all_representationsDT1 <- holdoutDT_1
  all_representationsDT1[, .N, by=.(Label, Outlier)]
  return(all_representationsDT = all_representationsDT1)
  
  
}



# Example of the pipeline -------------------------------------------------

# Step 1) unzip the outlier scores CSV 
# Step 2) us the above unzipped CSV to run the function "GetTabularOutlierScore" 
# Step 3) run the function "GetCsvFromArff" to transform the original data to CSV file
get_pipeline_res <- function(iteration, input_datasetname, folds_iterations) {
  
  dataset_name <- input_datasetname
  list_new_repres <- list()
  list_original <- list()  
  folds_id <- get_10folds_id_positive_scenario(given_datasetname = dataset_name, iterations = folds_iterations)
  print(paste0("START: Iter-",i, ". Augmented Representation part"))
  augmented_res <- run_OCSVM_augmented_representation(datasetname = dataset_name, 
                                                      given_folds = folds_id, 
                                                      number_of_representations = 30)
  augmented_res[, Iteration:= iteration]
  augmented_res[, Representation1:= "Augmented"]
  # augmented_aucDT <- augmented_res[, pROC::auc(Label, scores, quiet = T), by = .(Representation)]
  # augmented_aucDT[, Representation:= "Augmented"]
  # gg2 <- augmented_res[, sum(Outlier), by=.(id)]
  # gg2[, Label:= augmented_res[Representation=="Augmented_1", Label]]
  # gg2[V1>7, Outlier:= 1]
  # gg2[V1<=7, Outlier:= 0]
  # gg2[, .N, by = .(Label, Outlier)]
  # 
  # cor(augmented_res[Representation=="Augmented_1", Outlier], augmented_res[Representation=="Augmented_15", Outlier]) 
  # temp <- augmented_res[Representation %in% c("Augmented_1", "Augmented_4")]
  # temp2 <- temp[, sum(Outlier), by = id]
  # temp2[V1==2, outlier:= 1]
  # temp2[V1<2, outlier:= 0]
  # temp2[, Label:= augmented_res[Representation=="Augmented_1", Label]]
  # temp2[, .N, by = .(Label, outlier)]
  # #1,4
  
  # augmented_aucDT <- augmented_res[, pROC::auc(Label, scores, quiet = T), by = .(Representation)]
  print(paste0("END: Iter-",i, ". Augmented Representation part"))
  
  print(paste0("START: Iter-",i, ". Unsupervised Representation part"))
  unsupervised_res <- run_OCSVM_unsupervised_representation(datasetname = dataset_name, 
                                                            given_folds = folds_id, 
                                                            number_of_representations = 30)
  unsupervised_res[, Iteration:= iteration]
  unsupervised_res[, Representation1:= "Unsupervised"]
  # unsupervised_aucDT <- unsupervised_res[, pROC::auc(Label, scores, quiet = T), by = .(Representation)]
  # unsupervised_aucDT[, Representation:= "Unsupervised"]
  # gg1 <- unsupervised_res[, sum(Outlier), by=.(id)]
  # gg1[, Label:= unsupervised_res[Representation=="Unsupervised_1", Label]]
  # gg1[V1>14, Outlier:= 1]
  # gg1[V1<=14, Outlier:= 0]
  # gg1[, .N, by = .(Label, Outlier)]
  # unsupervised_aucDT <- unsupervised_res[, pROC::auc(Label, scores, quiet = T), by = .(Representation)]
  
  
  # cor(unsupervised_res[Representation=="Unsupervised_1", Outlier], unsupervised_res[Representation=="Unsupervised_10", Outlier]) 
  print(paste0("END: Iter-",i, ". Unsupervised Representation part"))
  
  print(paste0("START: Iter-",i, ". Original Representation part"))
  original_res <- run_OCSVM_original_representation(datasetname = dataset_name, 
                                                    given_folds = folds_id)
  original_res[, Iteration:= iteration]
  original_res[, Representation1:= "Original"]
  # original_res[, .N, by = .(Label, Outlier)]
  # original_aucDT <- original_res[, pROC::auc(Label, scores, quiet = T)]                                                  
  # 
  # DT <- rbindlist(list(augmented_aucDT, unsupervised_aucDT))
  # DT[, Representation:= as.factor(Representation)]
  print(paste0("END: Iter-",i, ". Original Representation part"))
  
  print(paste0("Iter-", iteration))
  return(list(augmented_res, unsupervised_res, original_res))
  }

  
args <- commandArgs(TRUE)
arg1 <- args[1]
arg2 <- args[2]
arg3 <- args[3]
arg4 <- args[4]
arg5 <- args[5]

if(arg5 == "yes"){
  list_new_repres <- list()
  for( i in 1:arg3){
    list_new_repres[[i]] <- get_pipeline_res(iteration = i, input_datasetname = arg2, folds_iterations = arg4)
  }
  
  augmentedDT <- data.table::rbindlist(purrr::map(list_new_repres, 1)) 
  unsupervisedDT <- data.table::rbindlist(purrr::map(list_new_repres, 2))
  originalDT <- data.table::rbindlist(purrr::map(list_new_repres, 3))
  
  df <- data.table::rbindlist(list(augmentedDT, unsupervisedDT, originalDT))
  
  
  fst::write.fst(df, paste0(final_path_to_save, "ECML_exp/", 
                            arg1, "/", arg2, "_OCSVM_DT_all_repres_", 
                            arg3,"_iters.fst"), 100)
  
} else{
  # GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v04")
  # GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v05")
  # GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v06")
  # GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v07")
  # GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v08")
  # GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v09")
  # GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v10")
  
  GetTabularOutlierScore(datasetname = "WBC_withoutdupl_norm_v02")
  GetTabularOutlierScore(datasetname = "WBC_withoutdupl_norm_v03")
  GetTabularOutlierScore(datasetname = "WBC_withoutdupl_norm_v04")
  GetTabularOutlierScore(datasetname = "WBC_withoutdupl_norm_v06")
  GetTabularOutlierScore(datasetname = "WBC_withoutdupl_norm_v07")
  GetTabularOutlierScore(datasetname = "WBC_withoutdupl_norm_v08")
  GetTabularOutlierScore(datasetname = "WBC_withoutdupl_norm_v09")
  GetTabularOutlierScore(datasetname = "WBC_withoutdupl_norm_v10")
  
  GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v02")
  GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v03")
  GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v04")
  GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v05")
  GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v06")
  GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v08")
  GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v09")
  GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v10")
  
  

  # GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v04")
  # GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v05")
  # GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v06")
  # GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v07")
  # GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v08")
  # GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v09")
  # GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v10")
  
  GetCsvFromArff(datasetname = "WBC_withoutdupl_norm_v02")
  GetCsvFromArff(datasetname = "WBC_withoutdupl_norm_v03")
  GetCsvFromArff(datasetname = "WBC_withoutdupl_norm_v04")
  GetCsvFromArff(datasetname = "WBC_withoutdupl_norm_v06")
  GetCsvFromArff(datasetname = "WBC_withoutdupl_norm_v07")
  GetCsvFromArff(datasetname = "WBC_withoutdupl_norm_v08")
  GetCsvFromArff(datasetname = "WBC_withoutdupl_norm_v09")
  GetCsvFromArff(datasetname = "WBC_withoutdupl_norm_v10")
 
  GetCsvFromArff(datasetname = "WDBC_withoutdupl_norm_v02")
  GetCsvFromArff(datasetname = "WDBC_withoutdupl_norm_v03")
  GetCsvFromArff(datasetname = "WDBC_withoutdupl_norm_v04")
  GetCsvFromArff(datasetname = "WDBC_withoutdupl_norm_v05")
  GetCsvFromArff(datasetname = "WDBC_withoutdupl_norm_v06")
  GetCsvFromArff(datasetname = "WDBC_withoutdupl_norm_v08")
  GetCsvFromArff(datasetname = "WDBC_withoutdupl_norm_v09")
  GetCsvFromArff(datasetname = "WDBC_withoutdupl_norm_v10")
  
  
}



# fst::write.fst(df, "~/Desktop/dataset.fst", 100)
# data.table::fwrite(df, "~/Desktop/dataset.csv")

# cl <- makeCluster(2)
# registerDoParallel(cl)
# system.time(r <- foreach( k=1:2, .packages =  c("data.table","magrittr")) %dopar% get_pipeline_res(iteration = k, input_datasetname = dataset_name, folds_iterations = 20))
# stopCluster(cl)
# registerDoSEQ()
# print(r)
# print("END")
# code to plot mean auc values from each iteration
# augmented_meanAUC <- list()
# unsupervised_meanAUC <- list()
# originalDT <- list()
# for(j in 1:30){
#   
#   augmentedDT <- list_new_repres[[j]][[1]][, pROC::auc(Label, scores, quiet = T), by = .(Representation)]
#   augmentedDT[, Representation1:= "Augmented"]
#   augmented_meanAUC[[j]] <- augmentedDT[, mean(V1)]
#   unsupervisedDT <- list_new_repres[[j]][[2]][, pROC::auc(Label, scores, quiet = T), by = .(Representation)]
#   unsupervisedDT[, Representation1:= "Unsupervised"]
#   unsupervised_meanAUC[[j]] <- unsupervisedDT[, mean(V1)]
#   originalDT[[j]] <- list_new_repres[[j]][[3]][, pROC::auc(Label, scores, quiet = T)]
#   
# }
# 
# aug1 <- as.data.table(unlist(augmented_meanAUC))
# aug1[, Repre:= "Augmented"]
# unsuper1 <- as.data.table(unlist(unsupervised_meanAUC))
# unsuper1[, Repre:= "Unsupervised"]
# or1 <- as.data.table(unlist(originalDT))
# or1[, Repre:= "Original"]
# 
# DT_all <- rbindlist(list(aug1, unsuper1, or1))
# esquisse::esquisser()
# j <- 5
# augmentedDT <- list_new_repres[[j]][[1]][, pROC::auc(Label, scores, quiet = T), by = .(Representation)]
# augmentedDT[, Representation1:= "Augmented"]
# augmented_meanAUC <- augmentedDT[, mean(V1)]
# unsupervisedDT <- list_new_repres[[j]][[2]][, pROC::auc(Label, scores, quiet = T), by = .(Representation)]
# unsupervisedDT[, Representation1:= "Unsupervised"]
# unsupervised_meanAUC <- unsupervisedDT[, mean(V1)]
# originalDT <- list_new_repres[[j]][[3]][, pROC::auc(Label, scores, quiet = T)]
# 
# DT1 <- rbindlist(list(augmentedDT, unsupervisedDT))
# 
# 
# p <- ggplot(data = DT1) +
#   aes(x = Representation1, y = V1, fill = Representation1) +
#   geom_boxplot() +
#   theme_bw()+
#   geom_hline(yintercept = originalDT[[1]], color = "red")+
#   geom_text(aes( 0, originalDT[[1]], label = "Original", hjust=-1, vjust=-1), size = 3)+
#   geom_hline(yintercept = augmented_meanAUC, color = "green")+
#   geom_text(aes( 0, augmented_meanAUC, label = "Augmented", hjust=-1, vjust=-1), size = 3)+
#   geom_hline(yintercept = unsupervised_meanAUC, color = "yellow")+
#   geom_text(aes( 0, unsupervised_meanAUC, label = "Unsupervised", hjust=-1, vjust=-1), size = 3)+
#   ggtitle(label = paste0("Experiment Iteration: ", j))
# p



