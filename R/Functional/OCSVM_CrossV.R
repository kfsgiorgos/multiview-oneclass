create_unsupervised_view <- function(datasetname, percentage_OD, mixed_view_features) {
  
  path_to_save <- config::get("path_to_save_derived_datasets", 
                              file = config_file_path,
                              config = loaded_config_type)
  path_to_read <- config::get("path_to_read_datasets", 
                              file = config_file_path,
                              config = loaded_config_type)
  DToutliers1 <- fread(paste0(path_to_read, "/", datasetname, ".results.csv"))
  
  
  KNNs1 <- paste0("KNN-00", 1:9)
  KNNs2 <- paste0("KNN-0", 10:99)
  KNNs <- c(KNNs1, KNNs2, "KNN-100")
  sampleKNNs <- sample(x = KNNs, size = percentage_OD * length(KNNs), replace = F)
  DToutliersKNNs <- DToutliers1[, .SD, .SDcols = sampleKNNs]
  
  KNNWs1 <- paste0("KNNW-00", 1:9)
  KNNWs2 <- paste0("KNNW-0", 10:99)
  KNNWs <- c(KNNWs1, KNNWs2, "KNNW-100")
  sampleKNNWs <- sample(x = KNNWs, size = percentage_OD * length(KNNWs), replace = F)
  DToutliersKNNWs <- DToutliers1[, .SD, .SDcols = sampleKNNWs]
  
  LOFs1 <- paste0("LOF-00", 1:9)
  LOFs2 <- paste0("LOF-0", 10:99)
  LOFs <- c(LOFs1, LOFs2, "LOF-100")
  sampleLOFs <- sample(x = LOFs, size = percentage_OD * length(LOFs), replace = F)
  DToutliersLOFs <- DToutliers1[, .SD, .SDcols = sampleLOFs]
  
  SimplifiedLOFs1 <- paste0("SimplifiedLOF-00", 2:9)
  SimplifiedLOFs2 <- paste0("SimplifiedLOF-0", 10:99)
  SimplifiedLOFs <- c(SimplifiedLOFs1, SimplifiedLOFs2, "SimplifiedLOF-100")
  sampleSimplifiedLOFs <- sample(x = SimplifiedLOFs, size = percentage_OD * length(SimplifiedLOFs), replace = F)
  DToutliersSimplifiedLOFs <- DToutliers1[, .SD, .SDcols = sampleSimplifiedLOFs]
  
  LoOPs1 <- paste0("LoOP-00", 1:9)
  LoOPs2 <- paste0("LoOP-0", 10:99)
  LoOPs <- c(LoOPs1, LoOPs2, "LoOP-100")
  sampleLoOPs <- sample(x = LoOPs, size = percentage_OD * length(LoOPs), replace = F)
  DToutliersLoOPs <- DToutliers1[, .SD, .SDcols = sampleLoOPs]
  
  LDOFs1 <- paste0("LDOF-00", 2:9)
  LDOFs2 <- paste0("LDOF-0", 10:99)
  LDOFs <- c(LDOFs1, LDOFs2, "LDOF-100")
  sampleLDOFs <- sample(x = LDOFs, size = percentage_OD * length(LDOFs), replace = F)
  DToutliersLDOFs <- DToutliers1[, .SD, .SDcols = sampleLDOFs]
  
  ODINs1 <- paste0("ODIN-00", 1:9)
  ODINs2 <- paste0("ODIN-0", 10:99)
  ODINs <- c(ODINs1, ODINs2, "ODIN-100")
  sampleODINs <- sample(x = ODINs, size = percentage_OD * length(ODINs), replace = F)
  DToutliersODINs <- DToutliers1[, .SD, .SDcols = sampleODINs]
  
  FastABODs1 <- paste0("FastABOD-00", 3:9)
  FastABODs2 <- paste0("FastABOD-0", 10:99)
  FastABODs <- c(FastABODs1, FastABODs2, "FastABOD-100")
  sampleFastABODs <- sample(x = FastABODs, size = percentage_OD * length(FastABODs), replace = F)
  DToutliersFastABODs <- DToutliers1[, .SD, .SDcols = sampleFastABODs]
  
  KDEOSs1 <- paste0("KDEOS-00", 2:9)
  KDEOSs2 <- paste0("KDEOS-0", 10:99)
  KDEOSs <- c(KDEOSs1, KDEOSs2, "KDEOS-100")
  sampleKDEOSs <- sample(x = KDEOSs, size = percentage_OD * length(KDEOSs), replace = F)
  DToutliersKDEOSs <- DToutliers1[, .SD, .SDcols = sampleKDEOSs]
  
  LDFs1 <- paste0("LDF-00", 1:9)
  LDFs2 <- paste0("LDF-0", 10:99)
  LDFs <- c(LDFs1, LDFs2, "LDF-100")
  sampleLDFs <- sample(x = LDFs, size = percentage_OD * length(LDFs), replace = F)
  DToutliersLDFs <- DToutliers1[, .SD, .SDcols = sampleLDFs]
  
  INFLOs1 <- paste0("INFLO-00", 1:9)
  INFLOs2 <- paste0("INFLO-0", 10:99)
  INFLOs <- c(INFLOs1, INFLOs2, "INFLO-100")
  sampleINFLOs <- sample(x = INFLOs, size = percentage_OD * length(INFLOs), replace = F)
  DToutliersINFLOs <- DToutliers1[, .SD, .SDcols = sampleINFLOs]
  
  COFs1 <- paste0("COF-00", 2:9)
  COFs2 <- paste0("COF-0", 10:99)
  COFs <- c(COFs1, COFs2, "COF-100")
  sampleCOFs <- sample(x = COFs, size = percentage_OD * length(COFs), replace = F)
  DToutliersCOFs <- DToutliers1[, .SD, .SDcols = sampleCOFs]
  
  
  
  DToutliers_all <- DToutliers1[, .SD, .SDcols = c(sample(x = KNNs, size = mixed_view_features, replace = F),
                                                   sample(x = KNNWs, size = mixed_view_features, replace = F),
                                                   sample(x = LOFs, size = mixed_view_features, replace = F),
                                                   sample(x = SimplifiedLOFs, size = mixed_view_features, replace = F),
                                                   sample(x = LoOPs, size = mixed_view_features, replace = F),
                                                   sample(x = LDOFs, size = mixed_view_features, replace = F),
                                                   sample(x = ODINs, size = mixed_view_features, replace = F),
                                                   sample(x = FastABODs, size = mixed_view_features, replace = F),
                                                   sample(x = KDEOSs, size = mixed_view_features, replace = F),
                                                   sample(x = LDFs, size = mixed_view_features, replace = F),
                                                   sample(x = INFLOs, size = mixed_view_features, replace = F),
                                                   sample(x = COFs, size = mixed_view_features, replace = F))]
  
  
  
  random_sample <- round((percentage_OD * 100)/12, digits = 0)
  
  
  DToutliers_all1 <- dplyr::bind_cols(DToutliersKNNs[, .SD, .SDcols = sample(names(DToutliersKNNs), size = random_sample, replace = F)],
                                      DToutliersKNNWs[, .SD, .SDcols = sample(names(DToutliersKNNWs), size = random_sample, replace = F)],
                                      DToutliersLOFs[, .SD, .SDcols = sample(names(DToutliersLOFs), size = random_sample, replace = F)],
                                      DToutliersSimplifiedLOFs[, .SD, .SDcols = sample(names(DToutliersSimplifiedLOFs), size= random_sample, replace = F)],
                                      DToutliersLoOPs[, .SD, .SDcols = sample(names(DToutliersLoOPs), size = random_sample, replace = F)],
                                      DToutliersLDOFs[, .SD, .SDcols = sample(names(DToutliersLDOFs), size = random_sample, replace = F)],
                                      DToutliersODINs[, .SD, .SDcols = sample(names(DToutliersODINs), size = random_sample, replace = F)],
                                      DToutliersFastABODs[, .SD, .SDcols = sample(names(DToutliersFastABODs), size = random_sample, replace = F)],
                                      DToutliersKDEOSs[, .SD, .SDcols = sample(names(DToutliersKDEOSs), size = random_sample, replace = F)],
                                      DToutliersLDFs[, .SD, .SDcols = sample(names(DToutliersLDFs), size = random_sample, replace = F)],
                                      DToutliersINFLOs[, .SD, .SDcols = sample(names(DToutliersINFLOs), size = random_sample, replace = F)],
                                      DToutliersCOFs[, .SD, .SDcols = sample(names(DToutliersCOFs), size = random_sample, replace = F)]
                                      
  )
  
  
  DToutliers_all <- DToutliers1[, .SD, .SDcols = c(sample(x = KNNs, size = mixed_view_features, replace = F),
                                                   sample(x = KNNWs, size = mixed_view_features, replace = F),
                                                   sample(x = LOFs, size = mixed_view_features, replace = F),
                                                   sample(x = SimplifiedLOFs, size = mixed_view_features, replace = F),
                                                   sample(x = LoOPs, size = mixed_view_features, replace = F),
                                                   sample(x = LDOFs, size = mixed_view_features, replace = F),
                                                   sample(x = ODINs, size = mixed_view_features, replace = F),
                                                   sample(x = FastABODs, size = mixed_view_features, replace = F),
                                                   sample(x = KDEOSs, size = mixed_view_features, replace = F),
                                                   sample(x = LDFs, size = mixed_view_features, replace = F),
                                                   sample(x = INFLOs, size = mixed_view_features, replace = F),
                                                   sample(x = COFs, size = mixed_view_features, replace = F))]
  

  
  DToutliers_all_normalized <- DToutliers_all[, lapply(.SD, function(x) (x - mean(x))/sd(x))]
  
  return(list(KNNs = DToutliersKNNs, KNNWs = DToutliersKNNWs,
              LOFs = DToutliersLOFs, SimplifiedLOFs = DToutliersSimplifiedLOFs,
              LoOPs = DToutliersLoOPs, LDOFs = DToutliersLDOFs,
              ODINs = DToutliersODINs, FastABODs = DToutliersFastABODs, 
              KDEOSs = DToutliersKDEOSs, LDFs = DToutliersLDFs, 
              INFLOs = DToutliersINFLOs, COFs = DToutliersCOFs, 
              mixed_arthur = DToutliers_all,
              mixed_random_percentage = DToutliers_all1,
              mixed_arthur_normalized = DToutliers_all_normalized))
}



get_CV_experiments <- function(datasetname, subfolder_name, experiments = "OC_combined_CV", CViterations) {
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
  
  DToriginal <- fread(paste0(path_to_read, "/", datasetname,".csv"))
  setnames(DToriginal, "outlier", "Label", skip_absent = T)
  DToriginal[, .N, by = Label]
  
  list_combined_1 <- list()
  list_combined_2 <- list()
  list_one_randomOD <- list()
  list_two_randomOD <- list()
  for(i in 1:21){
    unsupervised_DTs1 <- create_unsupervised_view(datasetname, percentage_OD=1, mixed_view_features=1)
    one_randomOD <- unsupervised_DTs1$mixed_arthur
    dimension <- dim(one_randomOD)[2]
    
    if(length(which(one_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension] != 0))){
      tempDT <- data.table::transpose(as.data.table(one_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension]))
      tempDT[, cols1:=names(one_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension])]
      cols_to_delete <- tempDT[V1!=0, cols1]
      print(cols_to_delete)
      cols_to_keep <- setdiff(names(one_randomOD), cols_to_delete)
      
      DT <- one_randomOD[, .SD, .SDcols = cols_to_keep]
    }else{DT <- one_randomOD}
    combinedDT_1 <- dplyr::bind_cols(DToriginal, DT)
    list_combined_1[[i]] <- combinedDT_1
    
    one_randomOD[, `:=` (id = combinedDT_1$id, Label = combinedDT_1$Label)]
    list_one_randomOD[[i]] <- one_randomOD
    
    # This part is for 2 OD parameters for each method
    # unsupervised_DTs2 <- create_unsupervised_view(datasetname, percentage_OD=1, mixed_view_features=1)
    # two_randomOD <- unsupervised_DTs2$mixed_arthur
    # dimension <- dim(two_randomOD)[2]
    # 
    # if(length(which(two_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension] != 0))){
    #   tempDT <- data.table::transpose(as.data.table(two_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension]))
    #   tempDT[, cols1:=names(two_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension])]
    #   
    #   cols_to_delete <- tempDT[V1!=0, cols1]
    #   print(cols_to_delete)
    #   cols_to_keep <- setdiff(names(two_randomOD), cols_to_delete)
    #   
    #   DT1 <- two_randomOD[, .SD, .SDcols = cols_to_keep]
    # }else{DT1 <- two_randomOD}
    # 
    # combinedDT_2 <- dplyr::bind_cols(DToriginal, DT1)
    # list_combined_2[[i]] <- combinedDT_2
    # 
    # two_randomOD[, `:=` (id = combinedDT_2$id, Label = combinedDT_2$Label)]
    # list_two_randomOD[[i]] <- two_randomOD
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  ### CV phase
  
  # Combined one-random --------------------------------------------------------------
  res_final <- list()

  for( ii in 1:length(list_combined_1)){
    gc()

    res_combined <- list()
    for(i in 1:CViterations){
      
      j <- list_combined_1[[ii]]
      print("==========")
      print(i)
      trainDT <- j[id %in% list_train_id[[i]]]
      print("Train")
      print(trainDT[, .N, by = Label])
      # We sample 90% of the train data to create the CVtrain dataset
      CVtrain_id <- trainDT[, sample(x = id, size = 0.9 * dim(trainDT)[1])]
      CVtrain_DT <- copy(trainDT[id %in% CVtrain_id & Label == "no"])
      print(CVtrain_DT[, .N, by = Label])
      outliers_train_DT <- copy(trainDT[ id %in% CVtrain_id & Label == "yes"])
      
      
      CVtest_id <- setdiff(trainDT$id, CVtrain_id)
      CVtest_DT1 <- copy(trainDT[id %in% CVtest_id])
      CVtest_DT <- rbindlist(list(CVtest_DT1, outliers_train_DT))
      
      if(CVtest_DT[Label=="yes", length(id)] == 0){
        CVtest_DT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
        }
      
      print("Test CV")
      print(CVtest_DT[, .N, by = Label])
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      testDT1 <- j[id %in% list_test_id[[i]]]
      if(testDT1[Label=="yes", length(id)] == 0){
        testDT <- rbindlist(list(outliers_train_DT[1:3], testDT1))
        testDT <- na.omit(testDT)
      }else{testDT <- testDT1}
      
      if(testDT[Label=="yes", length(id)] == 0){
        testDT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
        testDT <- na.omit(testDT)
      }else{testDT <- testDT}
      
      print("Test")
      print(testDT[, .N, by = Label])

      testDT_id_final <- testDT$id
      testDT_Label_final <- testDT$Label
      
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      testDT[, `:=` (id = NULL, Label = NULL)]
      
      # delete Label & id columns to train the OCSVM
      res <- list()
      iters <- 0
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(outliers_train_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters + 1
            print(glue("kernel: {kernels}."))
            print(glue("nu: {nus}."))
            print(glue("gamma: {gammas}."))
            print(glue("CV Iteration: {i}."))
            print(glue("Combined Iteration: {ii}."))
            print(start2)
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            
            scores_test <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = testDT,
                                                  given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            
            CVtest_DT[, `:=` (id = CVtest_id_final, Label = CVtest_Label_final)]
            testDT[, `:=` (id = testDT_id_final, Label = testDT_Label_final)]
            
            res[[iters]] <- data.table(auc(CVtest_DT$Label, scores_CV)[[1]],
                                       auc(testDT$Label, scores_test)[[1]],
                                       gamma = gammas,
                                       nu = nus,
                                       kernel = kernels)
            CVtest_DT[, `:=` (id = NULL, Label = NULL)]
            testDT[, `:=` (id = NULL, Label = NULL)]
          }
        }
      }
      temp_res <- rbindlist(res)
      temp_res[, "Cross-Validation":=i]
      
      res_combined[[i]] <- temp_res
    }
    res_final1 <- rbindlist(res_combined)
    res_final1[, features_Iteration:=ii]
    print(res_final1[, .SD[which.max(V1)], by = `Cross-Validation`])
    res_final[[ii]] <- res_final1
  }
  
  combined_DT <- rbindlist(res_final)
  fwrite(combined_DT, paste0(final_path_to_save, "figures/",  
                             subfolder_name, "/", datasetname, "_Combined_", CViterations,"CVnew_Normalized.csv"))
  # 1st strategy to find the best performing hyperparametrs
  comnined1_max_hyper <- combined_DT[, .SD[which.max(V1)], by = c("Cross-Validation", "features_Iteration")]
  comnined1_max_hyper[, features_Iteration:=as.factor(features_Iteration)]
  comnined1_max_hyper[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  comnined1_max_hyper[, Representation:="Combined-1"]
  
  
  # 2nd strategy to find the best performing hyperparameters
  quantile075DT <- combined_DT[, lapply(.SD, function(x) quantile(x, probs = 0.7)), .SDcols = "V1", by = c("Cross-Validation", "features_Iteration")]
  quantile095DT <- combined_DT[, lapply(.SD, function(x) quantile(x, probs = 0.9)), .SDcols = "V1", by = c("Cross-Validation", "features_Iteration")]
  quantile075DT[, quantile095:= quantile095DT$V1]
  setnames(quantile075DT, "V1", "quantile075")
  
  merged_combinedDT <- combined_DT[quantile075DT, on = c("Cross-Validation", "features_Iteration")]
  combinedDT_hyper <- merged_combinedDT[V1 %between% list(quantile075, quantile095)]
  
  iter <- 0
  list_feature_CV_best <- list()
  for(i in 1:21){
    for(j in 1:CViterations){
      iter <- iter + 1
      print(iter)
      combinedDT_hyper_meanDT <- combinedDT_hyper[features_Iteration==i & `Cross-Validation` == j][order(V1, decreasing = T)][, median(V1)]
      best_hyper_value_combined <- combinedDT_hyper[features_Iteration==i & `Cross-Validation`==j & V1 == combinedDT_hyper_meanDT]
      if(dim(best_hyper_value_combined)[1]==0){
        best_hyper_value_combined <- combinedDT_hyper[features_Iteration==i & `Cross-Validation`==j, .SD[which.max(V1)]]
        list_feature_CV_best[[iter]] <- best_hyper_value_combined[sample(1:dim(best_hyper_value_combined)[1], 1)]
      }
      list_feature_CV_best[[iter]] <- best_hyper_value_combined[sample(1:dim(best_hyper_value_combined)[1], 1)]
    }
  }
  combined_feature_CV_best <- rbindlist(list_feature_CV_best)
  combined_feature_CV_best[, features_Iteration:=as.factor(features_Iteration)]
  combined_feature_CV_best[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  combined_feature_CV_best[, Representation:="Combined-1"]
  
  # one-random --------------------------------------------------------------
  res_final11 <- list()
  for( ii in 1:length(list_one_randomOD)){
    gc()
    res_combined1 <- list()
    for(i in 1:CViterations){
      
      j <- list_one_randomOD[[ii]]
      
      trainDT <- j[id %in% list_train_id[[i]]]
      print(trainDT[, .N, by = Label])
      # We sample 90% of the train data to create the CVtrain dataset
      CVtrain_id <- trainDT[, sample(x = id, size = 0.9 * dim(trainDT)[1])]
      CVtrain_DT <- copy(trainDT[id %in% CVtrain_id & Label == "no"])
      outliers_train_DT <- copy(trainDT[ id %in% CVtrain_id & Label == "yes"])
      
      
      CVtest_id <- setdiff(trainDT$id, CVtrain_id)
      CVtest_DT1 <- copy(trainDT[id %in% CVtest_id])
      CVtest_DT <- rbindlist(list(CVtest_DT1, outliers_train_DT))
      
      print(CVtest_DT[, .N, by = Label])
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      testDT1 <- j[id %in% list_test_id[[i]]]
      if(testDT1[Label=="yes", length(id)] == 0){
        testDT <- rbindlist(list(outliers_train_DT[1:3], testDT1))
        testDT <- na.omit(testDT)
      }else{testDT <- testDT1}
      
      if(testDT[Label=="yes", length(id)] == 0){
        testDT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
        testDT <- na.omit(testDT)
      }else{testDT <- testDT}
      
      
      print("Test")
      print(testDT[, .N, by = Label])
      testDT_id_final <- testDT$id
      testDT_Label_final <- testDT$Label
      
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      testDT[, `:=` (id = NULL, Label = NULL)]
      
      
      # delete Label & id columns to train the OCSVM
      res <- list()
      iters <- 0
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(outliers_train_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters + 1
            print(glue("kernel: {kernels}."))
            print(glue("nu: {nus}."))
            print(glue("gamma: {gammas}."))
            print(glue("CV Iteration: {i}."))
            print(glue("Random Iteration: {ii}."))
            print(start2)
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT,
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            
            scores_test <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = testDT,
                                                  given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            
            CVtest_DT[, `:=` (id = CVtest_id_final, Label = CVtest_Label_final)]
            testDT[, `:=` (id = testDT_id_final, Label = testDT_Label_final)]
            
            res[[iters]] <- data.table(auc(CVtest_DT$Label, scores_CV)[[1]],
                                       auc(testDT$Label, scores_test)[[1]],
                                       gamma = gammas,
                                       nu = nus,
                                       kernel = kernels)
            CVtest_DT[, `:=` (id = NULL, Label = NULL)]
            testDT[, `:=` (id = NULL, Label = NULL)]
          }
        }
      }
      temp_res <- rbindlist(res)
      temp_res[, "Cross-Validation":=i]
      
      res_combined1[[i]] <- temp_res
    }
    res_final1_1 <- rbindlist(res_combined1)
    res_final1_1[, features_Iteration:=ii]
    print(res_final1_1[, .SD[which.max(V1)], by = `Cross-Validation`])
    res_final11[[ii]] <- res_final1_1
  }
  
  temp1_random <- rbindlist(res_final11)
  fwrite(temp1_random, paste0(final_path_to_save, "figures/",  
                              subfolder_name, "/", datasetname, "_1random_", CViterations, "CVnew.csv"))
  
  random1_max <- temp1_random[, .SD[which.max(V1)], by = c("Cross-Validation", "features_Iteration")]
  random1_max[, features_Iteration:=as.factor(features_Iteration)]
  random1_max[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  random1_max[, Representation:= "Random-1"]
  
  # 2nd strategy to find the best performing hyperparameters
  quantile075DT1 <- temp1_random[, lapply(.SD, function(x) quantile(x, probs = 0.75)), .SDcols = "V1", by = c("Cross-Validation", "features_Iteration")]
  quantile095DT1 <- temp1_random[, lapply(.SD, function(x) quantile(x, probs = 0.9)), .SDcols = "V1", by = c("Cross-Validation", "features_Iteration")]
  quantile075DT1[, quantile095:= quantile095DT1$V1]
  setnames(quantile075DT1, "V1", "quantile075")
  
  merged_1randomDT <- temp1_random[quantile075DT1, on = c("Cross-Validation", "features_Iteration")]
  random1DT_hyper <- merged_1randomDT[V1 %between% list(quantile075, quantile095)]
  
  iter <- 0
  list_feature_CV_best1 <- list()
  for(i in 1:21){
    for(j in 1:CViterations){
      iter <- iter + 1
      random1DT_hyper_meanDT <- random1DT_hyper[features_Iteration==i & `Cross-Validation` == j][order(V1, decreasing = T)][, median(V1)]
      best_hyper_value_random1 <- random1DT_hyper[features_Iteration==i & `Cross-Validation`==j & V1 == random1DT_hyper_meanDT]
      
      if(dim(best_hyper_value_random1)[1]==0){
        best_hyper_value_random1 <- random1DT_hyper[features_Iteration==i & `Cross-Validation`==j, .SD[which.max(V1)]]
        list_feature_CV_best1[[iter]] <- best_hyper_value_random1[sample(1:dim(best_hyper_value_random1)[1], 1)]
      }
      list_feature_CV_best1[[iter]] <- best_hyper_value_random1[sample(1:dim(best_hyper_value_random1)[1], 1)]
    }
  }
  random1_feature_CV_best <- rbindlist(list_feature_CV_best1)
  random1_feature_CV_best[, features_Iteration:=as.factor(features_Iteration)]
  random1_feature_CV_best[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  random1_feature_CV_best[, Representation:="Random-1"]
  
  # end - one random  --------------------------------------------------------
  
  
  # original --------------------------------------------------------------
  res_original <- list()
  for(i in 1:CViterations){
    
    j <- DToriginal
    
    trainDT <- j[id %in% list_train_id[[i]]]
    print(trainDT[, .N, by = Label])
    # We sample 90% of the train data to create the CVtrain dataset
    CVtrain_id <- trainDT[, sample(x = id, size = 0.9 * dim(trainDT)[1])]
    CVtrain_DT <- copy(trainDT[id %in% CVtrain_id & Label == "no"])
    outliers_train_DT <- copy(trainDT[ id %in% CVtrain_id & Label == "yes"])
    
    
    CVtest_id <- setdiff(trainDT$id, CVtrain_id)
    CVtest_DT1 <- copy(trainDT[id %in% CVtest_id])
    CVtest_DT <- rbindlist(list(CVtest_DT1, outliers_train_DT))
    
    if(CVtest_DT[Label=="yes", length(id)] == 0){
      CVtest_DT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
    }
    
    if(CVtest_DT[Label=="yes", length(id)] == 0){
      CVtest_DT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
    }
    
    print(CVtest_DT[, .N, by = Label])
    CVtest_id_final <- CVtest_DT$id
    CVtest_Label_final <- CVtest_DT$Label
    
    testDT1 <- j[id %in% list_test_id[[i]]]
    if(testDT1[Label=="yes", length(id)] == 0){
      testDT <- rbindlist(list(outliers_train_DT[1:3], testDT1))
      testDT <- na.omit(testDT)
    }else{testDT <- testDT1}
    
    if(testDT[Label=="yes", length(id)] == 0){
      testDT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
      testDT <- na.omit(testDT)
    }else{testDT <- testDT}
    
    
    print("Test")
    print(testDT[, .N, by = Label])
    testDT_id_final <- testDT$id
    testDT_Label_final <- testDT$Label
    
    
    CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
    CVtest_DT[, `:=` (id = NULL, Label = NULL)]
    testDT[, `:=` (id = NULL, Label = NULL)]
    
    
    # delete Label & id columns to train the OCSVM
    res <- list()
    iters <- 0
    for(kernels in c("linear", "rbf", "sigmoid")){
      for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
        for(gammas in c(1/dim(outliers_train_DT)[2], 0.01, 0.05, 0.1, 0.2)){
          iters <- iters + 1
          print(glue("kernel: {kernels}."))
          print(glue("nu: {nus}."))
          print(glue("gamma: {gammas}."))
          print(glue("CV Iteration: {i}."))
          print(start2)
          scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT,
                                              given_nu = nus, given_kernel = kernels, given_gamma = gammas)
          
          scores_test <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = testDT,
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
          
          CVtest_DT[, `:=` (id = CVtest_id_final, Label = CVtest_Label_final)]
          testDT[, `:=` (id = testDT_id_final, Label = testDT_Label_final)]
          
          res[[iters]] <- data.table(auc(CVtest_DT$Label, scores_CV)[[1]],
                                     auc(testDT$Label, scores_test)[[1]],
                                     gamma = gammas,
                                     nu = nus,
                                     kernel = kernels)
          CVtest_DT[, `:=` (id = NULL, Label = NULL)]
          testDT[, `:=` (id = NULL, Label = NULL)]
        }
      }
    }
    temp_res <- rbindlist(res)
    temp_res[, "Cross-Validation":=i]
    
    res_original[[i]] <- temp_res
  }
  res_final_original <- rbindlist(res_original)
  fwrite(res_final_original, paste0(final_path_to_save, "figures/",  
                                    subfolder_name, "/", datasetname, "_Original_", CViterations,"CVnew.csv"))
  
  original_maxDT <- res_final_original[, .SD[which.max(V1)], by = `Cross-Validation`]
  
  # 2nd strategy to find the best performing hyperparameters
  original_quantile075 <- res_final_original[, lapply(.SD, function(x) quantile(x, probs = 0.75)), .SDcols = "V1", by = c("Cross-Validation")]
  original_quantile095 <- res_final_original[, lapply(.SD, function(x) quantile(x, probs = 0.95)), .SDcols = "V1", by = c("Cross-Validation")]
  original_quantile075[, quantile095:= original_quantile095$V1]
  setnames(original_quantile075, "V1", "quantile075")
  original_merged <- res_final_original[original_quantile075, on = "Cross-Validation"]
  original_quantiles <- original_merged[V1 %between% list(quantile075, quantile095)]
  
  
  list_feature_CV_best2 <- list()
  for(j in 1:CViterations){
    number_rows <- nrow(original_quantiles[`Cross-Validation`==j][order(V1, decreasing = T)])
    original_hyper_meanDT <- original_quantiles[`Cross-Validation`==j][order(V1, decreasing = T)][sample(1:number_rows, 1), V1]
    best_hyper_value_original <- original_quantiles[`Cross-Validation`==j & V1 == original_hyper_meanDT]
    list_feature_CV_best2[[j]] <- best_hyper_value_original[sample(1:dim(best_hyper_value_original)[1], 1)]
  }
  
  original_feature_CV_best <- rbindlist(list_feature_CV_best2)
  original_feature_CV_best[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  # end - original --------------------------------------------------------
  
  
  # Strategy 1 - Create data ---------------------------------------
  
  res_final_DT <- rbindlist(list(comnined1_max_hyper, random1_max))
  res_final_DT[, Representation:= as.factor(Representation)]
  res_final_DT[, V3:= (V2 - original_maxDT[, mean(V2)])/original_maxDT[, sd(V2)]]
  
  winning_iterations_combined <- res_final_DT[Representation=="Combined-1", mean(V2)>original_maxDT[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  winning_iterations_random <- res_final_DT[Representation=="Random-1", mean(V2)>original_maxDT[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  
  res_final_DT[Representation=="Combined-1" & features_Iteration %in% winning_iterations_combined, Group:= "Win"]
  res_final_DT[Representation=="Random-1" & features_Iteration %in% winning_iterations_random, Group:= "Win"]
  res_final_DT[is.na(Group), Group:= "Defeat"]
  
  
  winning_combined <- paste0(100 * round(length(winning_iterations_combined)/length(list_combined_1), 2), "%")
  winning_random <- paste0(100 * round(length(winning_iterations_random)/length(list_one_randomOD), 2), "%")
  
  mean_val <- res_final_DT[, mean(V2), by=Representation]
  mean_val_sd <- res_final_DT[, mean(V3), by=Representation]
  
  supp.labs <- c(glue("{winning_combined} of the random Combined datasets outperform the Original features
                    Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val[Representation=='Combined-1',V1], 4)}
                    Black line is  {round(mean_val_sd[Representation=='Combined-1',V1], 4)} Standard Deviations away from Red line"), 
                 glue("{winning_random} of the random datasets composed only of Outlier Scores, outperform the Original features
                    Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val[Representation=='Random-1',V1], 4)}
                    Black line is {round(mean_val_sd[Representation=='Random-1',V1], 4)} Standard Deviations away from Red line"))
  names(supp.labs) <- c("Combined-1", "Random-1")
  
  
  p <- ggplot(data = res_final_DT) +
    aes(x = features_Iteration, y = V2, fill = Group) +
    geom_boxplot() +
    theme_bw()+
    facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.labs))+
    scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
    geom_hline(yintercept = original_maxDT[, mean(V2)], color = "red",)+
    geom_hline(data = mean_val, aes(yintercept=V1)) +
    # stat_summary(fun.y = mean, color = "darkred", 
    #              geom = "point", shape = 18, size = 3,
    #              show.legend = FALSE)+
    scale_fill_manual(values=c("#907509", "#0b1a8c"))+
    labs(x = "Random Combined Datasets", 
         y = "AUC",
         title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_maxDT[, round(mean(V2), 4)]}"))
  
  p
  
  ggsave(plot = p, filename = paste0(final_path_to_save, "figures/",  
                                     subfolder_name, "/", datasetname, "_",
                                     "AUCperformance_Maximum_hyper_", CViterations,"CVnew",".pdf"),
         width = 12, height = 6, units = "in", dpi = 300)
  
  
  p1 <- ggplot(data = res_final_DT) +
    aes(x = features_Iteration, y = V3, fill = Group) +
    geom_boxplot() +
    theme_bw()+
    facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.labs))+
    scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
    geom_hline(yintercept = 0, color = "red",)+
    geom_hline(data = mean_val_sd, aes(yintercept=V1)) +
    # stat_summary(fun.y = mean, color = "darkred", 
    #              geom = "point", shape = 18, size = 3,
    #              show.legend = FALSE)+
    scale_fill_manual(values=c("#907509", "#0b1a8c"))+
    labs(x = "Random Combined Datasets", 
         y = "Standard Deviations of Mean AUC of Original-View",
         title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_maxDT[, round(mean(V2), 4)]}"))
  
  p1
  
  ggsave(plot = p1, filename = paste0(final_path_to_save, "figures/",  
                                      subfolder_name, "/", datasetname, "_",
                                      "sd_AUCperformance_Maximum_hyper_", CViterations,"CVnew",".pdf"),
         width = 12, height = 6, units = "in", dpi = 300)
  
  # Strategy 2 - Create data ---------------------------------------
  
  res_final_DT1 <- rbindlist(list(combined_feature_CV_best, random1_feature_CV_best))
  res_final_DT1[, Representation:= as.factor(Representation)]
  res_final_DT1[, V3:= (V2 - original_feature_CV_best[, mean(V2)])/original_feature_CV_best[, sd(V2)]]
  
  
  winning_iterations_combined1 <- res_final_DT1[Representation=="Combined-1", mean(V2)>original_feature_CV_best[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  winning_iterations_random1 <- res_final_DT1[Representation=="Random-1", mean(V2)>original_feature_CV_best[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  
  res_final_DT1[Representation=="Combined-1" & features_Iteration %in% winning_iterations_combined1, Group:= "Win"]
  res_final_DT1[Representation=="Random-1" & features_Iteration %in% winning_iterations_random1, Group:= "Win"]
  res_final_DT1[is.na(Group), Group:= "Defeat"]
  
  
  winning_combined1 <- paste0(100 * round(length(winning_iterations_combined1)/length(list_combined_1), 2), "%")
  winning_random1 <- paste0(100 * round(length(winning_iterations_random1)/length(list_one_randomOD), 2), "%")
  
  mean_val1 <- res_final_DT1[, mean(V2), by=Representation]
  mean_val_sd1 <- res_final_DT1[, mean(V3), by=Representation]
  
  supp.lab <- c(glue("{winning_combined1} of the random Combined datasets outperform the Original features
                    Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val1[Representation=='Combined-1',V1], 4)}
                    Black line is  {round(mean_val_sd1[Representation=='Combined-1',V1], 4)} Standard Deviations away from Red line"), 
                glue("{winning_random1} of the random datasets composed only of Outlier Scores, outperform the Original features
                    Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val1[Representation=='Random-1',V1], 4)}
                    Black line is {round(mean_val_sd1[Representation=='Random-1',V1], 4)} Standard Deviations away from Red line"))
  names(supp.lab) <- c("Combined-1", "Random-1")
  
  
  p2 <- ggplot(data = res_final_DT1) +
    aes(x = features_Iteration, y = V2, fill = Group) +
    geom_boxplot() +
    theme_bw()+
    facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.lab))+
    scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
    geom_hline(yintercept = original_feature_CV_best[, mean(V2)], color = "red",)+
    geom_hline(data = mean_val1, aes(yintercept=V1)) +
    # stat_summary(fun.y = mean, color = "darkred", 
    #              geom = "point", shape = 18, size = 3,
    #              show.legend = FALSE)+
    scale_fill_manual(values=c("#907509", "#0b1a8c"))+
    labs(x = "Random Combined Datasets", 
         y = "AUC",
         title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_feature_CV_best[, round(mean(V2), 4)]}"))
  
  p2
  
  ggsave(plot = p2, filename = paste0(final_path_to_save, "figures/",  
                                      subfolder_name, "/", datasetname, "_",
                                      "AUCperformance_Median_hyper_", CViterations, "CVnew",".pdf"),
         width = 12, height = 6, units = "in", dpi = 300)
  
  
  p3 <- ggplot(data = res_final_DT1) +
    aes(x = features_Iteration, y = V3, fill = Group) +
    geom_boxplot() +
    theme_bw()+
    facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.lab))+
    scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
    geom_hline(yintercept = 0, color = "red",)+
    geom_hline(data = mean_val_sd1, aes(yintercept=V1)) +
    # stat_summary(fun.y = mean, color = "darkred", 
    #              geom = "point", shape = 18, size = 3,
    #              show.legend = FALSE)+
    scale_fill_manual(values=c("#907509", "#0b1a8c"))+
    labs(x = "Random Combined Datasets", 
         y = "Standard Deviations of Mean AUC of Original-View",
         title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_feature_CV_best[, round(mean(V2), 4)]}"))
  
  p3
  
  ggsave(plot = p3, filename = paste0(final_path_to_save, "figures/",  
                                      subfolder_name, "/", datasetname, "_",
                                      "sd_AUCperformance_Median_hyper_", CViterations ,"CVnew",".pdf"),
         width = 12, height = 6, units = "in", dpi = 300)
  
  
}


get_CV_experiments_normalized <- function(datasetname, subfolder_name, experiments = "OC_combined_CV", CViterations) {
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
  
  DToriginal <- fread(paste0(path_to_read, "/", datasetname,".csv"))
  setnames(DToriginal, "outlier", "Label", skip_absent = T)
  DToriginal[, .N, by = Label]
  
  list_combined_1 <- list()
  list_combined_2 <- list()
  list_one_randomOD <- list()
  list_two_randomOD <- list()
  for(i in 1:21){
    unsupervised_DTs1 <- create_unsupervised_view(datasetname, percentage_OD=1, mixed_view_features=1)
    one_randomOD <- unsupervised_DTs1$mixed_arthur_normalized
    dimension <- dim(one_randomOD)[2]
    
    if(length(which(one_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension] != 0))){
      tempDT <- data.table::transpose(as.data.table(one_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension]))
      tempDT[, cols1:=names(one_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension])]
      cols_to_delete <- tempDT[V1!=0, cols1]
      print(cols_to_delete)
      cols_to_keep <- setdiff(names(one_randomOD), cols_to_delete)
      
      DT <- one_randomOD[, .SD, .SDcols = cols_to_keep]
    }else{DT <- one_randomOD}
    combinedDT_1 <- dplyr::bind_cols(DToriginal, DT)
    list_combined_1[[i]] <- combinedDT_1
    
    one_randomOD[, `:=` (id = combinedDT_1$id, Label = combinedDT_1$Label)]
    list_one_randomOD[[i]] <- one_randomOD
    
    # This part is for 2 OD parameters for each method
    # unsupervised_DTs2 <- create_unsupervised_view(datasetname, percentage_OD=1, mixed_view_features=1)
    # two_randomOD <- unsupervised_DTs2$mixed_arthur
    # dimension <- dim(two_randomOD)[2]
    # 
    # if(length(which(two_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension] != 0))){
    #   tempDT <- data.table::transpose(as.data.table(two_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension]))
    #   tempDT[, cols1:=names(two_randomOD[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension])]
    #   
    #   cols_to_delete <- tempDT[V1!=0, cols1]
    #   print(cols_to_delete)
    #   cols_to_keep <- setdiff(names(two_randomOD), cols_to_delete)
    #   
    #   DT1 <- two_randomOD[, .SD, .SDcols = cols_to_keep]
    # }else{DT1 <- two_randomOD}
    # 
    # combinedDT_2 <- dplyr::bind_cols(DToriginal, DT1)
    # list_combined_2[[i]] <- combinedDT_2
    # 
    # two_randomOD[, `:=` (id = combinedDT_2$id, Label = combinedDT_2$Label)]
    # list_two_randomOD[[i]] <- two_randomOD
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  ### CV phase
  
  # Combined one-random --------------------------------------------------------------
  res_final <- list()
  
  for( ii in 1:length(list_combined_1)){
    gc()
    
    res_combined <- list()
    for(i in 1:CViterations){
      
      j <- list_combined_1[[ii]]
      print("==========")
      print(i)
      trainDT <- j[id %in% list_train_id[[i]]]
      print("Train")
      print(trainDT[, .N, by = Label])
      # We sample 90% of the train data to create the CVtrain dataset
      CVtrain_id <- trainDT[, sample(x = id, size = 0.9 * dim(trainDT)[1])]
      CVtrain_DT <- copy(trainDT[id %in% CVtrain_id & Label == "no"])
      print(CVtrain_DT[, .N, by = Label])
      outliers_train_DT <- copy(trainDT[ id %in% CVtrain_id & Label == "yes"])
      
      
      CVtest_id <- setdiff(trainDT$id, CVtrain_id)
      CVtest_DT1 <- copy(trainDT[id %in% CVtest_id])
      CVtest_DT <- rbindlist(list(CVtest_DT1, outliers_train_DT))
      
      if(CVtest_DT[Label=="yes", length(id)] == 0){
        CVtest_DT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
      }
      
      print("Test CV")
      print(CVtest_DT[, .N, by = Label])
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      testDT1 <- j[id %in% list_test_id[[i]]]
      if(testDT1[Label=="yes", length(id)] == 0){
        testDT <- rbindlist(list(outliers_train_DT[1:3], testDT1))
        testDT <- na.omit(testDT)
      }else{testDT <- testDT1}
      
      if(testDT[Label=="yes", length(id)] == 0){
        testDT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
        testDT <- na.omit(testDT)
      }else{testDT <- testDT}
      
      print("Test")
      print(testDT[, .N, by = Label])
      
      testDT_id_final <- testDT$id
      testDT_Label_final <- testDT$Label
      
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      testDT[, `:=` (id = NULL, Label = NULL)]
      
      # delete Label & id columns to train the OCSVM
      res <- list()
      iters <- 0
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(outliers_train_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters + 1
            print(glue("kernel: {kernels}."))
            print(glue("nu: {nus}."))
            print(glue("gamma: {gammas}."))
            print(glue("CV Iteration: {i}."))
            print(glue("Combined Iteration: {ii}."))
            print(start2)
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            
            scores_test <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = testDT,
                                                  given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            
            CVtest_DT[, `:=` (id = CVtest_id_final, Label = CVtest_Label_final)]
            testDT[, `:=` (id = testDT_id_final, Label = testDT_Label_final)]
            
            res[[iters]] <- data.table(auc(CVtest_DT$Label, scores_CV)[[1]],
                                       auc(testDT$Label, scores_test)[[1]],
                                       gamma = gammas,
                                       nu = nus,
                                       kernel = kernels)
            CVtest_DT[, `:=` (id = NULL, Label = NULL)]
            testDT[, `:=` (id = NULL, Label = NULL)]
          }
        }
      }
      temp_res <- rbindlist(res)
      temp_res[, "Cross-Validation":=i]
      
      res_combined[[i]] <- temp_res
    }
    res_final1 <- rbindlist(res_combined)
    res_final1[, features_Iteration:=ii]
    print(res_final1[, .SD[which.max(V1)], by = `Cross-Validation`])
    res_final[[ii]] <- res_final1
  }
  
  combined_DT <- rbindlist(res_final)
  fwrite(combined_DT, paste0(final_path_to_save, "figures/",  
                             subfolder_name, "/", datasetname, "_Combined_", CViterations,"CVnew_Normalized.csv"))
  # 1st strategy to find the best performing hyperparametrs
  comnined1_max_hyper <- combined_DT[, .SD[which.max(V1)], by = c("Cross-Validation", "features_Iteration")]
  comnined1_max_hyper[, features_Iteration:=as.factor(features_Iteration)]
  comnined1_max_hyper[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  comnined1_max_hyper[, Representation:="Combined-1"]
  
  
  # 2nd strategy to find the best performing hyperparameters
  quantile075DT <- combined_DT[, lapply(.SD, function(x) quantile(x, probs = 0.7)), .SDcols = "V1", by = c("Cross-Validation", "features_Iteration")]
  quantile095DT <- combined_DT[, lapply(.SD, function(x) quantile(x, probs = 0.9)), .SDcols = "V1", by = c("Cross-Validation", "features_Iteration")]
  quantile075DT[, quantile095:= quantile095DT$V1]
  setnames(quantile075DT, "V1", "quantile075")
  
  merged_combinedDT <- combined_DT[quantile075DT, on = c("Cross-Validation", "features_Iteration")]
  combinedDT_hyper <- merged_combinedDT[V1 %between% list(quantile075, quantile095)]
  
  iter <- 0
  list_feature_CV_best <- list()
  for(i in 1:21){
    for(j in 1:CViterations){
      iter <- iter + 1
      print(iter)
      combinedDT_hyper_meanDT <- combinedDT_hyper[features_Iteration==i & `Cross-Validation` == j][order(V1, decreasing = T)][, median(V1)]
      best_hyper_value_combined <- combinedDT_hyper[features_Iteration==i & `Cross-Validation`==j & V1 == combinedDT_hyper_meanDT]
      if(dim(best_hyper_value_combined)[1]==0){
        best_hyper_value_combined <- combinedDT_hyper[features_Iteration==i & `Cross-Validation`==j, .SD[which.max(V1)]]
        list_feature_CV_best[[iter]] <- best_hyper_value_combined[sample(1:dim(best_hyper_value_combined)[1], 1)]
      }
      list_feature_CV_best[[iter]] <- best_hyper_value_combined[sample(1:dim(best_hyper_value_combined)[1], 1)]
    }
  }
  combined_feature_CV_best <- rbindlist(list_feature_CV_best)
  combined_feature_CV_best[, features_Iteration:=as.factor(features_Iteration)]
  combined_feature_CV_best[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  combined_feature_CV_best[, Representation:="Combined-1"]
  
  # one-random --------------------------------------------------------------
  res_final11 <- list()
  for( ii in 1:length(list_one_randomOD)){
    gc()
    res_combined1 <- list()
    for(i in 1:CViterations){
      
      j <- list_one_randomOD[[ii]]
      
      trainDT <- j[id %in% list_train_id[[i]]]
      print(trainDT[, .N, by = Label])
      # We sample 90% of the train data to create the CVtrain dataset
      CVtrain_id <- trainDT[, sample(x = id, size = 0.9 * dim(trainDT)[1])]
      CVtrain_DT <- copy(trainDT[id %in% CVtrain_id & Label == "no"])
      outliers_train_DT <- copy(trainDT[ id %in% CVtrain_id & Label == "yes"])
      
      
      CVtest_id <- setdiff(trainDT$id, CVtrain_id)
      CVtest_DT1 <- copy(trainDT[id %in% CVtest_id])
      CVtest_DT <- rbindlist(list(CVtest_DT1, outliers_train_DT))
      
      print(CVtest_DT[, .N, by = Label])
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      testDT1 <- j[id %in% list_test_id[[i]]]
      if(testDT1[Label=="yes", length(id)] == 0){
        testDT <- rbindlist(list(outliers_train_DT[1:3], testDT1))
        testDT <- na.omit(testDT)
      }else{testDT <- testDT1}
      
      if(testDT[Label=="yes", length(id)] == 0){
        testDT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
        testDT <- na.omit(testDT)
      }else{testDT <- testDT}
      
      
      print("Test")
      print(testDT[, .N, by = Label])
      testDT_id_final <- testDT$id
      testDT_Label_final <- testDT$Label
      
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      testDT[, `:=` (id = NULL, Label = NULL)]
      
      
      # delete Label & id columns to train the OCSVM
      res <- list()
      iters <- 0
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(outliers_train_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters + 1
            print(glue("kernel: {kernels}."))
            print(glue("nu: {nus}."))
            print(glue("gamma: {gammas}."))
            print(glue("CV Iteration: {i}."))
            print(glue("Random Iteration: {ii}."))
            print(start2)
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT,
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            
            scores_test <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = testDT,
                                                  given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            
            CVtest_DT[, `:=` (id = CVtest_id_final, Label = CVtest_Label_final)]
            testDT[, `:=` (id = testDT_id_final, Label = testDT_Label_final)]
            
            res[[iters]] <- data.table(auc(CVtest_DT$Label, scores_CV)[[1]],
                                       auc(testDT$Label, scores_test)[[1]],
                                       gamma = gammas,
                                       nu = nus,
                                       kernel = kernels)
            CVtest_DT[, `:=` (id = NULL, Label = NULL)]
            testDT[, `:=` (id = NULL, Label = NULL)]
          }
        }
      }
      temp_res <- rbindlist(res)
      temp_res[, "Cross-Validation":=i]
      
      res_combined1[[i]] <- temp_res
    }
    res_final1_1 <- rbindlist(res_combined1)
    res_final1_1[, features_Iteration:=ii]
    print(res_final1_1[, .SD[which.max(V1)], by = `Cross-Validation`])
    res_final11[[ii]] <- res_final1_1
  }
  
  temp1_random <- rbindlist(res_final11)
  fwrite(temp1_random, paste0(final_path_to_save, "figures/",  
                              subfolder_name, "/", datasetname, "_1random_", CViterations, "CVnew_Normalized.csv"))
  
  random1_max <- temp1_random[, .SD[which.max(V1)], by = c("Cross-Validation", "features_Iteration")]
  random1_max[, features_Iteration:=as.factor(features_Iteration)]
  random1_max[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  random1_max[, Representation:= "Random-1"]
  
  # 2nd strategy to find the best performing hyperparameters
  quantile075DT1 <- temp1_random[, lapply(.SD, function(x) quantile(x, probs = 0.75)), .SDcols = "V1", by = c("Cross-Validation", "features_Iteration")]
  quantile095DT1 <- temp1_random[, lapply(.SD, function(x) quantile(x, probs = 0.9)), .SDcols = "V1", by = c("Cross-Validation", "features_Iteration")]
  quantile075DT1[, quantile095:= quantile095DT1$V1]
  setnames(quantile075DT1, "V1", "quantile075")
  
  merged_1randomDT <- temp1_random[quantile075DT1, on = c("Cross-Validation", "features_Iteration")]
  random1DT_hyper <- merged_1randomDT[V1 %between% list(quantile075, quantile095)]
  
  iter <- 0
  list_feature_CV_best1 <- list()
  for(i in 1:21){
    for(j in 1:CViterations){
      iter <- iter + 1
      random1DT_hyper_meanDT <- random1DT_hyper[features_Iteration==i & `Cross-Validation` == j][order(V1, decreasing = T)][, median(V1)]
      best_hyper_value_random1 <- random1DT_hyper[features_Iteration==i & `Cross-Validation`==j & V1 == random1DT_hyper_meanDT]
      
      if(dim(best_hyper_value_random1)[1]==0){
        best_hyper_value_random1 <- random1DT_hyper[features_Iteration==i & `Cross-Validation`==j, .SD[which.max(V1)]]
        list_feature_CV_best1[[iter]] <- best_hyper_value_random1[sample(1:dim(best_hyper_value_random1)[1], 1)]
      }
      list_feature_CV_best1[[iter]] <- best_hyper_value_random1[sample(1:dim(best_hyper_value_random1)[1], 1)]
    }
  }
  random1_feature_CV_best <- rbindlist(list_feature_CV_best1)
  random1_feature_CV_best[, features_Iteration:=as.factor(features_Iteration)]
  random1_feature_CV_best[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  random1_feature_CV_best[, Representation:="Random-1"]
  
  # end - one random  --------------------------------------------------------
  
  
  # original --------------------------------------------------------------
  res_original <- list()
  for(i in 1:CViterations){
    
    j <- DToriginal
    
    trainDT <- j[id %in% list_train_id[[i]]]
    print(trainDT[, .N, by = Label])
    # We sample 90% of the train data to create the CVtrain dataset
    CVtrain_id <- trainDT[, sample(x = id, size = 0.9 * dim(trainDT)[1])]
    CVtrain_DT <- copy(trainDT[id %in% CVtrain_id & Label == "no"])
    outliers_train_DT <- copy(trainDT[ id %in% CVtrain_id & Label == "yes"])
    
    
    CVtest_id <- setdiff(trainDT$id, CVtrain_id)
    CVtest_DT1 <- copy(trainDT[id %in% CVtest_id])
    CVtest_DT <- rbindlist(list(CVtest_DT1, outliers_train_DT))
    
    if(CVtest_DT[Label=="yes", length(id)] == 0){
      CVtest_DT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
    }
    
    if(CVtest_DT[Label=="yes", length(id)] == 0){
      CVtest_DT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
    }
    
    print(CVtest_DT[, .N, by = Label])
    CVtest_id_final <- CVtest_DT$id
    CVtest_Label_final <- CVtest_DT$Label
    
    testDT1 <- j[id %in% list_test_id[[i]]]
    if(testDT1[Label=="yes", length(id)] == 0){
      testDT <- rbindlist(list(outliers_train_DT[1:3], testDT1))
      testDT <- na.omit(testDT)
    }else{testDT <- testDT1}
    
    if(testDT[Label=="yes", length(id)] == 0){
      testDT <- j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)]
      testDT <- na.omit(testDT)
    }else{testDT <- testDT}
    
    
    print("Test")
    print(testDT[, .N, by = Label])
    testDT_id_final <- testDT$id
    testDT_Label_final <- testDT$Label
    
    
    CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
    CVtest_DT[, `:=` (id = NULL, Label = NULL)]
    testDT[, `:=` (id = NULL, Label = NULL)]
    
    
    # delete Label & id columns to train the OCSVM
    res <- list()
    iters <- 0
    for(kernels in c("linear", "rbf", "sigmoid")){
      for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
        for(gammas in c(1/dim(outliers_train_DT)[2], 0.01, 0.05, 0.1, 0.2)){
          iters <- iters + 1
          print(glue("kernel: {kernels}."))
          print(glue("nu: {nus}."))
          print(glue("gamma: {gammas}."))
          print(glue("CV Iteration: {i}."))
          print(start2)
          scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT,
                                              given_nu = nus, given_kernel = kernels, given_gamma = gammas)
          
          scores_test <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = testDT,
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
          
          CVtest_DT[, `:=` (id = CVtest_id_final, Label = CVtest_Label_final)]
          testDT[, `:=` (id = testDT_id_final, Label = testDT_Label_final)]
          
          res[[iters]] <- data.table(auc(CVtest_DT$Label, scores_CV)[[1]],
                                     auc(testDT$Label, scores_test)[[1]],
                                     gamma = gammas,
                                     nu = nus,
                                     kernel = kernels)
          CVtest_DT[, `:=` (id = NULL, Label = NULL)]
          testDT[, `:=` (id = NULL, Label = NULL)]
        }
      }
    }
    temp_res <- rbindlist(res)
    temp_res[, "Cross-Validation":=i]
    
    res_original[[i]] <- temp_res
  }
  res_final_original <- rbindlist(res_original)
  fwrite(res_final_original, paste0(final_path_to_save, "figures/",  
                                    subfolder_name, "/", datasetname, "_Original_", CViterations,"CVnew_Normalized.csv"))
  
  original_maxDT <- res_final_original[, .SD[which.max(V1)], by = `Cross-Validation`]
  
  # 2nd strategy to find the best performing hyperparameters
  original_quantile075 <- res_final_original[, lapply(.SD, function(x) quantile(x, probs = 0.75)), .SDcols = "V1", by = c("Cross-Validation")]
  original_quantile095 <- res_final_original[, lapply(.SD, function(x) quantile(x, probs = 0.95)), .SDcols = "V1", by = c("Cross-Validation")]
  original_quantile075[, quantile095:= original_quantile095$V1]
  setnames(original_quantile075, "V1", "quantile075")
  original_merged <- res_final_original[original_quantile075, on = "Cross-Validation"]
  original_quantiles <- original_merged[V1 %between% list(quantile075, quantile095)]
  
  
  list_feature_CV_best2 <- list()
  for(j in 1:CViterations){
    number_rows <- nrow(original_quantiles[`Cross-Validation`==j][order(V1, decreasing = T)])
    original_hyper_meanDT <- original_quantiles[`Cross-Validation`==j][order(V1, decreasing = T)][sample(1:number_rows, 1), V1]
    best_hyper_value_original <- original_quantiles[`Cross-Validation`==j & V1 == original_hyper_meanDT]
    list_feature_CV_best2[[j]] <- best_hyper_value_original[sample(1:dim(best_hyper_value_original)[1], 1)]
  }
  
  original_feature_CV_best <- rbindlist(list_feature_CV_best2)
  original_feature_CV_best[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  # end - original --------------------------------------------------------
  
  
  # Strategy 1 - Create data ---------------------------------------
  
  res_final_DT <- rbindlist(list(comnined1_max_hyper, random1_max))
  res_final_DT[, Representation:= as.factor(Representation)]
  res_final_DT[, V3:= (V2 - original_maxDT[, mean(V2)])/original_maxDT[, sd(V2)]]
  
  winning_iterations_combined <- res_final_DT[Representation=="Combined-1", mean(V2)>original_maxDT[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  winning_iterations_random <- res_final_DT[Representation=="Random-1", mean(V2)>original_maxDT[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  
  res_final_DT[Representation=="Combined-1" & features_Iteration %in% winning_iterations_combined, Group:= "Win"]
  res_final_DT[Representation=="Random-1" & features_Iteration %in% winning_iterations_random, Group:= "Win"]
  res_final_DT[is.na(Group), Group:= "Defeat"]
  
  
  winning_combined <- paste0(100 * round(length(winning_iterations_combined)/length(list_combined_1), 2), "%")
  winning_random <- paste0(100 * round(length(winning_iterations_random)/length(list_one_randomOD), 2), "%")
  
  mean_val <- res_final_DT[, mean(V2), by=Representation]
  mean_val_sd <- res_final_DT[, mean(V3), by=Representation]
  
  supp.labs <- c(glue("{winning_combined} of the random Combined datasets outperform the Original features
                    Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val[Representation=='Combined-1',V1], 4)}
                    Black line is  {round(mean_val_sd[Representation=='Combined-1',V1], 4)} Standard Deviations away from Red line"), 
                 glue("{winning_random} of the random datasets composed only of Outlier Scores, outperform the Original features
                    Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val[Representation=='Random-1',V1], 4)}
                    Black line is {round(mean_val_sd[Representation=='Random-1',V1], 4)} Standard Deviations away from Red line"))
  names(supp.labs) <- c("Combined-1", "Random-1")
  
  
  p <- ggplot(data = res_final_DT) +
    aes(x = features_Iteration, y = V2, fill = Group) +
    geom_boxplot() +
    theme_bw()+
    facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.labs))+
    scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
    geom_hline(yintercept = original_maxDT[, mean(V2)], color = "red",)+
    geom_hline(data = mean_val, aes(yintercept=V1)) +
    # stat_summary(fun.y = mean, color = "darkred", 
    #              geom = "point", shape = 18, size = 3,
    #              show.legend = FALSE)+
    scale_fill_manual(values=c("#907509", "#0b1a8c"))+
    labs(x = "Random Combined Datasets", 
         y = "AUC",
         title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_maxDT[, round(mean(V2), 4)]}"))
  
  p
  
  ggsave(plot = p, filename = paste0(final_path_to_save, "figures/",  
                                     subfolder_name, "/", datasetname, "_",
                                     "AUCperformance_Maximum_hyper_", CViterations,"CVnew_Normalized",".pdf"),
         width = 12, height = 6, units = "in", dpi = 300)
  
  
  p1 <- ggplot(data = res_final_DT) +
    aes(x = features_Iteration, y = V3, fill = Group) +
    geom_boxplot() +
    theme_bw()+
    facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.labs))+
    scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
    geom_hline(yintercept = 0, color = "red",)+
    geom_hline(data = mean_val_sd, aes(yintercept=V1)) +
    # stat_summary(fun.y = mean, color = "darkred", 
    #              geom = "point", shape = 18, size = 3,
    #              show.legend = FALSE)+
    scale_fill_manual(values=c("#907509", "#0b1a8c"))+
    labs(x = "Random Combined Datasets", 
         y = "Standard Deviations of Mean AUC of Original-View",
         title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_maxDT[, round(mean(V2), 4)]}"))
  
  p1
  
  ggsave(plot = p1, filename = paste0(final_path_to_save, "figures/",  
                                      subfolder_name, "/", datasetname, "_",
                                      "sd_AUCperformance_Maximum_hyper_", CViterations,"CVnew_Normalized",".pdf"),
         width = 12, height = 6, units = "in", dpi = 300)
  
  # Strategy 2 - Create data ---------------------------------------
  
  res_final_DT1 <- rbindlist(list(combined_feature_CV_best, random1_feature_CV_best))
  res_final_DT1[, Representation:= as.factor(Representation)]
  res_final_DT1[, V3:= (V2 - original_feature_CV_best[, mean(V2)])/original_feature_CV_best[, sd(V2)]]
  
  
  winning_iterations_combined1 <- res_final_DT1[Representation=="Combined-1", mean(V2)>original_feature_CV_best[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  winning_iterations_random1 <- res_final_DT1[Representation=="Random-1", mean(V2)>original_feature_CV_best[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  
  res_final_DT1[Representation=="Combined-1" & features_Iteration %in% winning_iterations_combined1, Group:= "Win"]
  res_final_DT1[Representation=="Random-1" & features_Iteration %in% winning_iterations_random1, Group:= "Win"]
  res_final_DT1[is.na(Group), Group:= "Defeat"]
  
  
  winning_combined1 <- paste0(100 * round(length(winning_iterations_combined1)/length(list_combined_1), 2), "%")
  winning_random1 <- paste0(100 * round(length(winning_iterations_random1)/length(list_one_randomOD), 2), "%")
  
  mean_val1 <- res_final_DT1[, mean(V2), by=Representation]
  mean_val_sd1 <- res_final_DT1[, mean(V3), by=Representation]
  
  supp.lab <- c(glue("{winning_combined1} of the random Combined datasets outperform the Original features
                    Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val1[Representation=='Combined-1',V1], 4)}
                    Black line is  {round(mean_val_sd1[Representation=='Combined-1',V1], 4)} Standard Deviations away from Red line"), 
                glue("{winning_random1} of the random datasets composed only of Outlier Scores, outperform the Original features
                    Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val1[Representation=='Random-1',V1], 4)}
                    Black line is {round(mean_val_sd1[Representation=='Random-1',V1], 4)} Standard Deviations away from Red line"))
  names(supp.lab) <- c("Combined-1", "Random-1")
  
  
  p2 <- ggplot(data = res_final_DT1) +
    aes(x = features_Iteration, y = V2, fill = Group) +
    geom_boxplot() +
    theme_bw()+
    facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.lab))+
    scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
    geom_hline(yintercept = original_feature_CV_best[, mean(V2)], color = "red",)+
    geom_hline(data = mean_val1, aes(yintercept=V1)) +
    # stat_summary(fun.y = mean, color = "darkred", 
    #              geom = "point", shape = 18, size = 3,
    #              show.legend = FALSE)+
    scale_fill_manual(values=c("#907509", "#0b1a8c"))+
    labs(x = "Random Combined Datasets", 
         y = "AUC",
         title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_feature_CV_best[, round(mean(V2), 4)]}"))
  
  p2
  
  ggsave(plot = p2, filename = paste0(final_path_to_save, "figures/",  
                                      subfolder_name, "/", datasetname, "_",
                                      "AUCperformance_Median_hyper_", CViterations, "CVnew_Normalized",".pdf"),
         width = 12, height = 6, units = "in", dpi = 300)
  
  
  p3 <- ggplot(data = res_final_DT1) +
    aes(x = features_Iteration, y = V3, fill = Group) +
    geom_boxplot() +
    theme_bw()+
    facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.lab))+
    scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
    geom_hline(yintercept = 0, color = "red",)+
    geom_hline(data = mean_val_sd1, aes(yintercept=V1)) +
    # stat_summary(fun.y = mean, color = "darkred", 
    #              geom = "point", shape = 18, size = 3,
    #              show.legend = FALSE)+
    scale_fill_manual(values=c("#907509", "#0b1a8c"))+
    labs(x = "Random Combined Datasets", 
         y = "Standard Deviations of Mean AUC of Original-View",
         title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_feature_CV_best[, round(mean(V2), 4)]}"))
  
  p3
  
  ggsave(plot = p3, filename = paste0(final_path_to_save, "figures/",  
                                      subfolder_name, "/", datasetname, "_",
                                      "sd_AUCperformance_Median_hyper_", CViterations ,"CVnew_Normalized",".pdf"),
         width = 12, height = 6, units = "in", dpi = 300)
  
  
}



# start_exp <- Sys.time()
# get_CV_experiments(datasetname = "Waveform_withoutdupl_norm_v01", 
#                    subfolder_name = "Waveform")
# stop_exp <- Sys.time()
# 
# 
# get_CV_experiments(datasetname = "Pima_withoutdupl_norm_02_v01",
#                    subfolder_name = "Pima")
# print(Sys.time())
# get_CV_experiments(datasetname = "Pima_withoutdupl_norm_05_v07",
#                    subfolder_name = "Pima")
# print(Sys.time())
# 
# 
# get_CV_experiments(datasetname = "Shuttle_withoutdupl_norm_v01",
#                    subfolder_name = "Shuttle")
# print(Sys.time())
# 
# 
# get_CV_experiments(datasetname = "Shuttle_withoutdupl_norm_v05", 
#                    subfolder_name = "Shuttle")
# 
# get_CV_experiments(datasetname = "Stamps_withoutdupl_norm_02_v06",
#                    subfolder_name = "Stamps")
# print(Sys.time())
# 
# get_CV_experiments(datasetname = "WDBC_withoutdupl_norm_v07", 
#                    subfolder_name = "WDBC")


# get_CV_experiments(datasetname = "Ionosphere_withoutdupl_norm",
#                    experiments = "OC_combined_CV",
#                    subfolder_name = "Ionosphere",
#                    CViterations =  300)
# print(Sys.time())
# 
# 
get_CV_experiments(datasetname = "HeartDisease_withoutdupl_norm_02_v01",
                    experiments = "OC_combined_CV",
                    subfolder_name = "HeartDisease",
                    CViterations =  10)
print(Sys.time())


get_CV_experiments(datasetname = "Stamps_withoutdupl_norm_02_v01",
                   experiments = "OC_combined_CV",
                   subfolder_name = "Stamps",
                   CViterations =  10)
print(Sys.time())




# get_CV_experiments(datasetname = "Shuttle_withoutdupl_norm_v05",
#                    subfolder_name = "Shuttle")
# 
# print(Sys.time())
