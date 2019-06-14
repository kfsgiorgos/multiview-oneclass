# load packages & source scripts ------------------------------------------
setwd("~/GitHub_projects/multiview-oneclass/")
source("R/load-packages.R")
#use_condaenv("r-reticulate")
reticulate::source_python("Python/sklearn-outlier-algos.py")


create_unsupervised_view <- function(datasetname, percentage_OD, mixed_view_features) {
  
  # DToutliers1 <- fread(paste0("data/derived-data/", datasetname, ".results.csv"))
  DToutliers1 <- fread(paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".results.csv"))
  
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
  
  SimplifiedLOFs1 <- paste0("SimplifiedLOF-00", 1:9)
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
  
  COFs1 <- paste0("COF-00", 1:9)
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
  
  
  
  return(list(KNNs = DToutliersKNNs, KNNWs = DToutliersKNNWs,
              LOFs = DToutliersLOFs, SimplifiedLOFs = DToutliersSimplifiedLOFs,
              LoOPs = DToutliersLoOPs, LDOFs = DToutliersLDOFs,
              ODINs = DToutliersODINs, FastABODs = DToutliersFastABODs, 
              KDEOSs = DToutliersKDEOSs, LDFs = DToutliersLDFs, 
              INFLOs = DToutliersINFLOs, COFs = DToutliersCOFs, 
              mixed_arthur = DToutliers_all,
              mixed_random_percentage = DToutliers_all1))
}


get_random_class_sample <- function(normal_sample_size, datasetname, Iter) {
  
  # DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  DToriginal <- fread(paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname,".csv"))
  # The outlier column has to be renamed to Label for consistency.
  setnames(DToriginal, "outlier", "Label")
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:Iter){
    list_train_id[[i]] <- DToriginal[Label == "no", sample(x = id, size = normal_sample_size * dim(DToriginal[Label == "no"])[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  return(list(train = list_train_id, test = list_test_id))
}


get_original_view_scores <- function(datasetname, Iter, random_normal) {
  
  
  # Change paths to make it work for multiple datsets. 

  # DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  DToriginal <- fread(paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname,".csv"))
  setnames(DToriginal, "outlier", "Label")
  # The outlier column has to be renamed to Label for consistency.
  
  
  random_sample <- random_normal
  
  auc_original <- list()
  for(j in 1:Iter){
    
    
    trainDToriginal <- copy(DToriginal[id %in% random_sample[["train"]][[j]]])
    specificsDTtrainorig <- copy(trainDToriginal[, .(id, Label)])
    trainDToriginal[, `:=`(id = NULL, Label = NULL)]
    
    testDToriginal <- copy(DToriginal[id %in% random_sample[["test"]][[j]]])
    specificsDTtestorig <- copy(testDToriginal[, .(id, Label)])
    testDToriginal[, `:=`(id = NULL, Label = NULL)]
    
    
    OCSVM_scoresDT_original <- data.table(Scores = calculate_OCSVM(DTtrain = trainDToriginal, DTtest = testDToriginal),
                                          Label = specificsDTtestorig$Label,
                                          id = specificsDTtestorig$id)
    
    OCSVM_scoresDT_original[, Iteration:=j]
    auc_original[[j]] <- OCSVM_scoresDT_original
    #rm(trainDToriginal)
    #rm(testDToriginal)
  }
  
  auc_original <- as.data.table(rbindlist(auc_original))
  auc_original[, Representation:= rep("Original-View", dim(auc_original)[1])]
  return(auc_original)
  
}


run_unsupervised_multiview_multipletimes <- function(datasetname, percentage_OD, mixed_view_features, Iter_outlier_features, normal_size, Iters_normal_class) {
  

  # DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  DToriginal <- fread(paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname,".csv"))
  setnames(DToriginal, "outlier", "Label")
  
  scores_all_iters_list <- list()
  auc_all_iters_list <- list()
  
  for(Iter_normal in 1:Iters_normal_class){
    print(Iter_normal)
    random_sample <- get_random_class_sample(datasetname = datasetname, 
                                             normal_sample_size = normal_size, Iter = 1)

    list_random_outlier_features <- list()
    final_DT_list <<- list()
    all_views <- list()
    ii <- 1
    for(Iter_features in 1:Iter_outlier_features){
      
      print("Iter_features")
      print(Iter_features)
      list_DTview2 <- create_unsupervised_view(datasetname, percentage_OD, mixed_view_features)

      iter_dtasets <- 0
      for(list_elements in list_DTview2){
        iter_dtasets <- iter_dtasets + 1
        dimension <- dim(list_elements)[2]
        if(length(which(list_elements[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension] != 0)) != 0){
          tempDT <- data.table::transpose(as.data.table(list_elements[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension]))
          tempDT[, cols1:=names(list_elements[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension])]
          cols_to_delete <- tempDT[V1!=0, cols1]
          DT <- list_DTview2[[iter_dtasets]]
          cols_to_keep <- setdiff(names(DT), cols_to_delete) 
          list_DTview2[[iter_dtasets]] <- DT[, .SD, .SDcols = cols_to_keep]
          
        }
      }
      
      # exclude columns that have NA values
      iter_dtasets <- 0
      for(list_elements in list_DTview2){
        iter_dtasets <- iter_dtasets + 1
        dimension <- dim(list_elements)[2]
        if(length(which(list_elements[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dimension ] != 0)) != 0){
          tempDT <- data.table::transpose(as.data.table(list_elements[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dimension]))
          tempDT[, cols1:=names(list_elements[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dimension])]
          cols_to_delete <- tempDT[V1!=0, cols1]
          DT <- list_DTview2[[iter_dtasets]]
          cols_to_keep <- setdiff(names(DT), cols_to_delete) 
          list_DTview2[[iter_dtasets]] <- DT[, .SD, .SDcols = cols_to_keep]
        }
      }
      
      
      auc_DT_list <- list()
      DT_list <- list()
      jj <- 1
      for(dataset_view_element in list_DTview2){
        
        dataset_view <- copy(dataset_view_element)
        
        dataset_view[, Label:= DToriginal$Label]
        dataset_view[, id:= DToriginal$id]
        
        train_id_outliers <- random_sample[["train"]][[1]]
        test_id_outliers <- random_sample[["test"]][[1]]
        
        traindataset_view <- dataset_view[id %in% train_id_outliers]
        specificsDTtrain <- copy(traindataset_view[, .(id, Label)])
        traindataset_view[, `:=`(id = NULL, Label = NULL)]
        
        testdataset_view <- dataset_view[id %in% test_id_outliers]
        specificsDTtest <- copy(testdataset_view[, .(id, Label)])
        testdataset_view[, `:=`(id = NULL, Label = NULL)]
        
        OCSVM_scoresDT_out <- data.table(Scores = calculate_OCSVM(DTtrain = traindataset_view, DTtest = testdataset_view),
                                         Label = specificsDTtest$Label,
                                         id = specificsDTtest$id)
        
        # OCSVM_scoresDT_out[, Iteration:= ii]
        auc_DT_list[[jj]] <- OCSVM_scoresDT_out[, auc(Label, Scores)][[1]]
        DT_list[[jj]] <- OCSVM_scoresDT_out
        jj <- jj + 1
      }
      datapoints <- dim(DT_list[[1]])[1]
      DTs <<- copy(rbindlist(DT_list))
      
      auc_DT <<- copy(as.data.table(unlist(auc_DT_list)))
      auc_DT[, Representation:=   c(rep("KNN", 1),
                                    rep("KNNW", 1),
                                    rep("LOF", 1),
                                    rep("SimplifiedLOF", 1),
                                    rep("LoOP", 1),
                                    rep("LDOFs", 1),
                                    rep("ODIN", 1),
                                    rep("FastABOD", 1),
                                    rep("KDEOS", 1),
                                    rep("LDF", 1),
                                    rep("INFLO", 1),
                                    rep("COF", 1),
                                    rep("1-random", 1),
                                    rep("many-random", 1))]
      auc_DT[, `:=` (Normal_Size = normal_size, 
                     Percentage_Random_Features  = percentage_OD)]
      
      
      DTs[, Representation:=   c(rep("KNN", datapoints),
                                 rep("KNNW", datapoints),
                                 rep("LOF", datapoints),
                                 rep("SimplifiedLOF", datapoints),
                                 rep("LoOP", datapoints),
                                 rep("LDOFs", datapoints),
                                 rep("ODIN", datapoints),
                                 rep("FastABOD", datapoints),
                                 rep("KDEOS", datapoints),
                                 rep("LDF", datapoints),
                                 rep("INFLO", datapoints),
                                 rep("COF", datapoints),
                                 rep("1-random", datapoints),
                                 rep("many-random", datapoints))]
      
      DTs[, `:=` (Normal_Size = normal_size,
                  Percentage_Random_Features  = percentage_OD)]
      
      
      # original-view resulted performance ----------------------------------------------------
      # for this specific random sample we want to test the performance of 
      # the OCSVM on the originalview
      original_performance <- get_original_view_scores(datasetname = datasetname, 
                                                       Iter = 1, 
                                                       random_normal = random_sample)
      print(DTs)
      print(original_performance)
      
      original_auc <- data.table(V1 = original_performance[, auc(Label, Scores)][[1]],
                                  Representation = "Original-View",
                                  Normal_Size = normal_size,
                                  Percentage_Random_Features = percentage_OD)
      
      
      auc_DT_final <- rbindlist(list(auc_DT, original_auc))
      
      auc_DT_final[, Normal_Iteration:= Iter_normal]
      auc_DT_final[, Random_Feature_Sample:= ii]
      
      original_performance[, Iteration:= NULL]
      original_performance[, `:=` (Normal_Size = normal_size,
                                   Percentage_Random_Features = percentage_OD)]
      
      final_scores_DT <- rbindlist(list(DTs, original_performance))
      print(final_scores_DT)
      final_scores_DT[, Normal_Iteration:= Iter_normal]
      final_scores_DT[, Random_Feature_Sample:= ii]
      print(final_scores_DT)
      
      final_DT_list[[Iter_features]] <- final_scores_DT
      all_views[[Iter_features]] <- auc_DT_final
      
      ii<- ii+1

    }
    scores_all_iters_list[[Iter_normal]] <<- rbindlist(final_DT_list)
    auc_all_iters_list[[Iter_normal]] <- rbindlist(all_views)
  }
  
  DTscores <- rbindlist(scores_all_iters_list)
  DTauc <- rbindlist(auc_all_iters_list) 
  
  fwrite(DTscores, paste0("~/Downloads/DAMI_datasets/derived_data/OCSVM-1random/", datasetname, 
                          "_OCSVM_scores_1random_normal_class_", 100*normal_size, ".csv"), nThread = 5)
  fwrite(DTauc, paste0("~/Downloads/DAMI_datasets/derived_data/OCSVM-1random/", datasetname, 
                       "_OCSVM_auc_1random_normal_class_", 100*normal_size, ".csv"), nThread = 5)
  
  # fwrite(DTscores, paste0("data/derived-data/OCSVM-multiview/", datasetname, 
  #                         "_OCSVM_scores_1random_normal_class_", 100*normal_size, ".csv"), nThread = 5)
  # fwrite(DTauc, paste0("data/derived-data/OCSVM-multiview/", datasetname, 
  #                      "_OCSVM_auc_1random_normal_class_", 100*normal_size, ".csv"), nThread = 5)
  return(list(DTscores, DTauc))
}



run_unsupervised_multiview_1random <- function(datasetname, mixed_view_features, Iter_outlier_features, normal_size, Iters_normal_class, percentage_OD) {
  
  # DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  DToriginal <- fread(paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname,".csv"))
  setnames(DToriginal, "outlier", "Label")
  
  scores_outter_iters_list <- list()
  auc_outter_iters_list <- list()
  for(Iter_normal in 1:Iters_normal_class){
  
    random_sample <- get_random_class_sample(datasetname = datasetname, 
                                             normal_sample_size = normal_size, Iter = 1)
    
    
    scores_inner_iters_list <- list()
    auc_inner_iters_list <- list()
    
    for(Iter_features in 1:Iter_outlier_features){
      print(glue("Normal sampling iteration {Iter_normal} ."))
      print(glue("Features sampling iteration {Iter_features} ."))
      list_DTview2 <- create_unsupervised_view(datasetname, percentage_OD, mixed_view_features)
      list_elements <- list_DTview2$mixed_arthur
      dimension <- dim(list_elements)[2]
      
      
      
      if(length(which(list_elements[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension] != 0))){
        tempDT <<- data.table::transpose(as.data.table(list_elements[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension]))
        tempDT[, cols1:=names(list_elements[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dimension])]
        cols_to_delete <<- tempDT[V1!=0, cols1]
        print(cols_to_delete)
        cols_to_keep <- setdiff(names(list_elements), cols_to_delete)
        DT <- list_elements[, .SD, .SDcols = cols_to_keep]
        dataset_view <- copy(DT)
      }
      
      # exclude columns that have NA values
      if(length(which(list_elements[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dimension] != 0)) != 0){
        tempDT <- data.table::transpose(as.data.table(list_elements[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dimension]))
        tempDT[, cols1:=names(list_elements[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dimension])]
        cols_to_delete <- tempDT[V1!=0, cols1]
        print(cols_to_delete)
        cols_to_keep <- setdiff(names(list_elements), cols_to_delete)
        DT <- copy(list_elements[, .SD, .SDcols = cols_to_keep])
        dataset_view <- copy(DT)
      }
      
      dataset_view <- copy(list_elements)
      dataset_view[, Label:= DToriginal$Label]
      dataset_view[, id:= DToriginal$id]
      
      train_id_outliers <- random_sample[["train"]][[1]]
      test_id_outliers <- random_sample[["test"]][[1]]
      
      traindataset_view <- dataset_view[id %in% train_id_outliers]
      specificsDTtrain <- copy(traindataset_view[, .(id, Label)])
      traindataset_view[, `:=`(id = NULL, Label = NULL)]
      
      testdataset_view <- dataset_view[id %in% test_id_outliers]
      specificsDTtest <- copy(testdataset_view[, .(id, Label)])
      testdataset_view[, `:=`(id = NULL, Label = NULL)]
      
      OCSVM_scoresDT_out <- data.table(Scores = calculate_OCSVM(DTtrain = traindataset_view, DTtest = testdataset_view),
                                       Label = specificsDTtest$Label,
                                       id = specificsDTtest$id)
      
      
      auc_DT <- as.data.table(OCSVM_scoresDT_out[, auc(Label, Scores)][[1]])
      auc_DT[, `:=` (Normal_Size = normal_size, Representation = "12-Scores-random")]
      
      original_performance <- get_original_view_scores(datasetname = datasetname, 
                                                       Iter = 1, 
                                                       random_normal = random_sample)
      original_auc <- data.table(V1 = original_performance[, auc(Label, Scores)][[1]],
                                 Normal_Size = normal_size,
                                 Representation = "Original-View")
      Iter_auc_DT <- rbindlist(list(auc_DT, original_auc))
      
      
      DT_scores <- data.table::copy(OCSVM_scoresDT_out)
      DT_scores[, `:=` (Representation = "12-Scores-random", 
                        Normal_Size = normal_size)]
      
      original_performance[, `:=` (Normal_Size = normal_size, Iteration = NULL)]
      Iter_scores_DT <- rbindlist(list(DT_scores, original_performance))
      
      Iter_auc_DT[, `:=` (Normal_Iteration = Iter_normal, 
                          Features_Iteration = Iter_features)]
      Iter_scores_DT[, `:=` (Normal_Iteration = Iter_normal,
                             Features_Iteration = Iter_features)]
      
      
      scores_inner_iters_list[[Iter_features]] <- Iter_scores_DT
      auc_inner_iters_list[[Iter_features]] <- Iter_auc_DT
    }
    
    scores_outter_iters_list[[Iter_normal]] <- rbindlist(scores_inner_iters_list)
    auc_outter_iters_list[[Iter_normal]] <- rbindlist(auc_inner_iters_list)
    
  }
    DTscores <- rbindlist(scores_outter_iters_list)
    DTauc <- rbindlist(auc_outter_iters_list) 
  
    
    fwrite(DTscores, paste0("~/Downloads/DAMI_datasets/derived_data/OCSVM-1random/", 
                            datasetname, "_Scores_1random_normal_class_", 100*normal_size, ".csv"), nThread = 5)
    fwrite(DTauc, paste0("~/Downloads/DAMI_datasets/derived_data/OCSVM-1random/", 
                         datasetname, "_AUC_1random_normal_class_", 100*normal_size, ".csv"), nThread = 5)
    
    
  # fwrite(DTscores, paste0("data/derived-data/OCSVM-multiview/", datasetname, "_OCSVM_scores_1random_normal_class_", 100*normal_size, ".csv"), nThread = 5)
  # 
  # fwrite(DTauc, paste0("data/derived-data/OCSVM-multiview/", datasetname, "_OCSVM_auc_1random_normal_class_", 100*normal_size, ".csv"), nThread = 5)
  return(list(DTscores, DTauc))
}





run_unsupervised_multiview_per_dataset <- function(datasetname){
  
  iterations_normal <- 50
  
  list_winners <- list()
  list_auc_ensemble <- list()
  for(normal_ratio in c(0.01, 0.05, 0.1, 0.2)){
    iter <- 1
    results_unsupervised <- run_unsupervised_multiview_1random(datasetname = datasetname, 
                                                               mixed_view_features = 1, 
                                                               Iter_outlier_features = 30, 
                                                               normal_size = normal_ratio, 
                                                               percentage_OD = 1, 
                                                               Iters_normal_class = iterations_normal)
    
    
    auc_results <- results_unsupervised[[2]]
    auc_results[, Features_Iteration:= as.factor(Features_Iteration)]
    
    p <- ggplot(data = auc_results) +
      aes(x = Features_Iteration, y = V1, fill = Representation) +
      geom_boxplot() +
      theme_minimal() +
      theme_minimal() + scale_y_continuous(breaks = seq(0.3, 1.0,0.05)) +
      labs(title = paste0("Multiple-views vs Original-views ", datasetname, 
                          ". Random normal-class: ", normal_ratio), y = "AUC")
    
    ggsave(plot = p, filename = paste0("~/Downloads/DAMI_datasets/figures_OCSVM_1random/",  
                                       datasetname, "_Normalperc_", 100*normal_ratio, "_features_iterations",".pdf"),
           width = 12, height = 6, units = "in", dpi = 300)
    
    
    scores_results <<- results_unsupervised[[1]]
    average_ensemble <<- scores_results[, mean(Scores), by = c("Representation", "Normal_Iteration", "id")]
    Labels <<- scores_results[Normal_Iteration == 1 & Representation == "12-Scores-random" & Features_Iteration == 1, Label]
    times_datapoints_unique <<- dim(average_ensemble)[1]/length(Labels)
    
    average_ensemble[, Label:= rep(Labels, times_datapoints_unique)]
    auc_ensemble <- average_ensemble[, auc(Label, V1), by =  c("Representation", "Normal_Iteration")]
    
    winner_me <- 0
    winner_original <- 0
    for(i in 1:iterations_normal){
      
      tempDT <- auc_ensemble[Normal_Iteration==i]
      if(tempDT[Representation == "12-Scores-random", V1] > tempDT[Representation == "Original-View", V1]){
        winner_me <- winner_me + 1
      } else{
        winner_original <- winner_original + 1
      }
    }
    list_winners[[iter]] <- c(winner_me, winner_original)
    list_auc_ensemble[[iter]] <- auc_ensemble
    
    p1 <- ggplot(data = auc_ensemble) +
      aes(x = Representation, y = V1, fill = Representation) +
      geom_boxplot() +
      theme_minimal() +
      theme_minimal() + scale_y_continuous(breaks = seq(0.3, 1.0,0.05)) +
      theme(legend.position = "none") +
      labs(y = "AUC")
    
    ggsave(plot = p1, filename = paste0("~/Downloads/DAMI_datasets/figures_OCSVM_1random/", 
                                        datasetname, "_Normalperc_", 100*normal_ratio, "_Ensemble", ".pdf"),
           width = 12, height = 6, units = "in", dpi = 300)
    p1
    iter <- iter + 1
  }
  saveRDS(list_winners, paste0("~/Downloads/DAMI_datasets/derived_data/OCSVM-1random/", 
                               datasetname, "_Winners_1random_normal_class_", 100*normal_ratio, ".rds"))
  saveRDS(list_auc_ensemble, paste0("~/Downloads/DAMI_datasets/derived_data/OCSVM-1random/", 
                        datasetname, "_AUCensemble_1random_normal_class_", 100*normal_ratio, ".rds"))
  
  return(list(list_winners, list_auc_ensemble))
  }




