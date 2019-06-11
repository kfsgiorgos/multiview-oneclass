# load packages & source scripts ------------------------------------------

setwd("~/R Language Default Dir/Github-projects/multiview-oneclass/")
source("R/load-packages.R")
use_condaenv("r-reticulate")
reticulate::source_python("Python/sklearn-outlier-algos.py")


run_unsupervised_view <- function(datasetname, percentage_OD, mixed_view_features) {
  
  DToutliers1 <- fread(paste0("data/derived-data/", datasetname, ".results.csv"))
  
  
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
  
  DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:Iter){
    list_train_id[[i]] <- DToriginal[Label == "no", sample(x = id, size = normal_sample_size * dim(DToriginal[Label == "no"])[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  return(list(train = list_train_id, test = list_test_id))
}


run_original_view <- function(datasetname, Iter, random_normal) {
  
  DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  
  random_sample <- random_normal
  
  auc_original <- list()
  for(j in 1:Iter){
    
    # train_id_original[[Iter]] <- random_sample[["train"]][[Iter]]
    # test_id_original[[Iter]] <- random_sample[["test"]][[Iter]]
    
    trainDToriginal <- copy(DToriginal[id %in% random_sample[["train"]][[j]]])
    print(head(trainDToriginal))
    specificsDTtrainorig <- copy(trainDToriginal[, .(id, Label)])
    trainDToriginal[, `:=`(id = NULL, Label = NULL)]
    
    testDToriginal <- copy(DToriginal[id %in% random_sample[["test"]][[j]]])
    specificsDTtestorig <- copy(testDToriginal[, .(id, Label)])
    testDToriginal[, `:=`(id = NULL, Label = NULL)]
    
    
    OCSVM_scoresDT_original <- data.table(Scores = calculate_OCSVM(DTtrain = trainDToriginal, DTtest = testDToriginal),
                                          Label = specificsDTtestorig$Label)
    
    
    auc_original[[j]] <- OCSVM_scoresDT_original[, pROC::auc(Label, Scores)]
    #rm(trainDToriginal)
    #rm(testDToriginal)
  }
  
  auc_original <- as.data.table(unlist(auc_original))
  auc_original[, Representation:= rep("Original-View", dim(auc_original)[1])]
  return(auc_original)
  
}


run_outlier_representation <- function(datasetname, Iter, normal_sample_size, percentage_OD, mixed_view_features, random_normal) {
  
  DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  original_data <- run_original_view(datasetname = datasetname, normal_sample_size = normal_sample_size, Iter = Iter)
  random_sample <- random_normal
  
  
  list_DTview2 <- run_unsupervised_view(datasetname, percentage_OD, mixed_view_features)
  
  # exclude columns that have infinite values
  qq <- 0
  for(w in list_DTview2){
    qq <- qq + 1
    if(length(which(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:(dim(w)[2]) ] != 0)) != 0){
      print(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dim(w)[2]])
      print(qq)
      hh <- data.table::transpose(as.data.table(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dim(w)[2]]))
      hh[, cols1:=names(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dim(w)[2]])]
      cols_to_delete <- hh[V1!=0, cols1]
      DT <- list_DTview2[[qq]]
      cols_to_keep <- setdiff(names(DT), cols_to_delete) 
      list_DTview2[[qq]] <- DT[, .SD, .SDcols = cols_to_keep]
      
    }
  }
  
  # exclude columns that have NA values
  qq <- 0
  for(w in list_DTview2){
    qq <- qq + 1
    if(length(which(w[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:(dim(w)[2]) ] != 0)) != 0){
      print(w[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dim(w)[2]])
      print(qq)
      hh <- data.table::transpose(as.data.table(w[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dim(w)[2]]))
      hh[, cols1:=names(w[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dim(w)[2]])]
      cols_to_delete <- hh[V1!=0, cols1]
      DT <- list_DTview2[[qq]]
      cols_to_keep <- setdiff(names(DT), cols_to_delete) 
      list_DTview2[[qq]] <- DT[, .SD, .SDcols = cols_to_keep]
      
    }
  }
  
  
  auc_DT_list <- list()
  jj <- 1
  for(dataset_view in list_DTview2){
    
    dataset_view[, Label:= DToriginal$Label]
    dataset_view[, id:= DToriginal$id]
    
    auc_dataset_view <- list()
    for(ii in 1:Iter){
      
      train_id_outliers <- random_sample[["train"]][[ii]]
      test_id_outliers <- random_sample[["test"]][[ii]]
      
      traindataset_view <- dataset_view[id %in% train_id_outliers]
      specificsDTtrain <- copy(traindataset_view[, .(id, Label)])
      traindataset_view[, `:=`(id = NULL, Label = NULL)]
      
      testdataset_view <- dataset_view[id %in% test_id_outliers]
      specificsDTtest <- copy(testdataset_view[, .(id, Label)])
      testdataset_view[, `:=`(id = NULL, Label = NULL)]
      
      
      OCSVM_scoresDT_out <- data.table(Scores = calculate_OCSVM(DTtrain = traindataset_view, DTtest = testdataset_view),
                                       Label = specificsDTtest$Label)
      
      auc_dataset_view[[ii]] <- OCSVM_scoresDT_out[, auc(Label, Scores)]
    }
    temp <- as.data.table(unlist(auc_dataset_view))
    auc_DT_list[[jj]] <- temp
    jj <- jj + 1
  } 
  auc_DT_view2 <- rbindlist(auc_DT_list)
  auc_DT_view2[, Representation:=   c(rep("KNN", Iter),
                                      rep("KNNW", Iter),
                                      rep("LOF", Iter),
                                      rep("SimplifiedLOF", Iter),
                                      rep("LoOP", Iter),
                                      rep("LDOFs", Iter),
                                      rep("ODIN", Iter),
                                      rep("FastABOD", Iter),
                                      rep("KDEOS", Iter),
                                      rep("LDF", Iter),
                                      rep("INFLO", Iter),
                                      rep("COF", Iter),
                                      rep("1-random", Iter),
                                      rep("many-random", Iter))]
	
  
  
  final_DT <- rbindlist(list(auc_DT_view2, original_data))
  # final_DT[, as.factor(Representation)]
  final_DT[Representation == "Original-View", group:= "1"]
  final_DT[Representation == "many-random", group:= "2"]
  final_DT[Representation == "1-random", group:= "3"]
  final_DT[!(group %in% c("1", "2", "3")) , group:= "4"]
  final_DT[, group:= as.factor(group)]
  final_DT[, V1:= as.numeric(V1)]
  
  yintercept1 <- final_DT[Representation == "Original-View", mean(V1)]
  p <- ggplot(data = final_DT) +
    aes(x = Representation, y = as.numeric(V1), fill = group) +
    geom_boxplot() +
    theme_minimal() + 
    geom_hline(yintercept = yintercept1) +
    labs(title = paste0(percentage_OD * 100, 
                        "% randomly (just once) selected features for each Representation Algo & ",
                        normal_sample_size* 100, "% randomly selected Normal data (", Iter ," times)."),
         subtitle = paste0("Dataset: ", datasetname), y = "AUC") +
    scale_y_continuous(breaks = seq(0.3, 1.0,0.05))   
  
  p
}


# run_outlier_representation(datasetname = "Ionosphere_withoutdupl_norm", Iter = 50, normal_sample_size = 0.05, percentage_OD = 0.3, mixed_view_features = 1)
# run_outlier_representation(datasetname = "Ionosphere_withoutdupl_norm", Iter = 50, normal_sample_size = 0.1, percentage_OD = 0.3, mixed_view_features = 1)
# run_outlier_representation(datasetname = "Ionosphere_withoutdupl_norm", Iter = 50, normal_sample_size = 0.2, percentage_OD = 0.3, mixed_view_features = 1)
# run_outlier_representation(datasetname = "Ionosphere_withoutdupl_norm", Iter = 50, normal_sample_size = 0.3, percentage_OD = 0.3, mixed_view_features = 1)
# 
# 
# run_outlier_representation(datasetname = "Shuttle_withoutdupl_norm_v01", Iter = 50, normal_sample_size = 0.05, percentage_OD = 0.3, mixed_view_features = 1)
# run_outlier_representation(datasetname = "Shuttle_withoutdupl_norm_v01", Iter = 50, normal_sample_size = 0.1, percentage_OD = 0.3, mixed_view_features = 1)
# run_outlier_representation(datasetname = "Shuttle_withoutdupl_norm_v01", Iter = 50, normal_sample_size = 0.2, percentage_OD = 0.3, mixed_view_features = 1)
# run_outlier_representation(datasetname = "Shuttle_withoutdupl_norm_v01", Iter = 50, normal_sample_size = 0.3, percentage_OD = 0.2, mixed_view_features = 1)
# 
# 
# run_outlier_representation(datasetname = "Stamps_withoutdupl_norm_02_v06", Iter = 50, normal_sample_size = 0.05, percentage_OD = 0.3, mixed_view_features = 1)
# run_outlier_representation(datasetname = "Stamps_withoutdupl_norm_02_v06", Iter = 50, normal_sample_size = 0.1, percentage_OD = 0.3, mixed_view_features = 1)
# run_outlier_representation(datasetname = "Stamps_withoutdupl_norm_02_v06", Iter = 50, normal_sample_size = 0.2, percentage_OD = 0.3, mixed_view_features = 1)
# run_outlier_representation(datasetname = "Stamps_withoutdupl_norm_02_v06", Iter = 50, normal_sample_size = 0.3, percentage_OD = 0.2, mixed_view_features = 1)



run_unsupervised_many_times <- function(datasetname, Iter, percentage_OD, mixed_view_features, Iter_outlier_features, random_normal) {
  
  DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  random_sample <- random_normal
  
  
  final_DT_list <- list()
  for(k in 1:Iter_outlier_features){
    list_DTview2 <- run_unsupervised_view(datasetname, percentage_OD, mixed_view_features)
    
    qq <- 0
    for(w in list_DTview2){
      qq <- qq + 1
      if(length(which(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:(dim(w)[2]) ] != 0)) != 0){
        #print(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dim(w)[2]])
        #print(qq)
        hh <- data.table::transpose(as.data.table(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dim(w)[2]]))
        hh[, cols1:=names(w[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dim(w)[2]])]
        cols_to_delete <- hh[V1!=0, cols1]
        DT <- list_DTview2[[qq]]
        cols_to_keep <- setdiff(names(DT), cols_to_delete) 
        list_DTview2[[qq]] <- DT[, .SD, .SDcols = cols_to_keep]
        
      }
    }
    
    # exclude columns that have NA values
    qq <- 0
    for(w in list_DTview2){
      qq <- qq + 1
      if(length(which(w[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:(dim(w)[2]) ] != 0)) != 0){
        #print(w[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dim(w)[2]])
        #print(qq)
        hh <- data.table::transpose(as.data.table(w[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dim(w)[2]]))
        hh[, cols1:=names(w[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dim(w)[2]])]
        cols_to_delete <- hh[V1!=0, cols1]
        DT <- list_DTview2[[qq]]
        cols_to_keep <- setdiff(names(DT), cols_to_delete) 
        list_DTview2[[qq]] <- DT[, .SD, .SDcols = cols_to_keep]
        
      }
    }
    
    
    auc_DT_list <- list()
    jj <- 1
    for(dataset_view in list_DTview2){
      
      dataset_view[, Label:= DToriginal$Label]
      dataset_view[, id:= DToriginal$id]
      
      auc_dataset_view <- list()
      for(ii in 1:Iter){
        
        train_id_outliers <- random_sample[["train"]][[ii]]
        test_id_outliers <- random_sample[["test"]][[ii]]
        
        traindataset_view <- dataset_view[id %in% train_id_outliers]
        specificsDTtrain <- copy(traindataset_view[, .(id, Label)])
        traindataset_view[, `:=`(id = NULL, Label = NULL)]
        
        testdataset_view <- dataset_view[id %in% test_id_outliers]
        specificsDTtest <- copy(testdataset_view[, .(id, Label)])
        testdataset_view[, `:=`(id = NULL, Label = NULL)]
        
        
        OCSVM_scoresDT_out <- data.table(Scores = calculate_OCSVM(DTtrain = traindataset_view, DTtest = testdataset_view),
                                         Label = specificsDTtest$Label)
        
        auc_dataset_view[[ii]] <- OCSVM_scoresDT_out[, auc(Label, Scores)]
      }
      temp <- as.data.table(unlist(auc_dataset_view))
      auc_DT_list[[jj]] <- temp
      jj <- jj + 1
    } 
    auc_DT_view2 <- rbindlist(auc_DT_list)
    auc_DT_view2[, Representation:=   c(rep("KNN", Iter),
                                        rep("KNNW", Iter),
                                        rep("LOF", Iter),
                                        rep("SimplifiedLOF", Iter),
                                        rep("LoOP", Iter),
                                        rep("LDOFs", Iter),
                                        rep("ODIN", Iter),
                                        rep("FastABOD", Iter),
                                        rep("KDEOS", Iter),
                                        rep("LDF", Iter),
                                        rep("INFLO", Iter),
                                        rep("COF", Iter),
                                        rep("1-random", Iter),
                                        rep("many-random", Iter))]
    
    
    
    auc_DT_view2[, Iteration:=k]
    final_DT_list[[k]] <- auc_DT_view2
    
    
    
    
  }
  all_iterations_DT <- rbindlist(final_DT_list)
  }


run_original_view_outlier_view <- function(datasetname1, Iter1, percentage_OD1, mixed_view_features1, Iter_outlier_features1, normal_sample_size1, random_normal) {
  
  res <- run_unsupervised_many_times(datasetname = datasetname1, Iter = Iter1, 
                                     percentage_OD = percentage_OD1, 
                                     mixed_view_features = mixed_view_features1, 
                                     Iter_outlier_features = Iter_outlier_features1,
                                     random_normal =  random_sample1)
  
  res_original <- run_original_view(datasetname = datasetname1, Iter = Iter1, random_normal = random_sample1)
  res_original[, Iteration:= 1]
  yintercept1 <- res_original[Representation == "Original-View", mean(V1)]
  final_res <- rbindlist(list(res, res_original))
  final_res[, Iteration:= as.factor(Iteration)]
  
  
  p <- ggplot(data = final_res) +
    aes(x = Representation, y = V1, fill = Iteration) +
    geom_boxplot() +
    theme_minimal() + scale_y_continuous(breaks = seq(0.3, 1.0,0.05))+ 
    geom_hline(yintercept = yintercept1) +
   labs(title = paste0(percentage_OD1 * 100,
                       "% randomly selected features for each Representation Algo & ",
                       normal_sample_size1 * 100, "% randomly selected Normal data (", Iter1 ," times)."),
        subtitle = paste0("Dataset: ", datasetname1), y = "AUC")
   
  
  ggsave(plot = p, filename = paste0("figures/sample_OD_many/", datasetname1, 
                                     "_normalsize_", normal_sample_size1, 
                                     "_percentageOD_", percentage_OD1, ".pdf"), 
         width = 14, height = 7, units = "in", dpi = 300)
  
  return(p)
}





DTlist <- c("Ionosphere_withoutdupl_norm", "Shuttle_withoutdupl_norm_v01", 
            "Pima_withoutdupl_norm_05_v07", "Stamps_withoutdupl_norm_02_v06", 
            "Waveform_withoutdupl_norm_v02")

for(dt in DTlist){
  for(i in c(0.05, 0.01, 0.1, 0.2, 0.3)){
    random_sample1 <- get_random_class_sample(normal_sample_size = i, datasetname = dt, Iter = 50)
    for(j in c(0.1, 0.2, 0.3, 0.4, 0.5)){
    start2 <- Sys.time()
    print(start2)
    print(i)
    run_original_view_outlier_view(datasetname1 =  dt, 
                                   Iter1 = 50,
                                   normal_sample_size1 = i, 
                                   percentage_OD1 = j,
                                   mixed_view_features1 = 1, 
                                   Iter_outlier_features1 = 10, 
                                   random_normal = random_sample1)
    Sys.time() - start2
    }
  }
}








