create_unsupervised_view <- function(datasetname, percentage_OD, mixed_view_features) {
  
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


get_original_view_scores <- function(datasetname, Iter, random_normal) {
  
  DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  
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


run_unsupervised_many_times1 <- function(datasetname, Iter, percentage_OD, mixed_view_features, Iter_outlier_features, random_normal) {
  
  DToriginal <- fread(paste0("data/derived-data/", datasetname,".csv"))
  random_sample <- random_normal
  
  list_random_outlier_features <- list()
  final_DT_list <- list()
  all_views <- list()
  for(k in 1:Iter_outlier_features){
    list_DTview2 <- create_unsupervised_view(datasetname, percentage_OD, mixed_view_features)
    list_random_outlier_features[[k]] <- purrr::map(list_DTview2, names)
    
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
    DT_list <- list()
    jj <- 1
    for(dataset_view in list_DTview2){
      
      dataset_view[, Label:= DToriginal$Label]
      dataset_view[, id:= DToriginal$id]
      
      auc_dataset_view <- list()
      res_dataset_view <- list()
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
                                         Label = specificsDTtest$Label,
                                         id = specificsDTtest$id)
        
        # OCSVM_scoresDT_out[, Iteration:= ii]
        auc_dataset_view[[ii]] <- OCSVM_scoresDT_out[, auc(Label, Scores)]
        res_dataset_view[[ii]] <- OCSVM_scoresDT_out
      }
      temp <- as.data.table(unlist(auc_dataset_view))
      auc_DT_list[[jj]] <- temp
      
      DT_list[[jj]] <- as.data.table(rbindlist(res_dataset_view))
      
      jj <- jj + 1
      
    } 
    auc_DT_view2 <- rbindlist(auc_DT_list)
    DT_view2 <- rbindlist(DT_list)
    
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
    DT_view2[, Iteration:=k]
    final_DT_list[[k]] <- auc_DT_view2
    all_views[[k]] <- DT_view2
    
     }
  all_iterations_DT <- rbindlist(final_DT_list)
  DT_view3 <- rbindlist(all_views)
  return(list(all_iterations_DT, list_random_outlier_features, DT_view3))
}



# Run the experiment ------------------------------------------------------
get_hdbscan_res <- function(selected_dataset, times_random_outlier_features, input_percentage_OD, given_minPts, random_normal_input1) {
  
  Iterations <- 1
  # get all the datasets for all the outlier-views
  random_outlier_view <<- create_unsupervised_view(datasetname = selected_dataset,
                                                  percentage_OD = input_percentage_OD, 
                                                  mixed_view_features = 1)
  # get the OCSVM scores for the original-view
  original_view_scores1 <- get_original_view_scores(datasetname = selected_dataset, 
                                                   Iter = Iterations, 
                                                   random_normal = random_normal_input1)
  
  # get the randomly selected features for the original-view
  outlier_views <- run_unsupervised_many_times1(datasetname = selected_dataset, 
                                                Iter = Iterations, 
                                                percentage_OD = input_percentage_OD, 
                                                mixed_view_features = 1, 
                                                Iter_outlier_features = times_random_outlier_features, 
                                                random_normal = random_normal_input1)
  
  outlier_views_scores <- outlier_views[[3]]
  
  test_points <- dim(outlier_views_scores)[1]/(times_random_outlier_features * 14)
  representation1 <- c(rep("KNN", test_points),
                       rep("KNNW", test_points),
                       rep("LOF", test_points),
                       rep("SimplifiedLOF", test_points),
                       rep("LoOP", test_points),
                       rep("LDOFs", test_points),
                       rep("ODIN", test_points),
                       rep("FastABOD", test_points),
                       rep("KDEOS", test_points),
                       rep("LDF", test_points),
                       rep("INFLO", test_points),
                       rep("COF", test_points),
                       rep("1-random", test_points),
                       rep("many-random", test_points))
  
  outlier_views_scores[, Representation:=   rep(representation1, times_random_outlier_features)]
  
  
  # check the auc and find which view performs better than the original & which worse
  auc_outlier_views <- outlier_views[[1]]
  auc_original_view <- original_view_scores1[, auc(Label, Scores)][[1]]
  
  better <- auc_outlier_views[V1 > auc_original_view]
  better[, `Representation-Iter`:= paste0(Representation, "_", Iteration)]
  worse <- auc_outlier_views[V1 < auc_original_view]
  worse[, `Representation-Iter`:= paste0(Representation, "_", Iteration)]
  
  

  all_scores1 <- rbindlist(list(outlier_views_scores, original_view_scores1))
  Labels_id <- copy(all_scores1[, .(Label, id)])
  all_scores1[, `Representation-Iter`:= paste0(Representation, "_", Iteration)]
  all_scores1[, `:=` (Label = NULL, id = NULL, Iteration = NULL, Representation = NULL)]
  print(all_scores1)
  all_scores <- all_scores1[, (Scores - mean(Scores))/ sd(Scores), by =`Representation-Iter`]
  print(all_scores)
  setnames(all_scores, "V1", "Scores")
  
  all_scores[, group:= rep(1:test_points, (14 * times_random_outlier_features) + 1)]
  all_scores[, `Representation-Iter`:= as.factor(`Representation-Iter`)]
  #all_scores[, id:= 1:.N]
  all_scores_casted <<- dcast(all_scores, group~`Representation-Iter`, value.var = "Scores")
  all_scores_casted[, group:=NULL]
  all_scores_transp <<- data.table::transpose(all_scores_casted)
  
  print(all_scores_transp[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dim(all_scores_transp)[2]])
  # dist_all_scores <- dist(all_scores_transp, method = "euclidean")
  hdbscan_res <- dbscan::hdbscan(all_scores_transp, minPts = given_minPts)
  
  return(list(hdbscan_res = hdbscan_res, 
              output_all_scores_casted = all_scores_casted, 
              better = better, 
              worse = worse, 
              output_auc_original_view = auc_original_view,
              output_original_view_scores = original_view_scores1,
              output_Labels_id = Labels_id, 
              output_outlier_views = outlier_views, 
              selected_random_normal = random_normal_input1))
  
}


# create multiple train-test to give it as input for the dbscan
selected_dataset <- "Pima_withoutdupl_norm_02_v01"
random_normal_input_001 <- list()
for(i in 1:3){
  random_normal_input_001[[i]] <- get_random_class_sample(datasetname = selected_dataset, 
                                                     normal_sample_size = 0.01, 
                                                     Iter = 1)
  }

random_normal_input_005 <- list()
for(i in 1:3){
random_normal_input_005[[i]] <- get_random_class_sample(datasetname = selected_dataset, 
                                                   normal_sample_size = 0.05, 
                                                   Iter = 1)
  }

random_normal_input_01 <- list()
for(i in 1:3){
random_normal_input_01[[i]] <- get_random_class_sample(datasetname = selected_dataset, 
                                                   normal_sample_size = 0.1, 
                                                   Iter = 1)
  }

random_normal_input_02<- list()
for(i in 1:3){
random_normal_input_02[[i]] <- get_random_class_sample(datasetname = selected_dataset, 
                                                  normal_sample_size = 0.2, 
                                                  Iter = 1)
}

# We run the get_hdbscan for each random sample ---------------------------


input_iter_outlier_features <- 10
input_percentage_ODfeatures <- j


input_dataset <- "Pima_withoutdupl_norm_02_v01"
rrr <- get_hdbscan_res(selected_dataset = input_dataset,
                times_random_outlier_features = input_iter_outlier_features,
                input_percentage_OD = 0.2,
                given_minPts = 2, 
                random_normal_input1 = random_normal_input_001[[3]])



list_outer <- list()
for(outter in 1:(length(random_normal_input_001))){
  list_Pima_withoutdupl_norm_02_v01 <- list()
  k <- 1
    for(j in c(0.2)){

      performance_res <- get_hdbscan_res(selected_dataset = input_dataset,
                                         times_random_outlier_features = input_iter_outlier_features,
                                         input_percentage_OD = j,
                                         given_minPts = 2, 
                                         random_normal_input1 = random_normal_input_001[[k]])
      
      
      # Check at which cluster the "Original-View_1" belongs
      for(i in 0:max(performance_res$hdbscan_res[[1]])){
        if("Original-View_1" %in%  names(performance_res$output_all_scores_casted[, .SD, .SDcols = which(performance_res$hdbscan_res$cluster == i)])==T){
          print(i)
          found_algos <- names(performance_res$output_all_scores_casted[, .SD, .SDcols = which(performance_res$hdbscan_res$cluster == i)])
          print(found_algos)
        }
      }
      
      random_cluster_ensemble <- list()
      for(i in 1:max(performance_res$hdbscan_res[[1]])){
      temp <- sample(names(performance_res$output_all_scores_casted[, .SD, .SDcols = which(performance_res$hdbscan_res$cluster == i)]), size = 1)
      print(paste0("cluster: ", i))
      random_cluster_ensemble[[i]] <- All_ScoresDT[`Representation-Iter` %in% temp]
      print("========")
      }
      
      temp99 <- rbindlist(random_cluster_ensemble)
      temp99_res <- temp99[, mean(Scores), by = id]
      temp99_res[, Label:= performance_res[[7]][1:(dim(temp99_res)[1]), Label]]
      
      random_cluster_ensembleDT <- data.table(V1 = temp99_res[, auc(Label, V1)][1], 
                               `Representation-Iter` = "random_cluster_ensemble")
      
      DT <- rbindlist(list(performance_res$better, performance_res$worse))
      DT[, Representation:= NULL]
      DT[, Iteration:= NULL]
      originalDT <- data.table(V1 = performance_res$output_auc_original_view[[1]], `Representation-Iter` = "Original-View_1")
      DTall <- bind_rows(DT, originalDT)
      
      All_ScoresDT <- performance_res[[8]][[3]]
      All_ScoresDT[, `Representation-Iter`:= paste0(Representation, "_", Iteration)]
      All_ScoresDT[, `:=` (Iteration = NULL, Representation = NULL)]
      
      original_scoresDT <- performance_res[[6]]
      original_scoresDT[, `Representation-Iter`:= paste0(Representation, "_", Iteration)]
      original_scoresDT[, `:=` (Iteration = NULL, Representation = NULL)]
      final_DT <- rbindlist(list(All_ScoresDT, original_scoresDT))
      
      found_alos_scoresDT <- copy(final_DT[`Representation-Iter` %in% found_algos])
      found_alos_scoresDT[, .N, by = `Representation-Iter`]
      
      
      # double chack performance of the found algos
      performance_res$better[`Representation-Iter`%in%  found_algos]
      performance_res$worse[`Representation-Iter`%in%  found_algos]
      DTall[`Representation-Iter`%in%  found_algos]
      
      
      
      mean_Scores <- found_alos_scoresDT[, mean(Scores), by = id]
      mean_Scores[, Label:= performance_res[[7]][1:(dim(mean_Scores)[1]), Label]]
      ensembleDT <- data.table(V1 = mean_Scores[, auc(Label, V1)][1], 
                               `Representation-Iter` = "Ensemble")
      
      
      print(random_cluster_ensembleDT)
      print(originalDT)
      print(ensembleDT)
      
      
      
      k <- k +1
      }}
      
  
      

      
      
      
      
      
      

      
      
      

# random ensemble ---------------------------------------------------------
      random_1feature <- All_ScoresDT[`Representation-Iter` %in% paste0("1-random_", 1:input_iter_outlier_features)] 
      random_1feature_mean <- random_1feature[, mean(Scores), by = id]
      random_1feature_mean[, Label:= random_1feature[`Representation-Iter` == "1-random_1", Label]]
      random_1DT <- data.table(V1 = random_1feature_mean[, auc(Label, V1)][1], 
                               `Representation-Iter` = "Random_Ensemble_1")
      
      random_20feature <- All_ScoresDT[`Representation-Iter` %in% sample(unique(`Representation-Iter`), size = 20)] 
      random_20feature_mean <- random_20feature[, mean(Scores), by = id]
      random_20feature_mean[, Label:= random_20feature[`Representation-Iter` == random_20feature[1, `Representation-Iter`], Label]]
      
      random_20DT <- data.table(V1 = random_20feature_mean[, auc(Label, V1)][1], 
                              `Representation-Iter` = "Random_Ensemble_many")
      
      
      resDT <- DTall[`Representation-Iter` %in%  found_algos]
      
      
      list_Pima_withoutdupl_norm_02_v01[[k]] <- rbindlist(list(resDT, ensembleDT, random_1DT, random_20DT))
      k <- k+1
    }
  }
  
  ens <- list()
  orig <- list()
  for(v in 1:length(list_Pima_withoutdupl_norm_02_v01)){
    ens[[v]] <- list_Pima_withoutdupl_norm_02_v01[[v]][`Representation-Iter` == "Ensemble", V1]
    orig[[v]] <- list_Pima_withoutdupl_norm_02_v01[[v]][`Representation-Iter` == "Original-View_1", V1]
  }
  
  
  
  performance_DT <- data.table(V1 = c(unlist(orig), unlist(ens)),
                                representation = c(rep("Original", 9), rep("Selective-Ensemble", 9)),
                                Iteration = outter)
  list_outer[[outter]] <- performance_DT
  }

scores_resultsDT <- rbindlist(list_outer)
scores_resultsDT[, Iteration:=as.factor(Iteration)]
fwrite(scores_resultsDT, "data/derived-data/Pima_withoutdupl_norm_02_v01_cross_training.csv")


scores_resultsDT <- fread("data/derived-data/Ionosphere_withoutdupl_norm_cross_training.csv")
scores_resultsDT[, Iteration:=as.factor(Iteration)]
p <- ggplot(data = scores_resultsDT) +
  aes(x = Iteration, y = V1, fill = representation) +
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="line", size = 1.3, aes(group=representation)) +
  theme_minimal() + 
  labs(title = "Ionosphere_withoutdupl_norm", y = "AUC")


p + scale_color_manual(values=c("#fa360c", "#b8b500")) 

ggsave(plot = p, 
       filename = "figures/sample_OD_many/Ionosphere_withoutdupl_norm1_cross_training.pdf", 
       width = 14, height = 7, units = "in", dpi = 300)

esquisse::esquisser()
library(ggplot2)

p1 <- ggplot(data = scores_resultsDT) +
  aes(x = representation, y = V1, fill = representation) +
  geom_boxplot() +
  theme_minimal() + 
  labs(title = "Pima_withoutdupl_norm_02_v01", y = "AUC")

ggsave(plot = p1, 
       filename = "figures/sample_OD_many/Pima_withoutdupl_norm_02_v01_cross_training_allIter.pdf", 
       width = 14, height = 7, units = "in", dpi = 300)

# standardize results based on Original 
oirg_V1 <- scores_resultsDT[representation == "Original", V1]
scores_resultsDT[representation == "Selective-Ensemble", V1:= (V1-mean(oirg_V1))/sd(V1)]

p2 <- ggplot(data = scores_resultsDT) +
  aes(x = representation, y = V1, fill = representation) +
  geom_boxplot() +
  theme_minimal() + 
  labs(title = "Pima_withoutdupl_norm_02_v01", y = "AUC")


ggsave(plot = p2, 
       filename = "figures/sample_OD_many/Pima_withoutdupl_norm_02_v01_cross_training_allIter_Standardized.pdf", 
       width = 14, height = 7, units = "in", dpi = 300)




# randomly select OS ------------------------------------------------------



# # find the best scores and compare it with Original-view ------------------
# algo1 <- as.data.table(performance_res[[8]][[3]][`Representation-Iter` == "1-random_10", Scores])
# algo1[, Method:= "1-random_10"]
# 
# algo2 <- as.data.table(performance_res[[6]][, Scores])
# algo2[, Method:= "Original"]
# 
# algo3 <- as.data.table(performance_res[[8]][[3]][`Representation-Iter` == "FastABOD_8", Scores])
# algo3[, Method:= "FastABOD_8"]
# 
# algos <- rbindlist(list(algo1, algo2, algo3))
# 
# p<-ggplot(algos, aes(x=V1, fill=Method)) +
#   geom_density(alpha=0.4)





