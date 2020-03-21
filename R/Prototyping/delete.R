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
    
    # train_id_original[[Iter]] <- random_sample[["train"]][[Iter]]
    # test_id_original[[Iter]] <- random_sample[["test"]][[Iter]]
    
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

get_hdbscan_res <- function(selected_dataset, Iterations, times_random_outlier_features, input_percentage_OD, input_normal_sample_size, given_minPts) {
  
  # get all the datasets for all the outlier-views
  random_outlier_view <- create_unsupervised_view(datasetname = selected_dataset,
                                                  percentage_OD = input_percentage_OD, 
                                                  mixed_view_features = 1)
  # create the train-test datasets
  random_normal_input <- get_random_class_sample(datasetname = selected_dataset, 
                                                 normal_sample_size = input_normal_sample_size, 
                                                 Iter = Iterations)
  # get the OCSVM scores for the original-view
  original_view_scores1 <- get_original_view_scores(datasetname = selected_dataset, 
                                                    Iter = Iterations, 
                                                    random_normal = random_normal_input)
  
  # get the randomly selected features for the original-view
  outlier_views <- run_unsupervised_many_times1(datasetname = selected_dataset, 
                                                Iter = Iterations, 
                                                percentage_OD = input_percentage_OD, 
                                                mixed_view_features = 1, 
                                                Iter_outlier_features = times_random_outlier_features, 
                                                random_normal = random_normal_input)
  
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
  
  
  # better views
  # better_euclidean <- list()
  # better_minkowski <- list()
  # better_intersection <- list()
  # for(i in 1:dim(better)[1]){
  #   outlier_density_better <- outlier_views_scores[Representation == better[i, Representation] & Iteration == better[i, Iteration], Scores]
  #   x <- rbind(original_density, outlier_density_better)
  #   better_euclidean[[i]] <- philentropy::distance(x, method = philentropy::getDistMethods()[1])
  #   better_minkowski[[i]] <- philentropy::distance(x, method = philentropy::getDistMethods()[3], p = 3)
  #   better_intersection[[i]] <- philentropy::distance(x, method = "intersection")
  # }
  # 
  # better[, euclidean:= as.data.table(unlist(better_euclidean))]
  # better[, minkowski:= as.data.table(unlist(better_minkowski))]
  # better[, intersection:= as.data.table(unlist(better_intersection))]
  # 
  # # worse views
  # worse_euclidean <- list()
  # worse_minkowski <- list()
  # worse_intersection <- list()
  # for(i in 1:dim(worse)[1]){
  #   outlier_density_worse <- outlier_views_scores[Representation == worse[i, Representation] & Iteration == worse[i, Iteration], Scores]
  #   x1 <- rbind(original_density, outlier_density_worse)
  #   worse_euclidean[[i]] <- philentropy::distance(x1, method = philentropy::getDistMethods()[1])
  #   worse_minkowski[[i]] <- philentropy::distance(x1, method = philentropy::getDistMethods()[3], p = 3)
  #   worse_intersection[[i]] <- philentropy::distance(x1, method = "intersection")
  # }
  # worse[, euclidean:= as.data.table(unlist(worse_euclidean))]
  # worse[, minkowski:= as.data.table(unlist(worse_minkowski))]
  # worse[, intersection:= as.data.table(unlist(worse_intersection))]
  
  
  
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
  all_scores_casted <- dcast(all_scores, group~`Representation-Iter`, value.var = "Scores")
  all_scores_casted[, group:=NULL]
  all_scores_transp <- data.table::transpose(all_scores_casted)
  
  
  dist_all_scores <- dist(all_scores_transp, method = "euclidean")
  hdbscan_res <- dbscan::hdbscan(all_scores_transp, minPts = given_minPts)
  
  return(list(hdbscan_res = hdbscan_res, 
              all_scores_casted = all_scores_casted, 
              better = better, 
              worse = worse, 
              auc_original_view = auc_original_view,
              original_view_scores = original_view_scores1,
              Labels_id = Labels_id, 
              outlier_views = outlier_views))
  
}


list_outer <- list()
for(outter in 1:50){
  
  list_Pima_withoutdupl_norm_02_v01 <- list()
  k <- 1
  for(iii in c(0.01, 0.05, 0.1)){
    for(j in c(0.2, 0.3, 0.4)){
      
      
      input_dataset <- "Ionosphere_withoutdupl_norm"#"Stamps_withoutdupl_norm_02_v06" #"Shuttle_withoutdupl_norm_v01"#"Waveform_withoutdupl_norm_v02"
      input_Iterations <- 1
      input_iter_outlier_features <- 10
      input_percentage_ODfeatures <- j
      input_normal_size <- iii
      input_minPts <- 2
      
      
      performance_res <- get_hdbscan_res(selected_dataset = input_dataset, 
                                         Iterations = input_Iterations, 
                                         times_random_outlier_features = input_iter_outlier_features, 
                                         input_percentage_OD =  input_percentage_ODfeatures,
                                         input_normal_sample_size = input_normal_size, 
                                         given_minPts = input_minPts)
      
      # Check at which cluster the "Original-View_1" belongs
      for(i in 0:max(performance_res$hdbscan_res[[1]])){
        if("Original-View_1" %in%  names(performance_res$all_scores_casted[, .SD, .SDcols = which(performance_res$hdbscan_res$cluster == i)])==T){
          print(i)
          found_algos <- names(performance_res$all_scores_casted[, .SD, .SDcols = which(performance_res$hdbscan_res$cluster == i)])
          print(found_algos)
        }
      }
      
      DT <- rbindlist(list(performance_res$better, performance_res$worse))
      DT[, Representation:= NULL]
      DT[, Iteration:= NULL]
      originalDT <- data.table(V1 = performance_res$auc_original_view[[1]], `Representation-Iter` = "Original-View_1")
      DTall <- bind_rows(DT, originalDT)
      
      
      # double chack performance of the found algos
      performance_res$better[`Representation-Iter`%in%  found_algos]
      performance_res$worse[`Representation-Iter`%in%  found_algos]
      DTall[`Representation-Iter`%in%  found_algos]
      
      
      All_ScoresDT <- performance_res[[8]][[3]]
      All_ScoresDT[, `Representation-Iter`:= paste0(Representation, "_", Iteration)]
      All_ScoresDT[, `:=` (Iteration = NULL, Representation = NULL)]
      
      original_scoresDT <- performance_res[[6]]
      original_scoresDT[, `Representation-Iter`:= paste0(Representation, "_", Iteration)]
      original_scoresDT[, `:=` (Iteration = NULL, Representation = NULL)]
      final_DT <- rbindlist(list(All_ScoresDT, original_scoresDT))
      
      found_alos_scoresDT <- copy(final_DT[`Representation-Iter` %in% found_algos])
      found_alos_scoresDT[, .N, by = `Representation-Iter`]
      
      
      mean_Scores <- found_alos_scoresDT[, mean(Scores), by = id]
      mean_Scores[, Label:= performance_res[[7]][1:(dim(mean_Scores)[1]), Label]]
      
      mean_Scores[, auc(Label, V1)]
      resDT <- DTall[`Representation-Iter` %in%  found_algos]
      ensembleDT <- data.table(V1 = mean_Scores[, auc(Label, V1)][1], 
                               `Representation-Iter` = "Ensemble")
      
      list_Pima_withoutdupl_norm_02_v01[[k]] <- rbindlist(list(resDT, ensembleDT))
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
fwrite(scores_resultsDT, paste0("data/derived-data/", input_dataset, "_cross_training.csv"))

p <- ggplot(data = scores_resultsDT) +
  aes(x = Iteration, y = V1, fill = representation) +
  geom_boxplot() +
  theme_minimal() + 
  labs(title = input_dataset, y = "AUC")


ggsave(plot = p, 
       filename = "figures/sample_OD_many/", input_dataset, "_cross_training.pdf", 
       width = 14, height = 7, units = "in", dpi = 300)




# DTlist <- c("Ionosphere_withoutdupl_norm", "Shuttle_withoutdupl_norm_v01", 
#             "Pima_withoutdupl_norm_05_v07", "Stamps_withoutdupl_norm_02_v06", 
#             "Waveform_withoutdupl_norm_v02")



Original_data <- fread("data/derived-data/Ionosphere_withoutdupl_norm.csv")
temp <- run_unsupervised_multiview_multipletimes(datasetname = "Ionosphere_withoutdupl_norm", 
                                                  percentage_OD = 0.2, 
                                                  mixed_view_features = 1, 
                                                  Iter_outlier_features = 2, 
                                                  normal_size = 0.1, 
                                                  Iters_normal_class = 2)
temp1 <- temp[[1]]
featuresKNN <- temp1[Representation=="KNN"  & Percentage_Random_Features==0.2&Normal_Iteration ==2]
tt1 <- as.data.table(featuresKNN[Iteration_Features==1, .(Scores, Label)])
tt1[, Feature:= "Features-Random-Sample 1"]
tt2 <- as.data.table(featuresKNN[Iteration_Features==2, Scores])
tt2[, Feature:= "Features-Random-Sample 2"]
tt <- dplyr::bind_cols(tt1, tt2)
tt[, Feature:= as.factor(Feature)]


original <- Original_data[id %in% temp1[Normal_Iteration==1, id], c(1, 3)]
temp_viz <- dplyr::bind_cols(tt, original)
esquisse::esquisser()


plot(x = featuresKNN[Iteration_Features==1, Scores], y= featuresKNN[Iteration_Features==2, Scores])
featureKNN2 <- temp1[Representation=="KNN" & Iteration_Features==1 & Percentage_Random_Features==0.2&Normal_Iteration==2]


# Read Origina DAMI datasets ----------------------------------------------
dd <- as.data.table(read.arff("realworld/glass.arff"))
dd[, unique(class)]


df1 <- fread("~/Desktop/WBC_withoutdupl_norm_v01.results.csv")
df2 <- fread("~/Desktop/Wilt_withoutdupl_norm_02_v02.results.csv")
#df1 <- fread("~/Downloads/WBC_withoutdupl_norm_v01.results.csv")
df2 <- df1[, lapply(.SD, function(x) round(x, 9)), .SD = 2:dim(df)[2]]
df2[, Label:= df1$Label]
setcolorder(x = df2, neworder = c(dim(df2)[2], 1:(dim(df2)[2]-1)))
df2[, 1:3][1:2]
#fwrite(df2, "~/Downloads/WBC_withoutdupl_norm_v09.results.csv")



data.table::transpose(df1[, lapply(.SD, function(x) max(nchar(x))), .SD = 1:dim(df1)[2]])[, max(V1)]


# for (j in names(df)){
#   set(df,which(is.na(df[[j]])),j,1)
# }
  

temp <- df1[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dim(df)[2]]
temp1 <- data.table::transpose(temp)
temp1[, names1:= names(temp)]
temp1[V1>0]
# fwrite(df, "~/Desktop/Lymphography_withoutdupl_norm_catremoved.results.csv")




read_all_parts_dataset <- function(folder, datasetName, boolean1, iterations1, 
                                   boolean2, iterations2, boolean3, iterations3, 
                                   boolean4, iterations4) {
  
  if(boolean1 == "yes"){
    DT1 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations1,"_iters.fst"), as.data.table = T)
  }
  
  if(boolean2 == "yes"){
    DT2 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations2,"_iters.fst"), as.data.table = T)
    DT2[, Iteration:= Iteration + DT1[, max(Iteration)]]
  }else{DT2 <- NULL}
  
  if(boolean3 == "yes"){
    DT3 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations3, "_iters.fst"), as.data.table = T)
    DT3[, Iteration:= Iteration + DT2[, max(Iteration)]]
  }else{DT3 <- NULL}
  
  if(boolean4 == "yes"){
    DT4 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations4, "_iters.fst"), as.data.table = T)
    DT4[, Iteration:= Iteration + DT3[, max(Iteration)]]
  }else{DT4 <- NULL}
  
  DT <- rbindlist(list(DT1, DT2, DT3, DT4))
  write.fst(x = DT,  path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_final.fst"), compress = 100)
  return(DT)
}

read_all_parts_NONnormalized_dataset <- function(folder, datasetName, boolean1, iterations1, 
                                   boolean2, iterations2, boolean3, iterations3, 
                                   boolean4, iterations4) {
  
  if(boolean1 == "yes"){
    DT1 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations1,"_iters_NONnormalizedOD.fst"), as.data.table = T)
  }
  
  if(boolean2 == "yes"){
    DT2 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations2,"_iters_NONnormalizedOD.fst"), as.data.table = T)
    DT2[, Iteration:= Iteration + DT1[, max(Iteration)]]
  }else{DT2 <- NULL}
  
  if(boolean3 == "yes"){
    DT3 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations3, "_iters_NONnormalizedOD.fst"), as.data.table = T)
    DT3[, Iteration:= Iteration + DT2[, max(Iteration)]]
  }else{DT3 <- NULL}
  
  if(boolean4 == "yes"){
    DT4 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations4, "_iters_NONnormalizedOD.fst"), as.data.table = T)
    DT4[, Iteration:= Iteration + DT3[, max(Iteration)]]
  }else{DT4 <- NULL}
  
  DT <- rbindlist(list(DT1, DT2, DT3, DT4))
  write.fst(x = DT,  path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_final_NONnormalizedOD.fst"), compress = 100)
  return(DT)
}

read_all_parts_corr_normalized_dataset <- function(folder, datasetName, boolean1, iterations1, 
                                                 boolean2, iterations2, boolean3, iterations3, 
                                                 boolean4, iterations4) {
  
  if(boolean1 == "yes"){
    DT1 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations1,"_iters_CorrFixed_and_Normalized.fst"), as.data.table = T)
  }
  
  if(boolean2 == "yes"){
    DT2 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations2,"_iters_CorrFixed_and_Normalized.fst"), as.data.table = T)
    DT2[, Iteration:= Iteration + DT1[, max(Iteration)]]
  }else{DT2 <- NULL}
  
  if(boolean3 == "yes"){
    DT3 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations3, "_iters_CorrFixed_and_Normalized.fst"), as.data.table = T)
    DT3[, Iteration:= Iteration + DT2[, max(Iteration)]]
  }else{DT3 <- NULL}
  
  if(boolean4 == "yes"){
    DT4 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations4, "_iters_CorrFixed_and_Normalized.fst"), as.data.table = T)
    DT4[, Iteration:= Iteration + DT3[, max(Iteration)]]
  }else{DT4 <- NULL}
  
  DT <- rbindlist(list(DT1, DT2, DT3, DT4))
  write.fst(x = DT,  path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_final_CorrFixed_and_Normalized.fst"), compress = 100)
  return(DT)
}


read_all_parts_dataset_repeat <- function(folder, datasetName, boolean1, iterations1, 
                                   boolean2, iterations2, boolean3, iterations3, 
                                   boolean4, iterations4) {
  
  if(boolean1 == "yes"){
    DT1 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations1,"_iters_Repeat.fst"), as.data.table = T)
  }
  
  if(boolean2 == "yes"){
    DT2 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations2,"_iters_Repeat.fst"), as.data.table = T)
    DT2[, Iteration:= Iteration + DT1[, max(Iteration)]]
  }else{DT2 <- NULL}
  
  if(boolean3 == "yes"){
    DT3 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations3, "_iters_Repeat.fst"), as.data.table = T)
    DT3[, Iteration:= Iteration + DT2[, max(Iteration)]]
  }else{DT3 <- NULL}
  
  if(boolean4 == "yes"){
    DT4 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations4, "_iters_Repeat.fst"), as.data.table = T)
    DT4[, Iteration:= Iteration + DT3[, max(Iteration)]]
  }else{DT4 <- NULL}
  
  DT <- rbindlist(list(DT1, DT2, DT3, DT4))
  write.fst(x = DT,  path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_final_Repeat.fst"), compress = 100)
  return(DT)
}


read_all_parts_dataset_Gauss <- function(folder, datasetName, boolean1, iterations1, 
                                          boolean2, iterations2, boolean3, iterations3, 
                                          boolean4, iterations4) {
  
  if(boolean1 == "yes"){
    DT1 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations1,"_iters_Gauss.fst"), as.data.table = T)
  }
  
  if(boolean2 == "yes"){
    DT2 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations2,"_iters_Gauss.fst"), as.data.table = T)
    DT2[, Iteration:= Iteration + DT1[, max(Iteration)]]
  }else{DT2 <- NULL}
  
  if(boolean3 == "yes"){
    DT3 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations3, "_iters_Gauss.fst"), as.data.table = T)
    DT3[, Iteration:= Iteration + DT2[, max(Iteration)]]
  }else{DT3 <- NULL}
  
  if(boolean4 == "yes"){
    DT4 <- fst::read_fst(path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_all_repres_", iterations4, "_iters_Gauss.fst"), as.data.table = T)
    DT4[, Iteration:= Iteration + DT3[, max(Iteration)]]
  }else{DT4 <- NULL}
  
  DT <- rbindlist(list(DT1, DT2, DT3, DT4))
  write.fst(x = DT,  path = paste0("~/Downloads/", folder, "/", datasetName, "_OCSVM_DT_final_Gauss.fst"), compress = 100)
  return(DT)
}


# ALOI --------------------------------------------------------------------
read_all_parts_dataset(folder = "ALOI", datasetName = "ALOI_withoutdupl_norm", 
                       boolean1 = "yes", iterations1 = 3, 
                       boolean2 = "yes", iterations2 = 4, 
                       boolean3 = "yes", iterations3 = 5, 
                       boolean4 = "yes", iterations4 = 6)
read_all_parts_dataset(folder = "ALOI", datasetName = "ALOI_withoutdupl_norm", 
                       boolean1 = "yes", iterations1 = 2, 
                       boolean2 = "noooo", 
                       boolean3 = "noooo", 
                       boolean4 = "noooo")

# Waveform ----------------------------------------------------------------

for( i in 1:9){
  read_all_parts_dataset(folder = "Waveform", datasetName = paste0("Waveform_withoutdupl_norm_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes", iterations2 = 6, 
                         boolean3 = "yes", iterations3 = 9, 
                         boolean4 = "yes", iterations4 = 10)
}
read_all_parts_dataset(folder = "Waveform", datasetName = "Waveform_withoutdupl_norm_v10", 
                       boolean1 = "yes", iterations1 = 5, 
                       boolean2 = "yes", iterations2 = 6, 
                       boolean3 = "yes", iterations3 = 9, 
                       boolean4 = "yes", iterations4 = 10)


for( i in 1:5){
  read_all_parts_corr_normalized_dataset(folder = "Waveform", datasetName = paste0("Waveform_withoutdupl_norm_v0", i), 
                                         boolean1 = "yes", iterations1 = 5, 
                                         boolean2 = "yes", iterations2 = 6, 
                                         boolean3 = "yes", iterations3 = 9, 
                                         boolean4 = "noooo")
  
}
for( i in 1:5){
  read_all_parts_NONnormalized_dataset(folder = "Waveform", datasetName = paste0("Waveform_withoutdupl_norm_v0", i), 
                                       boolean1 = "yes", iterations1 = 5, 
                                       boolean2 = "yes", iterations2 = 6, 
                                       boolean3 = "yes", iterations3 = 9, 
                                       boolean4 = "noooo")
  }


# Shuttle -----------------------------------------------------------------
for(i in 1:9){
  read_all_parts_dataset(folder = "Shuttle", datasetName = paste0("Shuttle_withoutdupl_norm_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes", iterations2 = 6, 
                         boolean3 = "yes", iterations3 = 9, 
                         boolean4 = "yes", iterations4 = 10)
  
}
read_all_parts_dataset(folder = "Shuttle", datasetName = "Shuttle_withoutdupl_norm_v10", 
                                 boolean1 = "yes", iterations1 = 5, 
                                 boolean2 = "yes", iterations2 = 6, 
                                 boolean3 = "yes", iterations3 = 9, 
                                 boolean4 = "yes", iterations4 = 10)


for(i in 1:5){
  read_all_parts_NONnormalized_dataset(folder = "Shuttle", datasetName = paste0("Shuttle_withoutdupl_norm_v0", i), 
                                       boolean1 = "yes", iterations1 = 5, 
                                       boolean2 = "yes", iterations2 = 10, 
                                       boolean3 = "yes", iterations3 = 6,
                                       boolean4 = "yes", iterations4 = 9)
  
}

for(i in 1:5){
  read_all_parts_corr_normalized_dataset(folder = "Shuttle", datasetName = paste0("Shuttle_withoutdupl_norm_v0", i), 
                                       boolean1 = "yes", iterations1 = 5, 
                                       boolean2 = "yes", iterations2 = 10, 
                                       boolean3 = "yes", iterations3 = 6,
                                       boolean4 = "yes", iterations4 = 9)
  
}



for(i in c(1:2, 4:9)){
  read_all_parts_dataset_repeat(folder = "Shuttle", datasetName = paste0("Shuttle_withoutdupl_norm_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes", iterations2 = 6, 
                         boolean3 = "yes", iterations3 = 4, 
                         boolean4 = "noooo")
  
}
read_all_parts_dataset_repeat(folder = "Shuttle", datasetName = "Shuttle_withoutdupl_norm_v10", 
                       boolean1 = "yes", iterations1 = 5, 
                       boolean2 = "yes", iterations2 = 6, 
                       boolean3 = "yes", iterations3 = 4, 
                       boolean4 = "nooo")


for(i in c(1:9)){
  read_all_parts_dataset_Gauss(folder = "Shuttle", datasetName = paste0("Shuttle_withoutdupl_norm_v0", i), 
                                boolean1 = "yes", iterations1 = 5, 
                                boolean2 = "yes", iterations2 = 6, 
                                boolean3 = "yes", iterations3 = 4, 
                                boolean4 = "noooo")
  
}
read_all_parts_dataset_Gauss(folder = "Shuttle", datasetName = "Shuttle_withoutdupl_norm_v10", 
                              boolean1 = "yes", iterations1 = 5, 
                              boolean2 = "yes", iterations2 = 6, 
                              boolean3 = "yes", iterations3 = 4, 
                              boolean4 = "nooo")





iono <- read_all_parts_dataset(folder = "Ionosphere", datasetName = "Ionosphere_withoutdupl_norm", 
                               boolean1 = "yes", iterations1 = 30, 
                               boolean2 = "noooo", 
                               boolean3 = "noooo", 
                               boolean4 = "noooo")

glass <- read_all_parts_dataset(folder = "Glass", datasetName = "Glass_withoutdupl_norm", 
                                boolean1 = "yes", iterations1 = 16, 
                                boolean2 = "yes", iterations2 = 15, 
                                boolean3 = "noooo", 
                                boolean4 = "noooo")


# Annthyroid --------------------------------------------------------------
for(i in 1:9){
  read_all_parts_dataset(folder = "Annthyroid", datasetName = paste0("Annthyroid_withoutdupl_norm_02_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes",iterations2 = 6, 
                         boolean3 = "yes",iterations3 = 9, 
                         boolean4 = "yes", iterations4 = 10)
  
}
read_all_parts_dataset(folder = "Annthyroid", datasetName = "Annthyroid_withoutdupl_norm_02_v10", 
                                boolean1 = "yes", iterations1 = 5, 
                                boolean2 = "yes",iterations2 = 6, 
                                boolean3 = "yes",iterations3 = 9, 
                                boolean4 = "yes", iterations4 = 10)



# Pendigits ---------------------------------------------------------------
for(i in c(1, 3)){
  read_all_parts_dataset(folder = "PenDigits", datasetName = paste0("PenDigits_withoutdupl_norm_v0", i), 
                                  boolean1 = "yes", iterations1 = 15, 
                                  boolean2 = "yes",iterations2 = 16, 
                                  boolean3 = "no", 
                                  boolean4 = "no")
  }
for(i in 4:9){
  print(i)
  read_all_parts_dataset(folder = "PenDigits", datasetName = paste0("PenDigits_withoutdupl_norm_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes",iterations2 = 6, 
                         boolean3 = "yes",iterations3 = 8, 
                         boolean4 = "yes", iterations4 = 11)
}
read_all_parts_dataset(folder = "PenDigits", datasetName = "PenDigits_withoutdupl_norm_v10", 
                       boolean1 = "yes", iterations1 = 5, 
                       boolean2 = "yes",iterations2 = 6, 
                       boolean3 = "yes",iterations3 = 8, 
                       boolean4 = "yes", iterations4 = 11)



# Cardio ---------------------------------------------------------------
for(i in 2:9){
  read_all_parts_dataset(folder = "Cardio", datasetName = paste0("Cardiotocography_withoutdupl_norm_02_v0", i), 
                         boolean1 = "yes", iterations1 = 10, 
                         boolean2 = "yes",iterations2 = 6, 
                         boolean3 = "yes", iterations3 = 9,
                         boolean4 = "yes", iterations4 = 5)
}
read_all_parts_dataset(folder = "Cardio", datasetName = "Cardiotocography_withoutdupl_norm_02_v10", 
                       boolean1 = "yes", iterations1 = 10, 
                       boolean2 = "yes",iterations2 = 6, 
                       boolean3 = "yes", iterations3 = 9,
                       boolean4 = "yes", iterations4 = 5)

# WPBC --------------------------------------------------------------------
read_all_parts_dataset(folder = "WPBC", datasetName = "WPBC_withoutdupl_norm", 
                       boolean1 = "yes", iterations1 = 2, 
                       boolean2 = "yes",iterations2 = 4, 
                       boolean3 = "yes",iterations3 = 24, 
                       boolean4 = "noooo")

# WDBC ---------------------------------------------------------------
for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset(folder = "WDBC", datasetName = paste0("WDBC_withoutdupl_norm_v0", i), 
                         boolean1 = "yes", iterations1 = 15, 
                         boolean2 = "yes",iterations2 = 16, 
                         boolean3 = "no", 
                         boolean4 = "no")
}
read_all_parts_dataset(folder = "WDBC", datasetName = "WDBC_withoutdupl_norm_v10", 
                       boolean1 = "yes", iterations1 = 15, 
                       boolean2 = "yes",iterations2 = 16, 
                       boolean3 = "no", 
                       boolean4 = "no")


# HeartDisease ---------------------------------------------------------------
for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset(folder = "HeartDisease", datasetName = paste0("HeartDisease_withoutdupl_norm_05_v0", i), 
                         boolean1 = "yes", iterations1 = 15, 
                         boolean2 = "yes",iterations2 = 16, 
                         boolean3 = "no", 
                         boolean4 = "no")
}
read_all_parts_dataset(folder = "HeartDisease", datasetName = "HeartDisease_withoutdupl_norm_05_v10", 
                       boolean1 = "yes", iterations1 = 15, 
                       boolean2 = "yes",iterations2 = 16, 
                       boolean3 = "no", 
                       boolean4 = "no")





# PageBlocks ---------------------------------------------------------------
for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset(folder = "PageBlocks", datasetName = paste0("PageBlocks_withoutdupl_norm_02_v0", i), 
                         boolean1 = "yes", iterations1 = 10, 
                         boolean2 = "yes",iterations2 = 6, 
                         boolean3 = "yes", iterations3 = 5, 
                         boolean4 = "no")
}
read_all_parts_dataset(folder = "PageBlocks", datasetName = "PageBlocks_withoutdupl_norm_02_v10", 
                       boolean1 = "yes", iterations1 = 10, 
                       boolean2 = "yes",iterations2 = 6, 
                       boolean3 = "yes", iterations3 = 5, 
                       boolean4 = "no")

for(i in c(1:9)) {
  print(i)
  read_all_parts_corr_normalized_dataset(folder = "PageBlocks", datasetName = paste0("PageBlocks_withoutdupl_norm_02_v0", i), 
                                         boolean1 = "yes", iterations1 = 10, 
                                         boolean2 = "yes",iterations2 = 8, 
                                         boolean3 = "yes", iterations3 = 9, 
                                         boolean4 = "no")
}

read_all_parts_corr_normalized_dataset(folder = "PageBlocks", datasetName = "PageBlocks_withoutdupl_norm_02_v10", 
                                       boolean1 = "yes", iterations1 = 10, 
                                       boolean2 = "yes",iterations2 = 8, 
                                       boolean3 = "yes", iterations3 = 9, 
                                       boolean4 = "no")


for(i in c(1:9)) {
  print(i)
  read_all_parts_NONnormalized_dataset(folder = "PageBlocks", datasetName = paste0("PageBlocks_withoutdupl_norm_02_v0", i), 
                                         boolean1 = "yes", iterations1 = 10, 
                                         boolean2 = "yes",iterations2 = 8, 
                                         boolean3 = "yes", iterations3 = 9, 
                                         boolean4 = "no")
}

read_all_parts_NONnormalized_dataset(folder = "PageBlocks", datasetName = "PageBlocks_withoutdupl_norm_02_v10", 
                                       boolean1 = "yes", iterations1 = 10, 
                                       boolean2 = "yes",iterations2 = 8, 
                                       boolean3 = "yes", iterations3 = 9, 
                                       boolean4 = "no")



# Arrhythmia ---------------------------------------------------------------
for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset(folder = "Arrhythmia", datasetName = paste0("Arrhythmia_withoutdupl_norm_05_v0", i), 
                         boolean1 = "yes", iterations1 = 2, 
                         boolean2 = "yes",iterations2 = 3, 
                         boolean3 = "yes", iterations3 = 4, 
                         boolean4 = "yes", iterations4 = 5)
}
read_all_parts_dataset(folder = "Arrhythmia", datasetName = "Arrhythmia_withoutdupl_norm_05_v10", 
                       boolean1 = "yes", iterations1 = 2, 
                       boolean2 = "yes",iterations2 = 3, 
                       boolean3 = "yes", iterations3 = 4, 
                       boolean4 = "yes", iterations4 = 5)


for(i in c(2:9)) {
  print(i)
  read_all_parts_dataset(folder = "Arrhythmia", datasetName = paste0("Arrhythmia_withoutdupl_norm_05_v0", i), 
                         boolean1 = "yes", iterations1 = 6, 
                         boolean2 = "yes",iterations2 = 8, 
                         boolean3 = "nooo", 
                         boolean4 = "nooo")
}
read_all_parts_dataset(folder = "Arrhythmia", datasetName = "Arrhythmia_withoutdupl_norm_05_v10", 
                       boolean1 = "yes", iterations1 = 6, 
                       boolean2 = "yes",iterations2 = 8, 
                       boolean3 = "nooo", 
                       boolean4 = "nooo")

# Pima ---------------------------------------------------------------
for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset(folder = "Pima", datasetName = paste0("Pima_withoutdupl_norm_02_v0", i), 
                         boolean1 = "yes", iterations1 = 7, 
                         boolean2 = "yes",iterations2 = 8, 
                         boolean3 = "yes", iterations3 = 6, 
                         boolean4 = "yes", iterations4 = 9)
}
read_all_parts_dataset(folder = "Pima", datasetName = "Pima_withoutdupl_norm_02_v10", 
                       boolean1 = "yes", iterations1 = 7, 
                       boolean2 = "yes",iterations2 = 8, 
                       boolean3 = "yes", iterations3 = 6, 
                       boolean4 = "yes", iterations4 = 9)




# SpamBase ---------------------------------------------------------------
for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset(folder = "SpamBase", datasetName = paste0("SpamBase_withoutdupl_norm_02_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes", iterations2 = 6, 
                         boolean3 = "yes", iterations3 = 9, 
                         boolean4 = "yes", iterations4 = 10)
}
read_all_parts_dataset(folder = "SpamBase", datasetName = "SpamBase_withoutdupl_norm_02_v10", 
                       boolean1 = "yes", iterations1 = 5, 
                       boolean2 = "yes", iterations2 = 6, 
                       boolean3 = "yes", iterations3 = 10, 
                       boolean4 = "noooo")


# Wilt --------------------------------------------------------------------

for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset(folder = "Wilt", datasetName = paste0("Wilt_withoutdupl_norm_02_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes",iterations2 = 7, 
                         boolean3 = "yes", iterations3 = 10,  
                         boolean4 = "noooo")
}
read_all_parts_dataset(folder = "Wilt", datasetName = "Wilt_withoutdupl_norm_02_v10", 
                       boolean1 = "yes", iterations1 = 5, 
                       boolean2 = "yes",iterations2 = 7, 
                       boolean3 = "yes", iterations3 = 10,  
                       boolean4 = "noooo")

for(i in c(1:4, 6)) {
  print(i)
  read_all_parts_NONnormalized_dataset(folder = "Wilt", datasetName = paste0("Wilt_withoutdupl_norm_02_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes",iterations2 = 10, 
                         boolean3 = "yes", iterations3 = 15,  
                         boolean4 = "noooo")
}

for(i in c(1:4, 6)) {
  print(i)
  read_all_parts_corr_normalized_dataset(folder = "Wilt", datasetName = paste0("Wilt_withoutdupl_norm_02_v0", i), 
                                       boolean1 = "yes", iterations1 = 5, 
                                       boolean2 = "yes",iterations2 = 6, 
                                       boolean3 = "yes", iterations3 = 9,  
                                       boolean4 = "yes", iterations4 = 10)
}

for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset_repeat(folder = "Wilt", datasetName = paste0("Wilt_withoutdupl_norm_02_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes",iterations2 = 6, 
                         boolean3 = "yes", iterations3 = 4,  
                         boolean4 = "noooo")
}
read_all_parts_dataset_repeat(folder = "Wilt", datasetName = "Wilt_withoutdupl_norm_02_v10", 
                       boolean1 = "yes", iterations1 = 5, 
                       boolean2 = "yes",iterations2 = 6, 
                       boolean3 = "yes", iterations3 = 4,  
                       boolean4 = "noooo")

for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset_Gauss(folder = "Wilt", datasetName = paste0("Wilt_withoutdupl_norm_02_v0", i), 
                                boolean1 = "yes", iterations1 = 5, 
                                boolean2 = "yes",iterations2 = 6, 
                                boolean3 = "yes", iterations3 = 4,  
                                boolean4 = "noooo")
}
read_all_parts_dataset_Gauss(folder = "Wilt", datasetName = "Wilt_withoutdupl_norm_02_v10", 
                              boolean1 = "yes", iterations1 = 5, 
                              boolean2 = "yes",iterations2 = 6, 
                              boolean3 = "yes", iterations3 = 4,  
                              boolean4 = "noooo")




# Stamps --------------------------------------------------------------------
for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset(folder = "Stamps", datasetName = paste0("Stamps_withoutdupl_norm_05_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes",iterations2 = 6, 
                         boolean3 = "yes", iterations3 = 9,  
                         boolean4 = "yes", iterations4 = 10)
}
read_all_parts_dataset(folder = "Stamps", datasetName = "Stamps_withoutdupl_norm_05_v10", 
                       boolean1 = "yes", iterations1 = 5, 
                       boolean2 = "yes",iterations2 = 6, 
                       boolean3 = "yes", iterations3 = 9,  
                       boolean4 = "yes", iterations4 = 10)




# Hepatitis --------------------------------------------------------------------
for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset(folder = "Hepatitis", datasetName = paste0("Hepatitis_withoutdupl_norm_10_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes",iterations2 = 15, 
                         boolean3 = "nooo",  
                         boolean4 = "noooo")
}
read_all_parts_dataset(folder = "Hepatitis", datasetName = "Hepatitis_withoutdupl_norm_10_v10", 
                       boolean1 = "yes", iterations1 = 5, 
                       boolean2 = "yes",iterations2 = 15, 
                       boolean3 = "nooo",  
                       boolean4 = "noooo")

for(i in 1:5) {
  print(i)
  read_all_parts_NONnormalized_dataset(folder = "Hepatitis", datasetName = paste0("Hepatitis_withoutdupl_norm_10_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes",iterations2 = 15, 
                         boolean3 = "yes", iterations3 = 10,  
                         boolean4 = "noooo")
}





# Parkinson --------------------------------------------------------------------
for(i in c(1:9)) {
  print(i)
  read_all_parts_dataset(folder = "Parkinson", datasetName = paste0("Parkinson_withoutdupl_norm_20_v0", i), 
                         boolean1 = "yes", iterations1 = 5, 
                         boolean2 = "yes",iterations2 = 6, 
                         boolean3 = "yes", iterations3 = 9,  
                         boolean4 = "yes", iterations4 = 10)
}
read_all_parts_dataset(folder = "Parkinson", datasetName = "Parkinson_withoutdupl_norm_20_v10", 
                       boolean1 = "yes", iterations1 = 5, 
                       boolean2 = "yes",iterations2 = 6, 
                       boolean3 = "yes", iterations3 = 9,  
                       boolean4 = "yes", iterations4 = 10)



get_normalized_rank <- function(dataset, representation_i, iteration_j) {
  
  DT_augmented <- copy(dataset[Representation == paste0("Augmented_", representation_i) & Iteration == iteration_j])
  DT_unsupervised <- copy(dataset[Representation == paste0("Unsupervised_", representation_i) & Iteration == iteration_j])
  
  ordered_aug <- DT_augmented[order(scores, decreasing = F)]
  ordered_aug[, rank:= 1:.N]
  ordered_aug[, normalized_rank:= (rank-1)/(.N-1)]
  setkey(ordered_aug, id)
  
  ordered_un <- DT_unsupervised[order(scores, decreasing = F)]
  ordered_un[, rank:= 1:.N]
  ordered_un[, normalized_rank:= (rank-1)/(.N-1)]
  setkey(ordered_un, id)
  DT_normalized_rank <- rbindlist(list(ordered_un, ordered_aug))
  return(DT_normalized_rank)
}



get_normalized_DT <- function(given_dataset, max_iterations, number_of_representations) {
  
  
  list_1 <- list()
  k <- 0
  for(i in sample(x = 1:30, size = number_of_representations, replace = F)){
    for(j in 1:max_iterations){
      k <- k + 1
      list_1[[k]] <- get_normalized_rank(dataset = given_dataset, representation_i = i, iteration_j = j)
    }
  }
  
  

  list_2 <- list()
  for(i in 1:max_iterations){
    DT_or1 <- copy(given_dataset[Representation == "Original" & Iteration == i])
    ordered_or <- DT_or1[order(scores, decreasing = F)]
    ordered_or[, rank:= 1:.N]
    ordered_or[, normalized_rank:= (rank-1)/(.N-1)]
    setkey(ordered_or, id)
    list_2[[i]] <- ordered_or
    }
  
  DT1 <- rbindlist(list_1)
  DT2 <- rbindlist(list_2)
  DT <- rbindlist(list(DT1, DT2))
  return(DT)
  
}


get_AUC <- function(given_dataset, dataset_string, number_of_representations1) {
  
  # given_dataset=shuttle10
  DT_normalized_rank <- copy(get_normalized_DT(given_dataset = given_dataset, 
                                               number_of_representations = number_of_representations1,
                                               max_iterations = given_dataset[, max(Iteration)]))
  
  DT_mean <- copy(DT_normalized_rank[, mean(normalized_rank), by = .(id, Representation1, Iteration)])
  temp <- copy(DT_normalized_rank[, .(id, Label)])
  temp1 <- unique(temp)
  DT_mean_labeled <- DT_mean[temp1, on = "id"]
  res <- DT_mean_labeled[, auc(Label, V1), by = .(Representation1, Iteration)]
  res[, col:= as.factor(Representation1)]
  setnames(res, "V1", "AUC")
  setnames(res, "Representation1", "Representation")
  results_DT <- res[, mean(AUC), by = "Representation"]
  results_DT[, dataset:= dataset_string]
  
  return(results_DT)
  
}

get_precision_recall <- function(given_dataset, dataset_string, number_of_representations1) {
  
  DT_normalized_rank <- copy(get_normalized_DT(given_dataset = given_dataset, 
                                               number_of_representations = number_of_representations1,
                                               max_iterations = given_dataset[, max(Iteration)]))
  
  hhh <- DT_normalized_rank[Representation1 != "Original", sum(Outlier), by = .(id, Iteration, Representation1)]
  ppb <- DT_normalized_rank[, .(id, Label)]
  ppb1 <- unique(ppb)
  hhh1 <- hhh[ppb1, on = c("id")]
  dt <- rbindlist(list(hhh1, DT_normalized_rank[Representation=="Original", .(id, Iteration, Representation1, Outlier, Label)]), use.names = F)
  dt[Representation1 %in% c("Unsupervised", 'Augmented') & V1 == 0, sum_Outlier:= 0]
  dt[Representation1 %in% c("Unsupervised", 'Augmented') & V1 != 0, sum_Outlier:= 1]
  dt[Representation1 %in% c("Original") , sum_Outlier:= V1]
  
  
  precision_unsuper <- list()
  precision_augmented <- list()
  precision_original <- list()
  
  recall_unsuper <- list()
  recall_augmented <- list()
  recall_original <- list()
  
  
  for(i in 1: hhh1[, max(Iteration)]){
    temp_iter1 <- dt[Iteration == i, .N, by = .(sum_Outlier, Label, Representation1)]
    
    # Unsupervised
    TP_unsupervised <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Unsupervised"]
    if(dim(TP_unsupervised)[1] == 0){
      TP_unsupervised <- 0
    }else{TP_unsupervised <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Unsupervised", N]}
    FP_unsupervised <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Unsupervised", N]
    
    FN_unsupervised <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Unsupervised"]
    if(dim(FN_unsupervised)[1] == 0 ){
      FN_unsupervised <- 0
    }else{ FN_unsupervised <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Unsupervised", N]}
    
    # Augmented
    TP_augmented <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Augmented"]
    if(dim(TP_augmented)[1] == 0){
      TP_augmented <- 0
    } else{TP_augmented <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Augmented", N]}
    FP_augmented <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Augmented", N]
    FN_augmented <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Augmented"]
    if(dim(FN_augmented)[1] == 0 ){
      FN_augmented <- 0
    }else{ FN_augmented <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Augmented", N]}
    
    
    # Original
    TP_original <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Original"]
    if(dim(TP_original)[1] == 0){
      TP_original <- 0
    }else{TP_original <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Original", N]}
    
    FP_original <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Original"]
    if(dim(FP_original)[1] == 0){
      FP_original <- 0
    }else{FP_original <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Original", N]}
    
    FN_original <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Original"]
    if(dim(FN_original)[1] == 0 ){
      FN_original <- 0
    }else{FN_original <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Original", N]}
    
    precision_unsuper[[i]] <- TP_unsupervised/(TP_unsupervised+FP_unsupervised)
    precision_augmented[[i]] <- TP_augmented/(TP_augmented+FP_augmented)
    precision_original[[i]] <- TP_original/(TP_original+FP_original)
    
    recall_unsuper[[i]] <- TP_unsupervised/(TP_unsupervised+FN_unsupervised)
    recall_augmented[[i]] <- TP_augmented/(TP_augmented+FN_augmented)
    recall_original[[i]] <- TP_original/(TP_original+FN_original)
    
  }
  # Recall
  recall_aug <- data.table(recall = unlist(recall_augmented))
  recall_aug[, Representation:= "Augmented"]
  recall_un <- data.table(recall = unlist(recall_unsuper))
  recall_un[, Representation:= "Unsupervised"]
  recall_or <- data.table(recall = unlist(recall_original))
  recall_or[, Representation:= "Original"]
  
  DT_recall <- rbindlist(list(recall_aug, recall_un, recall_or))
  for (jj in 1:ncol(DT_recall)) set(DT_recall, i = which(is.na(DT_recall[[jj]])), j = jj, v = 0)
  DT_recall_res <- DT_recall[, mean(recall), by = Representation]
  DT_recall_res[, dataset:= dataset_string]
  # Precision
  precision_aug <- data.table(precision = unlist(precision_augmented))
  precision_aug[, Representation:= "Augmented"]
  precision_un <- data.table(precision = unlist(precision_unsuper))
  precision_un[, Representation:= "Unsupervised"]
  precision_or <- data.table(precision = unlist(precision_original))
  precision_or[, Representation:= "Original"]
  
  DT_precision <- rbindlist(list(precision_aug, precision_un, precision_or))
  for (jj in 1:ncol(DT_precision)) set(DT_precision, i = which(is.na(DT_precision[[jj]])), j = jj, v = 0)
  DT_precision_res <- DT_precision[, mean(precision), by = Representation]
  DT_precision_res[, dataset:= dataset_string]
  
  return(list(DT_precision_res, DT_recall_res))
  
}


get_precision_recall_at_N <- function(given_dataset, dataset_string, number_of_representations1) {
  
  DT_normalized_rank <- copy(get_normalized_DT(given_dataset = given_dataset, 
                                               number_of_representations = number_of_representations1,
                                               max_iterations = given_dataset[, max(Iteration)]))
  
  number_of_outliers <- DT_normalized_rank[, .N, by = .(Label)][Label == "yes", N]
  ordered_DT_normalized_rank <- DT_normalized_rank[order(scores, decreasing = F)]
  ordered_DT_normalized_rank[1:(number_of_outliers), Outlier:= 1]
  ordered_DT_normalized_rank[(number_of_outliers+1):.N, Outlier:= 0]
  setkey(ordered_DT_normalized_rank, id, Iteration)
  
  hhh <- DT_normalized_rank[Representation1 != "Original", sum(Outlier), by = .(id, Iteration, Representation1)]
  ppb <- DT_normalized_rank[, .(id, Label)]
  ppb1 <- unique(ppb)
  hhh1 <- hhh[ppb1, on = c("id")]
  dt <- rbindlist(list(hhh1, DT_normalized_rank[Representation=="Original", .(id, Iteration, Representation1, Outlier, Label)]), use.names = F)
  dt[Representation1 %in% c("Unsupervised", 'Augmented') & V1 == 0, sum_Outlier:= 0]
  dt[Representation1 %in% c("Unsupervised", 'Augmented') & V1 != 0, sum_Outlier:= 1]
  dt[Representation1 %in% c("Original") , sum_Outlier:= V1]
  
  
  precision_unsuper <- list()
  precision_augmented <- list()
  precision_original <- list()
  
  recall_unsuper <- list()
  recall_augmented <- list()
  recall_original <- list()
  
  
  for(i in 1: hhh1[, max(Iteration)]){
    temp_iter1 <- dt[Iteration == i, .N, by = .(sum_Outlier, Label, Representation1)]
    
    # Unsupervised
    TP_unsupervised <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Unsupervised"]
    if(dim(TP_unsupervised)[1] == 0){
      TP_unsupervised <- 0
    }else{TP_unsupervised <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Unsupervised", N]}
    FP_unsupervised <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Unsupervised", N]
    
    FN_unsupervised <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Unsupervised"]
    if(dim(FN_unsupervised)[1] == 0 ){
      FN_unsupervised <- 0
    }else{ FN_unsupervised <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Unsupervised", N]}
    
    # Augmented
    TP_augmented <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Augmented"]
    if(dim(TP_augmented)[1] == 0){
      TP_augmented <- 0
    } else{TP_augmented <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Augmented", N]}
    FP_augmented <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Augmented", N]
    FN_augmented <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Augmented"]
    if(dim(FN_augmented)[1] == 0 ){
      FN_augmented <- 0
    }else{ FN_augmented <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Augmented", N]}
    
    
    # Original
    TP_original <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Original"]
    if(dim(TP_original)[1] == 0){
      TP_original <- 0
    }else{TP_original <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Original", N]}
    
    FP_original <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Original"]
    if(dim(FP_original)[1] == 0){
      FP_original <- 0
    }else{FP_original <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Original", N]}
    
    FN_original <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Original"]
    if(dim(FN_original)[1] == 0 ){
      FN_original <- 0
    }else{FN_original <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Original", N]}
    
    precision_unsuper[[i]] <- TP_unsupervised/(TP_unsupervised+FP_unsupervised)
    precision_augmented[[i]] <- TP_augmented/(TP_augmented+FP_augmented)
    precision_original[[i]] <- TP_original/(TP_original+FP_original)
    
    recall_unsuper[[i]] <- TP_unsupervised/(TP_unsupervised+FN_unsupervised)
    recall_augmented[[i]] <- TP_augmented/(TP_augmented+FN_augmented)
    recall_original[[i]] <- TP_original/(TP_original+FN_original)
    
  }
  # Recall
  recall_aug <- data.table(recall = unlist(recall_augmented))
  recall_aug[, Representation:= "Augmented"]
  recall_un <- data.table(recall = unlist(recall_unsuper))
  recall_un[, Representation:= "Unsupervised"]
  recall_or <- data.table(recall = unlist(recall_original))
  recall_or[, Representation:= "Original"]
  
  DT_recall <- rbindlist(list(recall_aug, recall_un, recall_or))
  for (jj in 1:ncol(DT_recall)) set(DT_recall, i = which(is.na(DT_recall[[jj]])), j = jj, v = 0)
  DT_recall_res <- DT_recall[, mean(recall), by = Representation]
  DT_recall_res[, dataset:= dataset_string]
  # Precision
  precision_aug <- data.table(precision = unlist(precision_augmented))
  precision_aug[, Representation:= "Augmented"]
  precision_un <- data.table(precision = unlist(precision_unsuper))
  precision_un[, Representation:= "Unsupervised"]
  precision_or <- data.table(precision = unlist(precision_original))
  precision_or[, Representation:= "Original"]
  
  DT_precision <- rbindlist(list(precision_aug, precision_un, precision_or))
  for (jj in 1:ncol(DT_precision)) set(DT_precision, i = which(is.na(DT_precision[[jj]])), j = jj, v = 0)
  DT_precision_res <- DT_precision[, mean(precision), by = Representation]
  DT_precision_res[, dataset:= dataset_string]
  
  return(list(DT_precision_res, DT_recall_res))
  
}



# Shuttle -----------------------------------------------------------------

shuttle1 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v01_OCSVM_DT_final.fst", as.data.table = T)
shuttle2 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v02_OCSVM_DT_final.fst", as.data.table = T)
shuttle3 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v03_OCSVM_DT_final.fst", as.data.table = T)
shuttle4 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v04_OCSVM_DT_final.fst", as.data.table = T)
shuttle5 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v05_OCSVM_DT_final.fst", as.data.table = T)
shuttle6 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v06_OCSVM_DT_final.fst", as.data.table = T)
shuttle7 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v07_OCSVM_DT_final.fst", as.data.table = T)
shuttle8 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v08_OCSVM_DT_final.fst", as.data.table = T)
shuttle9 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v09_OCSVM_DT_final.fst", as.data.table = T)
shuttle10 <- read_fst(path ="~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v10_OCSVM_DT_final.fst", as.data.table = T)


shuttle_non1 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v01_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
shuttle_non2 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v02_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
shuttle_non3 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v03_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
shuttle_non4 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v04_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
shuttle_non5 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v05_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)


shuttle_corr1 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v01_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
shuttle_corr2 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v02_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
shuttle_corr3 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v03_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
shuttle_corr4 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v04_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
shuttle_corr5 <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v05_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)


shuttle1_repeat <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v01_OCSVM_DT_final_Repeat.fst", as.data.table = T)
shuttle2_repeat <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v02_OCSVM_DT_final_Repeat.fst", as.data.table = T)
shuttle4_repeat <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v04_OCSVM_DT_final_Repeat.fst", as.data.table = T)
shuttle5_repeat <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v05_OCSVM_DT_final_Repeat.fst", as.data.table = T)
shuttle6_repeat <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v06_OCSVM_DT_final_Repeat.fst", as.data.table = T)
shuttle7_repeat <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v07_OCSVM_DT_final_Repeat.fst", as.data.table = T)
shuttle8_repeat <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v08_OCSVM_DT_final_Repeat.fst", as.data.table = T)
shuttle9_repeat <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v09_OCSVM_DT_final_Repeat.fst", as.data.table = T)
shuttle10_repeat <- read_fst(path ="~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v10_OCSVM_DT_final_Repeat.fst", as.data.table = T)


shuttle1_gauss <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v01_OCSVM_DT_final_Gauss.fst", as.data.table = T)
shuttle2_gauss <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v02_OCSVM_DT_final_Gauss.fst", as.data.table = T)
shuttle3_gauss <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v03_OCSVM_DT_final_Gauss.fst", as.data.table = T)
shuttle4_gauss <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v04_OCSVM_DT_final_Gauss.fst", as.data.table = T)
shuttle5_gauss <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v05_OCSVM_DT_final_Gauss.fst", as.data.table = T)
shuttle6_gauss <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v06_OCSVM_DT_final_Gauss.fst", as.data.table = T)
shuttle7_gauss <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v07_OCSVM_DT_final_Gauss.fst", as.data.table = T)
shuttle8_gauss <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v08_OCSVM_DT_final_Gauss.fst", as.data.table = T)
shuttle9_gauss <- read_fst(path = "~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v09_OCSVM_DT_final_Gauss.fst", as.data.table = T)
shuttle10_gauss <- read_fst(path ="~/Downloads/Shuttle/Shuttle_withoutdupl_norm_v10_OCSVM_DT_final_Gauss.fst", as.data.table = T)




# WDBC --------------------------------------------------------------------

wdbc1 <- read_fst(path = "~/Downloads/WDBC/WDBC_withoutdupl_norm_v01_OCSVM_DT_final.fst", as.data.table = T)
wdbc2 <- read_fst(path = "~/Downloads/WDBC/WDBC_withoutdupl_norm_v02_OCSVM_DT_final.fst", as.data.table = T)
wdbc3 <- read_fst(path = "~/Downloads/WDBC/WDBC_withoutdupl_norm_v03_OCSVM_DT_final.fst", as.data.table = T)
wdbc4 <- read_fst(path = "~/Downloads/WDBC/WDBC_withoutdupl_norm_v04_OCSVM_DT_final.fst", as.data.table = T)
wdbc5 <- read_fst(path = "~/Downloads/WDBC/WDBC_withoutdupl_norm_v05_OCSVM_DT_final.fst", as.data.table = T)
wdbc6 <- read_fst(path = "~/Downloads/WDBC/WDBC_withoutdupl_norm_v06_OCSVM_DT_final.fst", as.data.table = T)
wdbc7 <- read_fst(path = "~/Downloads/WDBC/WDBC_withoutdupl_norm_v07_OCSVM_DT_final.fst", as.data.table = T)
wdbc8 <- read_fst(path = "~/Downloads/WDBC/WDBC_withoutdupl_norm_v08_OCSVM_DT_final.fst", as.data.table = T)
wdbc9 <- read_fst(path = "~/Downloads/WDBC/WDBC_withoutdupl_norm_v09_OCSVM_DT_final.fst", as.data.table = T)
wdbc10 <- read_fst(path ="~/Downloads/WDBC/WDBC_withoutdupl_norm_v10_OCSVM_DT_final.fst", as.data.table = T)


# Cardio ------------------------------------------------------------------

cardio2 <- read_fst(path = "~/Downloads/Cardio/Cardiotocography_withoutdupl_norm_02_v02_OCSVM_DT_final.fst", as.data.table = T)
cardio3 <- read_fst(path = "~/Downloads/Cardio/Cardiotocography_withoutdupl_norm_02_v03_OCSVM_DT_final.fst", as.data.table = T)
cardio4 <- read_fst(path = "~/Downloads/Cardio/Cardiotocography_withoutdupl_norm_02_v04_OCSVM_DT_final.fst", as.data.table = T)
cardio5 <- read_fst(path = "~/Downloads/Cardio/Cardiotocography_withoutdupl_norm_02_v05_OCSVM_DT_final.fst", as.data.table = T)
cardio6 <- read_fst(path = "~/Downloads/Cardio/Cardiotocography_withoutdupl_norm_02_v06_OCSVM_DT_final.fst", as.data.table = T)
cardio7 <- read_fst(path = "~/Downloads/Cardio/Cardiotocography_withoutdupl_norm_02_v07_OCSVM_DT_final.fst", as.data.table = T)
cardio8 <- read_fst(path = "~/Downloads/Cardio/Cardiotocography_withoutdupl_norm_02_v08_OCSVM_DT_final.fst", as.data.table = T)
cardio9 <- read_fst(path = "~/Downloads/Cardio/Cardiotocography_withoutdupl_norm_02_v09_OCSVM_DT_final.fst", as.data.table = T)
cardio10 <- read_fst(path ="~/Downloads/Cardio/Cardiotocography_withoutdupl_norm_02_v10_OCSVM_DT_final.fst", as.data.table = T)


# Waveform ----------------------------------------------------------------
waveform1 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v01_OCSVM_DT_final.fst", as.data.table = T)
waveform2 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v02_OCSVM_DT_final.fst", as.data.table = T)
waveform3 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v03_OCSVM_DT_final.fst", as.data.table = T)
waveform4 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v04_OCSVM_DT_final.fst", as.data.table = T)
waveform5 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v05_OCSVM_DT_final.fst", as.data.table = T)
waveform6 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v06_OCSVM_DT_final.fst", as.data.table = T)
waveform7 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v07_OCSVM_DT_final.fst", as.data.table = T)
waveform8 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v08_OCSVM_DT_final.fst", as.data.table = T)
waveform9 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v09_OCSVM_DT_final.fst", as.data.table = T)
waveform10 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v10_OCSVM_DT_final.fst", as.data.table = T)


waveform_corr1 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v01_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
waveform_corr2 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v02_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
waveform_corr3 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v03_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
waveform_corr4 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v04_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
waveform_corr5 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v05_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)


waveform_non1 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v01_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
waveform_non2 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v02_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
waveform_non3 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v03_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
waveform_non4 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v04_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
waveform_non5 <- read_fst(path = "~/Downloads/Waveform/Waveform_withoutdupl_norm_v05_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)



# Annthyroid --------------------------------------------------------------

annthyroid1 <- read_fst(path = "~/Downloads/Annthyroid/Annthyroid_withoutdupl_norm_02_v01_OCSVM_DT_final.fst", as.data.table = T)
annthyroid2 <- read_fst(path = "~/Downloads/Annthyroid/Annthyroid_withoutdupl_norm_02_v02_OCSVM_DT_final.fst", as.data.table = T)
annthyroid3 <- read_fst(path = "~/Downloads/Annthyroid/Annthyroid_withoutdupl_norm_02_v03_OCSVM_DT_final.fst", as.data.table = T)
annthyroid4 <- read_fst(path = "~/Downloads/Annthyroid/Annthyroid_withoutdupl_norm_02_v04_OCSVM_DT_final.fst", as.data.table = T)
annthyroid5 <- read_fst(path = "~/Downloads/Annthyroid/Annthyroid_withoutdupl_norm_02_v05_OCSVM_DT_final.fst", as.data.table = T)
annthyroid6 <- read_fst(path = "~/Downloads/Annthyroid/Annthyroid_withoutdupl_norm_02_v06_OCSVM_DT_final.fst", as.data.table = T)
annthyroid7 <- read_fst(path = "~/Downloads/Annthyroid/Annthyroid_withoutdupl_norm_02_v07_OCSVM_DT_final.fst", as.data.table = T)
annthyroid8 <- read_fst(path = "~/Downloads/Annthyroid/Annthyroid_withoutdupl_norm_02_v08_OCSVM_DT_final.fst", as.data.table = T)
annthyroid9 <- read_fst(path = "~/Downloads/Annthyroid/Annthyroid_withoutdupl_norm_02_v09_OCSVM_DT_final.fst", as.data.table = T)
annthyroid10 <- read_fst(path = "~/Downloads/Annthyroid/Annthyroid_withoutdupl_norm_02_v10_OCSVM_DT_final.fst", as.data.table = T)


ionosphere <- read_fst(path = "~/Downloads/Ionosphere/Ionosphere_withoutdupl_norm_OCSVM_DT_final.fst", as.data.table = T)

glass <- read_fst(path = "~/Downloads/Glass/Glass_withoutdupl_norm_OCSVM_DT_final.fst", as.data.table = T)


# PenDigits ---------------------------------------------------------------

pendigits1 <- read_fst(path = "~/Downloads/PenDigits/PenDigits_withoutdupl_norm_v01_OCSVM_DT_final.fst", as.data.table = T)
pendigits3 <- read_fst(path = "~/Downloads/PenDigits/PenDigits_withoutdupl_norm_v03_OCSVM_DT_final.fst", as.data.table = T)
pendigits4 <- read_fst(path = "~/Downloads/PenDigits/PenDigits_withoutdupl_norm_v04_OCSVM_DT_final.fst", as.data.table = T)
pendigits5 <- read_fst(path = "~/Downloads/PenDigits/PenDigits_withoutdupl_norm_v05_OCSVM_DT_final.fst", as.data.table = T)
pendigits6 <- read_fst(path = "~/Downloads/PenDigits/PenDigits_withoutdupl_norm_v06_OCSVM_DT_final.fst", as.data.table = T)
pendigits7 <- read_fst(path = "~/Downloads/PenDigits/PenDigits_withoutdupl_norm_v07_OCSVM_DT_final.fst", as.data.table = T)
pendigits8 <- read_fst(path = "~/Downloads/PenDigits/PenDigits_withoutdupl_norm_v08_OCSVM_DT_final.fst", as.data.table = T)
pendigits9 <- read_fst(path = "~/Downloads/PenDigits/PenDigits_withoutdupl_norm_v09_OCSVM_DT_final.fst", as.data.table = T)
pendigits10 <- read_fst(path = "~/Downloads/PenDigits/PenDigits_withoutdupl_norm_v10_OCSVM_DT_final.fst", as.data.table = T)

# WPBC --------------------------------------------------------------------

wpbc <- read_fst(path = "~/Downloads/WPBC/WPBC_withoutdupl_norm_OCSVM_DT_final.fst", as.data.table = T)


# HeartDisease ------------------------------------------------------------
heart1 <- read_fst(path = "~/Downloads/HeartDisease/HeartDisease_withoutdupl_norm_05_v01_OCSVM_DT_final.fst", as.data.table = T)
heart2 <- read_fst(path = "~/Downloads/HeartDisease/HeartDisease_withoutdupl_norm_05_v02_OCSVM_DT_final.fst", as.data.table = T)
heart3 <- read_fst(path = "~/Downloads/HeartDisease/HeartDisease_withoutdupl_norm_05_v03_OCSVM_DT_final.fst", as.data.table = T)
heart4 <- read_fst(path = "~/Downloads/HeartDisease/HeartDisease_withoutdupl_norm_05_v04_OCSVM_DT_final.fst", as.data.table = T)
heart5 <- read_fst(path = "~/Downloads/HeartDisease/HeartDisease_withoutdupl_norm_05_v05_OCSVM_DT_final.fst", as.data.table = T)
heart6 <- read_fst(path = "~/Downloads/HeartDisease/HeartDisease_withoutdupl_norm_05_v06_OCSVM_DT_final.fst", as.data.table = T)
heart7 <- read_fst(path = "~/Downloads/HeartDisease/HeartDisease_withoutdupl_norm_05_v07_OCSVM_DT_final.fst", as.data.table = T)
heart8 <- read_fst(path = "~/Downloads/HeartDisease/HeartDisease_withoutdupl_norm_05_v08_OCSVM_DT_final.fst", as.data.table = T)
heart9 <- read_fst(path = "~/Downloads/HeartDisease/HeartDisease_withoutdupl_norm_05_v09_OCSVM_DT_final.fst", as.data.table = T)
heart10 <- read_fst(path = "~/Downloads/HeartDisease/HeartDisease_withoutdupl_norm_05_v10_OCSVM_DT_final.fst", as.data.table = T)


# SpamBase ------------------------------------------------------------
spam1 <- read_fst(path = "~/Downloads/SpamBase/SpamBase_withoutdupl_norm_02_v01_OCSVM_DT_final.fst", as.data.table = T)
spam2 <- read_fst(path = "~/Downloads/SpamBase/SpamBase_withoutdupl_norm_02_v02_OCSVM_DT_final.fst", as.data.table = T)
spam3 <- read_fst(path = "~/Downloads/SpamBase/SpamBase_withoutdupl_norm_02_v03_OCSVM_DT_final.fst", as.data.table = T)
spam4 <- read_fst(path = "~/Downloads/SpamBase/SpamBase_withoutdupl_norm_02_v04_OCSVM_DT_final.fst", as.data.table = T)
spam5 <- read_fst(path = "~/Downloads/SpamBase/SpamBase_withoutdupl_norm_02_v05_OCSVM_DT_final.fst", as.data.table = T)
spam6 <- read_fst(path = "~/Downloads/SpamBase/SpamBase_withoutdupl_norm_02_v06_OCSVM_DT_final.fst", as.data.table = T)
spam7 <- read_fst(path = "~/Downloads/SpamBase/SpamBase_withoutdupl_norm_02_v07_OCSVM_DT_final.fst", as.data.table = T)
spam8 <- read_fst(path = "~/Downloads/SpamBase/SpamBase_withoutdupl_norm_02_v08_OCSVM_DT_final.fst", as.data.table = T)
spam9 <- read_fst(path = "~/Downloads/SpamBase/SpamBase_withoutdupl_norm_02_v09_OCSVM_DT_final.fst", as.data.table = T)
spam10 <- read_fst(path = "~/Downloads/SpamBase/SpamBase_withoutdupl_norm_02_v10_OCSVM_DT_final.fst", as.data.table = T)


# Pima ------------------------------------------------------------
pima1 <- read_fst(path = "~/Downloads/Pima/Pima_withoutdupl_norm_02_v01_OCSVM_DT_final.fst", as.data.table = T)
pima2 <- read_fst(path = "~/Downloads/Pima/Pima_withoutdupl_norm_02_v02_OCSVM_DT_final.fst", as.data.table = T)
pima3 <- read_fst(path = "~/Downloads/Pima/Pima_withoutdupl_norm_02_v03_OCSVM_DT_final.fst", as.data.table = T)
pima4 <- read_fst(path = "~/Downloads/Pima/Pima_withoutdupl_norm_02_v04_OCSVM_DT_final.fst", as.data.table = T)
pima5 <- read_fst(path = "~/Downloads/Pima/Pima_withoutdupl_norm_02_v05_OCSVM_DT_final.fst", as.data.table = T)
pima6 <- read_fst(path = "~/Downloads/Pima/Pima_withoutdupl_norm_02_v06_OCSVM_DT_final.fst", as.data.table = T)
pima7 <- read_fst(path = "~/Downloads/Pima/Pima_withoutdupl_norm_02_v07_OCSVM_DT_final.fst", as.data.table = T)
pima8 <- read_fst(path = "~/Downloads/Pima/Pima_withoutdupl_norm_02_v08_OCSVM_DT_final.fst", as.data.table = T)
pima9 <- read_fst(path = "~/Downloads/Pima/Pima_withoutdupl_norm_02_v09_OCSVM_DT_final.fst", as.data.table = T)
pima10 <- read_fst(path = "~/Downloads/Pima/Pima_withoutdupl_norm_02_v10_OCSVM_DT_final.fst", as.data.table = T)



# Page ------------------------------------------------------------
page1 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v01_OCSVM_DT_final.fst", as.data.table = T)
page2 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v02_OCSVM_DT_final.fst", as.data.table = T)
page3 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v03_OCSVM_DT_final.fst", as.data.table = T)
page4 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v04_OCSVM_DT_final.fst", as.data.table = T)
page5 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v05_OCSVM_DT_final.fst", as.data.table = T)
page6 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v06_OCSVM_DT_final.fst", as.data.table = T)
page7 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v07_OCSVM_DT_final.fst", as.data.table = T)
page8 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v08_OCSVM_DT_final.fst", as.data.table = T)
page9 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v09_OCSVM_DT_final.fst", as.data.table = T)
page10 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v10_OCSVM_DT_final.fst", as.data.table = T)



page_corr1 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v01_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
page_corr2 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v02_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
page_corr3 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v03_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
page_corr4 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v04_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
page_corr5 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v05_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
page_corr6 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v06_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
page_corr7 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v07_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
page_corr8 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v08_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
page_corr9 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v09_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
page_corr10 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v10_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)


page_non1 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v01_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
page_non2 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v02_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
page_non3 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v03_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
page_non4 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v04_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
page_non5 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v05_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
page_non6 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v06_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
page_non7 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v07_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
page_non8 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v08_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
page_non9 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v09_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
page_non10 <- read_fst(path = "~/Downloads/PageBlocks/PageBlocks_withoutdupl_norm_02_v10_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)



# ALOI --------------------------------------------------------------------
aloi1 <- read_fst(path = "~/Downloads/ALOI/ALOI_withoutdupl_norm_OCSVM_DT_final_1.fst", as.data.table = T)
aloi2 <- read_fst(path = "~/Downloads/ALOI/ALOI_withoutdupl_norm_OCSVM_DT_final.fst", as.data.table = T)

aloi <- rbindlist(list(aloi1, aloi2))

# Arrhythmia --------------------------------------------------------------

# arrhythmia1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v01_OCSVM_DT_final.fst", as.data.table = T)
arrhythmia2 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v02_OCSVM_DT_final.fst", as.data.table = T)
arrhythmia3 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v03_OCSVM_DT_final.fst", as.data.table = T)
arrhythmia4 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v04_OCSVM_DT_final.fst", as.data.table = T)
arrhythmia5 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v05_OCSVM_DT_final.fst", as.data.table = T)
arrhythmia6 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v06_OCSVM_DT_final.fst", as.data.table = T)
arrhythmia7 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v07_OCSVM_DT_final.fst", as.data.table = T)
arrhythmia8 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v08_OCSVM_DT_final.fst", as.data.table = T)
arrhythmia9 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v09_OCSVM_DT_final.fst", as.data.table = T)
arrhythmia10 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v10_OCSVM_DT_final.fst", as.data.table = T)



# arrhythmia1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v01_OCSVM_DT_final.fst", as.data.table = T)
arrhythmia2_1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v02_OCSVM_DT_final_1.fst", as.data.table = T)
arrhythmia2_1[, Iteration:= 14 + Iteration]
arrhythmia3_1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v03_OCSVM_DT_final_1.fst", as.data.table = T)
arrhythmia3_1[, Iteration:= 14 + Iteration]
arrhythmia4_1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v04_OCSVM_DT_final_1.fst", as.data.table = T)
arrhythmia4_1[, Iteration:= 14 + Iteration]
arrhythmia5_1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v05_OCSVM_DT_final_1.fst", as.data.table = T)
arrhythmia5_1[, Iteration:= 14 + Iteration]
arrhythmia6_1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v06_OCSVM_DT_final_1.fst", as.data.table = T)
arrhythmia6_1[, Iteration:= 14 + Iteration]
arrhythmia7_1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v07_OCSVM_DT_final_1.fst", as.data.table = T)
arrhythmia7_1[, Iteration:= 14 + Iteration]
arrhythmia8_1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v08_OCSVM_DT_final_1.fst", as.data.table = T)
arrhythmia8_1[, Iteration:= 14 + Iteration]
arrhythmia9_1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v09_OCSVM_DT_final_1.fst", as.data.table = T)
arrhythmia9_1[, Iteration:= 14 + Iteration]
arrhythmia10_1 <- read_fst(path = "~/Downloads/Arrhythmia/Arrhythmia_withoutdupl_norm_05_v10_OCSVM_DT_final_1.fst", as.data.table = T)
arrhythmia10_1[, Iteration:= 14 + Iteration]

arrhythmia2 <- rbindlist(list(arrhythmia2, arrhythmia2_1))
arrhythmia3 <- rbindlist(list(arrhythmia3, arrhythmia3_1))
arrhythmia4 <- rbindlist(list(arrhythmia4, arrhythmia4_1))
arrhythmia5 <- rbindlist(list(arrhythmia5, arrhythmia5_1))
arrhythmia6 <- rbindlist(list(arrhythmia6, arrhythmia6_1))
arrhythmia7 <- rbindlist(list(arrhythmia7, arrhythmia7_1))
arrhythmia8 <- rbindlist(list(arrhythmia8, arrhythmia8_1))
arrhythmia9 <- rbindlist(list(arrhythmia9, arrhythmia9_1))
arrhythmia10 <- rbindlist(list(arrhythmia10, arrhythmia10_1))


# Stamps ------------------------------------------------------------
stamps1 <- read_fst(path = "~/Downloads/Stamps/Stamps_withoutdupl_norm_05_v01_OCSVM_DT_final.fst", as.data.table = T)
stamps2 <- read_fst(path = "~/Downloads/Stamps/Stamps_withoutdupl_norm_05_v02_OCSVM_DT_final.fst", as.data.table = T)
stamps3 <- read_fst(path = "~/Downloads/Stamps/Stamps_withoutdupl_norm_05_v03_OCSVM_DT_final.fst", as.data.table = T)
stamps4 <- read_fst(path = "~/Downloads/Stamps/Stamps_withoutdupl_norm_05_v04_OCSVM_DT_final.fst", as.data.table = T)
stamps5 <- read_fst(path = "~/Downloads/Stamps/Stamps_withoutdupl_norm_05_v05_OCSVM_DT_final.fst", as.data.table = T)
stamps6 <- read_fst(path = "~/Downloads/Stamps/Stamps_withoutdupl_norm_05_v06_OCSVM_DT_final.fst", as.data.table = T)
stamps7 <- read_fst(path = "~/Downloads/Stamps/Stamps_withoutdupl_norm_05_v07_OCSVM_DT_final.fst", as.data.table = T)
stamps8 <- read_fst(path = "~/Downloads/Stamps/Stamps_withoutdupl_norm_05_v08_OCSVM_DT_final.fst", as.data.table = T)
stamps9 <- read_fst(path = "~/Downloads/Stamps/Stamps_withoutdupl_norm_05_v09_OCSVM_DT_final.fst", as.data.table = T)
stamps10 <- read_fst(path = "~/Downloads/Stamps/Stamps_withoutdupl_norm_05_v10_OCSVM_DT_final.fst", as.data.table = T)


# Wilt ------------------------------------------------------------
wilt1 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v01_OCSVM_DT_final.fst", as.data.table = T)
wilt2 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v02_OCSVM_DT_final.fst", as.data.table = T)
wilt3 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v03_OCSVM_DT_final.fst", as.data.table = T)
wilt4 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v04_OCSVM_DT_final.fst", as.data.table = T)
wilt5 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v05_OCSVM_DT_final.fst", as.data.table = T)
wilt6 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v06_OCSVM_DT_final.fst", as.data.table = T)
wilt7 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v07_OCSVM_DT_final.fst", as.data.table = T)
wilt8 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v08_OCSVM_DT_final.fst", as.data.table = T)
wilt9 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v09_OCSVM_DT_final.fst", as.data.table = T)
wilt10 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v10_OCSVM_DT_final.fst", as.data.table = T)



wilt1_repeat <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v01_OCSVM_DT_final_Repeat.fst", as.data.table = T)
wilt2_repeat <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v02_OCSVM_DT_final_Repeat.fst", as.data.table = T)
wilt3_repeat <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v03_OCSVM_DT_final_Repeat.fst", as.data.table = T)
wilt4_repeat <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v04_OCSVM_DT_final_Repeat.fst", as.data.table = T)
wilt5_repeat <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v05_OCSVM_DT_final_Repeat.fst", as.data.table = T)
wilt6_repeat <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v06_OCSVM_DT_final_Repeat.fst", as.data.table = T)
wilt7_repeat <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v07_OCSVM_DT_final_Repeat.fst", as.data.table = T)
wilt8_repeat <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v08_OCSVM_DT_final_Repeat.fst", as.data.table = T)
wilt9_repeat <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v09_OCSVM_DT_final_Repeat.fst", as.data.table = T)
wilt10_repeat <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v10_OCSVM_DT_final_Repeat.fst", as.data.table = T)


wilt1_gauss <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v01_OCSVM_DT_final_Gauss.fst", as.data.table = T)
wilt2_gauss <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v02_OCSVM_DT_final_Gauss.fst", as.data.table = T)
wilt3_gauss <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v03_OCSVM_DT_final_Gauss.fst", as.data.table = T)
wilt4_gauss <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v04_OCSVM_DT_final_Gauss.fst", as.data.table = T)
wilt5_gauss <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v05_OCSVM_DT_final_Gauss.fst", as.data.table = T)
wilt6_gauss <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v06_OCSVM_DT_final_Gauss.fst", as.data.table = T)
wilt7_gauss <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v07_OCSVM_DT_final_Gauss.fst", as.data.table = T)
wilt8_gauss <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v08_OCSVM_DT_final_Gauss.fst", as.data.table = T)
wilt9_gauss <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v09_OCSVM_DT_final_Gauss.fst", as.data.table = T)
wilt10_gauss <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v10_OCSVM_DT_final_Gauss.fst", as.data.table = T)





wilt_non1 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v01_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
wilt_non2 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v02_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
wilt_non3 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v03_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
wilt_non4 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v04_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
wilt_non6 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v06_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)


wilt_corr1 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v01_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
wilt_corr2 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v02_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
wilt_corr3 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v03_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
wilt_corr4 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v04_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)
wilt_corr6 <- read_fst(path = "~/Downloads/Wilt/Wilt_withoutdupl_norm_02_v06_OCSVM_DT_final_CorrFixed_and_Normalized.fst", as.data.table = T)




# Parkinson ------------------------------------------------------------
parkinson1 <- read_fst(path = "~/Downloads/Parkinson/Parkinson_withoutdupl_norm_20_v01_OCSVM_DT_final.fst", as.data.table = T)
parkinson2 <- read_fst(path = "~/Downloads/Parkinson/Parkinson_withoutdupl_norm_20_v02_OCSVM_DT_final.fst", as.data.table = T)
parkinson3 <- read_fst(path = "~/Downloads/Parkinson/Parkinson_withoutdupl_norm_20_v03_OCSVM_DT_final.fst", as.data.table = T)
parkinson4 <- read_fst(path = "~/Downloads/Parkinson/Parkinson_withoutdupl_norm_20_v04_OCSVM_DT_final.fst", as.data.table = T)
parkinson5 <- read_fst(path = "~/Downloads/Parkinson/Parkinson_withoutdupl_norm_20_v05_OCSVM_DT_final.fst", as.data.table = T)
parkinson6 <- read_fst(path = "~/Downloads/Parkinson/Parkinson_withoutdupl_norm_20_v06_OCSVM_DT_final.fst", as.data.table = T)
parkinson7 <- read_fst(path = "~/Downloads/Parkinson/Parkinson_withoutdupl_norm_20_v07_OCSVM_DT_final.fst", as.data.table = T)
parkinson8 <- read_fst(path = "~/Downloads/Parkinson/Parkinson_withoutdupl_norm_20_v08_OCSVM_DT_final.fst", as.data.table = T)
parkinson9 <- read_fst(path = "~/Downloads/Parkinson/Parkinson_withoutdupl_norm_20_v09_OCSVM_DT_final.fst", as.data.table = T)
parkinson10 <- read_fst(path = "~/Downloads/Parkinson/Parkinson_withoutdupl_norm_20_v10_OCSVM_DT_final.fst", as.data.table = T)


# Hepatitis ------------------------------------------------------------
hepatitis1 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v01_OCSVM_DT_final.fst", as.data.table = T)
hepatitis2 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v02_OCSVM_DT_final.fst", as.data.table = T)
hepatitis3 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v03_OCSVM_DT_final.fst", as.data.table = T)
hepatitis4 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v04_OCSVM_DT_final.fst", as.data.table = T)
hepatitis5 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v05_OCSVM_DT_final.fst", as.data.table = T)
hepatitis6 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v06_OCSVM_DT_final.fst", as.data.table = T)
hepatitis7 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v07_OCSVM_DT_final.fst", as.data.table = T)
hepatitis8 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v08_OCSVM_DT_final.fst", as.data.table = T)
hepatitis9 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v09_OCSVM_DT_final.fst", as.data.table = T)
hepatitis10 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v10_OCSVM_DT_final.fst", as.data.table = T)


hepatitis_non1 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v01_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
hepatitis_non2 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v02_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)
hepatitis_non3 <- read_fst(path = "~/Downloads/Hepatitis/Hepatitis_withoutdupl_norm_10_v03_OCSVM_DT_final_NONnormalizedOD.fst", as.data.table = T)

# create representation results -------------------------------------------

get_representation_results <- function(input_dataset, dataset_col) {
 
  dataset_list <- list()
  for( i in c(1, 3, 5, 8, 10, 15, 20, 25, 30)){
    DT <- get_AUC(given_dataset = input_dataset, 
                                     dataset_string = dataset_col, 
                                     number_of_representations1 = i)
    
    DT_casted <- data.table::dcast.data.table(data = DT, dataset~Representation, 
                                                              value.var = "V1")
    DT_casted[, Representations:= i]
    DT_casted[, dataset:= dataset_col]
    dataset_list[[i]] <- copy(DT_casted)
    rm(DT_casted)
  }
   DT_res <- rbindlist(dataset_list)
   DT_res1 <- data.table(dataset = DT_res[1, dataset], 
                         AFR_1 = DT_res[1, Augmented], AFR_3 = DT_res[2, Augmented],
                         AFR_5 = DT_res[3, Augmented], AFR_8 = DT_res[4, Augmented], 
                         AFR_10 = DT_res[5, Augmented], AFR_15 = DT_res[6, Augmented], 
                         AFR_20 = DT_res[7, Augmented], AFR_25 = DT_res[8, Augmented], 
                         AFR_30 = DT_res[9, Augmented],
                         UFR_1 = DT_res[1, Unsupervised], UFR_3 = DT_res[2, Unsupervised],
                         UFR_5 = DT_res[3, Unsupervised], UFR_8 = DT_res[4, Unsupervised], 
                         UFR_10 = DT_res[5, Unsupervised], 
                         UFR_15 = DT_res[6, Unsupervised], UFR_20 = DT_res[7, Unsupervised], 
                         UFR_25 = DT_res[8, Unsupervised], UFR_30 = DT_res[9, Unsupervised],
                         Original = DT_res[5, Original])
   
   return(DT_res1)
}

get_precision_results <- function(input_dataset, dataset_col) {
  
  dataset_list <- list()
  for( i in c(1, 3, 5, 8, 10, 15, 20, 25, 30)){
    DT <- get_precision_recall(given_dataset = input_dataset, 
                                        dataset_string = dataset_col, 
                                        number_of_representations1 = i)[[1]]
    
    DT_casted <- data.table::dcast.data.table(data = DT, dataset~Representation, 
                                              value.var = "V1")
    DT_casted[, Representations:= i]
    DT_casted[, dataset:= dataset_col]
    dataset_list[[i]] <- copy(DT_casted)
    rm(DT_casted)
  }
  DT_res <- rbindlist(dataset_list)
  DT_res1 <- data.table(dataset = DT_res[1, dataset], 
                        AFR_1 = DT_res[1, Augmented], AFR_3 = DT_res[2, Augmented],
                        AFR_5 = DT_res[3, Augmented], AFR_8 = DT_res[4, Augmented], 
                        AFR_10 = DT_res[5, Augmented], AFR_15 = DT_res[6, Augmented], 
                        AFR_20 = DT_res[7, Augmented], AFR_25 = DT_res[8, Augmented], 
                        AFR_30 = DT_res[9, Augmented],
                        UFR_1 = DT_res[1, Unsupervised], UFR_3 = DT_res[2, Unsupervised],
                        UFR_5 = DT_res[3, Unsupervised], UFR_8 = DT_res[4, Unsupervised], 
                        UFR_10 = DT_res[5, Unsupervised], 
                        UFR_15 = DT_res[6, Unsupervised], UFR_20 = DT_res[7, Unsupervised], 
                        UFR_25 = DT_res[8, Unsupervised], UFR_30 = DT_res[9, Unsupervised],
                        Original = DT_res[5, Original])
  
  return(DT_res1)
}

get_precision_results_atN <- function(input_dataset, dataset_col) {
  
  dataset_list <- list()
  for( i in c(1, 3, 5, 8, 10, 15, 20, 25, 30)){
    DT <- get_precision_recall_at_N(given_dataset = input_dataset, 
                               dataset_string = dataset_col, 
                               number_of_representations1 = i)[[1]]
    
    DT_casted <- data.table::dcast.data.table(data = DT, dataset~Representation, 
                                              value.var = "V1")
    DT_casted[, Representations:= i]
    DT_casted[, dataset:= dataset_col]
    dataset_list[[i]] <- copy(DT_casted)
    rm(DT_casted)
  }
  DT_res <- rbindlist(dataset_list)
  DT_res1 <- data.table(dataset = DT_res[1, dataset], 
                        AFR_1 = DT_res[1, Augmented], AFR_3 = DT_res[2, Augmented],
                        AFR_5 = DT_res[3, Augmented], AFR_8 = DT_res[4, Augmented], 
                        AFR_10 = DT_res[5, Augmented], AFR_15 = DT_res[6, Augmented], 
                        AFR_20 = DT_res[7, Augmented], AFR_25 = DT_res[8, Augmented], 
                        AFR_30 = DT_res[9, Augmented],
                        UFR_1 = DT_res[1, Unsupervised], UFR_3 = DT_res[2, Unsupervised],
                        UFR_5 = DT_res[3, Unsupervised], UFR_8 = DT_res[4, Unsupervised], 
                        UFR_10 = DT_res[5, Unsupervised], 
                        UFR_15 = DT_res[6, Unsupervised], UFR_20 = DT_res[7, Unsupervised], 
                        UFR_25 = DT_res[8, Unsupervised], UFR_30 = DT_res[9, Unsupervised],
                        Original = DT_res[5, Original])
  
  return(DT_res1)
}

get_recall_results <- function(input_dataset, dataset_col) {
  
  dataset_list <- list()
  for( i in c(1, 3, 5, 8, 10, 15, 20, 25, 30)){
    DT <- get_precision_recall(given_dataset = input_dataset, 
                               dataset_string = dataset_col, 
                               number_of_representations1 = i)[[2]]
    
    DT_casted <- data.table::dcast.data.table(data = DT, dataset~Representation, 
                                              value.var = "V1")
    DT_casted[, Representations:= i]
    DT_casted[, dataset:= dataset_col]
    dataset_list[[i]] <- copy(DT_casted)
    rm(DT_casted)
  }
  DT_res <- rbindlist(dataset_list)
  DT_res1 <- data.table(dataset = DT_res[1, dataset], 
                        AFR_1 = DT_res[1, Augmented], AFR_3 = DT_res[2, Augmented],
                        AFR_5 = DT_res[3, Augmented], AFR_8 = DT_res[4, Augmented], 
                        AFR_10 = DT_res[5, Augmented], AFR_15 = DT_res[6, Augmented], 
                        AFR_20 = DT_res[7, Augmented], AFR_25 = DT_res[8, Augmented], 
                        AFR_30 = DT_res[9, Augmented],
                        UFR_1 = DT_res[1, Unsupervised], UFR_3 = DT_res[2, Unsupervised],
                        UFR_5 = DT_res[3, Unsupervised], UFR_8 = DT_res[4, Unsupervised], 
                        UFR_10 = DT_res[5, Unsupervised], 
                        UFR_15 = DT_res[6, Unsupervised], UFR_20 = DT_res[7, Unsupervised], 
                        UFR_25 = DT_res[8, Unsupervised], UFR_30 = DT_res[9, Unsupervised],
                        Original = DT_res[5, Original])
  
  return(DT_res1)
}

get_recall_results_atN <- function(input_dataset, dataset_col) {
  
  dataset_list <- list()
  for( i in c(1, 3, 5, 8, 10, 15, 20, 25, 30)){
    DT <- get_precision_recall_at_N(given_dataset = input_dataset, 
                                    dataset_string = dataset_col, 
                                    number_of_representations1 = i)[[2]]
    
    DT_casted <- data.table::dcast.data.table(data = DT, dataset~Representation, 
                                              value.var = "V1")
    DT_casted[, Representations:= i]
    DT_casted[, dataset:= dataset_col]
    dataset_list[[i]] <- copy(DT_casted)
    rm(DT_casted)
  }
  DT_res <- rbindlist(dataset_list)
  DT_res1 <- data.table(dataset = DT_res[1, dataset], 
                        AFR_1 = DT_res[1, Augmented], AFR_3 = DT_res[2, Augmented],
                        AFR_5 = DT_res[3, Augmented], AFR_8 = DT_res[4, Augmented], 
                        AFR_10 = DT_res[5, Augmented], AFR_15 = DT_res[6, Augmented], 
                        AFR_20 = DT_res[7, Augmented], AFR_25 = DT_res[8, Augmented], 
                        AFR_30 = DT_res[9, Augmented],
                        UFR_1 = DT_res[1, Unsupervised], UFR_3 = DT_res[2, Unsupervised],
                        UFR_5 = DT_res[3, Unsupervised], UFR_8 = DT_res[4, Unsupervised], 
                        UFR_10 = DT_res[5, Unsupervised], 
                        UFR_15 = DT_res[6, Unsupervised], UFR_20 = DT_res[7, Unsupervised], 
                        UFR_25 = DT_res[8, Unsupervised], UFR_30 = DT_res[9, Unsupervised],
                        Original = DT_res[5, Original])
  
  return(DT_res1)
}

# Waveform ----------------------------------------------------------------

wave_list <- list(waveform1, waveform2, waveform3, waveform4, waveform5, 
                  waveform6, waveform7, waveform8, waveform9, waveform10)
wave_string <- list("wave1", "wave2", "wave3", "wave4", "wave5", "wave6", 
                    "wave7", "wave8", "wave9", "wave10")

temp_wave <- list()
for( i in 1:length(wave_list)){
  print(i)
  temp_wave[[i]] <- get_representation_results(input_dataset = wave_list[[i]], dataset_col = wave_string[[i]])
}

wave_DT <- rbindlist(temp_wave)
wave_DT[, dataset:= NULL]
wave_DT[, dataset:= unlist(wave_string)]

friedmanTest(wave_DT_non)
friedmanAlignedRanksTest(wave_DT_non)
imanDavenportTest(wave_DT_non)
quadeTest(wave_DT_non)
plotCD(wave_DT_non, alpha = 0.1)
colMeans(rankMatrix(wave_DT_non))


# Waveform - Precision ----------------------------------------------------
wave_list <- list(waveform1, waveform2, waveform3, waveform4, waveform5, 
                  waveform6, waveform7, waveform8, waveform9, waveform10)
wave_string <- list("wave1", "wave2", "wave3", "wave4", "wave5", "wave6", 
                    "wave7", "wave8", "wave9", "wave10")

temp_wave <- list()
for( i in 1:length(wave_list)){
  print(i)
  temp_wave[[i]] <- get_precision_results(input_dataset = wave_list[[i]], dataset_col = wave_string[[i]])
}

wave_DT_precision <- rbindlist(temp_wave)
wave_DT_precision[, dataset:= NULL]

friedmanTest(wave_DT_precision)
friedmanAlignedRanksTest(wave_DT_precision)
imanDavenportTest(wave_DT_precision)
quadeTest(wave_DT_precision)
plotCD(wave_DT_precision, alpha = 0.1)
colMeans(rankMatrix(wave_DT_precision))


# Waveform - recall ---------------------------------------------------------
wave_list <- list(waveform1, waveform2, waveform3, waveform4, waveform5, 
                  waveform6, waveform7, waveform8, waveform9, waveform10)
wave_string <- list("wave1", "wave2", "wave3", "wave4", "wave5", "wave6", 
                    "wave7", "wave8", "wave9", "wave10")

temp_wave <- list()
for( i in 1:length(wave_list)){
  print(i)
  temp_wave[[i]] <- get_recall_results(input_dataset = wave_list[[i]], dataset_col = wave_string[[i]])
}

wave_DT_recall <- rbindlist(temp_wave)
wave_DT_recall[, dataset:= NULL]

friedmanTest(wave_DT_recall)
friedmanAlignedRanksTest(wave_DT_recall)
imanDavenportTest(wave_DT_recall)
quadeTest(wave_DT_recall)
plotCD(wave_DT_recall, alpha = 0.1)
colMeans(rankMatrix(wave_DT_recall))


# Waveform - Precision @N ----------------------------------------------------
wave_list <- list(waveform1, waveform2, waveform3, waveform4, waveform5, 
                  waveform6, waveform7, waveform8, waveform9, waveform10)
wave_string <- list("wave1", "wave2", "wave3", "wave4", "wave5", "wave6", 
                    "wave7", "wave8", "wave9", "wave10")

temp_wave <- list()
for( i in 1:length(wave_list)){
  print(i)
  temp_wave[[i]] <- get_precision_results_atN(input_dataset = wave_list[[i]], dataset_col = wave_string[[i]])
}

wave_DT_precision_atN <- rbindlist(temp_wave)
wave_DT_precision_atN[, dataset:= NULL]

friedmanTest(wave_DT_precision_atN)
friedmanAlignedRanksTest(wave_DT_precision_atN)
imanDavenportTest(wave_DT_precision_atN)
quadeTest(wave_DT_precision_atN)
plotCD(wave_DT_precision_atN, alpha = 0.1)
colMeans(rankMatrix(wave_DT_precision_atN))


# Waveform - recall ---------------------------------------------------------
wave_list <- list(waveform1, waveform2, waveform3, waveform4, waveform5, 
                  waveform6, waveform7, waveform8, waveform9, waveform10)
wave_string <- list("wave1", "wave2", "wave3", "wave4", "wave5", "wave6", 
                    "wave7", "wave8", "wave9", "wave10")

temp_wave <- list()
for( i in 1:length(wave_list)){
  print(i)
  temp_wave[[i]] <- get_recall_results_atN(input_dataset = wave_list[[i]], dataset_col = wave_string[[i]])
}

wave_DT_recall_atN <- rbindlist(temp_wave)
wave_DT_recall_atN[, dataset:= NULL]

friedmanTest(wave_DT_recall_atN)
friedmanAlignedRanksTest(wave_DT_recall_atN)
imanDavenportTest(wave_DT_recall_atN)
quadeTest(wave_DT_recall_atN)
plotCD(wave_DT_recall_atN, alpha = 0.1)
colMeans(rankMatrix(wave_DT_recall_atN))


# PenDigits ---------------------------------------------------------------
pendigits_list <- list(pendigits1, pendigits3, pendigits4, pendigits5, 
                  pendigits6, pendigits7, pendigits8, pendigits9, pendigits10)
pendigits_string <- list("pendigits1", "pendigits3", "pendigits4", "pendigits5", "pendigits6", 
                    "pendigits7", "pendigits8", "pendigits9", "pendigits10")

temp_pendigits <- list()
for( i in 1:length(pendigits_list)){
  print(i)
  temp_pendigits[[i]] <- get_representation_results(input_dataset = pendigits_list[[i]], dataset_col = pendigits_string[[i]])
}

pendigits_DT <- rbindlist(temp_pendigits)
pendigits_DT[, dataset:= NULL]
pendigits_DT[, dataset:= unlist(pendigits_string)]

friedmanTest(pendigits_DT)
friedmanAlignedRanksTest(pendigits_DT)
imanDavenportTest(pendigits_DT)
quadeTest(pendigits_DT)
plotCD(pendigits_DT, alpha = 0.05)
colMeans(rankMatrix(pendigits_DT))

# PenDigits - precision ---------------------------------------------------
pendigits_list <- list(pendigits1, pendigits3, pendigits4, pendigits5, 
                       pendigits6, pendigits7, pendigits8, pendigits9, pendigits10)
pendigits_string <- list("pendigits1", "pendigits3", "pendigits4", "pendigits5", "pendigits6", 
                         "pendigits7", "pendigits8", "pendigits9", "pendigits10")

temp_pendigits <- list()
for( i in 1:length(pendigits_list)){
  print(i)
  temp_pendigits[[i]] <- get_precision_results(input_dataset = pendigits_list[[i]], dataset_col = pendigits_string[[i]])
}

pendigits_DT_precision <- rbindlist(temp_pendigits)
pendigits_DT_precision[, dataset:= NULL]


friedmanTest(pendigits_DT_precision)
friedmanAlignedRanksTest(pendigits_DT_precision)
imanDavenportTest(pendigits_DT_precision)
quadeTest(pendigits_DT_precision)
plotCD(pendigits_DT_precision, alpha = 0.05)
colMeans(rankMatrix(pendigits_DT_precision))

# PenDigits - recall ---------------------------------------------------
pendigits_list <- list(pendigits1, pendigits3, pendigits4, pendigits5, 
                       pendigits6, pendigits7, pendigits8, pendigits9, pendigits10)
pendigits_string <- list("pendigits1", "pendigits3", "pendigits4", "pendigits5", "pendigits6", 
                         "pendigits7", "pendigits8", "pendigits9", "pendigits10")

temp_pendigits <- list()
for( i in 1:length(pendigits_list)){
  print(i)
  temp_pendigits[[i]] <- get_recall_results(input_dataset = pendigits_list[[i]], dataset_col = pendigits_string[[i]])
}

pendigits_DT_recall <- rbindlist(temp_pendigits)
pendigits_DT_recall[, dataset:= NULL]


friedmanTest(pendigits_DT_recall)
friedmanAlignedRanksTest(pendigits_DT_recall)
imanDavenportTest(pendigits_DT_recall)
quadeTest(pendigits_DT_recall)
plotCD(pendigits_DT_recall, alpha = 0.05)
colMeans(rankMatrix(pendigits_DT_recall))


# PenDigits - precision @N ---------------------------------------------------
pendigits_list <- list(pendigits1, pendigits3, pendigits4, pendigits5, 
                       pendigits6, pendigits7, pendigits8, pendigits9, pendigits10)
pendigits_string <- list("pendigits1", "pendigits3", "pendigits4", "pendigits5", "pendigits6", 
                         "pendigits7", "pendigits8", "pendigits9", "pendigits10")

temp_pendigits <- list()
for( i in 1:length(pendigits_list)){
  print(i)
  temp_pendigits[[i]] <- get_precision_results_atN(input_dataset = pendigits_list[[i]], dataset_col = pendigits_string[[i]])
}

pendigits_DT_precision_atN <- rbindlist(temp_pendigits)
pendigits_DT_precision_atN[, dataset:= NULL]


friedmanTest(pendigits_DT_precision_atN)
friedmanAlignedRanksTest(pendigits_DT_precision_atN)
imanDavenportTest(pendigits_DT_precision_atN)
quadeTest(pendigits_DT_precision_atN)
plotCD(pendigits_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(pendigits_DT_precision_atN))

# PenDigits - recall @N---------------------------------------------------
pendigits_list <- list(pendigits1, pendigits3, pendigits4, pendigits5, 
                       pendigits6, pendigits7, pendigits8, pendigits9, pendigits10)
pendigits_string <- list("pendigits1", "pendigits3", "pendigits4", "pendigits5", "pendigits6", 
                         "pendigits7", "pendigits8", "pendigits9", "pendigits10")

temp_pendigits <- list()
for( i in 1:length(pendigits_list)){
  print(i)
  temp_pendigits[[i]] <- get_recall_results_atN(input_dataset = pendigits_list[[i]], dataset_col = pendigits_string[[i]])
}

pendigits_DT_recall_atN <- rbindlist(temp_pendigits)
pendigits_DT_recall_atN[, dataset:= NULL]


friedmanTest(pendigits_DT_recall_atN)
friedmanAlignedRanksTest(pendigits_DT_recall_atN)
imanDavenportTest(pendigits_DT_recall_atN)
quadeTest(pendigits_DT_recall_atN)
plotCD(pendigits_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(pendigits_DT_recall_atN))

# WDBC ---------------------------------------------------------------

wdbc_list <- list(wdbc1, wdbc2, wdbc3, wdbc4, wdbc5, 
                       wdbc6, wdbc7, wdbc8, wdbc9, wdbc10)
wdbc_string <- list("wdbc1", "wdbc2", "wdbc3", "wdbc4", "wdbc5", "wdbc6", 
                         "wdbc7", "wdbc8", "wdbc9", "wdbc10")

temp_wdbc <- list()
for( i in 1:length(wdbc_list)){
  print(i)
  temp_wdbc[[i]] <- get_representation_results(input_dataset = wdbc_list[[i]], dataset_col = wdbc_string[[i]])
}

wdbc_DT <- rbindlist(temp_wdbc)
wdbc_DT[, dataset:= NULL]
wdbc_DT[, dataset:= unlist(wdbc_string)]

friedmanTest(wdbc_DT)
friedmanAlignedRanksTest(wdbc_DT)
imanDavenportTest(wdbc_DT)
quadeTest(wdbc_DT)
plotCD(wdbc_DT, alpha = 0.05)
colMeans(rankMatrix(wdbc_DT))

# WDBC - precision --------------------------------------------------------
wdbc_list <- list(wdbc1, wdbc2, wdbc3, wdbc4, wdbc5, 
                  wdbc6, wdbc7, wdbc8, wdbc9, wdbc10)
wdbc_string <- list("wdbc1", "wdbc2", "wdbc3", "wdbc4", "wdbc5", "wdbc6", 
                    "wdbc7", "wdbc8", "wdbc9", "wdbc10")

temp_wdbc <- list()
for( i in 1:length(wdbc_list)){
  print(i)
  temp_wdbc[[i]] <- get_precision_results(input_dataset = wdbc_list[[i]], dataset_col = wdbc_string[[i]])
}

wdbc_DT_precision <- rbindlist(temp_wdbc)
wdbc_DT_precision[, dataset:= NULL]

friedmanTest(wdbc_DT_precision)
friedmanAlignedRanksTest(wdbc_DT_precision)
imanDavenportTest(wdbc_DT_precision)
quadeTest(wdbc_DT_precision)
plotCD(wdbc_DT_precision, alpha = 0.05)
colMeans(rankMatrix(wdbc_DT_precision))


# WDBC - recall --------------------------------------------------------
wdbc_list <- list(wdbc1, wdbc2, wdbc3, wdbc4, wdbc5, 
                  wdbc6, wdbc7, wdbc8, wdbc9, wdbc10)
wdbc_string <- list("wdbc1", "wdbc2", "wdbc3", "wdbc4", "wdbc5", "wdbc6", 
                    "wdbc7", "wdbc8", "wdbc9", "wdbc10")

temp_wdbc <- list()
for( i in 1:length(wdbc_list)){
  print(i)
  temp_wdbc[[i]] <- get_recall_results(input_dataset = wdbc_list[[i]], dataset_col = wdbc_string[[i]])
}

wdbc_DT_recall <- rbindlist(temp_wdbc)
wdbc_DT_recall[, dataset:= NULL]

friedmanTest(wdbc_DT_recall)
friedmanAlignedRanksTest(wdbc_DT_recall)
imanDavenportTest(wdbc_DT_recall)
quadeTest(wdbc_DT_recall)
plotCD(wdbc_DT_recall, alpha = 0.05)
colMeans(rankMatrix(wdbc_DT_recall))

# WDBC - precision @N--------------------------------------------------------
wdbc_list <- list(wdbc1, wdbc2, wdbc3, wdbc4, wdbc5, 
                  wdbc6, wdbc7, wdbc8, wdbc9, wdbc10)
wdbc_string <- list("wdbc1", "wdbc2", "wdbc3", "wdbc4", "wdbc5", "wdbc6", 
                    "wdbc7", "wdbc8", "wdbc9", "wdbc10")

temp_wdbc <- list()
for( i in 1:length(wdbc_list)){
  print(i)
  temp_wdbc[[i]] <- get_precision_results_atN(input_dataset = wdbc_list[[i]], dataset_col = wdbc_string[[i]])
}

wdbc_DT_precision_atN <- rbindlist(temp_wdbc)
wdbc_DT_precision_atN[, dataset:= NULL]

friedmanTest(wdbc_DT_precision_atN)
friedmanAlignedRanksTest(wdbc_DT_precision_atN)
imanDavenportTest(wdbc_DT_precision_atN)
quadeTest(wdbc_DT_precision_atN)
plotCD(wdbc_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(wdbc_DT_precision_atN))


# WDBC - recall @N--------------------------------------------------------
wdbc_list <- list(wdbc1, wdbc2, wdbc3, wdbc4, wdbc5, 
                  wdbc6, wdbc7, wdbc8, wdbc9, wdbc10)
wdbc_string <- list("wdbc1", "wdbc2", "wdbc3", "wdbc4", "wdbc5", "wdbc6", 
                    "wdbc7", "wdbc8", "wdbc9", "wdbc10")

temp_wdbc <- list()
for( i in 1:length(wdbc_list)){
  print(i)
  temp_wdbc[[i]] <- get_recall_results_atN(input_dataset = wdbc_list[[i]], dataset_col = wdbc_string[[i]])
}

wdbc_DT_recall_atN <- rbindlist(temp_wdbc)
wdbc_DT_recall_atN[, dataset:= NULL]

friedmanTest(wdbc_DT_recall_atN)
friedmanAlignedRanksTest(wdbc_DT_recall_atN)
imanDavenportTest(wdbc_DT_recall_atN)
quadeTest(wdbc_DT_recall_atN)
plotCD(wdbc_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(wdbc_DT_recall_atN))





# Cardio ---------------------------------------------------------------
cardio_list <- list(cardio2, cardio3, cardio4, cardio5, 
                  cardio6, cardio7, cardio8, cardio9, cardio10)
cardio_string <- list("cardio2", "cardio3", "cardio4", "cardio5", "cardio6", 
                    "cardio7", "cardio8", "cardio9", "cardio10")

temp_cardio <- list()
for( i in 1:length(cardio_list)){
  print(i)
  temp_cardio[[i]] <- get_representation_results(input_dataset = cardio_list[[i]], dataset_col = cardio_string[[i]])
}

cardio_DT <- rbindlist(temp_cardio)
cardio_DT[, dataset:= NULL]
cardio_DT[, dataset:= unlist(cardio_string)]

friedmanTest(cardio_DT)
friedmanAlignedRanksTest(cardio_DT)
imanDavenportTest(cardio_DT)
quadeTest(cardio_DT)
plotCD(cardio_DT, alpha = 0.05)
colMeans(rankMatrix(cardio_DT))


# Cardio - Precision ------------------------------------------------------
cardio_list <- list(cardio2, cardio3, cardio4, cardio5, 
                    cardio6, cardio7, cardio8, cardio9, cardio10)
cardio_string <- list("cardio2", "cardio3", "cardio4", "cardio5", "cardio6", 
                      "cardio7", "cardio8", "cardio9", "cardio10")

temp_cardio <- list()
for( i in 1:length(cardio_list)){
  print(i)
  temp_cardio[[i]] <- get_precision_results_atN(input_dataset = cardio_list[[i]], dataset_col = cardio_string[[i]])
}

cardio_DT_precision <- rbindlist(temp_cardio)
cardio_DT_precision[, dataset:= NULL]


friedmanTest(cardio_DT_precision)
friedmanAlignedRanksTest(cardio_DT_precision)
imanDavenportTest(cardio_DT_precision)
quadeTest(cardio_DT_precision)
plotCD(cardio_DT_precision, alpha = 0.05)
colMeans(rankMatrix(cardio_DT_precision))

# Cardio - Recall  --------------------------------------------------------

cardio_list <- list(cardio2, cardio3, cardio4, cardio5, 
                    cardio6, cardio7, cardio8, cardio9, cardio10)
cardio_string <- list("cardio2", "cardio3", "cardio4", "cardio5", "cardio6", 
                      "cardio7", "cardio8", "cardio9", "cardio10")

temp_cardio <- list()
for( i in 1:length(cardio_list)){
  print(i)
  temp_cardio[[i]] <- get_recall_results(input_dataset = cardio_list[[i]], dataset_col = cardio_string[[i]])
}

cardio_DT_recall <- rbindlist(temp_cardio)
cardio_DT_recall[, dataset:= NULL]


friedmanTest(cardio_DT_recall)
friedmanAlignedRanksTest(cardio_DT_recall)
imanDavenportTest(cardio_DT_recall)
quadeTest(cardio_DT_recall)
plotCD(cardio_DT_recall, alpha = 0.05)
colMeans(rankMatrix(cardio_DT_recall))

# Cardio - Precision @N------------------------------------------------------
cardio_list <- list(cardio2, cardio3, cardio4, cardio5, 
                    cardio6, cardio7, cardio8, cardio9, cardio10)
cardio_string <- list("cardio2", "cardio3", "cardio4", "cardio5", "cardio6", 
                      "cardio7", "cardio8", "cardio9", "cardio10")

temp_cardio <- list()
for( i in 1:length(cardio_list)){
  print(i)
  temp_cardio[[i]] <- get_precision_results_atN(input_dataset = cardio_list[[i]], dataset_col = cardio_string[[i]])
}

cardio_DT_precision_atN <- rbindlist(temp_cardio)
cardio_DT_precision_atN[, dataset:= NULL]


friedmanTest(cardio_DT_precision_atN)
friedmanAlignedRanksTest(cardio_DT_precision_atN)
imanDavenportTest(cardio_DT_precision_atN)
quadeTest(cardio_DT_precision_atN)
plotCD(cardio_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(cardio_DT_precision_atN))

# Cardio - Recall @N--------------------------------------------------------

cardio_list <- list(cardio2, cardio3, cardio4, cardio5, 
                    cardio6, cardio7, cardio8, cardio9, cardio10)
cardio_string <- list("cardio2", "cardio3", "cardio4", "cardio5", "cardio6", 
                      "cardio7", "cardio8", "cardio9", "cardio10")

temp_cardio <- list()
for( i in 1:length(cardio_list)){
  print(i)
  temp_cardio[[i]] <- get_recall_results_atN(input_dataset = cardio_list[[i]], dataset_col = cardio_string[[i]])
}

cardio_DT_recall_atN <- rbindlist(temp_cardio)
cardio_DT_recall_atN[, dataset:= NULL]

friedmanTest(cardio_DT_recall_atN)
friedmanAlignedRanksTest(cardio_DT_recall_atN)
imanDavenportTest(cardio_DT_recall_atN)
quadeTest(cardio_DT_recall_atN)
plotCD(cardio_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(cardio_DT_recall_atN))


# Annthyroid ---------------------------------------------------------------------
annthyroid_list <- list(annthyroid1, annthyroid2, annthyroid3, annthyroid4, annthyroid5, 
                    annthyroid6, annthyroid7, annthyroid8, annthyroid9, annthyroid10)
annthyroid_string <- list("annthyroid1", "annthyroid2", "annthyroid3", "annthyroid4", "annthyroid5", "annthyroid6", 
                      "annthyroid7", "annthyroid8", "annthyroid9", "annthyroid10")

temp_annthyroid <- list()
for( i in 1:length(annthyroid_list)){
  print(i)
  temp_annthyroid[[i]] <- get_representation_results(input_dataset = annthyroid_list[[i]], dataset_col = annthyroid_string[[i]])
}

annthyroid_DT <- rbindlist(temp_annthyroid)
annthyroid_DT[, dataset:= NULL]
annthyroid_DT[, dataset:= unlist(annthyroid_string)]

friedmanTest(annthyroid_DT)
friedmanAlignedRanksTest(annthyroid_DT)
imanDavenportTest(annthyroid_DT)
quadeTest(annthyroid_DT)
plotCD(annthyroid_DT, alpha = 0.05)
colMeans(rankMatrix(annthyroid_DT))


# Annthyroid - Precision -------------------------------------------------
annthyroid_list <- list(annthyroid1, annthyroid2, annthyroid3, annthyroid4, annthyroid5, 
                        annthyroid6, annthyroid7, annthyroid8, annthyroid9, annthyroid10)
annthyroid_string <- list("annthyroid1", "annthyroid2", "annthyroid3", "annthyroid4", "annthyroid5", "annthyroid6", 
                          "annthyroid7", "annthyroid8", "annthyroid9", "annthyroid10")

temp_annthyroid <- list()
for( i in 1:length(annthyroid_list)){
  print(i)
  temp_annthyroid[[i]] <- get_precision_results(input_dataset = annthyroid_list[[i]], dataset_col = annthyroid_string[[i]])
}

annthyroid_DT_precision <- rbindlist(temp_annthyroid)
annthyroid_DT_precision[, dataset:=NULL]

friedmanTest(annthyroid_DT_precision)
friedmanAlignedRanksTest(annthyroid_DT_precision)
imanDavenportTest(annthyroid_DT_precision)
quadeTest(annthyroid_DT_precision)
plotCD(annthyroid_DT_precision, alpha = 0.05)
colMeans(rankMatrix(annthyroid_DT_precision))

# Annthyroid - Recall -------------------------------------------------
annthyroid_list <- list(annthyroid1, annthyroid2, annthyroid3, annthyroid4, annthyroid5, 
                        annthyroid6, annthyroid7, annthyroid8, annthyroid9, annthyroid10)
annthyroid_string <- list("annthyroid1", "annthyroid2", "annthyroid3", "annthyroid4", "annthyroid5", "annthyroid6", 
                          "annthyroid7", "annthyroid8", "annthyroid9", "annthyroid10")

temp_annthyroid <- list()
for( i in 1:length(annthyroid_list)){
  print(i)
  temp_annthyroid[[i]] <- get_recall_results(input_dataset = annthyroid_list[[i]], dataset_col = annthyroid_string[[i]])
}

annthyroid_DT_recall <- rbindlist(temp_annthyroid)
annthyroid_DT_recall[, dataset:= NULL]

friedmanTest(annthyroid_DT_recall)
friedmanAlignedRanksTest(annthyroid_DT_recall)
imanDavenportTest(annthyroid_DT_recall)
quadeTest(annthyroid_DT_recall)
plotCD(annthyroid_DT_recall, alpha = 0.05)
colMeans(rankMatrix(annthyroid_DT_recall))

# Annthyroid - Precision @N-------------------------------------------------
annthyroid_list <- list(annthyroid1, annthyroid2, annthyroid3, annthyroid4, annthyroid5, 
                        annthyroid6, annthyroid7, annthyroid8, annthyroid9, annthyroid10)
annthyroid_string <- list("annthyroid1", "annthyroid2", "annthyroid3", "annthyroid4", "annthyroid5", "annthyroid6", 
                          "annthyroid7", "annthyroid8", "annthyroid9", "annthyroid10")

temp_annthyroid <- list()
for( i in 1:length(annthyroid_list)){
  print(i)
  temp_annthyroid[[i]] <- get_precision_results_atN(input_dataset = annthyroid_list[[i]], dataset_col = annthyroid_string[[i]])
}

annthyroid_DT_precision_atN <- rbindlist(temp_annthyroid)
annthyroid_DT_precision_atN[, dataset:=NULL]

friedmanTest(annthyroid_DT_precision_atN)
friedmanAlignedRanksTest(annthyroid_DT_precision_atN)
imanDavenportTest(annthyroid_DT_precision_atN)
quadeTest(annthyroid_DT_precision_atN)
plotCD(annthyroid_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(annthyroid_DT_precision_atN))

# Annthyroid - Recall @N-------------------------------------------------
annthyroid_list <- list(annthyroid1, annthyroid2, annthyroid3, annthyroid4, annthyroid5, 
                        annthyroid6, annthyroid7, annthyroid8, annthyroid9, annthyroid10)
annthyroid_string <- list("annthyroid1", "annthyroid2", "annthyroid3", "annthyroid4", "annthyroid5", "annthyroid6", 
                          "annthyroid7", "annthyroid8", "annthyroid9", "annthyroid10")

temp_annthyroid <- list()
for( i in 1:length(annthyroid_list)){
  print(i)
  temp_annthyroid[[i]] <- get_recall_results_atN(input_dataset = annthyroid_list[[i]], dataset_col = annthyroid_string[[i]])
}

annthyroid_DT_recall_atN <- rbindlist(temp_annthyroid)
annthyroid_DT_recall_atN[, dataset:= NULL]

friedmanTest(annthyroid_DT_recall_atN)
friedmanAlignedRanksTest(annthyroid_DT_recall_atN)
imanDavenportTest(annthyroid_DT_recall_atN)
quadeTest(annthyroid_DT_recall_atN)
plotCD(annthyroid_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(annthyroid_DT_recall_atN))



# Shuttle ---------------------------------------------------------------------
shuttle_list <- list(shuttle1, shuttle2, shuttle3, shuttle4, shuttle5, shuttle6, shuttle7, shuttle8, shuttle9, shuttle10)
shuttle_string <- list("shuttle1", "shuttle2", "shuttle3","shuttle4", "shuttle5",
                       "shuttle6", "shuttle7", "shuttle8", "shuttle9", "shuttle10"
                          )

temp_shuttle <- list()
for( i in 1:length(shuttle_list)){
  print(i)
  temp_shuttle[[i]] <- get_representation_results(input_dataset = shuttle_list[[i]], dataset_col = shuttle_string[[i]])
}

shuttle_DT <- rbindlist(temp_shuttle)
shuttle_DT[, dataset:= NULL]


friedmanTest(shuttle_DT)
friedmanAlignedRanksTest(shuttle_DT)
imanDavenportTest(shuttle_DT)
quadeTest(shuttle_DT)
plotCD(shuttle_DT, alpha = 0.1)
colMeans(rankMatrix(shuttle_DT))
shuttle_DT[, lapply(.SD, function(x) mean(x)), .SDcols = 1:19]

# Shuttle Repeat ----------------------------------------------------------
shuttle_list <- list(shuttle1_repeat, shuttle2_repeat, shuttle4_repeat, shuttle5_repeat, 
                     shuttle6_repeat, shuttle7_repeat, shuttle8_repeat, shuttle9_repeat, shuttle10_repeat)
shuttle_string <- list("shuttle1", "shuttle2","shuttle4", "shuttle5",
                       "shuttle6", "shuttle7", "shuttle8", "shuttle9", "shuttle10"
)

temp_shuttle <- list()
for( i in 1:length(shuttle_list)){
  print(i)
  temp_shuttle[[i]] <- get_representation_results(input_dataset = shuttle_list[[i]], dataset_col = shuttle_string[[i]])
}

shuttle_DT_repeat <- rbindlist(temp_shuttle)
shuttle_DT_repeat[, dataset:= NULL]
shuttle_DT_repeat[, dataset:= unlist(shuttle_string)]

friedmanTest(shuttle_DT_repeat)
friedmanAlignedRanksTest(shuttle_DT_repeat)
imanDavenportTest(shuttle_DT_repeat)
quadeTest(shuttle_DT_repeat)
plotCD(shuttle_DT_repeat, alpha = 0.05)
colMeans(rankMatrix(shuttle_DT_repeat))


# shuttle gauss -----------------------------------------------------------
shuttle_list <- list(shuttle1_gauss, shuttle2_gauss, shuttle3_gauss, shuttle4_gauss, shuttle5_gauss, 
                     shuttle6_gauss, shuttle7_gauss, shuttle8_gauss, shuttle9_gauss, shuttle10_gauss)
shuttle_string <- list("shuttle1", "shuttle2", "shuttle3", "shuttle4", "shuttle5",
                       "shuttle6", "shuttle7", "shuttle8", "shuttle9", "shuttle10"
)

temp_shuttle <- list()
for( i in 1:length(shuttle_list)){
  print(i)
  temp_shuttle[[i]] <- get_representation_results(input_dataset = shuttle_list[[i]], dataset_col = shuttle_string[[i]])
}

shuttle_DT_gauss <- rbindlist(temp_shuttle)
shuttle_DT_gauss[, dataset:= NULL]
shuttle_DT_gauss[, dataset:= unlist(shuttle_string)]

friedmanTest(shuttle_DT_gauss)
friedmanAlignedRanksTest(shuttle_DT_gauss)
imanDavenportTest(shuttle_DT_gauss)
quadeTest(shuttle_DT_gauss)
plotCD(shuttle_DT_gauss, alpha = 0.05)
colMeans(rankMatrix(shuttle_DT_gauss))
# Shuttle - Precision -----------------------------------------------------
shuttle_list <- list(shuttle1, shuttle2, shuttle3, shuttle4, shuttle5, shuttle6, shuttle7, shuttle8, shuttle9, shuttle10)
shuttle_string <- list("shuttle1", "shuttle2", "shuttle3","shuttle4", "shuttle5",
                       "shuttle6", "shuttle7", "shuttle8", "shuttle9", "shuttle10"
)

temp_shuttle <- list()
for( i in 1:length(shuttle_list)){
  print(i)
  temp_shuttle[[i]] <- get_precision_results(input_dataset = shuttle_list[[i]], dataset_col = shuttle_string[[i]])
}

shuttle_DT_precision <- rbindlist(temp_shuttle)
shuttle_DT_precision[, dataset:= NULL]


friedmanTest(shuttle_DT_precision)
friedmanAlignedRanksTest(shuttle_DT_precision)
imanDavenportTest(shuttle_DT_precision)
quadeTest(shuttle_DT_precision)
plotCD(shuttle_DT_precision, alpha = 0.1)
colMeans(rankMatrix(shuttle_DT_precision))

# Shuttle - Recall -----------------------------------------------------
shuttle_list <- list(shuttle1, shuttle2, shuttle3, shuttle4, shuttle5, shuttle6, shuttle7, shuttle8, shuttle9, shuttle10)
shuttle_string <- list("shuttle1", "shuttle2", "shuttle3","shuttle4", "shuttle5",
                       "shuttle6", "shuttle7", "shuttle8", "shuttle9", "shuttle10"
)

temp_shuttle <- list()
for( i in 1:length(shuttle_list)){
  print(i)
  temp_shuttle[[i]] <- get_recall_results(input_dataset = shuttle_list[[i]], dataset_col = shuttle_string[[i]])
}

shuttle_DT_recall <- rbindlist(temp_shuttle)
shuttle_DT_recall[, dataset:= NULL]


friedmanTest(shuttle_DT_recall)
friedmanAlignedRanksTest(shuttle_DT_recall)
imanDavenportTest(shuttle_DT_recall)
quadeTest(shuttle_DT_recall)
plotCD(shuttle_DT_recall, alpha = 0.1)
colMeans(rankMatrix(shuttle_DT_recall))

# Shuttle - Precision @N-----------------------------------------------------
shuttle_list <- list(shuttle1, shuttle2, shuttle3, shuttle4, shuttle5, shuttle6, shuttle7, shuttle8, shuttle9, shuttle10)
shuttle_string <- list("shuttle1", "shuttle2", "shuttle3","shuttle4", "shuttle5",
                       "shuttle6", "shuttle7", "shuttle8", "shuttle9", "shuttle10"
)

temp_shuttle <- list()
for( i in 1:length(shuttle_list)){
  print(i)
  temp_shuttle[[i]] <- get_precision_results_atN(input_dataset = shuttle_list[[i]], dataset_col = shuttle_string[[i]])
}

shuttle_DT_precision_atN <- rbindlist(temp_shuttle)
shuttle_DT_precision_atN[, dataset:= NULL]

friedmanTest(shuttle_DT_precision_atN)
friedmanAlignedRanksTest(shuttle_DT_precision_atN)
imanDavenportTest(shuttle_DT_precision_atN)
quadeTest(shuttle_DT_precision_atN)
plotCD(shuttle_DT_precision_atN, alpha = 0.1)
colMeans(rankMatrix(shuttle_DT_precision_atN))

# Shuttle - Recall @N-----------------------------------------------------
shuttle_list <- list(shuttle1, shuttle2, shuttle3, shuttle4, shuttle5, shuttle6, shuttle7, shuttle8, shuttle9, shuttle10)
shuttle_string <- list("shuttle1", "shuttle2", "shuttle3","shuttle4", "shuttle5",
                       "shuttle6", "shuttle7", "shuttle8", "shuttle9", "shuttle10"
)

temp_shuttle <- list()
for( i in 1:length(shuttle_list)){
  print(i)
  temp_shuttle[[i]] <- get_recall_results_atN(input_dataset = shuttle_list[[i]], dataset_col = shuttle_string[[i]])
}

shuttle_DT_recall_atN <- rbindlist(temp_shuttle)
shuttle_DT_recall_atN[, dataset:= NULL]

friedmanTest(shuttle_DT_recall_atN)
friedmanAlignedRanksTest(shuttle_DT_recall_atN)
imanDavenportTest(shuttle_DT_recall_atN)
quadeTest(shuttle_DT_recall_atN)
plotCD(shuttle_DT_recall_atN, alpha = 0.1)
colMeans(rankMatrix(shuttle_DT_recall_atN))

# Pima ---------------------------------------------------------------------
pima_list <- list(pima1, pima2, pima3, pima4, pima5, 
                     pima6, pima7, pima8, pima9, pima10)
pima_string <- list("pima1", "pima2", "pima3", "pima4", "pima5", "pima6", 
                       "pima7", "pima8", "pima9", "pima10")

temp_pima <- list()
for( i in 1:length(pima_list)){
  print(i)
  temp_pima[[i]] <- get_representation_results(input_dataset = pima_list[[i]], dataset_col = pima_string[[i]])
}

pima_DT <- rbindlist(temp_pima)
pima_DT[, dataset:= NULL]
pima_DT[, dataset:= unlist(pima_string)]

friedmanTest(pima_DT)
friedmanAlignedRanksTest(pima_DT)
imanDavenportTest(pima_DT)
quadeTest(pima_DT)
plotCD(pima_DT, alpha = 0.05)
colMeans(rankMatrix(pima_DT))


# Pima - Precision ---------------------------------------------------------------
pima_list <- list(pima1, pima2, pima3, pima4, pima5, 
                  pima6, pima7, pima8, pima9, pima10)
pima_string <- list("pima1", "pima2", "pima3", "pima4", "pima5", "pima6", 
                    "pima7", "pima8", "pima9", "pima10")

temp_pima <- list()
for( i in 1:length(pima_list)){
  print(i)
  temp_pima[[i]] <- get_precision_results(input_dataset = pima_list[[i]], dataset_col = pima_string[[i]])
}

pima_DT_precision <- rbindlist(temp_pima)
pima_DT_precision[, dataset:= NULL]

friedmanTest(pima_DT_precision)
friedmanAlignedRanksTest(pima_DT_precision)
imanDavenportTest(pima_DT_precision)
quadeTest(pima_DT_precision)
plotCD(pima_DT_precision, alpha = 0.05)
colMeans(rankMatrix(pima_DT_precision))


# Pima - Recall -----------------------------------------------------------
pima_list <- list(pima1, pima2, pima3, pima4, pima5, 
                  pima6, pima7, pima8, pima9, pima10)
pima_string <- list("pima1", "pima2", "pima3", "pima4", "pima5", "pima6", 
                    "pima7", "pima8", "pima9", "pima10")

temp_pima <- list()
for( i in 1:length(pima_list)){
  print(i)
  temp_pima[[i]] <- get_recall_results(input_dataset = pima_list[[i]], dataset_col = pima_string[[i]])
}

pima_DT_recall <- rbindlist(temp_pima)
pima_DT_recall[, dataset:= NULL]

friedmanTest(pima_DT_recall)
friedmanAlignedRanksTest(pima_DT_recall)
imanDavenportTest(pima_DT_recall)
quadeTest(pima_DT_recall)
plotCD(pima_DT_recall, alpha = 0.05)
colMeans(rankMatrix(pima_DT_recall))

# Pima - Precision @N---------------------------------------------------------------
pima_list <- list(pima1, pima2, pima3, pima4, pima5, 
                  pima6, pima7, pima8, pima9, pima10)
pima_string <- list("pima1", "pima2", "pima3", "pima4", "pima5", "pima6", 
                    "pima7", "pima8", "pima9", "pima10")

temp_pima <- list()
for( i in 1:length(pima_list)){
  print(i)
  temp_pima[[i]] <- get_precision_results_atN(input_dataset = pima_list[[i]], dataset_col = pima_string[[i]])
}

pima_DT_precision_atN <- rbindlist(temp_pima)
pima_DT_precision_atN[, dataset:= NULL]

friedmanTest(pima_DT_precision_atN)
friedmanAlignedRanksTest(pima_DT_precision_atN)
imanDavenportTest(pima_DT_precision_atN)
quadeTest(pima_DT_precision_atN)
plotCD(pima_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(pima_DT_precision_atN))


# Pima - Recall @N-----------------------------------------------------------
pima_list <- list(pima1, pima2, pima3, pima4, pima5, 
                  pima6, pima7, pima8, pima9, pima10)
pima_string <- list("pima1", "pima2", "pima3", "pima4", "pima5", "pima6", 
                    "pima7", "pima8", "pima9", "pima10")

temp_pima <- list()
for( i in 1:length(pima_list)){
  print(i)
  temp_pima[[i]] <- get_recall_results_atN(input_dataset = pima_list[[i]], dataset_col = pima_string[[i]])
}

pima_DT_recall_atN <- rbindlist(temp_pima)
pima_DT_recall_atN[, dataset:= NULL]

friedmanTest(pima_DT_recall_atN)
friedmanAlignedRanksTest(pima_DT_recall_atN)
imanDavenportTest(pima_DT_recall_atN)
quadeTest(pima_DT_recall_atN)
plotCD(pima_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(pima_DT_recall_atN))


# Arrythmia ---------------------------------------------------------------------
arrhythmia_list <- list(arrhythmia2, arrhythmia3, arrhythmia4, arrhythmia5, 
                  arrhythmia6, arrhythmia7, arrhythmia8, arrhythmia9, arrhythmia10)
arrhythmia_string <- list("arrhythmia2", "arrhythmia3", "arrhythmia4", "arrhythmia5", "arrhythmia6", 
                    "arrhythmia7", "arrhythmia8", "arrhythmia9", "arrhythmia10")

temp_arrhythmia <- list()
for( i in 1:length(arrhythmia_list)){
  print(i)
  temp_arrhythmia[[i]] <- get_representation_results(input_dataset = arrhythmia_list[[i]], dataset_col = arrhythmia_string[[i]])
}

arrhythmia_DT <- rbindlist(temp_arrhythmia)
arrhythmia_DT[, dataset:= NULL]
# arrhythmia_DT[, dataset:= unlist(arrhythmia_string)]

friedmanTest(arrhythmia_DT)
friedmanAlignedRanksTest(arrhythmia_DT)
imanDavenportTest(arrhythmia_DT)
quadeTest(arrhythmia_DT)
plotCD(arrhythmia_DT, alpha = 0.05)
colMeans(rankMatrix(arrhythmia_DT))

# arrhythmia_DT1 <- arrhythmia_DT[, lapply(.SD, function(x) mean(x)), .SDcols = 1:19]
# arrhythmia_DT1[, dataset:= "arrhythmia"]
# arrhythmia_DT1[, UFR_1:= ""]
# setcolorder(x = arrhythmia_DT1, neworder = c(20, 1:19))

# Arrythmia - Precision ---------------------------------------------------------------------
arrhythmia_list <- list(arrhythmia2, arrhythmia3, arrhythmia4, arrhythmia5, 
                        arrhythmia6, arrhythmia7, arrhythmia8, arrhythmia9, arrhythmia10)
arrhythmia_string <- list("arrhythmia2", "arrhythmia3", "arrhythmia4", "arrhythmia5", "arrhythmia6", 
                          "arrhythmia7", "arrhythmia8", "arrhythmia9", "arrhythmia10")

temp_arrhythmia <- list()
for( i in 1:length(arrhythmia_list)){
  print(i)
  temp_arrhythmia[[i]] <- get_precision_results(input_dataset = arrhythmia_list[[i]], dataset_col = arrhythmia_string[[i]])
}

arrhythmia_DT_precision <- rbindlist(temp_arrhythmia)
arrhythmia_DT_precision[, dataset:= NULL]
# arrhythmia_DT[, dataset:= unlist(arrhythmia_string)]

friedmanTest(arrhythmia_DT_precision)
friedmanAlignedRanksTest(arrhythmia_DT_precision)
imanDavenportTest(arrhythmia_DT_precision)
quadeTest(arrhythmia_DT_precision)
plotCD(arrhythmia_DT_precision, alpha = 0.05)
colMeans(rankMatrix(arrhythmia_DT_precision))

# Arrythmia - Recall ---------------------------------------------------------------------
arrhythmia_list <- list(arrhythmia2, arrhythmia3, arrhythmia4, arrhythmia5, 
                        arrhythmia6, arrhythmia7, arrhythmia8, arrhythmia9, arrhythmia10)
arrhythmia_string <- list("arrhythmia2", "arrhythmia3", "arrhythmia4", "arrhythmia5", "arrhythmia6", 
                          "arrhythmia7", "arrhythmia8", "arrhythmia9", "arrhythmia10")

temp_arrhythmia <- list()
for( i in 1:length(arrhythmia_list)){
  print(i)
  temp_arrhythmia[[i]] <- get_recall_results(input_dataset = arrhythmia_list[[i]], dataset_col = arrhythmia_string[[i]])
}

arrhythmia_DT_recall <- rbindlist(temp_arrhythmia)
arrhythmia_DT_recall[, dataset:= NULL]
# arrhythmia_DT[, dataset:= unlist(arrhythmia_string)]

friedmanTest(arrhythmia_DT_recall)
friedmanAlignedRanksTest(arrhythmia_DT_recall)
imanDavenportTest(arrhythmia_DT_recall)
quadeTest(arrhythmia_DT_recall)
plotCD(arrhythmia_DT_recall, alpha = 0.05)
colMeans(rankMatrix(arrhythmia_DT_recall))


# Arrythmia - Precision @N---------------------------------------------------------------------
arrhythmia_list <- list(arrhythmia2, arrhythmia3, arrhythmia4, arrhythmia5, 
                        arrhythmia6, arrhythmia7, arrhythmia8, arrhythmia9, arrhythmia10)
arrhythmia_string <- list("arrhythmia2", "arrhythmia3", "arrhythmia4", "arrhythmia5", "arrhythmia6", 
                          "arrhythmia7", "arrhythmia8", "arrhythmia9", "arrhythmia10")

temp_arrhythmia <- list()
for( i in 1:length(arrhythmia_list)){
  print(i)
  temp_arrhythmia[[i]] <- get_precision_results_atN(input_dataset = arrhythmia_list[[i]], dataset_col = arrhythmia_string[[i]])
}

arrhythmia_DT_precision_atN <- rbindlist(temp_arrhythmia)
arrhythmia_DT_precision_atN[, dataset:= NULL]
# arrhythmia_DT[, dataset:= unlist(arrhythmia_string)]

friedmanTest(arrhythmia_DT_precision_atN)
friedmanAlignedRanksTest(arrhythmia_DT_precision_atN)
imanDavenportTest(arrhythmia_DT_precision_atN)
quadeTest(arrhythmia_DT_precision_atN)
plotCD(arrhythmia_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(arrhythmia_DT_precision_atN))

# Arrythmia - Recall @N---------------------------------------------------------------------
arrhythmia_list <- list(arrhythmia2, arrhythmia3, arrhythmia4, arrhythmia5, 
                        arrhythmia6, arrhythmia7, arrhythmia8, arrhythmia9, arrhythmia10)
arrhythmia_string <- list("arrhythmia2", "arrhythmia3", "arrhythmia4", "arrhythmia5", "arrhythmia6", 
                          "arrhythmia7", "arrhythmia8", "arrhythmia9", "arrhythmia10")

temp_arrhythmia <- list()
for( i in 1:length(arrhythmia_list)){
  print(i)
  temp_arrhythmia[[i]] <- get_recall_results_atN(input_dataset = arrhythmia_list[[i]], dataset_col = arrhythmia_string[[i]])
}

arrhythmia_DT_recall_atN <- rbindlist(temp_arrhythmia)
arrhythmia_DT_recall_atN[, dataset:= NULL]
# arrhythmia_DT[, dataset:= unlist(arrhythmia_string)]

friedmanTest(arrhythmia_DT_recall_atN)
friedmanAlignedRanksTest(arrhythmia_DT_recall_atN)
imanDavenportTest(arrhythmia_DT_recall_atN)
quadeTest(arrhythmia_DT_recall_atN)
plotCD(arrhythmia_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(arrhythmia_DT_recall_atN))



# Heart ---------------------------------------------------------------------
heart_list <- list(heart1, heart2, heart3, heart4, heart5, 
                  heart6, heart7, heart8, heart9, heart10)
heart_string <- list("heart1", "heart2", "heart3", "heart4", "heart5", "heart6", 
                    "heart7", "heart8", "heart9", "heart10")

temp_heart <- list()
for( i in 1:length(heart_list)){
  print(i)
  temp_heart[[i]] <- get_representation_results(input_dataset = heart_list[[i]], dataset_col = heart_string[[i]])
}

heart_DT <- rbindlist(temp_heart)
heart_DT[, dataset:= NULL]


friedmanTest(heart_DT)
friedmanAlignedRanksTest(heart_DT)
imanDavenportTest(heart_DT)
quadeTest(heart_DT)
plotCD(heart_DT, alpha = 0.05)
colMeans(rankMatrix(heart_DT))


# Heart - Precision -------------------------------------------------------
heart_list <- list(heart1, heart2, heart3, heart4, heart5, 
                   heart6, heart7, heart8, heart9, heart10)
heart_string <- list("heart1", "heart2", "heart3", "heart4", "heart5", "heart6", 
                     "heart7", "heart8", "heart9", "heart10")

temp_heart <- list()
for( i in 1:length(heart_list)){
  print(i)
  temp_heart[[i]] <- get_precision_results(input_dataset = heart_list[[i]], dataset_col = heart_string[[i]])
}

heart_DT_precision <- rbindlist(temp_heart)
heart_DT_precision[, dataset:= NULL]


friedmanTest(heart_DT_precision)
friedmanAlignedRanksTest(heart_DT_precision)
imanDavenportTest(heart_DT_precision)
quadeTest(heart_DT_precision)
plotCD(heart_DT_precision, alpha = 0.05)
colMeans(rankMatrix(heart_DT_precision))

# Heart - Recall -------------------------------------------------------
heart_list <- list(heart1, heart2, heart3, heart4, heart5, 
                   heart6, heart7, heart8, heart9, heart10)
heart_string <- list("heart1", "heart2", "heart3", "heart4", "heart5", "heart6", 
                     "heart7", "heart8", "heart9", "heart10")

temp_heart <- list()
for( i in 1:length(heart_list)){
  print(i)
  temp_heart[[i]] <- get_recall_results(input_dataset = heart_list[[i]], dataset_col = heart_string[[i]])
}

heart_DT_recall <- rbindlist(temp_heart)
heart_DT_recall[, dataset:= NULL]


friedmanTest(heart_DT_recall)
friedmanAlignedRanksTest(heart_DT_recall)
imanDavenportTest(heart_DT_recall)
quadeTest(heart_DT_recall)
plotCD(heart_DT_recall, alpha = 0.05)
colMeans(rankMatrix(heart_DT_recall))


# Heart - Precision @N-------------------------------------------------------
heart_list <- list(heart1, heart2, heart3, heart4, heart5, 
                   heart6, heart7, heart8, heart9, heart10)
heart_string <- list("heart1", "heart2", "heart3", "heart4", "heart5", "heart6", 
                     "heart7", "heart8", "heart9", "heart10")

temp_heart <- list()
for( i in 1:length(heart_list)){
  print(i)
  temp_heart[[i]] <- get_precision_results_atN(input_dataset = heart_list[[i]], dataset_col = heart_string[[i]])
}

heart_DT_precision_atN <- rbindlist(temp_heart)
heart_DT_precision_atN[, dataset:= NULL]


friedmanTest(heart_DT_precision_atN)
friedmanAlignedRanksTest(heart_DT_precision_atN)
imanDavenportTest(heart_DT_precision_atN)
quadeTest(heart_DT_precision_atN)
plotCD(heart_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(heart_DT_precision_atN))

# Heart - Recall @N-------------------------------------------------------
heart_list <- list(heart1, heart2, heart3, heart4, heart5, 
                   heart6, heart7, heart8, heart9, heart10)
heart_string <- list("heart1", "heart2", "heart3", "heart4", "heart5", "heart6", 
                     "heart7", "heart8", "heart9", "heart10")

temp_heart <- list()
for( i in 1:length(heart_list)){
  print(i)
  temp_heart[[i]] <- get_recall_results_atN(input_dataset = heart_list[[i]], dataset_col = heart_string[[i]])
}

heart_DT_recall_atN <- rbindlist(temp_heart)
heart_DT_recall_atN[, dataset:= NULL]


friedmanTest(heart_DT_recall_atN)
friedmanAlignedRanksTest(heart_DT_recall_atN)
imanDavenportTest(heart_DT_recall_atN)
quadeTest(heart_DT_recall_atN)
plotCD(heart_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(heart_DT_recall_atN))

# SpamBase ---------------------------------------------------------------------
spam_list <- list(spam1, spam2, spam3, spam4, spam5, 
                   spam6, spam7, spam8, spam9, spam10)
spam_string <- list("spam1", "spam2", "spam3", "spam4", "spam5", "spam6", 
                     "spam7", "spam8", "spam9", "spam10")

temp_spam <- list()
for( i in 1:length(spam_list)){
  print(i)
  temp_spam[[i]] <- get_representation_results(input_dataset = spam_list[[i]], dataset_col = spam_string[[i]])
}

spam_DT <- rbindlist(temp_spam)
spam_DT[, dataset:= NULL]
spam_DT[, dataset:= unlist(spam_string)]

friedmanTest(spam_DT)
friedmanAlignedRanksTest(spam_DT)
imanDavenportTest(spam_DT)
quadeTest(spam_DT)
plotCD(spam_DT, alpha = 0.05)
colMeans(rankMatrix(spam_DT))


# SpamBase - Precision ----------------------------------------------------
spam_list <- list(spam1, spam2, spam3, spam4, spam5, 
                  spam6, spam7, spam8, spam9, spam10)
spam_string <- list("spam1", "spam2", "spam3", "spam4", "spam5", "spam6", 
                    "spam7", "spam8", "spam9", "spam10")

temp_spam <- list()
for( i in 1:length(spam_list)){
  print(i)
  temp_spam[[i]] <- get_precision_results(input_dataset = spam_list[[i]], dataset_col = spam_string[[i]])
}

spam_DT_precision <- rbindlist(temp_spam)
spam_DT_precision[, dataset:= NULL]


friedmanTest(spam_DT_precision)
friedmanAlignedRanksTest(spam_DT_precision)
imanDavenportTest(spam_DT_precision)
quadeTest(spam_DT_precision)
plotCD(spam_DT_precision, alpha = 0.05)
colMeans(rankMatrix(spam_DT_precision))

# SpamBase - Recall ----------------------------------------------------
spam_list <- list(spam1, spam2, spam3, spam4, spam5, 
                  spam6, spam7, spam8, spam9, spam10)
spam_string <- list("spam1", "spam2", "spam3", "spam4", "spam5", "spam6", 
                    "spam7", "spam8", "spam9", "spam10")

temp_spam <- list()
for( i in 1:length(spam_list)){
  print(i)
  temp_spam[[i]] <- get_recall_results(input_dataset = spam_list[[i]], dataset_col = spam_string[[i]])
}

spam_DT_recall <- rbindlist(temp_spam)
spam_DT_recall[, dataset:= NULL]

friedmanTest(spam_DT_recall)
friedmanAlignedRanksTest(spam_DT_recall)
imanDavenportTest(spam_DT_recall)
quadeTest(spam_DT_recall)
plotCD(spam_DT_recall, alpha = 0.05)
colMeans(rankMatrix(spam_DT_recall))

# SpamBase - Precision @N----------------------------------------------------
spam_list <- list(spam1, spam2, spam3, spam4, spam5, 
                  spam6, spam7, spam8, spam9, spam10)
spam_string <- list("spam1", "spam2", "spam3", "spam4", "spam5", "spam6", 
                    "spam7", "spam8", "spam9", "spam10")

temp_spam <- list()
for( i in 1:length(spam_list)){
  print(i)
  temp_spam[[i]] <- get_precision_results_atN(input_dataset = spam_list[[i]], dataset_col = spam_string[[i]])
}

spam_DT_precision_atN <- rbindlist(temp_spam)
spam_DT_precision_atN[, dataset:= NULL]

friedmanTest(spam_DT_precision_atN)
friedmanAlignedRanksTest(spam_DT_precision_atN)
imanDavenportTest(spam_DT_precision_atN)
quadeTest(spam_DT_precision_atN)
plotCD(spam_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(spam_DT_precision_atN))

# SpamBase - Recall @N----------------------------------------------------
spam_list <- list(spam1, spam2, spam3, spam4, spam5, 
                  spam6, spam7, spam8, spam9, spam10)
spam_string <- list("spam1", "spam2", "spam3", "spam4", "spam5", "spam6", 
                    "spam7", "spam8", "spam9", "spam10")

temp_spam <- list()
for( i in 1:length(spam_list)){
  print(i)
  temp_spam[[i]] <- get_recall_results_atN(input_dataset = spam_list[[i]], dataset_col = spam_string[[i]])
}

spam_DT_recall_atN <- rbindlist(temp_spam)
spam_DT_recall_atN[, dataset:= NULL]

friedmanTest(spam_DT_recall_atN)
friedmanAlignedRanksTest(spam_DT_recall_atN)
imanDavenportTest(spam_DT_recall_atN)
quadeTest(spam_DT_recall_atN)
plotCD(spam_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(spam_DT_recall_atN))


# Page ---------------------------------------------------------------------
page_list <- list(page1, page2, page3, page4, page5, 
                  page6, page7, page8, page9, page10)
page_string <- list("page1", "page2", "page3", "page4", "page5", "page6", 
                    "page7", "page8", "page9", "page10")

temp_page <- list()
for( i in 1:length(page_list)){
  print(i)
  temp_page[[i]] <- get_representation_results(input_dataset = page_list[[i]], dataset_col = page_string[[i]])
}

page_DT <- rbindlist(temp_page)
page_DT[, dataset:= NULL]
page_DT[, dataset:= unlist(page_string)]

friedmanTest(page_DT)
friedmanAlignedRanksTest(page_DT)
imanDavenportTest(page_DT)
quadeTest(page_DT)
plotCD(page_DT, alpha = 0.05)
colMeans(rankMatrix(page_DT))



# Page - Precision --------------------------------------------------------
page_list <- list(page1, page2, page3, page4, page5, 
                  page6, page7, page8, page9, page10)
page_string <- list("page1", "page2", "page3", "page4", "page5", "page6", 
                    "page7", "page8", "page9", "page10")

temp_page <- list()
for( i in 1:length(page_list)){
  print(i)
  temp_page[[i]] <- get_precision_results(input_dataset = page_list[[i]], dataset_col = page_string[[i]])
}

page_DT_precision <- rbindlist(temp_page)
page_DT_precision[, dataset:= NULL]


friedmanTest(page_DT_precision)
friedmanAlignedRanksTest(page_DT_precision)
imanDavenportTest(page_DT_precision)
quadeTest(page_DT_precision)
plotCD(page_DT_precision, alpha = 0.05)
colMeans(rankMatrix(page_DT_precision))

# Page - Recall --------------------------------------------------------
page_list <- list(page1, page2, page3, page4, page5, 
                  page6, page7, page8, page9, page10)
page_string <- list("page1", "page2", "page3", "page4", "page5", "page6", 
                    "page7", "page8", "page9", "page10")

temp_page <- list()
for( i in 1:length(page_list)){
  print(i)
  temp_page[[i]] <- get_recall_results(input_dataset = page_list[[i]], dataset_col = page_string[[i]])
}

page_DT_recall <- rbindlist(temp_page)
page_DT_recall[, dataset:= NULL]


friedmanTest(page_DT_recall)
friedmanAlignedRanksTest(page_DT_recall)
imanDavenportTest(page_DT_recall)
quadeTest(page_DT_recall)
plotCD(page_DT_recall, alpha = 0.05)
colMeans(rankMatrix(page_DT_recall))

# Page - Precision @N--------------------------------------------------------
page_list <- list(page1, page2, page3, page4, page5, 
                  page6, page7, page8, page9, page10)
page_string <- list("page1", "page2", "page3", "page4", "page5", "page6", 
                    "page7", "page8", "page9", "page10")

temp_page <- list()
for( i in 1:length(page_list)){
  print(i)
  temp_page[[i]] <- get_precision_results_atN(input_dataset = page_list[[i]], dataset_col = page_string[[i]])
}

page_DT_precision_atN <- rbindlist(temp_page)
page_DT_precision_atN[, dataset:= NULL]

friedmanTest(page_DT_precision_atN)
friedmanAlignedRanksTest(page_DT_precision_atN)
imanDavenportTest(page_DT_precision_atN)
quadeTest(page_DT_precision_atN)
plotCD(page_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(page_DT_precision_atN))

# Page - Recall @N--------------------------------------------------------
page_list <- list(page1, page2, page3, page4, page5, 
                  page6, page7, page8, page9, page10)
page_string <- list("page1", "page2", "page3", "page4", "page5", "page6", 
                    "page7", "page8", "page9", "page10")

temp_page <- list()
for( i in 1:length(page_list)){
  print(i)
  temp_page[[i]] <- get_recall_results_atN(input_dataset = page_list[[i]], dataset_col = page_string[[i]])
}

page_DT_recall_atN <- rbindlist(temp_page)
page_DT_recall_atN[, dataset:= NULL]


friedmanTest(page_DT_recall_atN)
friedmanAlignedRanksTest(page_DT_recall_atN)
imanDavenportTest(page_DT_recall_atN)
quadeTest(page_DT_recall_atN)
plotCD(page_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(page_DT_recall_atN))

# Stamps ---------------------------------------------------------------------
stamps_list <- list(stamps1, stamps2, stamps3, stamps4, stamps5, 
                  stamps6, stamps7, stamps8, stamps9, stamps10)
stamps_string <- list("stamps1", "stamps2", "stamps3", "stamps4", "stamps5", "stamps6", 
                    "stamps7", "stamps8", "stamps9", "stamps10")

temp_stamps <- list()
for( i in 1:length(stamps_list)){
  print(i)
  temp_stamps[[i]] <- get_representation_results(input_dataset = stamps_list[[i]], dataset_col = stamps_string[[i]])
}

stamps_DT <- rbindlist(temp_stamps)
stamps_DT[, dataset:= NULL]
stamps_DT[, dataset:= unlist(stamps_string)]

friedmanTest(stamps_DT)
friedmanAlignedRanksTest(stamps_DT)
imanDavenportTest(stamps_DT)
quadeTest(stamps_DT)
plotCD(stamps_DT, alpha = 0.05)
colMeans(rankMatrix(stamps_DT))

# Stamps - Precision ------------------------------------------------------
stamps_list <- list(stamps1, stamps2, stamps3, stamps4, stamps5, 
                    stamps6, stamps7, stamps8, stamps9, stamps10)
stamps_string <- list("stamps1", "stamps2", "stamps3", "stamps4", "stamps5", "stamps6", 
                      "stamps7", "stamps8", "stamps9", "stamps10")

temp_stamps <- list()
for( i in 1:length(stamps_list)){
  print(i)
  temp_stamps[[i]] <- get_precision_results(input_dataset = stamps_list[[i]], dataset_col = stamps_string[[i]])
}

stamps_DT_precision <- rbindlist(temp_stamps)
stamps_DT_precision[, dataset:= NULL]


friedmanTest(stamps_DT_precision)
friedmanAlignedRanksTest(stamps_DT_precision)
imanDavenportTest(stamps_DT_precision)
quadeTest(stamps_DT_precision)
plotCD(stamps_DT_precision, alpha = 0.05)
colMeans(rankMatrix(stamps_DT_precision))

# Stamps - Recall ------------------------------------------------------
stamps_list <- list(stamps1, stamps2, stamps3, stamps4, stamps5, 
                    stamps6, stamps7, stamps8, stamps9, stamps10)
stamps_string <- list("stamps1", "stamps2", "stamps3", "stamps4", "stamps5", "stamps6", 
                      "stamps7", "stamps8", "stamps9", "stamps10")

temp_stamps <- list()
for( i in 1:length(stamps_list)){
  print(i)
  temp_stamps[[i]] <- get_recall_results(input_dataset = stamps_list[[i]], dataset_col = stamps_string[[i]])
}

stamps_DT_recall <- rbindlist(temp_stamps)
stamps_DT_recall[, dataset:= NULL]


friedmanTest(stamps_DT_recall)
friedmanAlignedRanksTest(stamps_DT_recall)
imanDavenportTest(stamps_DT_recall)
quadeTest(stamps_DT_recall)
plotCD(stamps_DT_recall, alpha = 0.05)
colMeans(rankMatrix(stamps_DT_recall))

# Stamps - Precision @N------------------------------------------------------
stamps_list <- list(stamps1, stamps2, stamps3, stamps4, stamps5, 
                    stamps6, stamps7, stamps8, stamps9, stamps10)
stamps_string <- list("stamps1", "stamps2", "stamps3", "stamps4", "stamps5", "stamps6", 
                      "stamps7", "stamps8", "stamps9", "stamps10")

temp_stamps <- list()
for( i in 1:length(stamps_list)){
  print(i)
  temp_stamps[[i]] <- get_precision_results_atN(input_dataset = stamps_list[[i]], dataset_col = stamps_string[[i]])
}

stamps_DT_precision_atN <- rbindlist(temp_stamps)
stamps_DT_precision_atN[, dataset:= NULL]

friedmanTest(stamps_DT_precision_atN)
friedmanAlignedRanksTest(stamps_DT_precision_atN)
imanDavenportTest(stamps_DT_precision_atN)
quadeTest(stamps_DT_precision_atN)
plotCD(stamps_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(stamps_DT_precision_atN))

# Stamps - Recall @N------------------------------------------------------
stamps_list <- list(stamps1, stamps2, stamps3, stamps4, stamps5, 
                    stamps6, stamps7, stamps8, stamps9, stamps10)
stamps_string <- list("stamps1", "stamps2", "stamps3", "stamps4", "stamps5", "stamps6", 
                      "stamps7", "stamps8", "stamps9", "stamps10")

temp_stamps <- list()
for( i in 1:length(stamps_list)){
  print(i)
  temp_stamps[[i]] <- get_recall_results_atN(input_dataset = stamps_list[[i]], dataset_col = stamps_string[[i]])
}

stamps_DT_recall_atN <- rbindlist(temp_stamps)
stamps_DT_recall_atN[, dataset:= NULL]

friedmanTest(stamps_DT_recall_atN)
friedmanAlignedRanksTest(stamps_DT_recall_atN)
imanDavenportTest(stamps_DT_recall_atN)
quadeTest(stamps_DT_recall_atN)
plotCD(stamps_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(stamps_DT_recall_atN))

# Wilt ---------------------------------------------------------------------
wilt_list <- list(wilt1, wilt2, wilt3, wilt4,wilt5, wilt6, wilt7,
                  wilt8, wilt9, wilt10)
wilt_string <- list("wilt1", "wilt2", "wilt3", "wilt4", "wilt5", 
                    "wilt6", "wilt7", "wilt8", "wilt9", "wilt10" )

temp_wilt <- list()
for( i in 1:length(wilt_list)){
  print(i)
  temp_wilt[[i]] <- get_representation_results(input_dataset = wilt_list[[i]], dataset_col = wilt_string[[i]])
}

wilt_DT <- rbindlist(temp_wilt)
wilt_DT[, dataset:= NULL]
wilt_DT[, dataset:= unlist(wilt_string)]

friedmanTest(wilt_DT)
friedmanAlignedRanksTest(wilt_DT)
imanDavenportTest(wilt_DT)
quadeTest(wilt_DT)
plotCD(wilt_DT, alpha = 0.1)
colMeans(rankMatrix(wilt_DT))


# Wilt Repeat -------------------------------------------------------------
wilt_list <- list(wilt1_repeat, wilt2_repeat, wilt3_repeat, wilt4_repeat,wilt5_repeat, wilt6_repeat, wilt7_repeat,
                  wilt8_repeat, wilt9_repeat, wilt10_repeat)
wilt_string <- list("wilt1", "wilt2", "wilt3", "wilt4", "wilt5", 
                    "wilt6", "wilt7", "wilt8", "wilt9", "wilt10" )

temp_wilt <- list()
for( i in 1:length(wilt_list)){
  print(i)
  temp_wilt[[i]] <- get_representation_results(input_dataset = wilt_list[[i]], dataset_col = wilt_string[[i]])
}

wilt_DT_repeat <- rbindlist(temp_wilt)
wilt_DT_repeat[, dataset:= NULL]
wilt_DT_repeat[, dataset:= unlist(wilt_string)]
wilt_DT_repeat[, dataset:=NULL]

friedmanTest(wilt_DT_repeat)
friedmanAlignedRanksTest(wilt_DT_repeat)
imanDavenportTest(wilt_DT_repeat)
quadeTest(wilt_DT_repeat)
plotCD(wilt_DT_repeat, alpha = 0.1)
colMeans(rankMatrix(wilt_DT_repeat))

# Wilt Gauss -------------------------------------------------------------
wilt_list <- list(wilt1_gauss, wilt2_gauss, wilt3_gauss, wilt4_gauss,wilt5_gauss, wilt6_gauss, wilt7_gauss,
                  wilt8_gauss, wilt9_gauss, wilt10_gauss)
wilt_string <- list("wilt1", "wilt2", "wilt3", "wilt4", "wilt5", 
                    "wilt6", "wilt7", "wilt8", "wilt9", "wilt10" )

temp_wilt <- list()
for( i in 1:length(wilt_list)){
  print(i)
  temp_wilt[[i]] <- get_representation_results(input_dataset = wilt_list[[i]], dataset_col = wilt_string[[i]])
}

wilt_DT_gauss <- rbindlist(temp_wilt)
wilt_DT_gauss[, dataset:= NULL]
wilt_DT_gauss[, dataset:= unlist(wilt_string)]
wilt_DT_gauss[, dataset:=NULL]

friedmanTest(wilt_DT_gauss)
friedmanAlignedRanksTest(wilt_DT_gauss)
imanDavenportTest(wilt_DT_gauss)
quadeTest(wilt_DT_gauss)
plotCD(wilt_DT_gauss, alpha = 0.1)
colMeans(rankMatrix(wilt_DT_gauss))



# Wilt _ Precision ---------------------------------------------------------------------
wilt_list <- list(wilt1, wilt2, wilt3, wilt4,wilt5, wilt6, wilt7,
                  wilt8, wilt9, wilt10)
wilt_string <- list("wilt1", "wilt2", "wilt3", "wilt4", "wilt5", 
                    "wilt6", "wilt7", "wilt8", "wilt9", "wilt10" )

temp_wilt <- list()
for( i in 1:length(wilt_list)){
  print(i)
  temp_wilt[[i]] <- get_precision_results(input_dataset = wilt_list[[i]], dataset_col = wilt_string[[i]])
}

wilt_DT_precision <- rbindlist(temp_wilt)
wilt_DT_precision[, dataset:= NULL]

friedmanTest(wilt_DT_precision)
friedmanAlignedRanksTest(wilt_DT_precision)
imanDavenportTest(wilt_DT_precision)
quadeTest(wilt_DT_precision)
plotCD(wilt_DT_precision, alpha = 0.1)
colMeans(rankMatrix(wilt_DT_precision))

# Wilt _ Recall ---------------------------------------------------------------------
wilt_list <- list(wilt1, wilt2, wilt3, wilt4,wilt5, wilt6, wilt7,
                  wilt8, wilt9, wilt10)
wilt_string <- list("wilt1", "wilt2", "wilt3", "wilt4", "wilt5", 
                    "wilt6", "wilt7", "wilt8", "wilt9", "wilt10" )

temp_wilt <- list()
for( i in 1:length(wilt_list)){
  print(i)
  temp_wilt[[i]] <- get_recall_results(input_dataset = wilt_list[[i]], dataset_col = wilt_string[[i]])
}

wilt_DT_recall <- rbindlist(temp_wilt)
wilt_DT_recall[, dataset:= NULL]

friedmanTest(wilt_DT_recall)
friedmanAlignedRanksTest(wilt_DT_recall)
imanDavenportTest(wilt_DT_recall)
quadeTest(wilt_DT_recall)
plotCD(wilt_DT_recall, alpha = 0.1)
colMeans(rankMatrix(wilt_DT_recall))

# Wilt _ Precision @N---------------------------------------------------------------------
wilt_list <- list(wilt1, wilt2, wilt3, wilt4,wilt5, wilt6, wilt7,
                  wilt8, wilt9, wilt10)
wilt_string <- list("wilt1", "wilt2", "wilt3", "wilt4", "wilt5", 
                    "wilt6", "wilt7", "wilt8", "wilt9", "wilt10" )

temp_wilt <- list()
for( i in 1:length(wilt_list)){
  print(i)
  temp_wilt[[i]] <- get_precision_results_atN(input_dataset = wilt_list[[i]], dataset_col = wilt_string[[i]])
}

wilt_DT_precision_atN <- rbindlist(temp_wilt)
wilt_DT_precision_atN[, dataset:= NULL]

friedmanTest(wilt_DT_precision_atN)
friedmanAlignedRanksTest(wilt_DT_precision_atN)
imanDavenportTest(wilt_DT_precision_atN)
quadeTest(wilt_DT_precision_atN)
plotCD(wilt_DT_precision_atN, alpha = 0.1)
colMeans(rankMatrix(wilt_DT_precision_atN))

# Wilt - Recall @N---------------------------------------------------------------------
wilt_list <- list(wilt1, wilt2, wilt3, wilt4,wilt5, wilt6, wilt7,
                  wilt8, wilt9, wilt10)
wilt_string <- list("wilt1", "wilt2", "wilt3", "wilt4", "wilt5", 
                    "wilt6", "wilt7", "wilt8", "wilt9", "wilt10" )

temp_wilt <- list()
for( i in 1:length(wilt_list)){
  print(i)
  temp_wilt[[i]] <- get_recall_results_atN(input_dataset = wilt_list[[i]], dataset_col = wilt_string[[i]])
}

wilt_DT_recall_atN <- rbindlist(temp_wilt)
wilt_DT_recall_atN[, dataset:= NULL]

friedmanTest(wilt_DT_recall_atN)
friedmanAlignedRanksTest(wilt_DT_recall_atN)
imanDavenportTest(wilt_DT_recall_atN)
quadeTest(wilt_DT_recall_atN)
plotCD(wilt_DT_recall_atN, alpha = 0.1)
colMeans(rankMatrix(wilt_DT_recall_atN))


# Parkinson ---------------------------------------------------------------------
parkinson_list <- list(parkinson1, parkinson2, parkinson3, parkinson4, parkinson5, 
                  parkinson6, parkinson7, parkinson8, parkinson9, parkinson10)
parkinson_string <- list("parkinson1", "parkinson2", "parkinson3", "parkinson4", "parkinson5", "parkinson6", 
                    "parkinson7", "parkinson8", "parkinson9", "parkinson10")

temp_parkinson <- list()
for( i in 1:length(parkinson_list)){
  print(i)
  temp_parkinson[[i]] <- get_representation_results(input_dataset = parkinson_list[[i]], dataset_col = parkinson_string[[i]])
}

parkinson_DT <- rbindlist(temp_parkinson)
parkinson_DT[, dataset:= NULL]
parkinson_DT[, dataset:= unlist(parkinson_string)]

friedmanTest(parkinson_DT)
friedmanAlignedRanksTest(parkinson_DT)
imanDavenportTest(parkinson_DT)
quadeTest(parkinson_DT)
plotCD(parkinson_DT, alpha = 0.05)
colMeans(rankMatrix(parkinson_DT))
# parkinson_DT1 <- copy(parkinson_DT[, lapply(.SD, function(x) as.numeric(x)), .SDcols = 1:13])

# Parkinson - Precision---------------------------------------------------------------------
parkinson_list <- list(parkinson1, parkinson2, parkinson3, parkinson4, parkinson5, 
                       parkinson6, parkinson7, parkinson8, parkinson9, parkinson10)
parkinson_string <- list("parkinson1", "parkinson2", "parkinson3", "parkinson4", "parkinson5", "parkinson6", 
                         "parkinson7", "parkinson8", "parkinson9", "parkinson10")

temp_parkinson <- list()
for( i in 1:length(parkinson_list)){
  print(i)
  temp_parkinson[[i]] <- get_precision_results(input_dataset = parkinson_list[[i]], dataset_col = parkinson_string[[i]])
}

parkinson_DT_precision <- rbindlist(temp_parkinson)
parkinson_DT_precision[, dataset:= NULL]

friedmanTest(parkinson_DT_precision)
friedmanAlignedRanksTest(parkinson_DT_precision)
imanDavenportTest(parkinson_DT_precision)
quadeTest(parkinson_DT_precision)
plotCD(parkinson_DT_precision, alpha = 0.05)
colMeans(rankMatrix(parkinson_DT_precision))


# Parkinson - Recall---------------------------------------------------------------------
parkinson_list <- list(parkinson1, parkinson2, parkinson3, parkinson4, parkinson5, 
                       parkinson6, parkinson7, parkinson8, parkinson9, parkinson10)
parkinson_string <- list("parkinson1", "parkinson2", "parkinson3", "parkinson4", "parkinson5", "parkinson6", 
                         "parkinson7", "parkinson8", "parkinson9", "parkinson10")

temp_parkinson <- list()
for( i in 1:length(parkinson_list)){
  print(i)
  temp_parkinson[[i]] <- get_recall_results(input_dataset = parkinson_list[[i]], dataset_col = parkinson_string[[i]])
}

parkinson_DT_recall <- rbindlist(temp_parkinson)
parkinson_DT_recall[, dataset:= NULL]

friedmanTest(parkinson_DT_recall)
friedmanAlignedRanksTest(parkinson_DT_recall)
imanDavenportTest(parkinson_DT_recall)
quadeTest(parkinson_DT_recall)
plotCD(parkinson_DT_recall, alpha = 0.05)
colMeans(rankMatrix(parkinson_DT_recall))

# Parkinson - Precision @N---------------------------------------------------------------------
parkinson_list <- list(parkinson1, parkinson2, parkinson3, parkinson4, parkinson5, 
                       parkinson6, parkinson7, parkinson8, parkinson9, parkinson10)
parkinson_string <- list("parkinson1", "parkinson2", "parkinson3", "parkinson4", "parkinson5", "parkinson6", 
                         "parkinson7", "parkinson8", "parkinson9", "parkinson10")

temp_parkinson <- list()
for( i in 1:length(parkinson_list)){
  print(i)
  temp_parkinson[[i]] <- get_precision_results_atN(input_dataset = parkinson_list[[i]], dataset_col = parkinson_string[[i]])
}

parkinson_DT_precision_atN <- rbindlist(temp_parkinson)
parkinson_DT_precision_atN[, dataset:= NULL]

friedmanTest(parkinson_DT_precision_atN)
friedmanAlignedRanksTest(parkinson_DT_precision_atN)
imanDavenportTest(parkinson_DT_precision_atN)
quadeTest(parkinson_DT_precision_atN)
plotCD(parkinson_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(parkinson_DT_precision_atN))


# Parkinson - Recall @N---------------------------------------------------------------------
parkinson_list <- list(parkinson1, parkinson2, parkinson3, parkinson4, parkinson5, 
                       parkinson6, parkinson7, parkinson8, parkinson9, parkinson10)
parkinson_string <- list("parkinson1", "parkinson2", "parkinson3", "parkinson4", "parkinson5", "parkinson6", 
                         "parkinson7", "parkinson8", "parkinson9", "parkinson10")

temp_parkinson <- list()
for( i in 1:length(parkinson_list)){
  print(i)
  temp_parkinson[[i]] <- get_recall_results_atN(input_dataset = parkinson_list[[i]], dataset_col = parkinson_string[[i]])
}

parkinson_DT_recall_atN <- rbindlist(temp_parkinson)
parkinson_DT_recall_atN[, dataset:= NULL]

friedmanTest(parkinson_DT_recall_atN)
friedmanAlignedRanksTest(parkinson_DT_recall_atN)
imanDavenportTest(parkinson_DT_recall_atN)
quadeTest(parkinson_DT_recall_atN)
plotCD(parkinson_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(parkinson_DT_recall_atN))

# Hepatitis ---------------------------------------------------------------------
hepatitis_list <- list(hepatitis1, hepatitis2, hepatitis3, hepatitis4, 
                       hepatitis5, hepatitis6, hepatitis7,hepatitis8, hepatitis9, hepatitis10)
hepatitis_string <- list("hepatitis1", "hepatitis2", "hepatitis3", "hepatitis4", 
                         "hepatitis5", "hepatitis6", "hepatitis7","hepatitis8", "hepatitis9", "hepatitis10")

temp_hepatitis <- list()
for( i in 1:length(hepatitis_list)){
  print(i)
  temp_hepatitis[[i]] <- get_representation_results(input_dataset = hepatitis_list[[i]], dataset_col = hepatitis_string[[i]])
}

hepatitis_DT <- rbindlist(temp_hepatitis)
hepatitis_DT[, dataset:= NULL]
hepatitis_DT[, dataset:= unlist(hepatitis_string)]

friedmanTest(hepatitis_DT)
friedmanAlignedRanksTest(hepatitis_DT)
imanDavenportTest(hepatitis_DT)
quadeTest(hepatitis_DT)
plotCD(hepatitis_DT, alpha = 0.05)
colMeans(rankMatrix(hepatitis_DT))
hepatitis_DT1 <- copy(hepatitis_DT[, lapply(.SD, function(x) as.numeric(x)), .SDcols = 1:18])

# Hepatitis - Precision ---------------------------------------------------
hepatitis_list <- list(hepatitis1, hepatitis2, hepatitis3, hepatitis4, 
                       hepatitis5, hepatitis6, hepatitis7,hepatitis8, hepatitis9, hepatitis10)
hepatitis_string <- list("hepatitis1", "hepatitis2", "hepatitis3", "hepatitis4", 
                         "hepatitis5", "hepatitis6", "hepatitis7","hepatitis8", "hepatitis9", "hepatitis10")

temp_hepatitis <- list()
for( i in 1:length(hepatitis_list)){
  print(i)
  temp_hepatitis[[i]] <- get_precision_results(input_dataset = hepatitis_list[[i]], dataset_col = hepatitis_string[[i]])
}

hepatitis_DT_precision <- rbindlist(temp_hepatitis)
hepatitis_DT_precision[, dataset:= NULL]


friedmanTest(hepatitis_DT_precision)
friedmanAlignedRanksTest(hepatitis_DT_precision)
imanDavenportTest(hepatitis_DT_precision)
quadeTest(hepatitis_DT_precision)
plotCD(hepatitis_DT_precision, alpha = 0.05)
colMeans(rankMatrix(hepatitis_DT_precision_precision))

# Hepatitis - Recall ---------------------------------------------------
hepatitis_list <- list(hepatitis1, hepatitis2, hepatitis3, hepatitis4, 
                       hepatitis5, hepatitis6, hepatitis7,hepatitis8, hepatitis9, hepatitis10)
hepatitis_string <- list("hepatitis1", "hepatitis2", "hepatitis3", "hepatitis4", 
                         "hepatitis5", "hepatitis6", "hepatitis7","hepatitis8", "hepatitis9", "hepatitis10")

temp_hepatitis <- list()
for( i in 1:length(hepatitis_list)){
  print(i)
  temp_hepatitis[[i]] <- get_recall_results(input_dataset = hepatitis_list[[i]], dataset_col = hepatitis_string[[i]])
}

hepatitis_DT_recall <- rbindlist(temp_hepatitis)
hepatitis_DT_recall[, dataset:= NULL]


friedmanTest(hepatitis_DT_recall)
friedmanAlignedRanksTest(hepatitis_DT_recall)
imanDavenportTest(hepatitis_DT_recall)
quadeTest(hepatitis_DT_recall)
plotCD(hepatitis_DT_recall, alpha = 0.05)
colMeans(rankMatrix(hepatitis_DT_recall_recall))

# Hepatitis - Precision @N---------------------------------------------------
hepatitis_list <- list(hepatitis1, hepatitis2, hepatitis3, hepatitis4, 
                       hepatitis5, hepatitis6, hepatitis7,hepatitis8, hepatitis9, hepatitis10)
hepatitis_string <- list("hepatitis1", "hepatitis2", "hepatitis3", "hepatitis4", 
                         "hepatitis5", "hepatitis6", "hepatitis7","hepatitis8", "hepatitis9", "hepatitis10")

temp_hepatitis <- list()
for( i in 1:length(hepatitis_list)){
  print(i)
  temp_hepatitis[[i]] <- get_precision_results_atN(input_dataset = hepatitis_list[[i]], dataset_col = hepatitis_string[[i]])
}

hepatitis_DT_precision_atN <- rbindlist(temp_hepatitis)
hepatitis_DT_precision_atN[, dataset:= NULL]


friedmanTest(hepatitis_DT_precision_atN)
friedmanAlignedRanksTest(hepatitis_DT_precision_atN)
imanDavenportTest(hepatitis_DT_precision_atN)
quadeTest(hepatitis_DT_precision_atN)
plotCD(hepatitis_DT_precision_atN, alpha = 0.05)
colMeans(rankMatrix(hepatitis_DT_precision_precision_atN))

# Hepatitis - Recall @N---------------------------------------------------
hepatitis_list <- list(hepatitis1, hepatitis2, hepatitis3, hepatitis4, 
                       hepatitis5, hepatitis6, hepatitis7,hepatitis8, hepatitis9, hepatitis10)
hepatitis_string <- list("hepatitis1", "hepatitis2", "hepatitis3", "hepatitis4", 
                         "hepatitis5", "hepatitis6", "hepatitis7","hepatitis8", "hepatitis9", "hepatitis10")

temp_hepatitis <- list()
for( i in 1:length(hepatitis_list)){
  print(i)
  temp_hepatitis[[i]] <- get_recall_results_atN(input_dataset = hepatitis_list[[i]], dataset_col = hepatitis_string[[i]])
}

hepatitis_DT_recall_atN <- rbindlist(temp_hepatitis)
hepatitis_DT_recall_atN[, dataset:= NULL]

friedmanTest(hepatitis_DT_recall_atN)
friedmanAlignedRanksTest(hepatitis_DT_recall_atN)
imanDavenportTest(hepatitis_DT_recall_atN)
quadeTest(hepatitis_DT_recall_atN)
plotCD(hepatitis_DT_recall_atN, alpha = 0.05)
colMeans(rankMatrix(hepatitis_DT_recall_atN))

# Glass -------------------------------------------------------------------
glass_DT <- get_representation_results(input_dataset = glass, dataset_col = "glass")
glass_DT[, dataset:=NULL]
glass_DT[, dataset:="glass"]
glass_DT1 <- copy(glass_DT[, lapply(.SD, function(x) as.numeric(x)), .SDcols = 1:18])

# Glass - Precision -------------------------------------------------------
glass_DT_Precision <- get_precision_results(input_dataset = glass, dataset_col = "glass")
glass_DT_Precision[, dataset:=NULL]
# Glass - Recall -------------------------------------------------------
glass_DT_Recall <- get_recall_results(input_dataset = glass, dataset_col = "glass")
glass_DT_Recall[, dataset:=NULL]

# Glass - Precision @N-------------------------------------------------------
glass_DT_Precision_atN <- get_precision_results_atN(input_dataset = glass, dataset_col = "glass")
glass_DT_Precision_atN[, dataset:=NULL]
# Glass - Recall @N-------------------------------------------------------
glass_DT_Recall_atN <- get_recall_results_atN(input_dataset = glass, dataset_col = "glass")
glass_DT_Recall_atN[, dataset:=NULL]


# ALOI -------------------------------------------------------------------
aloi_DT <- get_representation_results(input_dataset = aloi, dataset_col = "aloi")
aloi_DT[, dataset:="aloi"]
aloi_DT1 <- copy(aloi_DT[, lapply(.SD, function(x) as.numeric(x)), .SDcols = 1:18])

# ALOI- Precision ---------------------------------------------------------
aloi_DT_precision <- get_precision_results(input_dataset = aloi, dataset_col = "aloi")
aloi_DT_precision[, dataset:=NULL]
# ALOI - Recall ---------------------------------------------------------
aloi_DT_recall <- get_recall_results(input_dataset = aloi, dataset_col = "aloi")
aloi_DT_recall[, dataset:=NULL]

# ALOI- Precision @N---------------------------------------------------------
aloi_DT_precision_atN <- get_precision_results_atN(input_dataset = aloi, dataset_col = "aloi")
aloi_DT_precision_atN[, dataset:=NULL]
# ALOI - Recall @N---------------------------------------------------------
aloi_DT_recall_atN <- get_recall_results_atN(input_dataset = aloi, dataset_col = "aloi")
aloi_DT_recall_atN[, dataset:=NULL]


# WPBC -------------------------------------------------------------------
wpbc_DT_11 <- get_representation_results(input_dataset = wpbc, dataset_col = "wpbc")
wpbc_DT_11[, dataset:=NULL]
wpbc_DT_11[, dataset:="wpbc"]
wpbc_DT1 <- copy(wpbc_DT_11[, lapply(.SD, function(x) as.numeric(x)), .SDcols = 1:18])


# WPBC - Precision --------------------------------------------------------
wpbc_DT_Precision <- get_precision_results(input_dataset = wpbc, dataset_col = "wpbc")
wpbc_DT_Precision[, dataset:= NULL]
# WPBC - Recall --------------------------------------------------------
wpbc_DT_Recall <- get_recall_results(input_dataset = wpbc, dataset_col = "wpbc")
wpbc_DT_Recall[, dataset:= NULL]
# WPBC - Precision @N --------------------------------------------------------
wpbc_DT_Precision_atN <- get_precision_results_atN(input_dataset = wpbc, dataset_col = "wpbc")
wpbc_DT_Precision_atN[, dataset:= NULL]
# WPBC - Recall @N--------------------------------------------------------
wpbc_DT_Recall_atN <- get_recall_results_atN(input_dataset = wpbc, dataset_col = "wpbc")
wpbc_DT_Recall_atN[, dataset:= NULL]


# Ionoshpere -------------------------------------------------------------------
ionosphere_DT <-  get_representation_results(input_dataset = ionosphere, dataset_col = "ionosphere")
ionosphere_DT[, dataset:=NULL]
ionosphere_DT[, dataset:="ionosphere"]
ionosphere_DT1 <- copy(ionosphere_DT[, lapply(.SD, function(x) as.numeric(x)), .SDcols = 1:18])

# Ionoshpere - Precision-------------------------------------------------------------------
ionosphere_DT_precision <-  get_precision_results(input_dataset = ionosphere, dataset_col = "ionosphere")
ionosphere_DT_precision[, dataset:= NULL]
# Ionoshpere - Recall-------------------------------------------------------------------
ionosphere_DT_recall <-  get_recall_results(input_dataset = ionosphere, dataset_col = "ionosphere")
ionosphere_DT_recall[, dataset:= NULL]

# Ionoshpere - Precision @N-------------------------------------------------------------------
ionosphere_DT_precision_atN <-  get_precision_results_atN(input_dataset = ionosphere, dataset_col = "ionosphere")
ionosphere_DT_precision_atN[, dataset:= NULL]
# Ionoshpere - Recall @N-------------------------------------------------------------------
ionosphere_DT_recall_atN <-  get_recall_results_atN(input_dataset = ionosphere, dataset_col = "ionosphere")
ionosphere_DT_recall_atN[, dataset:= NULL]


# Gather -------------------------------------------------------------------
fft1 <- rbindlist(list(annthyroid_DT, 
                      pendigits_DT, 
                      glass_DT, 
                      ionosphere_DT, 
                      wave_DT, 
                      wpbc_DT_11,
                      shuttle_DT, 
                      cardio_DT, 
                      wdbc_DT, 
                      spam_DT, 
                      page_DT, 
                      pima_DT, 
                      heart_DT, 
                      wilt_DT,
                      stamps_DT, 
                      aloi_DT, 
                      parkinson_DT, 
                      hepatitis_DT,
                      arrhythmia_DT
                      ))

#fft[, `:=` (UFR_5 = NULL, UFR_10 = NULL, UFR_15 = NULL, UFR_20 = NULL, UFR_25 = NULL, UFR_30 = NULL)]
# write_fst(fft, "~/Desktop/gather_AYC.fst")
# fft <- read_fst("~/Desktop/gather_AYC.fst", as.data.table = T)
fft1[, dataset:= NULL]
#fft[, `:=` (AFR_5 = NULL, AFR_15 = NULL, AFR_25 = NULL, UFR_5 = NULL, UFR_15 = NULL, UFR_25 = NULL)]
# fft <- rbindlist(list(fft, hepatitis_DT1, parkinson_DT1))

fft1[1:10, dataset:= "annthyroid"]
fft1[11:19, dataset:= "pendigits"]
fft1[22:31, dataset:= "wave"]
fft1[32:42, dataset:= "shuttle"]
fft1[43:51, dataset:= "cardio"]
fft1[52:61, dataset:= "wdbc"]
fft1[62:71, dataset:= "spam"]

fft1[72:81, dataset:= "page"]
fft1[82:91, dataset:= "pima"]
fft1[92:101, dataset:= "heart"]
fft1[102:111, dataset:= "wilt"]
fft1[112:121, dataset:= "stamps"]
fft1[123:132, dataset:= "parkinson"]
fft1[123:132, dataset:= "parkinson"]
fft1[133:.N, dataset:= "hepatitis"]


wbc <- fft1[dataset == "wdbc"]
wbc[, dataset:= "wbc"]
fft2 <- rbindlist(list(fft1, wbc))



temp00 <- fft2[, lapply(.SD, function(x) mean(x)), .SDcols = 1:19, by = .(dataset)]
lymphography <- temp00[3]
lymphography[ ,dataset:= "lymphography"]
temp00_00 <- rbindlist(list(temp00, lymphography))


#write_fst(temp00_00, "~/Desktop/mean_AUC_allDatasets.fst")
temp00_00 <- read_fst("~/Desktop/mean_AUC_allDatasets.fst", as.data.table = T)
temp00_00[16, UFR_1:=0.533885]
# temp00_00[, Win:= c("No", "Yes", "Yes", "Yes", "No", "Yes", "No", "No", "Yes", "No", "No", "No", "No", "No", "No", "No", "No", "No", "Yes", "Yes")]
temp111 <- copy(temp00_00[(dataset %in% c("wave", "shuttle", "pendigits", "aloi", "wpbc", "shuttle", "glass", "ionosphere", "wdbc", "wbc", 
                                          "lymphography") )])

temp00_00[, dataset:= NULL]
temp00_00[, `:=` (UFR_3 = NULL, UFR_8 = NULL)]
temp00_00[, `:=` (AFR_30 = NULL, AFR_25 = NULL, AFR_20 = NULL, AFR_15 = NULL, AFR_10 = NULL, AFR_5 = NULL, AFR_8 = NULL, AFR_3 = NULL, AFR_1 = NULL)]

# temp00_00[, `:=` (AFR_3 = NULL, AFR_8 = NULL)]
# temp00_00[, `:=` (UFR_30 = NULL, UFR_25 = NULL, UFR_20 = NULL, UFR_15 = NULL, UFR_10 = NULL, UFR_5 = NULL, UFR_8 = NULL, UFR_3 = NULL, UFR_1 = NULL)]

# temp111[, `:=` (UFR_3 = NULL, UFR_8 = NULL)]
# temp111[, `:=` (AFR_30 = NULL, AFR_25 = NULL, AFR_20 = NULL, AFR_15 = NULL, AFR_10 = NULL, AFR_5 = NULL, AFR_3 = NULL, AFR_1 = NULL)]

# temp111[, `:=` (AFR_3 = NULL, AFR_8 = NULL)]
# temp111[, `:=` (UFR_30 = NULL, UFR_25 = NULL, UFR_20 = NULL, UFR_15 = NULL, UFR_10 = NULL, UFR_5 = NULL, UFR_3 = NULL, UFR_1 = NULL)]

friedmanAlignedRanksTest(temp00_00)
plotCD(temp00_00, alpha = 0.05)
colMeans(rankMatrix(temp00_00))


temp111[ , dataset:= NULL]

friedmanTest(temp111)
imanDavenportTest(temp111)
plotCD(temp111, alpha = 0.1)
colMeans(rankMatrix(temp111))





# ggplot(res) +
#   aes(x = Representation, y = AUC, fill = col) +
#   geom_boxplot() +
#   scale_fill_hue() +
#   theme_minimal()+
#   ggtitle(label = paste0(title_ggplot))


# fix NA or infinite outlier scores ---------------------------------------

dt <- fread("~/Downloads/WBC_withoutdupl_norm_v01.results.csv")
# Check NAs
temp <- dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:dim(dt)[2]]
temp1 <- data.table::transpose(temp)
temp1[, names1:= names(temp)]
temp1[V1>0]
# Check infinite
temp <- dt[, lapply(.SD, function(x) sum(is.infinite(x))), .SDcols = 1:dim(dt)[2]]
temp1 <- data.table::transpose(temp)
temp1[, names1:= names(temp)]
temp1[V1>0]





# another approach for ensemble -------------------------------------------

tempDT <- copy(DT_normalized_rank[Iteration==1])
ff <- tempDT[, unique(normalized_rank), by = .(Representation1, id)]
ff1 <- ff[, mean(V1), by = .(Representation1, id)]
temp <- copy(tempDT[, .(id, Label)])
temp1 <- unique(temp)

DT_mean_labeled <- ff1[temp1, on = "id"]
res <- DT_mean_labeled[, auc(Label, V1), by = .(Representation1)]


# another approach for ensemble -------------------------------------------
tempDT <- copy(DT_normalized_rank[Iteration==2])
ff <- tempDT[, unique(normalized_rank), by = .(Representation1, id)]
ff_1 <- ff[Representation1 != "Augmented"]

ff1 <- ff[, mean(V1), by = .(id)]
temp <- copy(tempDT[, .(id, Label)])
temp1 <- unique(temp)

DT_mean_labeled <- ff1[temp1, on = "id"]
res <- DT_mean_labeled[, auc(Label, V1), by = .(Representation1)]





number_of_representations1 <- 15
DT_normalized_rank <- copy(get_normalized_DT(given_dataset = iono, number_of_representations = number_of_representations1,
                                             max_iterations = 30))
jjj <- DT_normalized_rank[, sum(Outlier), by = .(id, Representation1, Iteration)]
lli <- DT_normalized_rank[, .(id, Label)]
lli_1 <- unique(lli)

iiiis <- jjj[lli_1, on = "id"]
iter <- 24
glue('Iteration {iter}')
iiiis[V1==0 & Label == "no" & Iteration == iter, .N, by = "Representation1"]
iiiis[V1==0 & Label == "yes" & Iteration == iter, .N, by = "Representation1"]
iiiis[V1>0 & Label == "yes" & Iteration == iter, .N, by = "Representation1"]
iiiis[V1>0 & Label == "no" & Iteration == iter, .N, by = "Representation1"]


# prototype ---------------------------------------------------------------



ggg <- DT_normalized_rank[Iteration==10, .(id, Label, Representation, normalized_rank, Outlier, Representation1)]
ggg_label <- copy(ggg[Representation %in% c("Augmented_1"), .(id, Label)])

# ggg[, `:=` (Label = NULL)]
# 
# dcasted_gg <- dcast.data.table(ggg, id ~ Representation, value.var = "normalized_rank")
# M <- corrplot::corrplot(M)

tt <- ggg[, sum(Outlier), by = .(id, Representation1)]
tt[, Label:= rep(ggg_label[, Label], 3)]
tt[V1 == 0, .N, by = .(Label, Representation1)]

tt[, auc(Label, V1)]


tt1 <- ggg[Representation %in% c("Original", "Augmented_11"), mean(normalized_rank), by = .(id)]
tt1[, Label:= ggg_label[, Label]]
tt1[, auc(Label, V1)]


tt000 <- ggg[Representation %in% c("Original")]
tt000[, Label:= ggg_label[, Label]]
tt000[Outlier == 0, .N, by = Label]
tt000[, auc(Label, normalized_rank)]




# prototype2 --------------------------------------------------------------

temp_list <- list()
k <- 0
for( i in 1:15){
  for(j in 1:30){
    k <- k + 1
    temp_list[[k]] <- get_normalized_rank(dataset = wilt_non1, representation_i = j, iteration_j = i)  
  }
  
} 


list_2 <- list()
for(i in 1:15){
  DT_or1 <- copy(wilt_non1[Representation == "Original" & Iteration == i])
  ordered_or <- DT_or1[order(scores, decreasing = F)]
  ordered_or[, rank:= 1:.N]
  ordered_or[, normalized_rank:= (rank-1)/(.N-1)]
  setkey(ordered_or, id)
  list_2[[i]] <- ordered_or
}

a1 <- rbindlist(list_2)
a2 <- rbindlist(temp_list)
final1 <- rbindlist(list(a1, a2))


final1[, `:=` (scores = NULL, Label = NULL, Outlier = NULL, Representation1 = NULL, rank = NULL)]
jj <- final1[Iteration==1]
yyy <- dcast.data.table(jj, id~Representation+Iteration, value.var = "normalized_rank")
yyy[, id:=NULL]
M1 <- cor(yyy)
corrplot::corrplot(M1)

fff <- final1[, auc(Label, normalized_rank), by = .(Iteration, Representation)]
fff[Representation == "Original", col:= "red"]
fff[Representation != "Original", col:= "yellow"]
esquisser()


kmeans(x = M1, centers = 2)


pppp <- final1[Representation %in% c("Unsupervised_5", "Unsupervised_6") & Iteration==1, mean(normalized_rank), by = id]
pppp[, Label:= final1[Representation %in% c("Unsupervised_5") & Iteration==1, Label]]
pppp[, auc(Label, V1)]

final1[Representation %in% c("Unsupervised_5", "Unsupervised_6") & Iteration==1, auc(Label, normalized_rank), by = Representation]

ggplot(fff) +
  aes(x = Representation, y = V1, fill = col) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal()




# prototype3 --------------------------------------------------------------

shuttle_scores <- fread("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/Shuttle_withoutdupl_norm_v05.results.csv")
shuttle_original <- fread("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/Shuttle_withoutdupl_norm_v05.csv")

glass_scores <- fread("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/Glass_withoutdupl_norm.results.csv")
glass_original <- fread("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/Glass_withoutdupl_norm.csv")


wilt_scores <- fread("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/Wilt_withoutdupl_norm_02_v06.results.csv")
wilt_original <- fread("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/Wilt_withoutdupl_norm_02_v06.csv")




res2 <- list()
for( i in 1:10){
  shuutle_res <- create_unsupervised_scoresDT_99(dataset = shuttle_scores, percentage_OD = 1, mixed_view_features = 1)
  shuttle_norm <- shuutle_res[[1]]
  shuttle_norm[, Label:= shuttle_scores$Label]
  
  
  unnormalized_shuutle <- shuttle_scores[,  .SD, .SDcols = shuutle_res[[2]]]
  unnormalized_shuutle[, Label:= shuttle_scores$Label]
  
  
  print(unnormalized_shuutle[, lapply(.SD, function(x) auc(Label, x, quiet=T)), .SDcols = patterns('ODIN')])
  print(shuttle_norm[, auc(Label, ODIN, quiet=T)])
  
  
  res2[[i]] <- data.table::transpose(shuttle_norm[, lapply(.SD ,function(x) auc(Label, x)), .SDcols = 1:12])[, mean(V1)]
  
}

mean(unlist(res1))
mean(unlist(res2))

sd(unlist(res1))
sd(unlist(res2))


shuttle_original <- glass_original
shuutle_res <- create_unsupervised_scoresDT_99(dataset = glass_scores, percentage_OD = 1, mixed_view_features = 1)
shuttle_norm <- shuutle_res[[1]]
shuttle_norm[, Label:= glass_scores$Label]


unnormalized_shuutle <- shuttle_scores[,  .SD, .SDcols = shuutle_res[[2]]]
unnormalized_shuutle[, Label:= shuttle_scores$Label]

#shuttle_original <- shuttle_original[, lapply(.SD, function(x) -1*x), .SDcols = 1:9]
augm <- dplyr::bind_cols(shuttle_original, shuttle_norm)
labels <- copy(augm[, Label])
augm[, `:=` (Label1 = NULL, Label = NULL, id = NULL)]
shuttle_norm[, Label:=NULL]
# shuttle_norm[, LoOP:= 1-LoOP]
# shuttle_norm[, KDEOS:= 1-KDEOS]
# shuttle_norm[, FastABOD:= 1-FastABOD]
# 
# augm[, LoOP:= 1-LoOP]
# augm[, KDEOS:= 1-KDEOS]
# augm[, FastABOD:= 1-FastABOD]


M <- cor(shuttle_norm)
corrplot::corrplot(M)

kmeans(M, 2)
esquisse::esquisser()



create_unsupervised_scores_non_normalized_DT(dataset = glass_scores, percentage_OD = 1, mixed_view_features = 1)



create_unsupervised_scoresDT_99 <- function(dataset, percentage_OD, mixed_view_features) {
  
  
  DToutliers1 <- dataset
  
  
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
  
  
  rr <- c(sampleKNN, sampleKNNW, sampleLOF, sampleSimplifiedLOF, 
          sampleLoOP, sampleLDOF, sampleODIN, sampleFastABOD, 
          sampleKDEOS, sampleLDF, sampleINFLO, sampleCOF)
  
  return(list(mixed_arthur = DToutliers_all, rr = rr))
}
create_unsupervised_scores_non_normalized_DT <- function(dataset, percentage_OD, mixed_view_features) {
  
  

  DToutliers1 <- dataset
  
  
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
    
  } else{
    KNN_DT <- NULL
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
  } else{
    KNNW_DT <- NULL
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
  } else{
    LOF_DT <- NULL
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
  } else{
    SimplifiedLOF_DT <- NULL
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
  } else{
    LDOF_DT <- NULL
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
  } else{
    ODIN_DT <- NULL
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
    
  } else{
    FastABOD_DT <- NULL
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
    
  } else{
    LDF_DT <- NULL
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
  } else{
    INFLO_DT <- NULL
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
    
  } else{
    COF_DT <- NULL
  }
  
  DToutliers_all <- dplyr::bind_cols(KNN_DT, KNNW_DT, LOF_DT, SimplifiedLOF_DT, 
                                     LoOP_DT2, LDOF_DT, ODIN_DT, FastABOD_DT,
                                     KDEOS_DT2, LDF_DT, INFLO_DT, COF_DT)
  
  
  
  
  return(mixed_arthur = DToutliers_all)
}



# prototype 4 -------------------------------------------------------------

aloi_ranking <- read_fst("~/Downloads/ALOInormalized_ranking_AUC.fst")
glass_ranking <- read_fst("~/Downloads/Glassnormalized_ranking_AUC.fst")
ionosphere_ranking <- read_fst("~/Downloads/Ionospherenormalized_ranking_AUC.fst")
KDD_ranking <- read_fst("~/Downloads/KDDCup99normalized_ranking_AUC.fst")
lympho_ranking <- read_fst("~/Downloads/Lymphographynormalized_ranking_AUC.fst")
pen_ranking <- read_fst("~/Downloads/PenDigitsnormalized_ranking_AUC.fst")
shuttle_ranking <- read_fst("~/Downloads/Shuttlenormalized_ranking_AUC.fst")
wave_ranking <- read_fst("~/Downloads/Waveformnormalized_ranking_AUC.fst")
WBC_ranking <- read_fst("~/Downloads/WBCnormalized_ranking_AUC.fst")
WDBC_ranking <- read_fst("~/Downloads/WDBCnormalized_ranking_AUC.fst")
WPBC_ranking <- read_fst("~/Downloads/WPBCnormalized_ranking_AUC.fst")

arrythmia_ranking <- read_fst("~/Downloads/Arrhythmianormalized_ranking_AUC.fst")
hepatitis_ranking <- read_fst("~/Downloads/Hepatitisnormalized_ranking_AUC.fst")
parkinson_ranking <- read_fst("~/Downloads/Parkinsonnormalized_ranking_AUC.fst")
wilt_ranking <- read_fst("~/Downloads/Wiltnormalized_ranking_AUC.fst")
cardio_ranking <- read_fst("~/Downloads/Cardiotocographynormalized_ranking_AUC.fst")
Annythyroid_ranking <- read_fst("~/Downloads/Annthyroidnormalized_ranking_AUC.fst")
heart_ranking <- read_fst("~/Downloads/HeartDiseasenormalized_ranking_AUC.fst")
internet_ranking <- read_fst("~/Downloads/InternetAdsnormalized_ranking_AUC.fst")
page_ranking <- read_fst("~/Downloads/PageBlocksnormalized_ranking_AUC.fst")
pima_ranking <- read_fst("~/Downloads/Pimanormalized_ranking_AUC.fst")
spam_ranking <- read_fst("~/Downloads/SpamBasenormalized_ranking_AUC.fst")
stamps_ranking <- read_fst("~/Downloads/Stampsnormalized_ranking_AUC.fst")

DT <- rbindlist(list(aloi_ranking, glass_ranking, ionosphere_ranking, KDD_ranking,
                     lympho_ranking, pen_ranking, shuttle_ranking,wave_ranking, WBC_ranking,
                     WDBC_ranking, WPBC_ranking, 
                     arrythmia_ranking, hepatitis_ranking, parkinson_ranking,
                     wilt_ranking, cardio_ranking, Annythyroid_ranking,
                     heart_ranking, internet_ranking, page_ranking, 
                     pima_ranking,  spam_ranking, stamps_ranking 
                     ))

DT[col %in% c("ALOI", "Glass", "Ionosphere", "KDDCup99", "Lymphography","PenDigits","Shuttle", 
              "Waveform", "WBC", "WDBC", "WPBC"), group:= "Literature"]
DT[!(col %in% c("ALOI", "Glass", "Ionosphere", "KDDCup99", "Lymphography","PenDigits","Shuttle", 
              "Waveform", "WBC", "WDBC", "WPBC")), group:= "Semantics"]
setnames(x = DT, c("V1", "col"), c("AUC", "Dataset"))


tempDT <- DT[, mean(AUC), by = .(Algorithm, group)]
tempDT[, id:= 1:.N, by = group]
tempDT1 <- dcast.data.table(data = tempDT, Algorithm~group, value.var = "V1")

tempDT1[, Algorithm:=NULL]
friedmanTest(data = tempDT1)
plotCD(tempDT1, alpha = 0.05)


esquisser()


library(tidyverse)
library(forcats)

p <- DT %>%
  mutate(Dataset = fct_relevel(Dataset, 
                               "ALOI", "Glass", "Ionosphere", "KDDCup99", 
                               "Lymphography","PenDigits","Shuttle", 
                               "Waveform", "WBC", "WDBC", "WPBC", 
                               "Annthyroid", "Arrhythmia", "Cardiotocography", 
                               "HeartDisease", "Hepatitis", "InternetAds",
                               "PageBlocks", "Parkinson", "Pima", 
                               "SpamBase", "Stamps", "Wilt")) %>%
  ggplot(DT) +
  aes(x = Dataset, y = AUC, fill = group) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal() +
  coord_flip()


DT[, Dataset:= as.factor(Dataset)]

DT2 <- DT %>%
  mutate(Dataset = fct_relevel(Dataset, 
                               "ALOI", "Glass", "Ionosphere", "KDDCup99", 
                               "Lymphography","PenDigits","Shuttle", 
                               "Waveform", "WBC", "WDBC", "WPBC", 
                               "Annthyroid", "Arrhythmia", "Cardiotocography", 
                               "HeartDisease", "Hepatitis", "InternetAds",
                               "PageBlocks", "Parkinson", "Pima", 
                               "SpamBase", "Stamps", "Wilt"))
ggplot(DT2) +
  aes(x = Dataset, y = AUC, fill = group) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal() +
  coord_flip()+
  geom_hline(yintercept = DT[, mean(AUC), by = .(group)][group == "Literature", V1], 
             color = "#9c7878", size=1)+
  geom_hline(yintercept = DT[, mean(AUC), by = .(group)][group == "Semantics", V1], 
             color = "#0095b6", size=1)+
  ggtitle(label = paste0('Lines represent: Avg AUC of Semantic datasets = ', 0.7487072, " & Avg AUC of Literature datasets = ", 0.8305352))





ggplot(DT) +
  aes(x = Dataset, y = AUC, fill = group) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal()
  




# prototype 5 -------------------------------------------------------------

ttt <- fread("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/Cardiotocography_withoutdupl_norm_02_v01.results.csv")
temp_unsuper <- create_unsupervised_scores_non_normalized_DT(datasetname = "Shuttle_withoutdupl_norm_v05", percentage_OD = 1, mixed_view_features = 1)
temp_unsuper[, id := NULL]

temp_unsuper1 <- create_unsupervised_scoresDT(datasetname = "Shuttle_withoutdupl_norm_v05", percentage_OD = 1, mixed_view_features = 1)



# read credit -------------------------------------------------------------

credit_DT <- fread("~/Downloads/creditcard.csv")
tempDT <- fread("~/Downloads/ELKI_1/Shuttle_dataset_ready.csv")

credit_DT1 <- read.arff("~/R Language Default Dir/Github-projects/multiview-oneclass/data/downloaded-data/Shuttle_withoutdupl_norm_v05.arff")


mean_values <- data.table::transpose(temp_unsuper[, lapply(.SD, function(x) mean(x)), .SDcols = 1:12])

squared_mean_values <- data.table::transpose(temp_unsuper[, lapply(.SD, function(x) (mean(x)^2)), .SDcols = 1:12])
squared_mean_values1 <- data.table::transpose(temp_unsuper[, lapply(.SD, function(x) mean(x^2)), .SDcols = 1:12])

yy <- list()
for(i in 1:12){
  ee <- as.data.table(dnorm(as.vector(unlist(temp_unsuper[, .SD, .SDcols = i])), 
                            mean = mean_values[i, V1], 
                            sd = (squared_mean_values1[i, V1] - squared_mean_values[i, V1])))
  ee[, algo:= names(temp_unsuper)[i]]
  ee[, Label:= credit_DT1$outlier]
  yy[[i]] <- ee
}


yy99 <- rbindlist(yy)
esquisser()

yy1 <- temp_unsuper1[, .(LDOF)]
setnames(x = yy1, old = "LDOF", "V1")
yy1[, Label:= credit_DT1$outlier]

esquisser()
ggplot(yy2) +
 aes(x = V1, fill = Label) +
 geom_density(adjust = 1L) +
 scale_fill_hue() +
 theme_minimal()

yy2 <- as.data.table(dgamma(temp_unsuper$SimplifiedLOF, shape = ((mean_values$LDOF)^2)/(squared_mean_values1$SimplifiedLOF - squared_mean_values$SimplifiedLOF), 
                           rate = sqrt((squared_mean_values1$SimplifiedLOF - squared_mean_values$SimplifiedLOF))/(mean_values$LDOF)^2))
yy2[, Label:= credit_DT1$outlier]




xseq<-seq(-4,4,.01)
densities<-dnorm(xseq, 0,1)
cumulative<-pnorm(xseq, 0, 1)
randomdeviates<-rnorm(1000,0,1)

par(mfrow=c(1,3), mar=c(3,4,4,2))


# evaluate OCSVM results --------------------------------------------------


tempDT <- DT_normalized_rank[, auc(Label, Outlier, quiet  =T), by = .(Iteration, Representation)]
tempDT[Representation %in% c("Unsupervised_15", "Unsupervised_26", "Unsupervised_13", "Unsupervised_28", "Unsupervised_5"), Representation:= "Unsupervised"]
tempDT[Representation %in% c("Augmented_15", "Augmented_26", "Augmented_13", "Augmented_28", "Augmented_5"), Representation:= "Augmented"]


esquisse::esquisser()

hhh <- DT_normalized_rank[Representation1 != "Original", sum(Outlier), by = .(id, Iteration, Representation1)]

ppb <- DT_normalized_rank[, .(id, Label)]
ppb1 <- unique(ppb)

hhh1 <- hhh[ppb1, on = c("id")]

dt <- rbindlist(list(hhh1, DT_normalized_rank[Representation=="Original", .(id, Iteration, Representation1, Outlier, Label)]), use.names = F)
dt[Representation1 %in% c("Unsupervised", 'Augmented') & V1 == 0, sum_Outlier:= 0]
dt[Representation1 %in% c("Unsupervised", 'Augmented') & V1 != 0, sum_Outlier:= 1]
dt[Representation1 %in% c("Original") , sum_Outlier:= V1]


precision_unsuper <- list()
precision_augmented <- list()
precision_original <- list()

recall_unsuper <- list()
recall_augmented <- list()
recall_original <- list()


for(i in 1: hhh1[, max(Iteration)]){
  temp_iter1 <- dt[Iteration == i, .N, by = .(sum_Outlier, Label, Representation1)]
  
  # Unsupervised
  TP_unsupervised <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Unsupervised"]
  if(dim(TP_unsupervised)[1] == 0){
    TP_unsupervised <- 0
  }else{TP_unsupervised <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Unsupervised", N]}
  FP_unsupervised <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Unsupervised", N]
  FN_unsupervised <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Unsupervised", N]
  
  # Augmented
  TP_augmented <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Augmented"]
  if(dim(TP_augmented)[1] == 0){
    TP_augmented <- 0
  } else{TP_augmented <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Augmented", N]}
  FP_augmented <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Augmented", N]
  FN_augmented <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Augmented", N]
  
  # Original
  TP_original <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Original"]
  if(dim(TP_original)[1] == 0){
    TP_original <- 0
  }else{TP_original <- temp_iter1[sum_Outlier == 1 & Label == "yes" & Representation1 == "Original", N]}
  
  FP_original <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Original"]
  if(dim(FP_original)[1] == 0){
    FP_original <- 0
  }else{FP_original <- temp_iter1[sum_Outlier == 1 & Label == "no" & Representation1 == "Original", N]}
  
  FN_original <- temp_iter1[sum_Outlier == 0 & Label == "yes" & Representation1 == "Original", N]
  
  
  precision_unsuper[[i]] <- TP_unsupervised/(TP_unsupervised+FP_unsupervised)
  precision_augmented[[i]] <- TP_augmented/(TP_augmented+FP_augmented)
  precision_original[[i]] <- TP_original/(TP_original+FP_original)
  
  recall_unsuper[[i]] <- TP_unsupervised/(TP_unsupervised+FN_unsupervised)
  recall_augmented[[i]] <- TP_augmented/(TP_augmented+FN_augmented)
  recall_original[[i]] <- TP_original/(TP_original+FN_original)
  
}

recall_aug <- data.table(recall = unlist(recall_augmented))
recall_aug[, Representation:= "Augmented"]
recall_un <- data.table(recall = unlist(recall_unsuper))
recall_un[, Representation:= "Unsupervised"]
recall_or <- data.table(recall = unlist(recall_original))
recall_or[, Representation:= "Original"]

DT_recall <- rbindlist(list(recall_aug, recall_un, recall_or))

DT_recall1 <- dplyr::bind_cols(recall_aug, recall_un, recall_or)
DT_recall1[, Representation:= NULL]
DT_recall1[, Representation1:= NULL]
DT_recall1[, Representation2:= NULL]


precision_aug <- data.table(precision = unlist(precision_augmented))
precision_aug[, Representation:= "Augmented"]
precision_un <- data.table(precision = unlist(precision_unsuper))
precision_un[, Representation:= "Unsupervised"]
precision_or <- data.table(precision = unlist(precision_original))
precision_or[, Representation:= "Original"]

DT_precision <- rbindlist(list(precision_aug, precision_un, precision_or))

DT_precision1 <- dplyr::bind_cols(precision_aug, precision_un, precision_or)
DT_precision1[, Representation:= NULL]
DT_precision1[, Representation1:= NULL]
DT_precision1[, Representation2:= NULL]

for (jj in 1:ncol(DT_precision1)) set(DT_precision1, i = which(is.na(DT_precision1[[jj]])), j = jj, v = 0)


plotCD(DT_recall1)
plotCD(DT_precision1)


ggplot(DT_recall) +
 aes(x = Representation, y = recall, fill = Representation) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal()


# evaluate precision ------------------------------------------------------

wave_DT_precision_avg <- wave_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wave_DT_precision)[2]] 
pendigits_DT_precision_avg <- pendigits_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(pendigits_DT_precision)[2]] 
wdbc_DT_precision_avg <- wdbc_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wdbc_DT_precision)[2]] 
annthyroid_DT_precision_avg <- annthyroid_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(annthyroid_DT_precision)[2]] 
shuttle_DT_precision_avg <- shuttle_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(shuttle_DT_precision)[2]]
pima_DT_precision_avg <- pima_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(pima_DT_precision)[2]] 
arrhythmia_DT_precision_avg <- arrhythmia_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(arrhythmia_DT_precision)[2]] 
heart_DT_precision_avg <- heart_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(heart_DT_precision)[2]] 
spam_DT_precision_avg <- spam_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(spam_DT_precision)[2]] 
page_DT_precision_avg <- page_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(page_DT_precision)[2]] 
stamps_DT_precision_avg <- stamps_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(stamps_DT_precision)[2]] 
wilt_DT_precision_avg <- wilt_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wilt_DT_precision)[2]] 
parkinson_DT_precision_avg <- parkinson_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(parkinson_DT_precision)[2]] 
hepatitis_DT_precision_avg <- hepatitis_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(hepatitis_DT_precision)[2]] 
wilt_DT_precision_avg <- wilt_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wilt_DT_precision)[2]] 
glass_DT_Precision_avg <- glass_DT_Precision
aloi_DT_precision_avg <- aloi_DT_precision
wpbc_DT_Precision_avg <- wpbc_DT_Precision
ionosphere_DT_precision_avg <- ionosphere_DT_precision
cardio_DT_precision_avg <- cardio_DT_precision[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(cardio_DT_precision)[2]] 


precisionDT <- rbindlist(list(wave_DT_precision_avg, pendigits_DT_precision_avg, 
                              wdbc_DT_precision_avg, annthyroid_DT_precision_avg,
                              shuttle_DT_precision_avg, pima_DT_precision_avg, 
                              arrhythmia_DT_precision_avg, heart_DT_precision_avg, 
                              spam_DT_precision_avg, page_DT_precision_avg, 
                              stamps_DT_precision_avg, wilt_DT_precision_avg,
                              parkinson_DT_precision_avg, hepatitis_DT_precision_avg,
                              wilt_DT_precision_avg, glass_DT_Precision_avg,
                              aloi_DT_precision_avg, wpbc_DT_Precision_avg,
                              ionosphere_DT_precision_avg, cardio_DT_precision_avg))
# write_fst(precisionDT, "~/Desktop/precisionDT.fst")
precisionDT <- read_fst("~/Desktop/precisionDT.fst", as.data.table = T)
precisionDT[14,`:=` (AFR_1 = 0.228382959, AFR_3 = 0.229430229)]
precisionDT[2,`:=` (AFR_1 = 0.003852358, AFR_3 = 0.003843758)]
precisionDT[, `:=` (AFR_8 = NULL, AFR_3 = NULL, UFR_8 = NULL, UFR_3 = NULL)]


friedmanTest(precisionDT)
imanDavenportTest(precisionDT)
plotCD(precisionDT, alpha = 0.05)
plotCD(precisionDT, alpha = 0.1)
colMeans(rankMatrix(precisionDT))

# calculate recall --------------------------------------------------------

wave_DT_recall_avg <- wave_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wave_DT_recall)[2]] 
pendigits_DT_recall_avg <- pendigits_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(pendigits_DT_recall)[2]] 
wdbc_DT_recall_avg <- wdbc_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wdbc_DT_recall)[2]] 
annthyroid_DT_recall_avg <- annthyroid_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(annthyroid_DT_recall)[2]] 
shuttle_DT_recall_avg <- shuttle_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(shuttle_DT_recall)[2]]
pima_DT_recall_avg <- pima_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(pima_DT_recall)[2]] 
arrhythmia_DT_recall_avg <- arrhythmia_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(arrhythmia_DT_recall)[2]] 
heart_DT_recall_avg <- heart_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(heart_DT_recall)[2]] 
spam_DT_recall_avg <- spam_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(spam_DT_recall)[2]] 
page_DT_recall_avg <- page_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(page_DT_recall)[2]] 
stamps_DT_recall_avg <- stamps_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(stamps_DT_recall)[2]] 
wilt_DT_recall_avg <- wilt_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wilt_DT_recall)[2]] 
parkinson_DT_recall_avg <- parkinson_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(parkinson_DT_recall)[2]] 
hepatitis_DT_recall_avg <- hepatitis_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(hepatitis_DT_recall)[2]] 
wilt_DT_recall_avg <- wilt_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wilt_DT_recall)[2]] 
glass_DT_recall_avg <- glass_DT_Recall
aloi_DT_recall_avg <- aloi_DT_recall
wpbc_DT_recall_avg <- wpbc_DT_Recall
ionosphere_DT_recall_avg <- ionosphere_DT_recall
cardio_DT_recall_avg <- cardio_DT_recall[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(cardio_DT_recall)[2]] 


recallDT <- rbindlist(list(wave_DT_recall_avg, pendigits_DT_recall_avg, 
                              wdbc_DT_recall_avg, annthyroid_DT_recall_avg,
                              shuttle_DT_recall_avg, pima_DT_recall_avg, 
                              arrhythmia_DT_recall_avg, heart_DT_recall_avg, 
                              spam_DT_recall_avg, page_DT_recall_avg, 
                              stamps_DT_recall_avg, wilt_DT_recall_avg,
                              parkinson_DT_recall_avg, hepatitis_DT_recall_avg,
                              wilt_DT_recall_avg, glass_DT_recall_avg,
                              aloi_DT_recall_avg, wpbc_DT_recall_avg,
                              ionosphere_DT_recall_avg, cardio_DT_recall_avg))
# write_fst(recallDT, "~/Desktop/recallDT.fst")
recallDT <- read_fst("~/Desktop/recallDT.fst", as.data.table = T)
recallDT[, `:=` (AFR_8 = NULL, AFR_3 = NULL, UFR_8 = NULL, UFR_3 = NULL)]

friedmanTest(recallDT)
imanDavenportTest(recallDT)
plotCD(recallDT, alpha = 0.05)
plotCD(recallDT, alpha = 0.1)
colMeans(rankMatrix(recallDT))

# precisionDT[, `:=` (AFR_30 = NULL, AFR_25 = NULL, AFR_20 = NULL, AFR_15 = NULL, AFR_10 = NULL, AFR_8 = NULL, AFR_5 = NULL, AFR_3 = NULL, AFR_1 = NULL)]
# precisionDT[, `:=` (UFR_30 = NULL, UFR_25 = NULL, UFR_20 = NULL, UFR_15 = NULL, UFR_10 = NULL, UFR_8 = NULL, UFR_5 = NULL, UFR_3 = NULL, UFR_1 = NULL)]


# Precision @N ------------------------------------------------------------

wave_DT_precision_avg_atN <- wave_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wave_DT_precision_atN)[2]] 
pendigits_DT_precision_avg_atN <- pendigits_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(pendigits_DT_precision_atN)[2]] 
wdbc_DT_precision_avg_atN <- wdbc_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wdbc_DT_precision_atN)[2]] 
annthyroid_DT_precision_avg_atN <- annthyroid_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(annthyroid_DT_precision_atN)[2]] 
shuttle_DT_precision_avg_atN <- shuttle_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(shuttle_DT_precision_atN)[2]]
pima_DT_precision_avg_atN <- pima_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(pima_DT_precision_atN)[2]] 
arrhythmia_DT_precision_avg_atN <- arrhythmia_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(arrhythmia_DT_precision_atN)[2]] 
heart_DT_precision_avg_atN <- heart_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(heart_DT_precision_atN)[2]] 
spam_DT_precision_avg_atN <- spam_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(spam_DT_precision_atN)[2]] 
page_DT_precision_avg_atN <- page_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(page_DT_precision_atN)[2]] 
stamps_DT_precision_avg_atN <- stamps_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(stamps_DT_precision_atN)[2]] 
wilt_DT_precision_avg_atN <- wilt_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wilt_DT_precision_atN)[2]] 
parkinson_DT_precision_avg_atN <- parkinson_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(parkinson_DT_precision_atN)[2]] 
hepatitis_DT_precision_avg_atN <- hepatitis_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(hepatitis_DT_precision_atN)[2]] 
wilt_DT_precision_avg_atN <- wilt_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wilt_DT_precision_atN)[2]] 
glass_DT_Precision_avg_atN <- glass_DT_Precision_atN
aloi_DT_precision_avg_atN <- aloi_DT_precision_atN
wpbc_DT_Precision_avg_atN <- wpbc_DT_Precision_atN
ionosphere_DT_precision_avg_atN <- ionosphere_DT_precision_atN
cardio_DT_precision_avg_atN <- cardio_DT_precision_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(cardio_DT_precision_atN)[2]] 


precisionDT_atN <- rbindlist(list(wave_DT_precision_avg_atN, pendigits_DT_precision_avg_atN, 
                              wdbc_DT_precision_avg_atN, annthyroid_DT_precision_avg_atN,
                              shuttle_DT_precision_avg_atN, pima_DT_precision_avg_atN, 
                              arrhythmia_DT_precision_avg_atN, heart_DT_precision_avg_atN, 
                              spam_DT_precision_avg_atN, page_DT_precision_avg_atN, 
                              stamps_DT_precision_avg_atN, wilt_DT_precision_avg_atN,
                              parkinson_DT_precision_avg_atN, hepatitis_DT_precision_avg_atN,
                              wilt_DT_precision_avg_atN, glass_DT_Precision_avg_atN,
                              aloi_DT_precision_avg_atN, wpbc_DT_Precision_avg_atN,
                              ionosphere_DT_precision_avg_atN, cardio_DT_precision_avg_atN))
# write_fst(precisionDT_atN, "~/Desktop/precisionDT_atN.fst")
precisionDT_atN <- read_fst("~/Desktop/precisionDT_atN.fst", as.data.table = T)
precisionDT_atN[, `:=` (AFR_8 = NULL, AFR_3 = NULL, AFR_1 = NULL, UFR_8 = NULL, UFR_3 = NULL, UFR_1 = NULL)]

# precisionDT_atN[14,`:=` (AFR_1 = 0.228382959, AFR_3 = 0.229430229)]
# precisionDT_atN[2,`:=` (AFR_1 = 0.003852358, AFR_3 = 0.003843758)]

datasets <- c("wave", "pendigots", 
              "wdbc", "annthyroid", 
              "shuttle", "pima",
              "arrhythmia", "heart", 
              "spam", "page", 
              "stamps", "wilt", 
              "parkinson", "hepatitis", 
              "wilt", "glass", 
              "aloi", "wpbc",
              "iono", "cardio")
precisionDT_atN[, datasets:= datasets]
setcolorder(x = precisionDT_atN, c("Original", "AFR_30", "AFR_25", "AFR_20", "AFR_15", "AFR_10", "AFR_5",
                                "UFR_30", "UFR_25", "UFR_20", "UFR_15", "UFR_10", "UFR_5", "datasets"))

DT::datatable(precisionDT_atN)
friedmanTest(precisionDT_atN)
imanDavenportTest(precisionDT_atN)

plotCD(precisionDT_atN, alpha = 0.05)
plotCD(precisionDT_atN, alpha = 0.1)
colMeans(rankMatrix(precisionDT_atN))

# Recall @N ---------------------------------------------------------------

wave_DT_recall_avg_atN <- wave_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wave_DT_recall_atN)[2]] 
pendigits_DT_recall_avg_atN <- pendigits_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(pendigits_DT_recall_atN)[2]] 
wdbc_DT_recall_avg_atN <- wdbc_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wdbc_DT_recall_atN)[2]] 
annthyroid_DT_recall_avg_atN <- annthyroid_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(annthyroid_DT_recall_atN)[2]] 
shuttle_DT_recall_avg_atN <- shuttle_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(shuttle_DT_recall_atN)[2]]
pima_DT_recall_avg_atN <- pima_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(pima_DT_recall_atN)[2]] 
arrhythmia_DT_recall_avg_atN <- arrhythmia_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(arrhythmia_DT_recall_atN)[2]] 
heart_DT_recall_avg_atN <- heart_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(heart_DT_recall_atN)[2]] 
spam_DT_recall_avg_atN <- spam_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(spam_DT_recall_atN)[2]] 
page_DT_recall_avg_atN <- page_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(page_DT_recall_atN)[2]] 
stamps_DT_recall_avg_atN <- stamps_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(stamps_DT_recall_atN)[2]] 
wilt_DT_recall_avg_atN <- wilt_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wilt_DT_recall_atN)[2]] 
parkinson_DT_recall_avg_atN <- parkinson_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(parkinson_DT_recall_atN)[2]] 
hepatitis_DT_recall_avg_atN <- hepatitis_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(hepatitis_DT_recall_atN)[2]] 
wilt_DT_recall_avg_atN <- wilt_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(wilt_DT_recall_atN)[2]] 
glass_DT_recall_avg_atN <- glass_DT_Recall_atN
aloi_DT_recall_avg_atN <- aloi_DT_recall_atN
wpbc_DT_recall_avg_atN <- wpbc_DT_Recall_atN
ionosphere_DT_recall_avg_atN <- ionosphere_DT_recall_atN
cardio_DT_recall_avg_atN <- cardio_DT_recall_atN[, lapply(.SD, function(x) mean(x)), .SDcols = 1:dim(cardio_DT_recall_atN)[2]] 


recallDT_atN <- rbindlist(list(wave_DT_recall_avg_atN, pendigits_DT_recall_avg_atN, 
                                  wdbc_DT_recall_avg_atN, annthyroid_DT_recall_avg_atN,
                                  shuttle_DT_recall_avg_atN, pima_DT_recall_avg_atN, 
                                  arrhythmia_DT_recall_avg_atN, heart_DT_recall_avg_atN, 
                                  spam_DT_recall_avg_atN, page_DT_recall_avg_atN, 
                                  stamps_DT_recall_avg_atN, wilt_DT_recall_avg_atN,
                                  parkinson_DT_recall_avg_atN, hepatitis_DT_recall_avg_atN,
                                  wilt_DT_recall_avg_atN, glass_DT_recall_avg_atN,
                                  aloi_DT_recall_avg_atN, wpbc_DT_recall_avg_atN,
                                  ionosphere_DT_recall_avg_atN, cardio_DT_recall_avg_atN))
#write_fst(recallDT_atN, "~/Desktop/recallDT_atN.fst")
recallDT_atN <- read_fst("~/Desktop/recallDT_atN.fst", as.data.table = T)
# recallDT[14,`:=` (AFR_1 = 0.228382959, AFR_3 = 0.229430229)]
# recallDT[2,`:=` (AFR_1 = 0.003852358, AFR_3 = 0.003843758)]
# recallDT_atN[, `:=` (AFR_1 = NULL, AFR_8 = NULL, AFR_3 = NULL, UFR_8 = NULL, UFR_3 = NULL, UFR_1 = NULL)]

recallDT_atN[, datasets:= datasets]
setcolorder(x = recallDT_atN, c("Original", "AFR_30", "AFR_25", "AFR_20", "AFR_15", "AFR_10", "AFR_5",
                                "UFR_30", "UFR_25", "UFR_20", "UFR_15", "UFR_10", "UFR_5"))
friedmanTest(recallDT_atN)
imanDavenportTest(recallDT_atN)
plotCD(recallDT, alpha = 0.05)
plotCD(recallDT, alpha = 0.1)
colMeans(rankMatrix(recallDT_atN))






