get_CV_experiments_paper <- function(datasetname, subfolder_name, experiments = "OC_combined_CV", CViterations) {
  
  
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

# 1. Start - Combined ------------------------------------------------
  res_final <- list()
  start_combined <- Sys.time()
  for( ii in 1:length(list_combined_1)){
    gc()
    
    res_combined <- list()
    for(i in 1:CViterations){
      
      j <- list_combined_1[[ii]]
      print("==========")
      print(i)
      
      
      #1.1 Combined - Chunks ------------------------------------------------------------------
      
      chunk1 <- sample(list_train_id[[i]], length(list_train_id[[i]])/10, replace = F)
      available_id1 <- setdiff(list_train_id[[i]], chunk1)
      length(available_id1)
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 1)
      
      chunk2 <- sample(available_id1, length(list_train_id[[i]])/10, replace = F)
      base::intersect(chunk1, chunk2)
      available_id2 <- setdiff(available_id1, chunk2)
      print(length(available_id2))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 2)
      
      chunk3 <- sample(available_id2, length(list_train_id[[i]])/10, replace = F)
      base::intersect(chunk3, chunk2)
      available_id3 <- setdiff(available_id2, chunk3)
      print(length(available_id3))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 3)
      
      chunk4 <- sample(available_id3, length(list_train_id[[i]])/10, replace = F)
      available_id4 <- setdiff(available_id3, chunk4)
      print(length(available_id4))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 4)
      
      chunk5 <- sample(available_id4, length(list_train_id[[i]])/10, replace = F)
      available_id5  <- setdiff(available_id4, chunk5)
      print(length(available_id5))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 5)
      
      chunk6 <- sample(available_id5, length(list_train_id[[i]])/10, replace = F)
      available_id6 <- setdiff(available_id5, chunk6)
      print(length(available_id6))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 6)
      
      chunk7 <- sample(available_id6, length(list_train_id[[i]])/10, replace = F)
      available_id7 <- setdiff(available_id6, chunk7)
      print(length(available_id7))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 7)
      
      chunk8 <- sample(available_id7, length(list_train_id[[i]])/10, replace = F)
      available_id8 <- setdiff(available_id7, chunk8)
      print(length(available_id8))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 8)
      
      chunk9 <- sample(available_id8, length(list_train_id[[i]])/10, replace = F)
      available_id9 <- setdiff(available_id8, chunk9)
      print(length(available_id9))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 9)
      
      chunk10 <- available_id9
      
      
      
      list_chunks <- list(chunk1, chunk2, chunk3, chunk4, chunk5, chunk6,
                          chunk7, chunk8, chunk9, chunk10)
      
      train_chunk1 <- unlist(list_chunks[-1])
      test_chunk1 <- list_chunks[[1]]
      
      train_chunk2 <- unlist(list_chunks[-2])
      test_chunk2 <- list_chunks[[2]]
      
      train_chunk3 <- unlist(list_chunks[-3])
      test_chunk3 <- list_chunks[[3]]
      
      train_chunk4 <- unlist(list_chunks[-4])
      test_chunk4 <- list_chunks[[4]]
      
      train_chunk5 <- unlist(list_chunks[-5])
      test_chunk5 <- list_chunks[[5]]
      
      train_chunk6 <- unlist(list_chunks[-6])
      test_chunk6 <- list_chunks[[6]]
      
      train_chunk7 <- unlist(list_chunks[-7])
      test_chunk7 <- list_chunks[[7]]
      
      train_chunk8 <- unlist(list_chunks[-8])
      test_chunk8 <- list_chunks[[8]]
      
      train_chunk9 <- unlist(list_chunks[-9])
      test_chunk9 <- list_chunks[[9]]
      
      train_chunk10 <- unlist(list_chunks[-10])
      test_chunk10 <- list_chunks[[10]]
      
      
      list_train_chunks <- list(train_chunk1, train_chunk2,train_chunk3,train_chunk4,
                                train_chunk5,train_chunk6,train_chunk7,train_chunk8,
                                train_chunk9,train_chunk10)
      
      list_test_chunks <- list(test_chunk1, test_chunk2,test_chunk3,test_chunk4,
                               test_chunk5,test_chunk6,test_chunk7,test_chunk8,
                               test_chunk9,test_chunk10)
      
      iters <- 0
      res <- list()
      for(ij in 1:10){
        
        trainDT <- j[id %in% list_train_chunks[[ij]]]
        trainDT_pure <- copy(j[id %in% list_train_id[[i]]][Label=="no"])
        trainDT_pure[, `:=` (Label = NULL, id = NULL) ]
        print("Train")
        print(trainDT[, .N, by = Label])
        # We sample 90% of the train data to create the CVtrain dataset
        
        CVtrain_DT <- copy(trainDT[id %in% list_train_chunks[[ij]] & Label == "no"])
        print(CVtrain_DT[, .N, by = Label])
        outliers_train_DT <- copy(trainDT[ id %in% list_train_chunks[[ij]] & Label == "yes"])
        
        CVtest_DT1 <- j[id %in% list_test_chunks[[ij]]]
        print(CVtest_DT1[, .N, by = Label])
        
        
        if(CVtest_DT1[Label=="yes", length(id)] < 2){
          CVtest_DT <- rbindlist(list(j[Label=="yes"][sample(nrow(j[Label=="yes"]), 2)], CVtest_DT1))
          CVtest_DT <- na.omit(CVtest_DT)
          CVtest_DT <- unique(CVtest_DT)
        }else{CVtest_DT <- CVtest_DT1}
        print(CVtest_DT[, .N, by = Label])
        
        CVtest_id_final <- CVtest_DT$id
        CVtest_Label_final <- CVtest_DT$Label
        
        
        testDT1 <- j[id %in% list_test_id[[i]]]
        if(testDT1[Label=="yes", length(id)] < 2){
          testDT <- rbindlist(list(j[Label=="yes"][sample(nrow(j[Label=="yes"]), 2)], testDT1))
          testDT <- na.omit(testDT)
          testDT <- unique(testDT)
        }else{testDT <- testDT1}
        
        print("Test")
        print(testDT[, .N, by = Label])
        
        testDT_id_final <- testDT$id
        testDT_Label_final <- testDT$Label
        
        CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
        CVtest_DT[, `:=` (id = NULL, Label = NULL)]
        testDT[, `:=` (id = NULL, Label = NULL)]
        
        print("Combined section")
        print(paste0("CViterations", i))
        print(paste0("Kfold: ", ij))
        print(ii)
        for(kernels in c("linear", "rbf", "sigmoid")){
          for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
            for(gammas in c(1/dim(outliers_train_DT)[2], 0.01, 0.05, 0.1, 0.2)){
              iters <- iters+1
              
              scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                  given_nu = nus, given_kernel = kernels, given_gamma = gammas)
              
              scores_test <- calculate_OCSVM_params(DTtrain = trainDT_pure, DTtest = testDT,
                                                    given_nu = nus, given_kernel = kernels, given_gamma = gammas)
              
              CVtest_DT[, `:=` (id = CVtest_id_final, Label = CVtest_Label_final)]
              testDT[, `:=` (id = testDT_id_final, Label = testDT_Label_final)]
              
              print("CV-test")
              print(CVtest_DT[, .N, by = Label])
              
              print("Test")
              print(testDT[, .N, by = Label])
              
              res[[iters]] <- data.table(auc(CVtest_DT$Label, scores_CV)[[1]],
                                         auc(testDT$Label, scores_test)[[1]],
                                         gamma = gammas,
                                         nu = nus,
                                         kernel = kernels,
                                         Kfold = ij)
              CVtest_DT[, `:=` (id = NULL, Label = NULL)]
              testDT[, `:=` (id = NULL, Label = NULL)]
            }
          }
        }
      }
      temp1 <- rbindlist(res)
      temp1[, CViteration:=i]
      res_combined[[i]] <- temp1
      
    }
    
    res_final1 <- rbindlist(res_combined)
    res_final1[, features_Iteration:=ii]
    # print(res_final1[, .SD[which.max(V1)], by = `Cross-Validation`])
    res_final[[ii]] <- res_final1
    
  }
  
  stop_combined <- Sys.time()
  combined_DT1 <- rbindlist(res_final)
  fwrite(combined_DT1, paste0(final_path_to_save, "figures/",  
                             subfolder_name, "/", datasetname, "_Combined_", CViterations,"_paper_CV.csv"))
  
  
  ddd <- combined_DT1
  ddd[, Model:= paste0(gamma, "-", nu, "-", kernel)]
  ddd[, `:=` (Model = as.factor(Model))]
  setnames(ddd, c("V1", "V2"), c("Training Error", "Test Error"))
  
  test_error <- ddd[, .(`Test Error`, Kfold, CViteration, features_Iteration, Model)]
  test_error[, Prediction:= "Test"]
  train_error <- ddd[, .(`Training Error`, Kfold, CViteration, features_Iteration, Model)]
  train_error[, Prediction:= "Training"]
  
  error <- rbindlist(list(test_error, train_error), use.names = F)
  setnames(error, "Test Error", "AUC")
  
  test1 <- error[Prediction == "Test", .SD[1], by= .(Model, CViteration, features_Iteration)]
  setcolorder(test1, names(error))
  error1 <- rbindlist(list(error[Prediction == "Training"], test1))
  
  error1[, sd:= sd(AUC), by = .(Model, Prediction, CViteration,features_Iteration)]
  error1[, mean:= mean(AUC), by = .(Model, Prediction, CViteration,features_Iteration)]
  
  final_error <- error1[, .SD[1], by= .(Model, Prediction, CViteration,features_Iteration)]
  final_error[is.na(sd), sd:=0]
  
  pd <- position_dodge(0.1)
  final_error[, CViteration:=as.factor(CViteration)]
  final_error[, features_Iteration:=as.factor(features_Iteration)]
  
  p <- ggplot(final_error, aes(x=Model, y=mean)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_line(position = pd) +
    geom_point(size=2, aes(color=Prediction, shape = features_Iteration))+
    coord_flip()+
    facet_wrap(vars(CViteration))+
    ggtitle(label = "Combined")
  
  # combined_DT <- combined_DT1[, mean(V1), by = .(gamma, nu, kernel, features_Iteration, CViteration)]
  # setkey(combined_DT, V1)
  # quantiles <- combined_DT[, .SD[1:.N, quantile(V1, probs = 0.90)], by = .(gamma, nu, kernel, features_Iteration, CViteration)]
  # merged <- combined_DT[quantiles, on = c("gamma", "nu", "kernel", "CViteration","features_Iteration")]
  # 
  # DT_hyper <- merged[V1 >= i.V1, .SD[sample(1:.N, 5, replace = T), mean(V1)], by = .(CViteration, features_Iteration)]
  
  
  # # 1st strategy to find the best performing hyperparametrs
  # comnined1_max_hyper <- combined_DT[, .SD[which.max(V1)], by = c("Cross-Validation", "features_Iteration")]
  # comnined1_max_hyper[, features_Iteration:=as.factor(features_Iteration)]
  # comnined1_max_hyper[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  # comnined1_max_hyper[, Representation:="Combined-1"]
  # 
  # 
  # # 2nd strategy to find the best performing hyperparameters
  # quantile075DT <- combined_DT[, lapply(.SD, function(x) quantile(x, probs = 0.7)), .SDcols = "V1", by = c("Cross-Validation", "features_Iteration")]
  # quantile095DT <- combined_DT[, lapply(.SD, function(x) quantile(x, probs = 0.9)), .SDcols = "V1", by = c("Cross-Validation", "features_Iteration")]
  # quantile075DT[, quantile095:= quantile095DT$V1]
  # setnames(quantile075DT, "V1", "quantile075")
  # 
  # merged_combinedDT <- combined_DT[quantile075DT, on = c("Cross-Validation", "features_Iteration")]
  # combinedDT_hyper <- merged_combinedDT[V1 %between% list(quantile075, quantile095)]
  # 
  # iter <- 0
  # list_feature_CV_best <- list()
  # for(i in 1:21){
  #   for(j in 1:CViterations){
  #     iter <- iter + 1
  #     print(iter)
  #     combinedDT_hyper_meanDT <- combinedDT_hyper[features_Iteration==i & `Cross-Validation` == j][order(V1, decreasing = T)][, median(V1)]
  #     best_hyper_value_combined <- combinedDT_hyper[features_Iteration==i & `Cross-Validation`==j & V1 == combinedDT_hyper_meanDT]
  #     if(dim(best_hyper_value_combined)[1]==0){
  #       best_hyper_value_combined <- combinedDT_hyper[features_Iteration==i & `Cross-Validation`==j, .SD[which.max(V1)]]
  #       list_feature_CV_best[[iter]] <- best_hyper_value_combined[sample(1:dim(best_hyper_value_combined)[1], 1)]
  #     }
  #     list_feature_CV_best[[iter]] <- best_hyper_value_combined[sample(1:dim(best_hyper_value_combined)[1], 1)]
  #   }
  # }
  # combined_feature_CV_best <- rbindlist(list_feature_CV_best)
  # combined_feature_CV_best[, features_Iteration:=as.factor(features_Iteration)]
  # combined_feature_CV_best[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  # combined_feature_CV_best[, Representation:="Combined-1"]
  
  #2.  Start - one-random -------------------------------------------------------------
  rm(j)
  res_final11 <- list()
  start_one_random <- Sys.time()
  for( ii in 1:length(list_one_randomOD)){
    gc()
    
    res_1random <- list()
    for(i in 1:CViterations){
      
      j <- list_one_randomOD[[ii]]
      print("==========")
      print(i)
      
      
      #2.1 one random - Chunks ------------------------------------------------------------------
      
      chunk1 <- sample(list_train_id[[i]], length(list_train_id[[i]])/10, replace = F)
      available_id1 <- setdiff(list_train_id[[i]], chunk1)
      length(available_id1)
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 1)
      
      chunk2 <- sample(available_id1, length(list_train_id[[i]])/10, replace = F)
      base::intersect(chunk1, chunk2)
      available_id2 <- setdiff(available_id1, chunk2)
      print(length(available_id2))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 2)
      
      chunk3 <- sample(available_id2, length(list_train_id[[i]])/10, replace = F)
      base::intersect(chunk3, chunk2)
      available_id3 <- setdiff(available_id2, chunk3)
      print(length(available_id3))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 3)
      
      chunk4 <- sample(available_id3, length(list_train_id[[i]])/10, replace = F)
      available_id4 <- setdiff(available_id3, chunk4)
      print(length(available_id4))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 4)
      
      chunk5 <- sample(available_id4, length(list_train_id[[i]])/10, replace = F)
      available_id5  <- setdiff(available_id4, chunk5)
      print(length(available_id5))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 5)
      
      chunk6 <- sample(available_id5, length(list_train_id[[i]])/10, replace = F)
      available_id6 <- setdiff(available_id5, chunk6)
      print(length(available_id6))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 6)
      
      chunk7 <- sample(available_id6, length(list_train_id[[i]])/10, replace = F)
      available_id7 <- setdiff(available_id6, chunk7)
      print(length(available_id7))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 7)
      
      chunk8 <- sample(available_id7, length(list_train_id[[i]])/10, replace = F)
      available_id8 <- setdiff(available_id7, chunk8)
      print(length(available_id8))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 8)
      
      chunk9 <- sample(available_id8, length(list_train_id[[i]])/10, replace = F)
      available_id9 <- setdiff(available_id8, chunk9)
      print(length(available_id9))
      print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 9)
      
      chunk10 <- available_id9
      
      
      
      list_chunks <- list(chunk1, chunk2, chunk3, chunk4, chunk5, chunk6,
                          chunk7, chunk8, chunk9, chunk10)
      
      train_chunk1 <- unlist(list_chunks[-1])
      test_chunk1 <- list_chunks[[1]]
      
      train_chunk2 <- unlist(list_chunks[-2])
      test_chunk2 <- list_chunks[[2]]
      
      train_chunk3 <- unlist(list_chunks[-3])
      test_chunk3 <- list_chunks[[3]]
      
      train_chunk4 <- unlist(list_chunks[-4])
      test_chunk4 <- list_chunks[[4]]
      
      train_chunk5 <- unlist(list_chunks[-5])
      test_chunk5 <- list_chunks[[5]]
      
      train_chunk6 <- unlist(list_chunks[-6])
      test_chunk6 <- list_chunks[[6]]
      
      train_chunk7 <- unlist(list_chunks[-7])
      test_chunk7 <- list_chunks[[7]]
      
      train_chunk8 <- unlist(list_chunks[-8])
      test_chunk8 <- list_chunks[[8]]
      
      train_chunk9 <- unlist(list_chunks[-9])
      test_chunk9 <- list_chunks[[9]]
      
      train_chunk10 <- unlist(list_chunks[-10])
      test_chunk10 <- list_chunks[[10]]
      
      
      list_train_chunks <- list(train_chunk1, train_chunk2,train_chunk3,train_chunk4,
                                train_chunk5,train_chunk6,train_chunk7,train_chunk8,
                                train_chunk9,train_chunk10)
      
      list_test_chunks <- list(test_chunk1, test_chunk2,test_chunk3,test_chunk4,
                               test_chunk5,test_chunk6,test_chunk7,test_chunk8,
                               test_chunk9,test_chunk10)
      
      iters <- 0
      res <- list()
      for(ij in 1:10){
        
        trainDT <- j[id %in% list_train_chunks[[ij]]]
        trainDT_pure <- copy(j[id %in% list_train_id[[i]]][Label=="no"])
        trainDT_pure[, `:=` (Label = NULL, id = NULL) ]
        print("Train")
        print(trainDT[, .N, by = Label])
        # We sample 90% of the train data to create the CVtrain dataset
        
        CVtrain_DT <- copy(trainDT[id %in% list_train_chunks[[ij]] & Label == "no"])
        print(CVtrain_DT[, .N, by = Label])
        outliers_train_DT <- copy(trainDT[ id %in% list_train_chunks[[ij]] & Label == "yes"])
        
        CVtest_DT1 <- j[id %in% list_test_chunks[[ij]]]
        print(CVtest_DT1[, .N, by = Label])
        
        
        if(CVtest_DT1[Label=="yes", length(id)] < 2){
          CVtest_DT <- rbindlist(list(j[Label=="yes"][sample(nrow(j[Label=="yes"]), 2)], CVtest_DT1))
          CVtest_DT <- na.omit(CVtest_DT)
          CVtest_DT <- unique(CVtest_DT)
        }else{CVtest_DT <- CVtest_DT1}
        print(CVtest_DT[, .N, by = Label])
        
        CVtest_id_final <- CVtest_DT$id
        CVtest_Label_final <- CVtest_DT$Label
        
        
        testDT1 <- j[id %in% list_test_id[[i]]]
        if(testDT1[Label=="yes", length(id)] < 2){
          testDT <- rbindlist(list(j[Label=="yes"][sample(nrow(j[Label=="yes"]), 2)], testDT1))
          testDT <- na.omit(testDT)
          testDT <- unique(testDT)
        }else{testDT <- testDT1}
        
        print("Test")
        print(testDT[, .N, by = Label])
        
        testDT_id_final <- testDT$id
        testDT_Label_final <- testDT$Label
        
        CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
        CVtest_DT[, `:=` (id = NULL, Label = NULL)]
        testDT[, `:=` (id = NULL, Label = NULL)]
        
        print("One-random section")
        print(paste0("CViterations", i))
        print(paste0("Kfold: ", ij))
        print(ii)
        for(kernels in c("linear", "rbf", "sigmoid")){
          for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
            for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
              iters <- iters+1
              
              scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                  given_nu = nus, given_kernel = kernels, given_gamma = gammas)
              
              scores_test <- calculate_OCSVM_params(DTtrain = trainDT_pure, DTtest = testDT,
                                                    given_nu = nus, given_kernel = kernels, given_gamma = gammas)
              
              CVtest_DT[, `:=` (id = CVtest_id_final, Label = CVtest_Label_final)]
              testDT[, `:=` (id = testDT_id_final, Label = testDT_Label_final)]
              
              res[[iters]] <- data.table(auc(CVtest_DT$Label, scores_CV)[[1]],
                                         auc(testDT$Label, scores_test)[[1]],
                                         gamma = gammas,
                                         nu = nus,
                                         kernel = kernels,
                                         Kfold = ij)
              CVtest_DT[, `:=` (id = NULL, Label = NULL)]
              testDT[, `:=` (id = NULL, Label = NULL)]
            }
          }
        }
      }
      temp1 <- rbindlist(res)
      temp1[, CViteration:=i]
      res_1random[[i]] <- temp1
      
    }
    
    res_final1 <- rbindlist(res_1random)
    res_final1[, features_Iteration:=ii]
    # print(res_final1[, .SD[which.max(V1)], by = `Cross-Validation`])
    res_final11[[ii]] <- res_final1
    
  }
  
  stop_one_random <- Sys.time()
  one_random_DT1 <- rbindlist(res_final11)
  fwrite(one_random_DT1, paste0(final_path_to_save, "figures/",  
                                subfolder_name, "/", datasetname, "_1random_", CViterations,"_paper_CV.csv"))
  
  
  ddd <- combined_DT1
  ddd[, Model:= paste0(gamma, "-", nu, "-", kernel)]
  ddd[, `:=` (Model = as.factor(Model))]
  setnames(ddd, c("V1", "V2"), c("Training Error", "Test Error"), skip_absent = T)
  
  test_error <- ddd[, .(`Test Error`, Kfold, CViteration, features_Iteration, Model)]
  test_error[, Prediction:= "Test"]
  train_error <- ddd[, .(`Training Error`, Kfold, CViteration, features_Iteration, Model)]
  train_error[, Prediction:= "Training"]
  
  error <- rbindlist(list(test_error, train_error), use.names = F)
  setnames(error, "Test Error", "AUC")
  
  test1 <- error[Prediction == "Test", .SD[1], by= .(Model, CViteration, features_Iteration)]
  setcolorder(test1, names(error))
  error1 <- rbindlist(list(error[Prediction == "Training"], test1))
  
  error1[, sd:= sd(AUC), by = .(Model, Prediction, CViteration,features_Iteration)]
  error1[, mean:= mean(AUC), by = .(Model, Prediction, CViteration,features_Iteration)]
  
  final_error <- error1[, .SD[1], by= .(Model, Prediction, CViteration,features_Iteration)]
  final_error[is.na(sd), sd:=0]
  
  pd <- position_dodge(0.1)
  final_error[, CViteration:=as.factor(CViteration)]
  final_error[, features_Iteration:=as.factor(features_Iteration)]
  
  p_1 <- ggplot(final_error, aes(x=Model, y=mean)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_line(position = pd) +
    geom_point(size=2, aes(color=Prediction, shape = features_Iteration))+
    coord_flip()+
    facet_wrap(vars(CViteration))+
    ggtitle(label = "1-Random")
  
  
  
  # 3. Start - Original --------------------------------------------------------------
  res_original <- list()
  start_original <- Sys.time()
  for(i in 1:CViterations){
    
    
    # 3.2 Original - Chunks ------------------------------------------------------------------
    chunk1 <- sample(list_train_id[[i]], length(list_train_id[[i]])/10, replace = F)
    available_id1 <- setdiff(list_train_id[[i]], chunk1)
    length(available_id1)
    print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 1)
    
    chunk2 <- sample(available_id1, length(list_train_id[[i]])/10, replace = F)
    base::intersect(chunk1, chunk2)
    available_id2 <- setdiff(available_id1, chunk2)
    print(length(available_id2))
    print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 2)
    
    chunk3 <- sample(available_id2, length(list_train_id[[i]])/10, replace = F)
    base::intersect(chunk3, chunk2)
    available_id3 <- setdiff(available_id2, chunk3)
    print(length(available_id3))
    print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 3)
    
    chunk4 <- sample(available_id3, length(list_train_id[[i]])/10, replace = F)
    available_id4 <- setdiff(available_id3, chunk4)
    print(length(available_id4))
    print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 4)
    
    chunk5 <- sample(available_id4, length(list_train_id[[i]])/10, replace = F)
    available_id5  <- setdiff(available_id4, chunk5)
    print(length(available_id5))
    print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 5)
    
    chunk6 <- sample(available_id5, length(list_train_id[[i]])/10, replace = F)
    available_id6 <- setdiff(available_id5, chunk6)
    print(length(available_id6))
    print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 6)
    
    chunk7 <- sample(available_id6, length(list_train_id[[i]])/10, replace = F)
    available_id7 <- setdiff(available_id6, chunk7)
    print(length(available_id7))
    print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 7)
    
    chunk8 <- sample(available_id7, length(list_train_id[[i]])/10, replace = F)
    available_id8 <- setdiff(available_id7, chunk8)
    print(length(available_id8))
    print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 8)
    
    chunk9 <- sample(available_id8, length(list_train_id[[i]])/10, replace = F)
    available_id9 <- setdiff(available_id8, chunk9)
    print(length(available_id9))
    print(length(list_train_id[[i]]) - length(list_train_id[[i]])/10 * 9)
    
    chunk10 <- available_id9
    
    
    
    list_chunks <<- list(chunk1, chunk2, chunk3, chunk4, chunk5, chunk6,
                        chunk7, chunk8, chunk9, chunk10)
    
    train_chunk1 <- unlist(list_chunks[-1])
    test_chunk1 <- list_chunks[[1]]
    
    train_chunk2 <- unlist(list_chunks[-2])
    test_chunk2 <- list_chunks[[2]]
    
    train_chunk3 <- unlist(list_chunks[-3])
    test_chunk3 <- list_chunks[[3]]
    
    train_chunk4 <- unlist(list_chunks[-4])
    test_chunk4 <- list_chunks[[4]]
    
    train_chunk5 <- unlist(list_chunks[-5])
    test_chunk5 <- list_chunks[[5]]
    
    train_chunk6 <- unlist(list_chunks[-6])
    test_chunk6 <- list_chunks[[6]]
    
    train_chunk7 <- unlist(list_chunks[-7])
    test_chunk7 <- list_chunks[[7]]
    
    train_chunk8 <- unlist(list_chunks[-8])
    test_chunk8 <- list_chunks[[8]]
    
    train_chunk9 <- unlist(list_chunks[-9])
    test_chunk9 <- list_chunks[[9]]
    
    train_chunk10 <- unlist(list_chunks[-10])
    test_chunk10 <- list_chunks[[10]]
    
    
    list_train_chunks <<- list(train_chunk1, train_chunk2,train_chunk3,train_chunk4,
                              train_chunk5,train_chunk6,train_chunk7,train_chunk8,
                              train_chunk9,train_chunk10)
    
    list_test_chunks <<- list(test_chunk1, test_chunk2,test_chunk3,test_chunk4,
                             test_chunk5,test_chunk6,test_chunk7,test_chunk8,
                             test_chunk9,test_chunk10)
    
    iters <- 0
    res <- list()
    for(ij in 1:10){
      
      trainDT <- DToriginal[id %in% list_train_chunks[[ij]]]
      trainDT_pure <- copy(DToriginal[id %in% list_train_id[[i]]][Label=="no"])
      print(trainDT_pure)
      
      trainDT_pure[, `:=` (Label = NULL, id = NULL) ]
      print(trainDT_pure)
      print("Train")
      print(trainDT[, .N, by = Label])
      # We sample 90% of the train data to create the CVtrain dataset
      CVtrain_DT <- copy(trainDT[id %in% list_train_chunks[[ij]]][Label == "no"])
      print(CVtrain_DT[, .N, by = Label])
      outliers_train_DT <- copy(trainDT[ id %in% list_train_chunks[[ij]]][Label == "yes"])
      
      CVtest_DT1 <- DToriginal[id %in% list_test_chunks[[ij]]]
      print(CVtest_DT1[, .N, by = Label])
      
      
      if(CVtest_DT1[Label=="yes", length(id)] < 2){
        CVtest_DT <- rbindlist(list(DToriginal[Label=="yes"][sample(nrow(DToriginal[Label=="yes"]), 2)], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      
      testDT1 <- DToriginal[id %in% list_test_id[[i]]]
      if(testDT1[Label=="yes", length(id)] < 2){
        testDT <- rbindlist(list(DToriginal[Label=="yes"][sample(nrow(DToriginal[Label=="yes"]), 2)], testDT1))
        testDT <- na.omit(testDT)
        testDT <- unique(testDT)
      }else{testDT <- testDT1}
      
      print("Test")
      print(testDT[, .N, by = Label])
      
      testDT_id_final <- testDT$id
      testDT_Label_final <- testDT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      testDT[, `:=` (id = NULL, Label = NULL)]
      
      print(CVtest_DT)
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters+1
            print("Original section")
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            
            scores_test <- calculate_OCSVM_params(DTtrain = trainDT_pure, DTtest = testDT,
                                                  given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            
            CVtest_DT[, `:=` (id = CVtest_id_final, Label = CVtest_Label_final)]
            testDT[, `:=` (id = testDT_id_final, Label = testDT_Label_final)]
            
            res[[iters]] <- data.table(auc(CVtest_DT$Label, scores_CV)[[1]],
                                       auc(testDT$Label, scores_test)[[1]],
                                       gamma = gammas,
                                       nu = nus,
                                       kernel = kernels,
                                       Kfold = ij)
            CVtest_DT[, `:=` (id = NULL, Label = NULL)]
            testDT[, `:=` (id = NULL, Label = NULL)]
          }
        }
      }
    }
    temp1 <- rbindlist(res)
    temp1[, CViteration:=i]
    res_original[[i]] <- temp1
    
  }
  res_final_original <- rbindlist(res_original)
  
  stop_original <- Sys.time()
  fwrite(res_final_original, paste0(final_path_to_save, "figures/",  
                                    subfolder_name, "/", datasetname, "_Original_", CViterations,"_paper_CV.csv"))
  
  
  
  
  ddd <- res_final_original
  ddd[, Model:= paste0(gamma, "-", nu, "-", kernel)]
  ddd[, `:=` (Model = as.factor(Model))]
  setnames(ddd, c("V1", "V2"), c("Training Error", "Test Error"))
  
  test_error <- ddd[, .(`Test Error`, Kfold, CViteration, Model)]
  test_error[, Prediction:= "Test"]
  train_error <- ddd[, .(`Training Error`, Kfold, CViteration, Model)]
  train_error[, Prediction:= "Training"]
  
  error <- rbindlist(list(test_error, train_error), use.names = F)
  setnames(error, "Test Error", "AUC")
  
  test1 <- error[Prediction == "Test", .SD[1], by= .(Model, CViteration)]
  setcolorder(test1, names(error))
  error1 <- rbindlist(list(error[Prediction == "Training"], test1))
  
  error1[, sd:= sd(AUC), by = .(Model, Prediction, CViteration)]
  error1[, mean:= mean(AUC), by = .(Model, Prediction, CViteration)]
  
  final_error <- error1[, .SD[1], by= .(Model, Prediction, CViteration)]
  final_error[is.na(sd), sd:=0]
  
  pd <- position_dodge(0.1)
  final_error[, CViteration:=as.factor(CViteration)]
  
  p_2 <- ggplot(final_error, aes(x=Model, y=mean)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_line(position = pd) +
    geom_point(size=2, aes(color=Prediction, shape = CViteration))+
    coord_flip()+
    ggtitle(label = "Original")
  
  
  # original_maxDT <- res_final_original[, .SD[which.max(V1)], by = `Cross-Validation`]
  # 
  # # 2nd strategy to find the best performing hyperparameters
  # original_quantile075 <- res_final_original[, lapply(.SD, function(x) quantile(x, probs = 0.75)), .SDcols = "V1", by = c("Cross-Validation")]
  # original_quantile095 <- res_final_original[, lapply(.SD, function(x) quantile(x, probs = 0.95)), .SDcols = "V1", by = c("Cross-Validation")]
  # original_quantile075[, quantile095:= original_quantile095$V1]
  # setnames(original_quantile075, "V1", "quantile075")
  # original_merged <- res_final_original[original_quantile075, on = "Cross-Validation"]
  # original_quantiles <- original_merged[V1 %between% list(quantile075, quantile095)]
  # 
  # 
  # list_feature_CV_best2 <- list()
  # for(j in 1:CViterations){
  #   number_rows <- nrow(original_quantiles[`Cross-Validation`==j][order(V1, decreasing = T)])
  #   original_hyper_meanDT <- original_quantiles[`Cross-Validation`==j][order(V1, decreasing = T)][sample(1:number_rows, 1), V1]
  #   best_hyper_value_original <- original_quantiles[`Cross-Validation`==j & V1 == original_hyper_meanDT]
  #   list_feature_CV_best2[[j]] <- best_hyper_value_original[sample(1:dim(best_hyper_value_original)[1], 1)]
  # }
  # 
  # original_feature_CV_best <- rbindlist(list_feature_CV_best2)
  # original_feature_CV_best[, `Cross-Validation`:=as.factor(`Cross-Validation`)]
  
  
  # Strategy 1 - Create data 
  
  # res_final_DT <- rbindlist(list(comnined1_max_hyper, random1_max))
  # res_final_DT[, Representation:= as.factor(Representation)]
  # res_final_DT[, V3:= (V2 - original_maxDT[, mean(V2)])/original_maxDT[, sd(V2)]]
  # 
  # winning_iterations_combined <- res_final_DT[Representation=="Combined-1", mean(V2)>original_maxDT[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  # winning_iterations_random <- res_final_DT[Representation=="Random-1", mean(V2)>original_maxDT[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  # 
  # res_final_DT[Representation=="Combined-1" & features_Iteration %in% winning_iterations_combined, Group:= "Win"]
  # res_final_DT[Representation=="Random-1" & features_Iteration %in% winning_iterations_random, Group:= "Win"]
  # res_final_DT[is.na(Group), Group:= "Defeat"]
  # 
  # 
  # winning_combined <- paste0(100 * round(length(winning_iterations_combined)/length(list_combined_1), 2), "%")
  # winning_random <- paste0(100 * round(length(winning_iterations_random)/length(list_one_randomOD), 2), "%")
  # 
  # mean_val <- res_final_DT[, mean(V2), by=Representation]
  # mean_val_sd <- res_final_DT[, mean(V3), by=Representation]
  # 
  # supp.labs <- c(glue("{winning_combined} of the random Combined datasets outperform the Original features
  #                   Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val[Representation=='Combined-1',V1], 4)}
  #                   Black line is  {round(mean_val_sd[Representation=='Combined-1',V1], 4)} Standard Deviations away from Red line"), 
  #                glue("{winning_random} of the random datasets composed only of Outlier Scores, outperform the Original features
  #                   Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val[Representation=='Random-1',V1], 4)}
  #                   Black line is {round(mean_val_sd[Representation=='Random-1',V1], 4)} Standard Deviations away from Red line"))
  # names(supp.labs) <- c("Combined-1", "Random-1")
  # 
  # 
  # p <- ggplot(data = res_final_DT) +
  #   aes(x = features_Iteration, y = V2, fill = Group) +
  #   geom_boxplot() +
  #   theme_bw()+
  #   facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.labs))+
  #   scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
  #   geom_hline(yintercept = original_maxDT[, mean(V2)], color = "red",)+
  #   geom_hline(data = mean_val, aes(yintercept=V1)) +
  #   # stat_summary(fun.y = mean, color = "darkred", 
  #   #              geom = "point", shape = 18, size = 3,
  #   #              show.legend = FALSE)+
  #   scale_fill_manual(values=c("#907509", "#0b1a8c"))+
  #   labs(x = "Random Combined Datasets", 
  #        y = "AUC",
  #        title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_maxDT[, round(mean(V2), 4)]}"))
  # 
  # p
  # 
  # ggsave(plot = p, filename = paste0(final_path_to_save, "figures/",  
  #                                    subfolder_name, "/", datasetname, "_",
  #                                    "AUCperformance_Maximum_hyper_", CViterations,"CVnew",".pdf"),
  #        width = 12, height = 6, units = "in", dpi = 300)
  # 
  # 
  # p1 <- ggplot(data = res_final_DT) +
  #   aes(x = features_Iteration, y = V3, fill = Group) +
  #   geom_boxplot() +
  #   theme_bw()+
  #   facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.labs))+
  #   scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
  #   geom_hline(yintercept = 0, color = "red",)+
  #   geom_hline(data = mean_val_sd, aes(yintercept=V1)) +
  #   # stat_summary(fun.y = mean, color = "darkred", 
  #   #              geom = "point", shape = 18, size = 3,
  #   #              show.legend = FALSE)+
  #   scale_fill_manual(values=c("#907509", "#0b1a8c"))+
  #   labs(x = "Random Combined Datasets", 
  #        y = "Standard Deviations of Mean AUC of Original-View",
  #        title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_maxDT[, round(mean(V2), 4)]}"))
  # 
  # p1
  # 
  # ggsave(plot = p1, filename = paste0(final_path_to_save, "figures/",  
  #                                     subfolder_name, "/", datasetname, "_",
  #                                     "sd_AUCperformance_Maximum_hyper_", CViterations,"CVnew",".pdf"),
  #        width = 12, height = 6, units = "in", dpi = 300)
  # 
  # # Strategy 2 - Create data
  
  # 
  # res_final_DT1 <- rbindlist(list(combined_feature_CV_best, random1_feature_CV_best))
  # res_final_DT1[, Representation:= as.factor(Representation)]
  # res_final_DT1[, V3:= (V2 - original_feature_CV_best[, mean(V2)])/original_feature_CV_best[, sd(V2)]]
  # 
  # 
  # winning_iterations_combined1 <- res_final_DT1[Representation=="Combined-1", mean(V2)>original_feature_CV_best[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  # winning_iterations_random1 <- res_final_DT1[Representation=="Random-1", mean(V2)>original_feature_CV_best[, mean(V2)], by = features_Iteration][V1==TRUE, features_Iteration]
  # 
  # res_final_DT1[Representation=="Combined-1" & features_Iteration %in% winning_iterations_combined1, Group:= "Win"]
  # res_final_DT1[Representation=="Random-1" & features_Iteration %in% winning_iterations_random1, Group:= "Win"]
  # res_final_DT1[is.na(Group), Group:= "Defeat"]
  # 
  # 
  # winning_combined1 <- paste0(100 * round(length(winning_iterations_combined1)/length(list_combined_1), 2), "%")
  # winning_random1 <- paste0(100 * round(length(winning_iterations_random1)/length(list_one_randomOD), 2), "%")
  # 
  # mean_val1 <- res_final_DT1[, mean(V2), by=Representation]
  # mean_val_sd1 <- res_final_DT1[, mean(V3), by=Representation]
  # 
  # supp.lab <- c(glue("{winning_combined1} of the random Combined datasets outperform the Original features
  #                   Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val1[Representation=='Combined-1',V1], 4)}
  #                   Black line is  {round(mean_val_sd1[Representation=='Combined-1',V1], 4)} Standard Deviations away from Red line"), 
  #               glue("{winning_random1} of the random datasets composed only of Outlier Scores, outperform the Original features
  #                   Black Line is the Mean AUC performance across all the presented Combined datasets: {round(mean_val1[Representation=='Random-1',V1], 4)}
  #                   Black line is {round(mean_val_sd1[Representation=='Random-1',V1], 4)} Standard Deviations away from Red line"))
  # names(supp.lab) <- c("Combined-1", "Random-1")
  # 
  # 
  # p2 <- ggplot(data = res_final_DT1) +
  #   aes(x = features_Iteration, y = V2, fill = Group) +
  #   geom_boxplot() +
  #   theme_bw()+
  #   facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.lab))+
  #   scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
  #   geom_hline(yintercept = original_feature_CV_best[, mean(V2)], color = "red",)+
  #   geom_hline(data = mean_val1, aes(yintercept=V1)) +
  #   # stat_summary(fun.y = mean, color = "darkred", 
  #   #              geom = "point", shape = 18, size = 3,
  #   #              show.legend = FALSE)+
  #   scale_fill_manual(values=c("#907509", "#0b1a8c"))+
  #   labs(x = "Random Combined Datasets", 
  #        y = "AUC",
  #        title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_feature_CV_best[, round(mean(V2), 4)]}"))
  # 
  # p2
  # 
  # ggsave(plot = p2, filename = paste0(final_path_to_save, "figures/",  
  #                                     subfolder_name, "/", datasetname, "_",
  #                                     "AUCperformance_Median_hyper_", CViterations, "CVnew",".pdf"),
  #        width = 12, height = 6, units = "in", dpi = 300)
  # 
  # 
  # p3 <- ggplot(data = res_final_DT1) +
  #   aes(x = features_Iteration, y = V3, fill = Group) +
  #   geom_boxplot() +
  #   theme_bw()+
  #   facet_wrap(~Representation, ncol = 1, labeller = labeller(Representation = supp.lab))+
  #   scale_color_manual(values=c("#9e1c00", "#0b1a8c"))+
  #   geom_hline(yintercept = 0, color = "red",)+
  #   geom_hline(data = mean_val_sd1, aes(yintercept=V1)) +
  #   # stat_summary(fun.y = mean, color = "darkred", 
  #   #              geom = "point", shape = 18, size = 3,
  #   #              show.legend = FALSE)+
  #   scale_fill_manual(values=c("#907509", "#0b1a8c"))+
  #   labs(x = "Random Combined Datasets", 
  #        y = "Standard Deviations of Mean AUC of Original-View",
  #        title = glue("Dataset: {datasetname}. Red Line is the Average AUC performance of the original features across all CVs: {original_feature_CV_best[, round(mean(V2), 4)]}"))
  # 
  # p3
  # 
  # ggsave(plot = p3, filename = paste0(final_path_to_save, "figures/",  
  #                                     subfolder_name, "/", datasetname, "_",
  #                                     "sd_AUCperformance_Median_hyper_", CViterations ,"CVnew",".pdf"),
  #        width = 12, height = 6, units = "in", dpi = 300)
  
  
  return(list(start_combined, stop_combined, 
              start_one_random, stop_one_random,
              start_original, stop_original))
  
}


get_CV_experiments_normalized <- function(datasetname, subfolder_name, experiments = "OC_combined_CV", CViterations){
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
        testDT <- rbindlist(list(j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)], testDT1))
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
        testDT <- rbindlist(list(j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)], testDT1))
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
      testDT <- rbindlist(list(j[Label=="yes"][sample(nrow(j[Label=="yes"]), 1)], testDT1))
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
# start_exp
# get_CV_experiments(datasetname = "Waveform_withoutdupl_norm_v01",
#                    subfolder_name = "Waveform", CViterations = 30)
# stop_exp <- Sys.time()


# 
# 

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
# get_CV_experiments(datasetname = "HeartDisease_withoutdupl_norm_02_v01",
#                     experiments = "OC_combined_CV",
#                     subfolder_name = "HeartDisease",
#                     CViterations =  10)
# print(Sys.time())
# 
# 
# get_CV_experiments(datasetname = "Stamps_withoutdupl_norm_02_v01",
#                    experiments = "OC_combined_CV",
#                    subfolder_name = "Stamps",
#                    CViterations =  10)
# print(Sys.time())
# 



# get_CV_experiments(datasetname = "Shuttle_withoutdupl_norm_v05",
#                    subfolder_name = "Shuttle")
# 
# print(Sys.time())



# get_CV_experiments_normalized(datasetname = "Ionosphere_withoutdupl_norm",
#                               experiments = "OC_combined_CV",
#                               subfolder_name = "Ionosphere",
#                               CViterations =  100)
# print(Sys.time())
