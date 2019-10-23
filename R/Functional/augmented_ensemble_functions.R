# Augmented space Ensemble ------------------------------------------------

get_OCSVM_augmented_ensemble_21MUR <- function(datasetname, experiments = "OC_combined_CV", CViterations, print_k) {
  
  
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
  
  list_combined <- list()
  list_one_randomOD <- list()
  
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
    list_combined[[i]] <- combinedDT_1
    
    one_randomOD[, `:=` (id = combinedDT_1$id, Label = combinedDT_1$Label)]
    list_one_randomOD[[i]] <- one_randomOD
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_combined[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_combined[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_combined[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_combined[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_combined[[5]])
  randomOD_6_train <- get_train_representations(representationDT = list_combined[[6]])
  randomOD_7_train <- get_train_representations(representationDT = list_combined[[7]])
  randomOD_8_train <- get_train_representations(representationDT = list_combined[[8]])
  randomOD_9_train <- get_train_representations(representationDT = list_combined[[9]])
  randomOD_10_train <- get_train_representations(representationDT = list_combined[[10]])
  randomOD_11_train <- get_train_representations(representationDT = list_combined[[11]])
  randomOD_12_train <- get_train_representations(representationDT = list_combined[[12]])
  randomOD_13_train <- get_train_representations(representationDT = list_combined[[13]])
  randomOD_14_train <- get_train_representations(representationDT = list_combined[[14]])
  randomOD_15_train <- get_train_representations(representationDT = list_combined[[15]])
  randomOD_16_train <- get_train_representations(representationDT = list_combined[[16]])
  randomOD_17_train <- get_train_representations(representationDT = list_combined[[17]])
  randomOD_18_train <- get_train_representations(representationDT = list_combined[[18]])
  randomOD_19_train <- get_train_representations(representationDT = list_combined[[19]])
  randomOD_20_train <- get_train_representations(representationDT = list_combined[[20]])
  randomOD_21_train <- get_train_representations(representationDT = list_combined[[21]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_combined[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_combined[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_combined[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_combined[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_combined[[5]])
  randomOD_6_test <- get_testCV_representations(representationDT = list_combined[[6]])
  randomOD_7_test <- get_testCV_representations(representationDT = list_combined[[7]])
  randomOD_8_test <- get_testCV_representations(representationDT = list_combined[[8]])
  randomOD_9_test <- get_testCV_representations(representationDT = list_combined[[9]])
  randomOD_10_test <- get_testCV_representations(representationDT = list_combined[[10]])
  randomOD_11_test <- get_testCV_representations(representationDT = list_combined[[11]])
  randomOD_12_test <- get_testCV_representations(representationDT = list_combined[[12]])
  randomOD_13_test <- get_testCV_representations(representationDT = list_combined[[13]])
  randomOD_14_test <- get_testCV_representations(representationDT = list_combined[[14]])
  randomOD_15_test <- get_testCV_representations(representationDT = list_combined[[15]])
  randomOD_16_test <- get_testCV_representations(representationDT = list_combined[[16]])
  randomOD_17_test <- get_testCV_representations(representationDT = list_combined[[17]])
  randomOD_18_test <- get_testCV_representations(representationDT = list_combined[[18]])
  randomOD_19_test <- get_testCV_representations(representationDT = list_combined[[19]])
  randomOD_20_test <- get_testCV_representations(representationDT = list_combined[[20]])
  randomOD_21_test <- get_testCV_representations(representationDT = list_combined[[21]])
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("One-random section")
      print("21 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      print(paste0("Iteration: ", print_k))
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters+1
            
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, gamma := gammas]
            DT_CV[, nu := nus]
            DT_CV[, kernel := kernels]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  representation6 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_6_train, randomOD_test = randomOD_6_test, feature_col = "Representation_6"))
  representation7 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_7_train, randomOD_test = randomOD_7_test, feature_col = "Representation_7"))  
  representation8 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_8_train, randomOD_test = randomOD_8_test, feature_col = "Representation_8"))  
  representation9 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_9_train, randomOD_test = randomOD_9_test, feature_col = "Representation_9"))  
  representation10 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_10_train, randomOD_test = randomOD_10_test, feature_col = "Representation_10"))
  representation11 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_11_train, randomOD_test = randomOD_11_test, feature_col = "Representation_11"))
  representation12 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_12_train, randomOD_test = randomOD_12_test, feature_col = "Representation_12"))
  representation13 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_13_train, randomOD_test = randomOD_13_test, feature_col = "Representation_13"))
  representation14 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_14_train, randomOD_test = randomOD_14_test, feature_col = "Representation_14"))
  representation15 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_15_train, randomOD_test = randomOD_15_test, feature_col = "Representation_15"))
  representation16 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_16_train, randomOD_test = randomOD_16_test, feature_col = "Representation_16"))
  representation17 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_17_train, randomOD_test = randomOD_17_test, feature_col = "Representation_17"))
  representation18 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_18_train, randomOD_test = randomOD_18_test, feature_col = "Representation_18"))
  representation19 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_19_train, randomOD_test = randomOD_19_test, feature_col = "Representation_19"))
  representation20 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_20_train, randomOD_test = randomOD_20_test, feature_col = "Representation_20"))
  representation21 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_21_train, randomOD_test = randomOD_21_test, feature_col = "Representation_21"))
  
  
  
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5, representation6,
                                      representation7, representation8,
                                      representation9, representation10,
                                      representation11, representation12,
                                      representation13, representation14,
                                      representation15, representation16,
                                      representation17, representation18,
                                      representation19, representation20,
                                      representation21
  ))
  gc()
  representationsDT[, Model:= paste0(kernel, "_", gamma, "_", nu)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_OCSVM_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                     given_kernel = best_hyper_representation[1], 
                                     given_gamma = as.numeric(best_hyper_representation[2]),
                                     given_nu = as.numeric(best_hyper_representation[3]))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  # dcasted_representations <- dcast.data.table(all_representationsDT, id+Label~representation, value.var = "scores")
  average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  
  max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  
  return(all_representationsDT)
}

get_OCSVM_augmented_ensemble_5MUR <- function(datasetname, experiments = "OC_combined_CV", CViterations, print_k) {
  
  
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
  list_one_randomOD <- list()
  for(i in 1:5){
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
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_combined_1[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_combined_1[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_combined_1[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_combined_1[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_combined_1[[5]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_combined_1[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_combined_1[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_combined_1[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_combined_1[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_combined_1[[5]])
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("5 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      print(paste0("Iteration: ", print_k))
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters+1
            
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, gamma := gammas]
            DT_CV[, nu := nus]
            DT_CV[, kernel := kernels]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5)
  )
  gc()
  representationsDT[, Model:= paste0(kernel, "_", gamma, "_", nu)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_OCSVM_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                     given_kernel = best_hyper_representation[1], 
                                     given_gamma = as.numeric(best_hyper_representation[2]),
                                     given_nu = as.numeric(best_hyper_representation[3]))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  
  # average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  # average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  # 
  # max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  # max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  return(all_representationsDT)
}


get_OCSVM_augmented_ensemble_10MUR <- function(datasetname, experiments = "OC_combined_CV", CViterations, print_k) {
  
  
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
  list_one_randomOD <- list()
  for(i in 1:10){
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
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_combined_1[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_combined_1[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_combined_1[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_combined_1[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_combined_1[[5]])
  randomOD_6_train <- get_train_representations(representationDT = list_combined_1[[6]])
  randomOD_7_train <- get_train_representations(representationDT = list_combined_1[[7]])
  randomOD_8_train <- get_train_representations(representationDT = list_combined_1[[8]])
  randomOD_9_train <- get_train_representations(representationDT = list_combined_1[[9]])
  randomOD_10_train <- get_train_representations(representationDT = list_combined_1[[10]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_combined_1[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_combined_1[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_combined_1[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_combined_1[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_combined_1[[5]])
  randomOD_6_test <- get_testCV_representations(representationDT = list_combined_1[[6]])
  randomOD_7_test <- get_testCV_representations(representationDT = list_combined_1[[7]])
  randomOD_8_test <- get_testCV_representations(representationDT = list_combined_1[[8]])
  randomOD_9_test <- get_testCV_representations(representationDT = list_combined_1[[9]])
  randomOD_10_test <- get_testCV_representations(representationDT = list_combined_1[[10]])
  
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("10 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      print(paste0("Iteration: ", print_k))
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters+1
            
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, gamma := gammas]
            DT_CV[, nu := nus]
            DT_CV[, kernel := kernels]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  representation6 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_6_train, randomOD_test = randomOD_6_test, feature_col = "Representation_6"))
  representation7 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_7_train, randomOD_test = randomOD_7_test, feature_col = "Representation_7"))  
  representation8 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_8_train, randomOD_test = randomOD_8_test, feature_col = "Representation_8"))  
  representation9 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_9_train, randomOD_test = randomOD_9_test, feature_col = "Representation_9"))  
  representation10 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_10_train, randomOD_test = randomOD_10_test, feature_col = "Representation_10"))
  
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5,
                                      representation6,representation6,
                                      representation7,representation8,
                                      representation9,representation10)
  )
  gc()
  representationsDT[, Model:= paste0(kernel, "_", gamma, "_", nu)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_OCSVM_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                     given_kernel = best_hyper_representation[1], 
                                     given_gamma = as.numeric(best_hyper_representation[2]),
                                     given_nu = as.numeric(best_hyper_representation[3]))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  
  # average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  # average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  # 
  # max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  # max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  return(all_representationsDT)
}


get_OCSVM_augmented_ensemble_15MUR <- function(datasetname, experiments = "OC_combined_CV", CViterations, print_k) {
  
  
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
  list_one_randomOD <- list()
  for(i in 1:15){
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
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_combined_1[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_combined_1[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_combined_1[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_combined_1[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_combined_1[[5]])
  randomOD_6_train <- get_train_representations(representationDT = list_combined_1[[6]])
  randomOD_7_train <- get_train_representations(representationDT = list_combined_1[[7]])
  randomOD_8_train <- get_train_representations(representationDT = list_combined_1[[8]])
  randomOD_9_train <- get_train_representations(representationDT = list_combined_1[[9]])
  randomOD_10_train <- get_train_representations(representationDT = list_combined_1[[10]])
  randomOD_11_train <- get_train_representations(representationDT = list_combined_1[[11]])
  randomOD_12_train <- get_train_representations(representationDT = list_combined_1[[12]])
  randomOD_13_train <- get_train_representations(representationDT = list_combined_1[[13]])
  randomOD_14_train <- get_train_representations(representationDT = list_combined_1[[14]])
  randomOD_15_train <- get_train_representations(representationDT = list_combined_1[[15]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_combined_1[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_combined_1[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_combined_1[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_combined_1[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_combined_1[[5]])
  randomOD_6_test <- get_testCV_representations(representationDT = list_combined_1[[6]])
  randomOD_7_test <- get_testCV_representations(representationDT = list_combined_1[[7]])
  randomOD_8_test <- get_testCV_representations(representationDT = list_combined_1[[8]])
  randomOD_9_test <- get_testCV_representations(representationDT = list_combined_1[[9]])
  randomOD_10_test <- get_testCV_representations(representationDT = list_combined_1[[10]])
  randomOD_11_test <- get_testCV_representations(representationDT = list_combined_1[[11]])
  randomOD_12_test <- get_testCV_representations(representationDT = list_combined_1[[12]])
  randomOD_13_test <- get_testCV_representations(representationDT = list_combined_1[[13]])
  randomOD_14_test <- get_testCV_representations(representationDT = list_combined_1[[14]])
  randomOD_15_test <- get_testCV_representations(representationDT = list_combined_1[[15]])
  
  
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("15 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      print(paste0("Iteration: ", print_k))
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters+1
            
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, gamma := gammas]
            DT_CV[, nu := nus]
            DT_CV[, kernel := kernels]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  representation6 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_6_train, randomOD_test = randomOD_6_test, feature_col = "Representation_6"))
  representation7 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_7_train, randomOD_test = randomOD_7_test, feature_col = "Representation_7"))  
  representation8 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_8_train, randomOD_test = randomOD_8_test, feature_col = "Representation_8"))  
  representation9 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_9_train, randomOD_test = randomOD_9_test, feature_col = "Representation_9"))  
  representation10 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_10_train, randomOD_test = randomOD_10_test, feature_col = "Representation_10"))
  representation11 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_11_train, randomOD_test = randomOD_11_test, feature_col = "Representation_11"))
  representation12 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_12_train, randomOD_test = randomOD_12_test, feature_col = "Representation_12"))
  representation13 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_13_train, randomOD_test = randomOD_13_test, feature_col = "Representation_13"))
  representation14 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_14_train, randomOD_test = randomOD_14_test, feature_col = "Representation_14"))
  representation15 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_15_train, randomOD_test = randomOD_15_test, feature_col = "Representation_15"))
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5,
                                      representation6,representation6,
                                      representation7,representation8,
                                      representation9,representation10,
                                      representation11,representation12,
                                      representation13,representation14,
                                      representation15)
  )
  gc()
  representationsDT[, Model:= paste0(kernel, "_", gamma, "_", nu)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_OCSVM_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                     given_kernel = best_hyper_representation[1], 
                                     given_gamma = as.numeric(best_hyper_representation[2]),
                                     given_nu = as.numeric(best_hyper_representation[3]))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  
  # average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  # average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  # 
  # max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  # max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  return(all_representationsDT)
}



get_iForest_augmented_ensemble_21MUR<- function(datasetname, experiments = "OC_combined_CV", CViterations) {
  
  
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
  list_one_randomOD <- list()
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
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_combined_1[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_combined_1[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_combined_1[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_combined_1[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_combined_1[[5]])
  randomOD_6_train <- get_train_representations(representationDT = list_combined_1[[6]])
  randomOD_7_train <- get_train_representations(representationDT = list_combined_1[[7]])
  randomOD_8_train <- get_train_representations(representationDT = list_combined_1[[8]])
  randomOD_9_train <- get_train_representations(representationDT = list_combined_1[[9]])
  randomOD_10_train <- get_train_representations(representationDT = list_combined_1[[10]])
  randomOD_11_train <- get_train_representations(representationDT = list_combined_1[[11]])
  randomOD_12_train <- get_train_representations(representationDT = list_combined_1[[12]])
  randomOD_13_train <- get_train_representations(representationDT = list_combined_1[[13]])
  randomOD_14_train <- get_train_representations(representationDT = list_combined_1[[14]])
  randomOD_15_train <- get_train_representations(representationDT = list_combined_1[[15]])
  randomOD_16_train <- get_train_representations(representationDT = list_combined_1[[16]])
  randomOD_17_train <- get_train_representations(representationDT = list_combined_1[[17]])
  randomOD_18_train <- get_train_representations(representationDT = list_combined_1[[18]])
  randomOD_19_train <- get_train_representations(representationDT = list_combined_1[[19]])
  randomOD_20_train <- get_train_representations(representationDT = list_combined_1[[20]])
  randomOD_21_train <- get_train_representations(representationDT = list_combined_1[[21]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_combined_1[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_combined_1[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_combined_1[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_combined_1[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_combined_1[[5]])
  randomOD_6_test <- get_testCV_representations(representationDT = list_combined_1[[6]])
  randomOD_7_test <- get_testCV_representations(representationDT = list_combined_1[[7]])
  randomOD_8_test <- get_testCV_representations(representationDT = list_combined_1[[8]])
  randomOD_9_test <- get_testCV_representations(representationDT = list_combined_1[[9]])
  randomOD_10_test <- get_testCV_representations(representationDT = list_combined_1[[10]])
  randomOD_11_test <- get_testCV_representations(representationDT = list_combined_1[[11]])
  randomOD_12_test <- get_testCV_representations(representationDT = list_combined_1[[12]])
  randomOD_13_test <- get_testCV_representations(representationDT = list_combined_1[[13]])
  randomOD_14_test <- get_testCV_representations(representationDT = list_combined_1[[14]])
  randomOD_15_test <- get_testCV_representations(representationDT = list_combined_1[[15]])
  randomOD_16_test <- get_testCV_representations(representationDT = list_combined_1[[16]])
  randomOD_17_test <- get_testCV_representations(representationDT = list_combined_1[[17]])
  randomOD_18_test <- get_testCV_representations(representationDT = list_combined_1[[18]])
  randomOD_19_test <- get_testCV_representations(representationDT = list_combined_1[[19]])
  randomOD_20_test <- get_testCV_representations(representationDT = list_combined_1[[20]])
  randomOD_21_test <- get_testCV_representations(representationDT = list_combined_1[[21]])
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("21 MURs")
      print("One-random section")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      
      
      for(estimators in c(25, 50, 100, 500)){
        for(maxSamples in c(0.2 * dim(CVtrain_DT)[1], 0.5 * dim(CVtrain_DT)[1], 0.8 * dim(CVtrain_DT)[1])){
          for(maxFeatures in c(0.3 * dim(CVtrain_DT)[2], 0.5 * dim(CVtrain_DT)[2], 0.7 * dim(CVtrain_DT)[2], 1.0 * dim(CVtrain_DT)[2])){
            iters <- iters+1
            if(maxFeatures <= 1){
              maxFeatures <- 2
            }
            
            scores_CV <- calculate_iForest_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                  given_nEstimators = estimators, 
                                                  given_maxSamples = round(maxSamples), 
                                                  given_maxFeatures = round(maxFeatures))
            
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, estimators := estimators]
            DT_CV[, maxSamples := maxSamples]
            DT_CV[, maxFeatures := maxFeatures]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  representation6 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_6_train, randomOD_test = randomOD_6_test, feature_col = "Representation_6"))
  representation7 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_7_train, randomOD_test = randomOD_7_test, feature_col = "Representation_7"))  
  representation8 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_8_train, randomOD_test = randomOD_8_test, feature_col = "Representation_8"))  
  representation9 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_9_train, randomOD_test = randomOD_9_test, feature_col = "Representation_9"))  
  representation10 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_10_train, randomOD_test = randomOD_10_test, feature_col = "Representation_10"))
  representation11 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_11_train, randomOD_test = randomOD_11_test, feature_col = "Representation_11"))
  representation12 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_12_train, randomOD_test = randomOD_12_test, feature_col = "Representation_12"))
  representation13 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_13_train, randomOD_test = randomOD_13_test, feature_col = "Representation_13"))
  representation14 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_14_train, randomOD_test = randomOD_14_test, feature_col = "Representation_14"))
  representation15 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_15_train, randomOD_test = randomOD_15_test, feature_col = "Representation_15"))
  representation16 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_16_train, randomOD_test = randomOD_16_test, feature_col = "Representation_16"))
  representation17 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_17_train, randomOD_test = randomOD_17_test, feature_col = "Representation_17"))
  representation18 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_18_train, randomOD_test = randomOD_18_test, feature_col = "Representation_18"))
  representation19 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_19_train, randomOD_test = randomOD_19_test, feature_col = "Representation_19"))
  representation20 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_20_train, randomOD_test = randomOD_20_test, feature_col = "Representation_20"))
  representation21 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_21_train, randomOD_test = randomOD_21_test, feature_col = "Representation_21"))
  
  
  
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5, representation6,
                                      representation7, representation8,
                                      representation9, representation10,
                                      representation11, representation12,
                                      representation13, representation14,
                                      representation15, representation16,
                                      representation17, representation18,
                                      representation19, representation20,
                                      representation21
  ))
  gc()
  representationsDT[, Model:= paste0(estimators, "_", maxSamples, "_", maxFeatures)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    print("best hyper")
    print(best_hyper_representation)
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_iForest_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                       given_nEstimators = best_hyper_representation[1], 
                                       given_maxSamples = round(as.numeric(best_hyper_representation[2])), 
                                       given_maxFeatures = round(as.numeric(best_hyper_representation[3])))
    
    
    testDT <- data.table(scores = scores, Label = ids_labels$Label, id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  # dcasted_representations <- dcast.data.table(all_representationsDT, id+Label~representation, value.var = "scores")
  average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  
  max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  # Original data  ----------------------------------------------------------
  
  iters <- 0
  res_CV <- list()
  for(ij in 1:10){
    
    trainDT <- DToriginal[id %in% list_train_chunks[[ij]]]
    outliers_train_DT <- copy(trainDT[Label == "yes"])
    CVtrain_DT <- copy(trainDT[Label == "no"])
    CVtest_DT1 <- DToriginal[id %in% list_test_chunks[[ij]]]
    
    if(CVtest_DT1[Label=="yes", length(id)] <= 1){
      CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
      CVtest_DT <- na.omit(CVtest_DT)
      CVtest_DT <- unique(CVtest_DT)
    }else{CVtest_DT <- CVtest_DT1}
    print(CVtest_DT[, .N, by = Label])
    
    CVtest_id_final <- CVtest_DT$id
    CVtest_Label_final <- CVtest_DT$Label
    
    CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
    CVtest_DT[, `:=` (id = NULL, Label = NULL)]
    
    print(paste0("Kfold: ", ij))
    print(paste0("Representation: ", "Original data"))
    
    for(estimators in c(25, 50, 100, 500)){
      for(maxSamples in c(0.2 * dim(CVtrain_DT)[1], 0.5 * dim(CVtrain_DT)[1], 0.8 * dim(CVtrain_DT)[1])){
        for(maxFeatures in c(0.3 * dim(CVtrain_DT)[2], 0.5 * dim(CVtrain_DT)[2], 0.7 * dim(CVtrain_DT)[2], 1.0 * dim(CVtrain_DT)[2])){
          iters <- iters+1
          if(maxFeatures <= 1){
            maxFeatures <- 2
          }
          scores_CV <- calculate_iForest_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nEstimators = estimators, 
                                                given_maxSamples = round(maxSamples), 
                                                given_maxFeatures = round(maxFeatures))
          
          DT_CV <- data.table(scores = scores_CV, 
                              id = CVtest_id_final,
                              Label = CVtest_Label_final)
          DT_CV[, Kfold:=ij]
          DT_CV[, estimators := estimators]
          DT_CV[, maxSamples := maxSamples]
          DT_CV[, maxFeatures := maxFeatures]
          DT_CV[, Representation_col := "Original"]
          res_CV[[iters]] <- DT_CV
        }
      }
    }
  }
  final_DT_original <- rbindlist(res_CV)
  
  final_DT_original[, Model:= paste0(estimators, "_", maxSamples, "_", maxFeatures)]
  aucCV_original <- final_DT_original[, auc(Label, scores), by = .(Kfold, Model)]
  best_hyper_original_CV <- aucCV_original[, mean(V1), by = Model][order(V1, decreasing = T)][1]
  
  
  # Train with the best hyperparameter all the 80% data  
  
  best_hyper_original <- best_hyper_original_CV[, stringr::str_split(Model, pattern = "_")[[1]]]
  print(best_hyper_original)
  representation_train_DT <- DToriginal[id %in% list_train_id[[1]]][Label == "no"]
  representation_train_DT[, .N, by = Label]
  
  representation_test_DT <- DToriginal[id %in% list_test_id[[1]]]
  representation_test_DT[, .N, by = Label]
  if(representation_test_DT[Label=="yes", length(id)] == 0){
    representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
    representation_test_DT <- na.omit(representation_test_DT)
  }else{representation_test_DT <- representation_test_DT}
  
  ids_labels <- representation_test_DT[, .(id, Label)]
  representation_train_DT[, `:=` (id = NULL, Label = NULL)]
  representation_test_DT[, `:=` (id = NULL, Label = NULL)]
  
  scores_original <- calculate_iForest_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                              given_nEstimators = best_hyper_original[1], 
                                              given_maxSamples = round(as.numeric(best_hyper_original[2])), 
                                              given_maxFeatures = round(as.numeric(best_hyper_original[3])))
  
  
  testDT_original <- data.table(scores = scores_original, 
                                Label = ids_labels$Label, 
                                id = ids_labels$id, representation = "Original")
  
  auc_original <- auc(testDT_original$Label, testDT_original$scores)
  
  
  
  return(list(ensemble_auc, ensemble_max_auc, all_representationsDT, testDT_original))
}


get_iForest_augmented_ensemble_5MUR <- function(datasetname, experiments = "OC_combined_CV", CViterations) {
  
  
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
  list_one_randomOD <- list()
  for(i in 1:5){
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
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_combined_1[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_combined_1[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_combined_1[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_combined_1[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_combined_1[[5]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_combined_1[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_combined_1[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_combined_1[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_combined_1[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_combined_1[[5]])
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("5 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      
      for(estimators in c(10, 25, 50, 100, 500)){
        for(maxSamples in c(0.2 * dim(CVtrain_DT)[1], 0.5 * dim(CVtrain_DT)[1], 0.8 * dim(CVtrain_DT)[1])){
          for(maxFeatures in c(0.3 * dim(CVtrain_DT)[2], 0.5 * dim(CVtrain_DT)[2], 0.7 * dim(CVtrain_DT)[2], 1.0 * dim(CVtrain_DT)[2])){
            iters <- iters+1
            if(round(maxFeatures) <= 1){
              maxFeatures <- 2
            }
            
            scores_CV <- calculate_iForest_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                  given_nEstimators = estimators, 
                                                  given_maxSamples = round(maxSamples), 
                                                  given_maxFeatures = round(maxFeatures))
            
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, estimators := estimators]
            DT_CV[, maxSamples := maxSamples]
            DT_CV[, maxFeatures := maxFeatures]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5)
  )
  gc()
  representationsDT[, Model:= paste0(estimators, "_", maxSamples, "_", maxFeatures)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_iForest_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                       given_nEstimators = best_hyper_representation[1], 
                                       given_maxSamples = round(as.numeric(best_hyper_representation[2])), 
                                       given_maxFeatures = round(as.numeric(best_hyper_representation[3])))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  
  average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  
  max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  return(all_representationsDT)
}


get_iForest_augmented_ensemble_10MUR <- function(datasetname, experiments = "OC_combined_CV", CViterations) {
  
  
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
  list_one_randomOD <- list()
  for(i in 1:10){
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
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_combined_1[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_combined_1[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_combined_1[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_combined_1[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_combined_1[[5]])
  randomOD_6_train <- get_train_representations(representationDT = list_combined_1[[6]])
  randomOD_7_train <- get_train_representations(representationDT = list_combined_1[[7]])
  randomOD_8_train <- get_train_representations(representationDT = list_combined_1[[8]])
  randomOD_9_train <- get_train_representations(representationDT = list_combined_1[[9]])
  randomOD_10_train <- get_train_representations(representationDT = list_combined_1[[10]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_combined_1[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_combined_1[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_combined_1[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_combined_1[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_combined_1[[5]])
  randomOD_6_test <- get_testCV_representations(representationDT = list_combined_1[[6]])
  randomOD_7_test <- get_testCV_representations(representationDT = list_combined_1[[7]])
  randomOD_8_test <- get_testCV_representations(representationDT = list_combined_1[[8]])
  randomOD_9_test <- get_testCV_representations(representationDT = list_combined_1[[9]])
  randomOD_10_test <- get_testCV_representations(representationDT = list_combined_1[[10]])
  
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("10 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      
      for(estimators in c(10, 25, 50, 100, 500)){
        for(maxSamples in c(0.2 * dim(CVtrain_DT)[1], 0.5 * dim(CVtrain_DT)[1], 0.8 * dim(CVtrain_DT)[1])){
          for(maxFeatures in c(0.3 * dim(CVtrain_DT)[2], 0.5 * dim(CVtrain_DT)[2], 0.7 * dim(CVtrain_DT)[2], 1.0 * dim(CVtrain_DT)[2])){
            iters <- iters+1
            if(round(maxFeatures) <= 1){
              maxFeatures <- 2
            }
            
            scores_CV <- calculate_iForest_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                  given_nEstimators = estimators, 
                                                  given_maxSamples = round(maxSamples), 
                                                  given_maxFeatures = round(maxFeatures))
            
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, estimators := estimators]
            DT_CV[, maxSamples := maxSamples]
            DT_CV[, maxFeatures := maxFeatures]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  gc()
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  representation6 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_6_train, randomOD_test = randomOD_6_test, feature_col = "Representation_6"))
  representation7 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_7_train, randomOD_test = randomOD_7_test, feature_col = "Representation_7"))  
  representation8 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_8_train, randomOD_test = randomOD_8_test, feature_col = "Representation_8"))  
  representation9 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_9_train, randomOD_test = randomOD_9_test, feature_col = "Representation_9"))  
  representation10 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_10_train, randomOD_test = randomOD_10_test, feature_col = "Representation_10"))
  
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5,
                                      representation6,representation6,
                                      representation7,representation8,
                                      representation9,representation10)
  )
  gc()
  representationsDT[, Model:= paste0(estimators, "_", maxSamples, "_", maxFeatures)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_iForest_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                       given_nEstimators = best_hyper_representation[1], 
                                       given_maxSamples = round(as.numeric(best_hyper_representation[2])), 
                                       given_maxFeatures = round(as.numeric(best_hyper_representation[3])))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  
  average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  
  max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  return(all_representationsDT)
}


get_iForest_augmented_ensemble_15MUR <- function(datasetname, experiments = "OC_combined_CV", CViterations) {
  
  
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
  list_one_randomOD <- list()
  for(i in 1:15){
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
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_combined_1[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_combined_1[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_combined_1[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_combined_1[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_combined_1[[5]])
  randomOD_6_train <- get_train_representations(representationDT = list_combined_1[[6]])
  randomOD_7_train <- get_train_representations(representationDT = list_combined_1[[7]])
  randomOD_8_train <- get_train_representations(representationDT = list_combined_1[[8]])
  randomOD_9_train <- get_train_representations(representationDT = list_combined_1[[9]])
  randomOD_10_train <- get_train_representations(representationDT = list_combined_1[[10]])
  randomOD_11_train <- get_train_representations(representationDT = list_combined_1[[11]])
  randomOD_12_train <- get_train_representations(representationDT = list_combined_1[[12]])
  randomOD_13_train <- get_train_representations(representationDT = list_combined_1[[13]])
  randomOD_14_train <- get_train_representations(representationDT = list_combined_1[[14]])
  randomOD_15_train <- get_train_representations(representationDT = list_combined_1[[15]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_combined_1[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_combined_1[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_combined_1[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_combined_1[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_combined_1[[5]])
  randomOD_6_test <- get_testCV_representations(representationDT = list_combined_1[[6]])
  randomOD_7_test <- get_testCV_representations(representationDT = list_combined_1[[7]])
  randomOD_8_test <- get_testCV_representations(representationDT = list_combined_1[[8]])
  randomOD_9_test <- get_testCV_representations(representationDT = list_combined_1[[9]])
  randomOD_10_test <- get_testCV_representations(representationDT = list_combined_1[[10]])
  randomOD_11_test <- get_testCV_representations(representationDT = list_combined_1[[11]])
  randomOD_12_test <- get_testCV_representations(representationDT = list_combined_1[[12]])
  randomOD_13_test <- get_testCV_representations(representationDT = list_combined_1[[13]])
  randomOD_14_test <- get_testCV_representations(representationDT = list_combined_1[[14]])
  randomOD_15_test <- get_testCV_representations(representationDT = list_combined_1[[15]])
  
  
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("15 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      
      for(estimators in c(10, 25, 50, 100, 500)){
        for(maxSamples in c(0.2 * dim(CVtrain_DT)[1], 0.5 * dim(CVtrain_DT)[1], 0.8 * dim(CVtrain_DT)[1])){
          for(maxFeatures in c(0.3 * dim(CVtrain_DT)[2], 0.5 * dim(CVtrain_DT)[2], 0.7 * dim(CVtrain_DT)[2], 1.0 * dim(CVtrain_DT)[2])){
            iters <- iters+1
            if(round(maxFeatures) <= 1){
              maxFeatures <- 2
            }
            
            scores_CV <- calculate_iForest_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                  given_nEstimators = estimators, 
                                                  given_maxSamples = round(maxSamples), 
                                                  given_maxFeatures = round(maxFeatures))
            
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, estimators := estimators]
            DT_CV[, maxSamples := maxSamples]
            DT_CV[, maxFeatures := maxFeatures]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  gc()
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  representation6 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_6_train, randomOD_test = randomOD_6_test, feature_col = "Representation_6"))
  representation7 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_7_train, randomOD_test = randomOD_7_test, feature_col = "Representation_7"))  
  representation8 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_8_train, randomOD_test = randomOD_8_test, feature_col = "Representation_8"))  
  representation9 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_9_train, randomOD_test = randomOD_9_test, feature_col = "Representation_9"))  
  representation10 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_10_train, randomOD_test = randomOD_10_test, feature_col = "Representation_10"))
  representation11 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_11_train, randomOD_test = randomOD_11_test, feature_col = "Representation_11"))
  representation12 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_12_train, randomOD_test = randomOD_12_test, feature_col = "Representation_12"))
  representation13 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_13_train, randomOD_test = randomOD_13_test, feature_col = "Representation_13"))
  representation14 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_14_train, randomOD_test = randomOD_14_test, feature_col = "Representation_14"))
  representation15 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_15_train, randomOD_test = randomOD_15_test, feature_col = "Representation_15"))
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5,
                                      representation6,representation6,
                                      representation7,representation8,
                                      representation9,representation10,
                                      representation11,representation12,
                                      representation13,representation14,
                                      representation15)
  )
  gc()
  representationsDT[, Model:= paste0(estimators, "_", maxSamples, "_", maxFeatures)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_iForest_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                       given_nEstimators = best_hyper_representation[1], 
                                       given_maxSamples = round(as.numeric(best_hyper_representation[2])), 
                                       given_maxFeatures = round(as.numeric(best_hyper_representation[3])))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  
  average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  
  max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  return(all_representationsDT)
}




get_CV_experiments_paper_ensemble_norm <- function(datasetname, experiments = "OC_combined_CV", CViterations) {
  
  
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
  list_one_randomOD <- list()
  for(i in 1:21){
    unsupervised_DTs1 <- create_unsupervised_view(datasetname, percentage_OD=1, mixed_view_features=1)
    one_randomOD1 <- unsupervised_DTs1$mixed_arthur
    one_randomOD <- one_randomOD1[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SDcols = 1:10]
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
    #list_combined_1[[i]] <- combinedDT_1
    
    one_randomOD[, `:=` (id = combinedDT_1$id, Label = combinedDT_1$Label)]
    list_one_randomOD[[i]] <- one_randomOD
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_one_randomOD[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_one_randomOD[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_one_randomOD[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_one_randomOD[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_one_randomOD[[5]])
  randomOD_6_train <- get_train_representations(representationDT = list_one_randomOD[[6]])
  randomOD_7_train <- get_train_representations(representationDT = list_one_randomOD[[7]])
  randomOD_8_train <- get_train_representations(representationDT = list_one_randomOD[[8]])
  randomOD_9_train <- get_train_representations(representationDT = list_one_randomOD[[9]])
  randomOD_10_train <- get_train_representations(representationDT = list_one_randomOD[[10]])
  randomOD_11_train <- get_train_representations(representationDT = list_one_randomOD[[11]])
  randomOD_12_train <- get_train_representations(representationDT = list_one_randomOD[[12]])
  randomOD_13_train <- get_train_representations(representationDT = list_one_randomOD[[13]])
  randomOD_14_train <- get_train_representations(representationDT = list_one_randomOD[[14]])
  randomOD_15_train <- get_train_representations(representationDT = list_one_randomOD[[15]])
  randomOD_16_train <- get_train_representations(representationDT = list_one_randomOD[[16]])
  randomOD_17_train <- get_train_representations(representationDT = list_one_randomOD[[17]])
  randomOD_18_train <- get_train_representations(representationDT = list_one_randomOD[[18]])
  randomOD_19_train <- get_train_representations(representationDT = list_one_randomOD[[19]])
  randomOD_20_train <- get_train_representations(representationDT = list_one_randomOD[[20]])
  randomOD_21_train <- get_train_representations(representationDT = list_one_randomOD[[21]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_one_randomOD[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_one_randomOD[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_one_randomOD[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_one_randomOD[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_one_randomOD[[5]])
  randomOD_6_test <- get_testCV_representations(representationDT = list_one_randomOD[[6]])
  randomOD_7_test <- get_testCV_representations(representationDT = list_one_randomOD[[7]])
  randomOD_8_test <- get_testCV_representations(representationDT = list_one_randomOD[[8]])
  randomOD_9_test <- get_testCV_representations(representationDT = list_one_randomOD[[9]])
  randomOD_10_test <- get_testCV_representations(representationDT = list_one_randomOD[[10]])
  randomOD_11_test <- get_testCV_representations(representationDT = list_one_randomOD[[11]])
  randomOD_12_test <- get_testCV_representations(representationDT = list_one_randomOD[[12]])
  randomOD_13_test <- get_testCV_representations(representationDT = list_one_randomOD[[13]])
  randomOD_14_test <- get_testCV_representations(representationDT = list_one_randomOD[[14]])
  randomOD_15_test <- get_testCV_representations(representationDT = list_one_randomOD[[15]])
  randomOD_16_test <- get_testCV_representations(representationDT = list_one_randomOD[[16]])
  randomOD_17_test <- get_testCV_representations(representationDT = list_one_randomOD[[17]])
  randomOD_18_test <- get_testCV_representations(representationDT = list_one_randomOD[[18]])
  randomOD_19_test <- get_testCV_representations(representationDT = list_one_randomOD[[19]])
  randomOD_20_test <- get_testCV_representations(representationDT = list_one_randomOD[[20]])
  randomOD_21_test <- get_testCV_representations(representationDT = list_one_randomOD[[21]])
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("One-random section")
      print("21 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters+1
            
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, gamma := gammas]
            DT_CV[, nu := nus]
            DT_CV[, kernel := kernels]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  representation6 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_6_train, randomOD_test = randomOD_6_test, feature_col = "Representation_6"))
  representation7 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_7_train, randomOD_test = randomOD_7_test, feature_col = "Representation_7"))  
  representation8 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_8_train, randomOD_test = randomOD_8_test, feature_col = "Representation_8"))  
  representation9 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_9_train, randomOD_test = randomOD_9_test, feature_col = "Representation_9"))  
  representation10 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_10_train, randomOD_test = randomOD_10_test, feature_col = "Representation_10"))
  representation11 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_11_train, randomOD_test = randomOD_11_test, feature_col = "Representation_11"))
  representation12 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_12_train, randomOD_test = randomOD_12_test, feature_col = "Representation_12"))
  representation13 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_13_train, randomOD_test = randomOD_13_test, feature_col = "Representation_13"))
  representation14 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_14_train, randomOD_test = randomOD_14_test, feature_col = "Representation_14"))
  representation15 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_15_train, randomOD_test = randomOD_15_test, feature_col = "Representation_15"))
  representation16 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_16_train, randomOD_test = randomOD_16_test, feature_col = "Representation_16"))
  representation17 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_17_train, randomOD_test = randomOD_17_test, feature_col = "Representation_17"))
  representation18 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_18_train, randomOD_test = randomOD_18_test, feature_col = "Representation_18"))
  representation19 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_19_train, randomOD_test = randomOD_19_test, feature_col = "Representation_19"))
  representation20 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_20_train, randomOD_test = randomOD_20_test, feature_col = "Representation_20"))
  representation21 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_21_train, randomOD_test = randomOD_21_test, feature_col = "Representation_21"))
  
  
  
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5, representation6,
                                      representation7, representation8,
                                      representation9, representation10,
                                      representation11, representation12,
                                      representation13, representation14,
                                      representation15, representation16,
                                      representation17, representation18,
                                      representation19, representation20,
                                      representation21
  ))
  gc()
  representationsDT[, Model:= paste0(kernel, "_", gamma, "_", nu)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_OCSVM_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                     given_kernel = best_hyper_representation[1], 
                                     given_gamma = as.numeric(best_hyper_representation[2]),
                                     given_nu = as.numeric(best_hyper_representation[3]))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  # dcasted_representations <- dcast.data.table(all_representationsDT, id+Label~representation, value.var = "scores")
  average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  
  max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  # Original data  ----------------------------------------------------------
  
  iters <- 0
  res_CV <- list()
  for(ij in 1:10){
    
    trainDT <- DToriginal[id %in% list_train_chunks[[ij]]]
    outliers_train_DT <- copy(trainDT[Label == "yes"])
    CVtrain_DT <- copy(trainDT[Label == "no"])
    CVtest_DT1 <- DToriginal[id %in% list_test_chunks[[ij]]]
    
    if(CVtest_DT1[Label=="yes", length(id)] <= 1){
      CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
      CVtest_DT <- na.omit(CVtest_DT)
      CVtest_DT <- unique(CVtest_DT)
    }else{CVtest_DT <- CVtest_DT1}
    print(CVtest_DT[, .N, by = Label])
    
    CVtest_id_final <- CVtest_DT$id
    CVtest_Label_final <- CVtest_DT$Label
    
    CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
    CVtest_DT[, `:=` (id = NULL, Label = NULL)]
    
    print(paste0("Kfold: ", ij))
    print(paste0("Representation: ", "Original data"))
    
    for(kernels in c("linear", "rbf", "sigmoid")){
      for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
        for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
          iters <- iters+1
          
          scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                              given_nu = nus, given_kernel = kernels, given_gamma = gammas)
          DT_CV <- data.table(scores = scores_CV, 
                              id = CVtest_id_final,
                              Label = CVtest_Label_final)
          DT_CV[, Kfold:=ij]
          DT_CV[, gamma := gammas]
          DT_CV[, nu := nus]
          DT_CV[, kernel := kernels]
          DT_CV[, Representation_col := "Original"]
          res_CV[[iters]] <- DT_CV
        }
      }
    }
  }
  final_DT_original <- rbindlist(res_CV)
  
  final_DT_original[, Model:= paste0(kernel, "_", gamma, "_", nu)]
  aucCV_original <- final_DT_original[, auc(Label, scores), by = .(Kfold, Model)]
  best_hyper_original_CV <- aucCV_original[, mean(V1), by = Model][order(V1, decreasing = T)][1]
  
  
  # Train with the best hyperparameter all the 80% data  
  
  best_hyper_original <- best_hyper_original_CV[, stringr::str_split(Model, pattern = "_")[[1]]]
  
  representation_train_DT <- DToriginal[id %in% list_train_id[[1]]][Label == "no"]
  representation_train_DT[, .N, by = Label]
  
  representation_test_DT <- DToriginal[id %in% list_test_id[[1]]]
  representation_test_DT[, .N, by = Label]
  if(representation_test_DT[Label=="yes", length(id)] == 0){
    representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
    representation_test_DT <- na.omit(representation_test_DT)
  }else{representation_test_DT <- representation_test_DT}
  
  ids_labels <- representation_test_DT[, .(id, Label)]
  representation_train_DT[, `:=` (id = NULL, Label = NULL)]
  representation_test_DT[, `:=` (id = NULL, Label = NULL)]
  
  scores_original <- calculate_OCSVM_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                            given_kernel = best_hyper_original[1], 
                                            given_gamma = as.numeric(best_hyper_original[2]),
                                            given_nu = as.numeric(best_hyper_original[3]))
  
  
  testDT_original <- data.table(scores = scores_original, 
                                Label = ids_labels$Label, 
                                id = ids_labels$id, representation = "Original")
  
  auc_original <- auc(testDT_original$Label, testDT_original$scores)
  
  
  
  return(list(ensemble_auc, ensemble_max_auc, all_representationsDT, testDT_original))
}


get_CV_experiments_paper_5_MUR_ensemble_norm <- function(datasetname, experiments = "OC_combined_CV", CViterations) {
  
  
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
  list_one_randomOD <- list()
  for(i in 1:5){
    unsupervised_DTs1 <- create_unsupervised_view(datasetname, percentage_OD=1, mixed_view_features=1)
    one_randomOD1 <- unsupervised_DTs1$mixed_arthur
    one_randomOD <- one_randomOD1[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SDcols = 1:10]
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
    #list_combined_1[[i]] <- combinedDT_1
    
    one_randomOD[, `:=` (id = combinedDT_1$id, Label = combinedDT_1$Label)]
    list_one_randomOD[[i]] <- one_randomOD
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_one_randomOD[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_one_randomOD[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_one_randomOD[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_one_randomOD[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_one_randomOD[[5]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_one_randomOD[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_one_randomOD[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_one_randomOD[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_one_randomOD[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_one_randomOD[[5]])
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("5 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters+1
            
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, gamma := gammas]
            DT_CV[, nu := nus]
            DT_CV[, kernel := kernels]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5)
  )
  gc()
  representationsDT[, Model:= paste0(kernel, "_", gamma, "_", nu)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_OCSVM_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                     given_kernel = best_hyper_representation[1], 
                                     given_gamma = as.numeric(best_hyper_representation[2]),
                                     given_nu = as.numeric(best_hyper_representation[3]))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  
  # average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  # average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  # 
  # max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  # max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  return(all_representationsDT)
}


get_CV_experiments_paper_10_MUR_ensemble_norm <- function(datasetname, experiments = "OC_combined_CV", CViterations) {
  
  
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
  list_one_randomOD <- list()
  for(i in 1:10){
    unsupervised_DTs1 <- create_unsupervised_view(datasetname, percentage_OD=1, mixed_view_features=1)
    one_randomOD1 <- unsupervised_DTs1$mixed_arthur
    one_randomOD <- one_randomOD1[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SDcols = 1:10]
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
    #list_combined_1[[i]] <- combinedDT_1
    
    one_randomOD[, `:=` (id = combinedDT_1$id, Label = combinedDT_1$Label)]
    list_one_randomOD[[i]] <- one_randomOD
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_one_randomOD[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_one_randomOD[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_one_randomOD[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_one_randomOD[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_one_randomOD[[5]])
  randomOD_6_train <- get_train_representations(representationDT = list_one_randomOD[[6]])
  randomOD_7_train <- get_train_representations(representationDT = list_one_randomOD[[7]])
  randomOD_8_train <- get_train_representations(representationDT = list_one_randomOD[[8]])
  randomOD_9_train <- get_train_representations(representationDT = list_one_randomOD[[9]])
  randomOD_10_train <- get_train_representations(representationDT = list_one_randomOD[[10]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_one_randomOD[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_one_randomOD[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_one_randomOD[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_one_randomOD[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_one_randomOD[[5]])
  randomOD_6_test <- get_testCV_representations(representationDT = list_one_randomOD[[6]])
  randomOD_7_test <- get_testCV_representations(representationDT = list_one_randomOD[[7]])
  randomOD_8_test <- get_testCV_representations(representationDT = list_one_randomOD[[8]])
  randomOD_9_test <- get_testCV_representations(representationDT = list_one_randomOD[[9]])
  randomOD_10_test <- get_testCV_representations(representationDT = list_one_randomOD[[10]])
  
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("10 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters+1
            
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, gamma := gammas]
            DT_CV[, nu := nus]
            DT_CV[, kernel := kernels]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  representation6 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_6_train, randomOD_test = randomOD_6_test, feature_col = "Representation_6"))
  representation7 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_7_train, randomOD_test = randomOD_7_test, feature_col = "Representation_7"))  
  representation8 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_8_train, randomOD_test = randomOD_8_test, feature_col = "Representation_8"))  
  representation9 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_9_train, randomOD_test = randomOD_9_test, feature_col = "Representation_9"))  
  representation10 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_10_train, randomOD_test = randomOD_10_test, feature_col = "Representation_10"))
  
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5,
                                      representation6,representation6,
                                      representation7,representation8,
                                      representation9,representation10)
  )
  gc()
  representationsDT[, Model:= paste0(kernel, "_", gamma, "_", nu)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_OCSVM_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                     given_kernel = best_hyper_representation[1], 
                                     given_gamma = as.numeric(best_hyper_representation[2]),
                                     given_nu = as.numeric(best_hyper_representation[3]))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  
  # average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  # average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  # 
  # max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  # max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  return(all_representationsDT)
}


get_CV_experiments_paper_15_MUR_ensemble_norm <- function(datasetname, experiments = "OC_combined_CV", CViterations) {
  
  
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
  list_one_randomOD <- list()
  for(i in 1:15){
    unsupervised_DTs1 <- create_unsupervised_view(datasetname, percentage_OD=1, mixed_view_features=1)
    one_randomOD1 <- unsupervised_DTs1$mixed_arthur
    one_randomOD <- one_randomOD1[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SDcols = 1:10]
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
    #list_combined_1[[i]] <- combinedDT_1
    
    one_randomOD[, `:=` (id = combinedDT_1$id, Label = combinedDT_1$Label)]
    list_one_randomOD[[i]] <- one_randomOD
  }
  
  # split 80% - 20% & Cross Validation
  
  list_train_id <- list()
  list_test_id <- list()
  for(i in 1:CViterations){
    list_train_id[[i]] <- DToriginal[, sample(x = id, size = 0.8 * dim(DToriginal)[1])]
    list_test_id[[i]] <- setdiff(DToriginal$id, list_train_id[[i]])
  }
  
  element_remove_train <- list()
  kk <- 0
  for(i in 1:CViterations){
    if(DToriginal[id %in% list_train_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_train[[kk]] <- i
    }
  }
  if(length(element_remove_train) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_train))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_train))
  }
  
  
  element_remove_test <- list()
  kk <- 0
  for(i in 1:length(list_test_id)){
    if(DToriginal[id %in% list_test_id[[i]]][Label == "yes", length(id)] == 0){
      kk <- kk +1
      element_remove_test[[kk]] <- i
    }
  }
  
  if(length(element_remove_test) > 0){
    list_train_id <- rlist::list.remove(list_train_id, unlist(element_remove_test))
    list_test_id <- rlist::list.remove(list_test_id, unlist(element_remove_test))
  }
  
  
  i <- 1
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
  
  
  get_train_representations <- function(representationDT) {
    
    randomOD_train <- list()
    for(i in 1:10){
      randomOD_train[[i]] <- representationDT[id %in% list_train_chunks[[i]]]
    }
    return(randomOD_train)
  }
  
  get_testCV_representations <- function(representationDT) {
    
    randomOD_testCV <- list()
    for(i in 1:10){
      randomOD_testCV[[i]] <- representationDT[id %in% list_test_chunks[[i]]]
    }
    return(randomOD_testCV)
  }
  
  randomOD_1_train <- get_train_representations(representationDT = list_one_randomOD[[1]])
  randomOD_2_train <- get_train_representations(representationDT = list_one_randomOD[[2]])
  randomOD_3_train <- get_train_representations(representationDT = list_one_randomOD[[3]])
  randomOD_4_train <- get_train_representations(representationDT = list_one_randomOD[[4]])
  randomOD_5_train <- get_train_representations(representationDT = list_one_randomOD[[5]])
  randomOD_6_train <- get_train_representations(representationDT = list_one_randomOD[[6]])
  randomOD_7_train <- get_train_representations(representationDT = list_one_randomOD[[7]])
  randomOD_8_train <- get_train_representations(representationDT = list_one_randomOD[[8]])
  randomOD_9_train <- get_train_representations(representationDT = list_one_randomOD[[9]])
  randomOD_10_train <- get_train_representations(representationDT = list_one_randomOD[[10]])
  randomOD_11_train <- get_train_representations(representationDT = list_one_randomOD[[11]])
  randomOD_12_train <- get_train_representations(representationDT = list_one_randomOD[[12]])
  randomOD_13_train <- get_train_representations(representationDT = list_one_randomOD[[13]])
  randomOD_14_train <- get_train_representations(representationDT = list_one_randomOD[[14]])
  randomOD_15_train <- get_train_representations(representationDT = list_one_randomOD[[15]])
  
  randomOD_1_test <- get_testCV_representations(representationDT = list_one_randomOD[[1]])
  randomOD_2_test <- get_testCV_representations(representationDT = list_one_randomOD[[2]])
  randomOD_3_test <- get_testCV_representations(representationDT = list_one_randomOD[[3]])
  randomOD_4_test <- get_testCV_representations(representationDT = list_one_randomOD[[4]])
  randomOD_5_test <- get_testCV_representations(representationDT = list_one_randomOD[[5]])
  randomOD_6_test <- get_testCV_representations(representationDT = list_one_randomOD[[6]])
  randomOD_7_test <- get_testCV_representations(representationDT = list_one_randomOD[[7]])
  randomOD_8_test <- get_testCV_representations(representationDT = list_one_randomOD[[8]])
  randomOD_9_test <- get_testCV_representations(representationDT = list_one_randomOD[[9]])
  randomOD_10_test <- get_testCV_representations(representationDT = list_one_randomOD[[10]])
  randomOD_11_test <- get_testCV_representations(representationDT = list_one_randomOD[[11]])
  randomOD_12_test <- get_testCV_representations(representationDT = list_one_randomOD[[12]])
  randomOD_13_test <- get_testCV_representations(representationDT = list_one_randomOD[[13]])
  randomOD_14_test <- get_testCV_representations(representationDT = list_one_randomOD[[14]])
  randomOD_15_test <- get_testCV_representations(representationDT = list_one_randomOD[[15]])
  
  
  
  get_scores_CV_per_randomOD <- function(randomOD_train, randomOD_test, feature_col) {
    
    iters <- 0
    res_CV <- list()
    for(ij in 1:10){
      
      trainDT <- randomOD_train[[ij]]
      outliers_train_DT <- copy(trainDT[Label == "yes"])
      CVtrain_DT <- copy(trainDT[Label == "no"])
      CVtest_DT1 <- randomOD_test[[ij]]
      
      if(CVtest_DT1[Label=="yes", length(id)] <= 1){
        CVtest_DT <- rbindlist(list(outliers_train_DT[1:3], CVtest_DT1))
        CVtest_DT <- na.omit(CVtest_DT)
        CVtest_DT <- unique(CVtest_DT)
      }else{CVtest_DT <- CVtest_DT1}
      print(CVtest_DT[, .N, by = Label])
      
      CVtest_id_final <- CVtest_DT$id
      CVtest_Label_final <- CVtest_DT$Label
      
      CVtrain_DT[, `:=` (id = NULL, Label = NULL)]
      CVtest_DT[, `:=` (id = NULL, Label = NULL)]
      
      print("15 MURs")
      print(paste0("Kfold: ", ij))
      print(paste0("Representation: ", feature_col))
      
      for(kernels in c("linear", "rbf", "sigmoid")){
        for(nus in c(0.001, 0.005, 0.01, 0.05, 0.1)){
          for(gammas in c(1/dim(CVtrain_DT)[2], 0.01, 0.05, 0.1, 0.2)){
            iters <- iters+1
            
            scores_CV <- calculate_OCSVM_params(DTtrain = CVtrain_DT, DTtest = CVtest_DT, 
                                                given_nu = nus, given_kernel = kernels, given_gamma = gammas)
            DT_CV <- data.table(scores = scores_CV, 
                                id = CVtest_id_final,
                                Label = CVtest_Label_final)
            DT_CV[, Kfold:=ij]
            DT_CV[, gamma := gammas]
            DT_CV[, nu := nus]
            DT_CV[, kernel := kernels]
            DT_CV[, Representation_col := feature_col]
            res_CV[[iters]] <- DT_CV
          }
        }
      }
    }
    # final_DT_CV <- rbindlist(res_CV)
    return(res_CV)
  }
  
  representation1 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_1_train, randomOD_test = randomOD_1_test, feature_col = "Representation_1"))
  representation2 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_2_train, randomOD_test = randomOD_2_test, feature_col = "Representation_2"))  
  representation3 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_3_train, randomOD_test = randomOD_3_test, feature_col = "Representation_3"))  
  representation4 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_4_train, randomOD_test = randomOD_4_test, feature_col = "Representation_4"))  
  representation5 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_5_train, randomOD_test = randomOD_5_test, feature_col = "Representation_5"))
  representation6 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_6_train, randomOD_test = randomOD_6_test, feature_col = "Representation_6"))
  representation7 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_7_train, randomOD_test = randomOD_7_test, feature_col = "Representation_7"))  
  representation8 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_8_train, randomOD_test = randomOD_8_test, feature_col = "Representation_8"))  
  representation9 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_9_train, randomOD_test = randomOD_9_test, feature_col = "Representation_9"))  
  representation10 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_10_train, randomOD_test = randomOD_10_test, feature_col = "Representation_10"))
  representation11 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_11_train, randomOD_test = randomOD_11_test, feature_col = "Representation_11"))
  representation12 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_12_train, randomOD_test = randomOD_12_test, feature_col = "Representation_12"))
  representation13 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_13_train, randomOD_test = randomOD_13_test, feature_col = "Representation_13"))
  representation14 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_14_train, randomOD_test = randomOD_14_test, feature_col = "Representation_14"))
  representation15 <- rbindlist(get_scores_CV_per_randomOD(randomOD_train = randomOD_15_train, randomOD_test = randomOD_15_test, feature_col = "Representation_15"))
  
  representationsDT <- rbindlist(list(representation1, representation2, 
                                      representation3, representation4, 
                                      representation5,
                                      representation6,representation6,
                                      representation7,representation8,
                                      representation9,representation10,
                                      representation11,representation12,
                                      representation13,representation14,
                                      representation15)
  )
  gc()
  representationsDT[, Model:= paste0(kernel, "_", gamma, "_", nu)]
  aucCV_representations <- representationsDT[, auc(Label, scores), by = .(Representation_col, Kfold, Model)]
  mean_aucCV_representations <- aucCV_representations[, mean(V1), by = .(Representation_col, Model)]
  best_hyper_CV <- mean_aucCV_representations[, .SD[which.max(V1)], by = .(Representation_col)]
  
  # Train with the best hyperparameter all the 80% data  
  list_testDT <- list()
  for(jj in 1:length(list_one_randomOD)){
    
    best_hyper_representation <- best_hyper_CV[Representation_col == paste0("Representation_", jj)][, stringr::str_split(Model, pattern = "_")[[1]]]
    
    representation_train_DT <- list_one_randomOD[[jj]][id %in% list_train_id[[1]]][Label == "no"]
    representation_train_DT[, .N, by = Label]
    
    representation_test_DT <- list_one_randomOD[[jj]][id %in% list_test_id[[1]]]
    representation_test_DT[, .N, by = Label]
    if(representation_test_DT[Label=="yes", length(id)] == 0){
      representation_test_DT <- rbindlist(list(representation_train_DT[Label == "yes"][1:3], representation_test_DT))
      representation_test_DT <- na.omit(representation_test_DT)
    }else{representation_test_DT <- representation_test_DT}
    
    ids_labels <- representation_test_DT[, .(id, Label)]
    representation_train_DT[, `:=` (id = NULL, Label = NULL)]
    representation_test_DT[, `:=` (id = NULL, Label = NULL)]
    
    scores <- calculate_OCSVM_params(DTtrain = representation_train_DT, DTtest = representation_test_DT, 
                                     given_kernel = best_hyper_representation[1], 
                                     given_gamma = as.numeric(best_hyper_representation[2]),
                                     given_nu = as.numeric(best_hyper_representation[3]))
    
    
    testDT <- data.table(scores = scores, 
                         Label = ids_labels$Label, 
                         id = ids_labels$id, representation = jj)
    list_testDT[[jj]] <- testDT
    rm(testDT)
  }
  
  all_representationsDT <- rbindlist(list_testDT)
  
  # average_scores_testDT <- all_representationsDT[, mean(scores), by = id]
  # average_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_auc <- auc(average_scores_testDT$Label, average_scores_testDT$V1)
  # 
  # max_scores_testDT <- all_representationsDT[, max(scores), by = id]
  # max_scores_testDT[, Label:= list_testDT[[1]][, Label]]
  # ensemble_max_auc <- auc(max_scores_testDT$Label, max_scores_testDT$V1)
  gc()
  
  return(all_representationsDT)
}






