source("~/GitHub_projects/multiview-oneclass/src.R")
# source("~/R Language Default Dir/Github-projects/multiview-oneclass/src.R")
args <- commandArgs(TRUE)
arg1 <- args[1]
arg2 <- args[2]
arg3 <- args[3]
arg4 <- args[4]
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

get_evaluation_MUR <- function(list_results, col_name) {
  
  list_res <- list_results
  
  list_metrics <- list()
  for(ij in 1:arg3){
    print(ij)
    dcasted_representations <- dcast.data.table(list_res[[ij]][[3]], id+Label~representation, value.var = "scores")
    dcasted_original <- dcast.data.table(list_res[[ij]][[4]], id+Label~representation, value.var = "scores") 
    dcasted_representations[, Original:= dcasted_original$Original]
    dcasted_representations[, Original:= (Original - mean(Original))/sd(Original)]
    
    norm_dcasted_representations <- dcasted_representations[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SD = 3:dim(dcasted_representations)[2]]
    average_representationsDT <- as.data.table(rowMeans(norm_dcasted_representations))
    average_representationsDT[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
    average_representationsDT[, Original:= dcasted_original$Original]
    average_representationsDT[, rowindex:= 1:.N]
    
    DT <- copy(average_representationsDT)
    # Average precision Representations
    representation_classification <- copy(DT[order(V1, decreasing = F)])
    representation_classification[, id:= 1:.N]
    representation_classification[order(V1, decreasing = F) & Label == "yes", id]
    ids_representation <- representation_classification[order(V1, decreasing = F) & Label == "yes", id]
    
    precision_temp <- list()
    for(i in 1:length(ids_representation)){
      yes <- representation_classification[1:ids_representation[i]][, .N, by = Label][Label == "yes", N]
      no <- representation_classification[1:ids_representation[i]][, .N, by = Label][Label == "no"]
      
      if(dim(no)[1] == 0){
        no <- 0
      }else{ no <- representation_classification[1:ids_representation[i]][, .N, by = Label][Label == "no", N]}
      
      precision_temp[[i]] <- yes/(yes+no)  
    }
    AP_representation <- sum(unlist(precision_temp))/length(ids_representation) 
    
    yes_representation <- representation_classification[1:ids_representation[length(ids_representation)]][, .N, by = Label][Label == "yes", N]
    no_representation <- representation_classification[1:ids_representation[length(ids_representation)]][, .N, by = Label][Label == "no", N]
    R_precision_representation <- yes_representation/(yes_representation + no_representation)
    
    
    # Average precision Original
    original_classification <- copy(DT[order(Original, decreasing = F)])
    original_classification[, id:= 1:.N]
    original_classification[order(Original, decreasing = F) & Label == "yes", id]
    original_classification[order(V1, decreasing = F) & Label == "yes", id]
    ids_original <- original_classification[order(V1, decreasing = F) & Label == "yes", id]
    
    precision_temp_1 <- list()
    for(i in 1:length(ids_original)){
      yes <- original_classification[1:ids_original[i]][, .N, by = Label][Label == "yes", N]
      no <- original_classification[1:ids_original[i]][, .N, by = Label][Label == "no"]
      
      if(dim(no)[1] == 0){
        no <- 0
      }else{ no <- original_classification[1:ids_original[i]][, .N, by = Label][Label == "no", N]}
      
      precision_temp_1[[i]] <- yes/(yes+no)  
    }
    AP_original <- sum(unlist(precision_temp_1))/length(ids_original)
    
    yes_original <- original_classification[1:ids_original[length(ids_original)]][, .N, by = Label][Label == "yes", N]
    no_original <- original_classification[1:ids_original[length(ids_original)]][, .N, by = Label][Label == "no", N]
    R_precision_original <- yes_original/(yes_original + no_original)
    
    # Calculate evaluation metrics
    
    original_eval <- average_representationsDT[, HMeasure(true.class = Label, scores = Original)]
    original_evalDT <- as.data.table(original_eval$metrics)
    original_metrics <- original_evalDT[1, .(H, Gini, AUC)]
    original_metrics[, R_Prec:= R_precision_original]
    original_metrics[, Average_Prec:= AP_original]
    original_metrics[, Representation:= "Original"]
    
    weighted_eval <- average_representationsDT[, HMeasure(true.class = Label, scores = V1)]
    weighted_evalDT <- as.data.table(weighted_eval$metrics)
    wighted_metrics <- weighted_evalDT[1, .(H, Gini, AUC)]
    wighted_metrics[, R_Prec:= R_precision_representation]
    wighted_metrics[, Average_Prec:= AP_representation]
    wighted_metrics[, Representation:= "Multiple_Representations"]
    
    metricsDT1 <- rbindlist(list(original_metrics, wighted_metrics))
    metricsDT1[, Iteration:= ij]
    list_metrics[[ij]] <- metricsDT1
  }
  
  DT_metrics_Representations_ensemble <- rbindlist(list_metrics)
  DT_metrics_Representations_ensemble[, Ensemble:= "Average Representations"]
  
  
  # 50% Original + 50% Ensemble of Representations --------------------
  
  
  list_metrics_2 <- list()
  for(ij in 1:arg3){
    print(ij)
    dcasted_representations <- dcast.data.table(list_res[[ij]][[3]], id+Label~representation, value.var = "scores")
    dcasted_original <- dcast.data.table(list_res[[ij]][[4]], id+Label~representation, value.var = "scores") 
    dcasted_representations[, Original:= dcasted_original$Original]
    dcasted_representations[, Original:= (Original - mean(Original))/sd(Original)]
    
    norm_dcasted_representations <- dcasted_representations[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SD = 3:dim(dcasted_representations)[2]]
    average_representationsDT <- as.data.table(rowMeans(norm_dcasted_representations))
    average_representationsDT[, Original:= dcasted_representations$Original]
    average_ensembleDT <- as.data.table(rowMeans(average_representationsDT))
    average_ensembleDT[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
    # average_ensembleDT[, Original:= dcasted_original$Original]
    
    # Calculate evaluation metrics
    
    DT_1 <- copy(average_ensembleDT)
    # Average precision Representations
    ensemble_classification <- copy(DT_1[order(V1, decreasing = F)])
    ensemble_classification[, id:= 1:.N]
    ensemble_classification[order(V1, decreasing = F) & Label == "yes", id]
    ids_ensemble <- ensemble_classification[order(V1, decreasing = F) & Label == "yes", id]
    
    precision_temp_2 <- list()
    for(i in 1:length(ids_ensemble)){
      yes <- ensemble_classification[1:ids_ensemble[i]][, .N, by = Label][Label == "yes", N]
      no <- ensemble_classification[1:ids_ensemble[i]][, .N, by = Label][Label == "no"]
      
      if(dim(no)[1] == 0){
        no <- 0
      }else{ no <- ensemble_classification[1:ids_ensemble[i]][, .N, by = Label][Label == "no", N]}
      
      precision_temp_2[[i]] <- yes/(yes+no)  
    }
    
    AP_ensemble <- sum(unlist(precision_temp_2))/length(ids_ensemble) 
    
    yes_ensemble <- ensemble_classification[1:ids_ensemble[length(ids_ensemble)]][, .N, by = Label][Label == "yes", N]
    no_ensemble <- ensemble_classification[1:ids_ensemble[length(ids_ensemble)]][, .N, by = Label][Label == "no", N]
    R_precision_ensemble <- yes_ensemble/(yes_ensemble + no_ensemble)
    
    weighted_eval1 <- average_ensembleDT[, HMeasure(true.class = Label, scores = V1)]
    weighted_evalDT1 <- as.data.table(weighted_eval1$metrics)
    wighted_metrics2 <- weighted_evalDT1[1, .(H, Gini, AUC)]
    wighted_metrics2[, R_Prec:= R_precision_ensemble]
    wighted_metrics2[, Average_Prec:= AP_ensemble]
    wighted_metrics2[, Representation:= "Multiple_Representations"]
    
    metricsDT2 <- rbindlist(list(wighted_metrics2))
    metricsDT2[, Iteration:= ij]
    list_metrics_2[[ij]] <- metricsDT2
  }
  
  DT_metrics_ensemble <- rbindlist(list_metrics_2)
  DT_metrics_ensemble[, Ensemble:= "Average Ensemble"]
  
  
  all_metricsDT <- rbindlist(list(DT_metrics_Representations_ensemble, DT_metrics_ensemble))
  all_metricsDT[, MUR:= col_name]
  return(all_metricsDT)
  
}


list_MUR_5 <- list()
for( k in 1:as.numeric(arg3)){
  print(Sys.time())
  list_MUR_5[[k]] <- get_CV_experiments_paper_5_MUR_ensemble(datasetname = arg1,
                                                             experiments = "OC_combined_CV",
                                                             CViterations = as.numeric(arg4),
                                                             print_k = k, total_k = arg3)
  gc()
}
MUR_5 <- get_evaluation_MUR(list_results = list_MUR_5, col_name = "5")


list_MUR_10 <- list()
for( k in 1:as.numeric(arg3)){
  print(Sys.time())
  list_MUR_10[[k]] <- get_CV_experiments_paper_10_MUR_ensemble(datasetname = arg1,
                                                               experiments = "OC_combined_CV",
                                                               CViterations = as.numeric(arg4),
                                                               print_k = k, total_k = arg3)
  gc()
}
MUR_10 <- get_evaluation_MUR(list_results = list_MUR_10, col_name = "10")


list_MUR_15 <- list()
for( k in 1:as.numeric(arg3)){
  print(Sys.time())
  list_MUR_15[[k]] <- get_CV_experiments_paper_15_MUR_ensemble(datasetname = arg1,
                                                               experiments = "OC_combined_CV",
                                                               CViterations = as.numeric(arg4),
                                                               print_k = k, total_k = arg3)
  gc()
}
MUR_15 <- get_evaluation_MUR(list_results = list_MUR_15, col_name = "15")


list_MUR_26 <- list()
for( k in 1:as.numeric(arg3)){
  print(Sys.time())
  list_MUR_26[[k]] <- get_CV_experiments_paper_26_MUR_ensemble(datasetname = arg1,
                                                               experiments = "OC_combined_CV",
                                                               CViterations = as.numeric(arg4),
                                                               print_k = k, total_k = arg3)
  gc()
}
MUR_26 <- get_evaluation_MUR(list_results = list_MUR_26, col_name = "26")


list_MUR_31 <- list()
for( k in 1:as.numeric(arg3)){
  print(Sys.time())
  list_MUR_31[[k]] <- get_CV_experiments_paper_31_MUR_ensemble(datasetname = arg1,
                                                               experiments = "OC_combined_CV",
                                                               CViterations = as.numeric(arg4),
                                                               print_k = k, total_k = arg3)
  gc()
}
MUR_31 <- get_evaluation_MUR(list_results = list_MUR_31, col_name = "31")


all_MUR <- rbindlist(list(MUR_5, MUR_10, MUR_15, MUR_26, MUR_31))

fwrite(all_MUR, paste0(final_path_to_save, "figures/",
                      arg2, "/", arg1, "_OCSVM_Multiple_Repres_5_10_15_15_30_", arg3,"_iters.csv"))

