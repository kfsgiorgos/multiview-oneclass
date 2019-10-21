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

arg1 <- "Shuttle_withoutdupl_norm_v01"
arg2 <- "Shuttle"
arg3 <- 2
arg4 <- 2
list_res_1 <- list()
for( k in 1:as.numeric(arg3)){
  print(Sys.time())
  list_res_1[[k]] <- get_OCSVM_augmented_ensemble_21MUR(datasetname = arg1, 
                                                       experiments = "OC_combined_CV", 
                                                       CViterations = as.numeric(arg4),
                                                       print_k = k)
  print(list_res_1[[k]])
}

gc()


MUR21_scores <- rbindlist(list_res_1[1:4])
fwrite(MUR21_scores, paste0(final_path_to_save, "figures/",
                            arg2, "/", arg1, "_OCSVM_Multiple_Repres_Scores_Augmented", arg3,"_iters.csv"))
Original_scores <- rbindlist(list_res_1[1:4])
fwrite(Original_scores, paste0(final_path_to_save, "figures/",
                               arg2, "/", arg1, "_OCSVM_Original_Scores_Augmented", arg3,"_iters.csv"))

list_MUR_5 <- list()
for( k in 1:as.numeric(arg3)){
  print(Sys.time())
  list_MUR_5[[k]] <- get_OCSVM_augmented_ensemble_5MUR(datasetname = arg1,
                                                       experiments = "OC_combined_CV",
                                                       CViterations = as.numeric(arg4), 
                                                       print_k = k)
  }

list_MUR_10 <- list()
for( k in 1:as.numeric(arg3)){
  print(Sys.time())
  list_MUR_10[[k]] <- get_OCSVM_augmented_ensemble_10MUR(datasetname = arg1,
                                                         experiments = "OC_combined_CV",
                                                         CViterations = as.numeric(arg4),
                                                         print_k = k)
}
list_MUR_15 <- list()
for( k in 1:as.numeric(arg3)){
  print(Sys.time())
  list_MUR_15[[k]] <- get_OCSVM_augmented_ensemble_15MUR(datasetname = arg1,
                                                         experiments = "OC_combined_CV",
                                                         CViterations = as.numeric(arg4),
                                                         print_k = k)
}



# Evaluation - Multiple Representations  ----------------------------------------

get_evaluation_augmented_MUR <- function(list_results, input_name_col) {
  
  list_res <- list_results
  
  list_metrics <- list()
  for(ij in 1:arg3){
    print(ij)
    dcasted_representations <- dcast.data.table(list_res[[ij]], id+Label~representation, value.var = "scores")
    norm_dcasted_representations <- dcasted_representations[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SD = 3:dim(dcasted_representations)[2]]
    
    average_representationsDT <- as.data.table(rowMeans(norm_dcasted_representations))
    average_representationsDT[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
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
    
    
    weighted_eval <- average_representationsDT[, HMeasure(true.class = Label, scores = V1)]
    weighted_evalDT <- as.data.table(weighted_eval$metrics)
    wighted_metrics <- weighted_evalDT[1, .(H, Gini, AUC)]
    wighted_metrics[, R_Prec:= R_precision_representation]
    wighted_metrics[, Average_Prec:= AP_representation]
    wighted_metrics[, Representation:= "Multiple_Representations"]
    
    metricsDT1 <- copy(wighted_metrics)
    metricsDT1[, Iteration:= ij]
    list_metrics[[ij]] <- metricsDT1
  }
  
  DT_metrics_Representations_ensemble <- rbindlist(list_metrics)
  DT_metrics_Representations_ensemble[, Ensemble:= "Average Combined Representations"]
  
  
  all_metricsDT <- copy(DT_metrics_Representations_ensemble)
  all_metricsDT[, MUR:= input_name_col]
  return(all_metricsDT)
  
}


MUR_21 <- get_evaluation_augmented_MUR(list_results = list_res_1, 
                                       input_name_col = "21")
MUR_15 <- get_evaluation_augmented_MUR(list_results = list_MUR_15, 
                                       input_name_col =  "15")
MUR_10 <- get_evaluation_augmented_MUR(list_results = list_MUR_10, 
                                       input_name_col = "10")
MUR_5 <- get_evaluation_augmented_MUR(list_results = list_MUR_5, 
                                      input_name_col = "5")

all_MUR <- rbindlist(list(MUR_21, MUR_15, MUR_10, MUR_5))

fwrite(all_MUR, paste0(final_path_to_save, "figures/",
                       arg2, "/", arg1, "_OCSVM_Multiple_Repres_allMetrics_Augmented", arg3,"_iters.csv"))

