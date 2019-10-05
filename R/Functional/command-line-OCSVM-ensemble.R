source("~/GitHub_projects/multiview-oneclass/src.R")
# source("~/R Language Default Dir/Github-projects/multiview-oneclass/src.R")
args <- commandArgs(TRUE)
arg1 <- args[1]
arg2 <- args[2]
arg3 <- args[3]
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
# arg1 <- "Shuttle_withoutdupl_norm_v01"
# arg2 <- "Shuttle"
# 
# arg1 <- "Cardiotocography_withoutdupl_norm_02_v02"
# arg2 <- "Cardio"
# 
# arg3 <- 10

 list_res <- list()
 for( k in 1:as.numeric(arg3)){
   print(Sys.time())
   list_res[[k]] <- get_CV_experiments_paper_ensemble(datasetname = arg1, 
                                                      experiments = "OC_combined_CV", 
                                                      CViterations = 10)
   print(list_res[[k]])
 }

#list_res <- list()
#for( k in 1:as.numeric(arg3)){
#  print(Sys.time())
#  list_res[[k]] <- get_CV_experiments_paper_ensemble_iForest(datasetname = arg1, 
#                                                             experiments = "OC_combined_CV", 
#                                                             CViterations = 5)
#  print(list_res[[k]])
#}


ensembleDT_average <- data.table(V1 = unlist(purrr::map(list_res, 1)), Representation = "Ensemble-Multiple Represenations")
ensembleDT_average[, mean(V1)]
ensembleDT_maximum <- data.table(V1 = unlist(purrr::map(list_res, 2)), Representation = "Ensemble-Multiple Represenations")
ensembleDT_maximum[, mean(V1)]


# fwrite(ensembleDT_average, paste0(final_path_to_save, "figures/",  
#                             arg2, "/", arg1, "_OCSVM_Ensemble_21_Multiple_Repres_average", arg3,"_iters.csv"))
# 
# 
# fwrite(ensembleDT_maximum, paste0(final_path_to_save, "figures/",  
#                                   arg2, "/", arg1, "_OCSVM_Ensemble_21_Multiple_Repres_maximum", arg3,"_iters.csv"))

# 0.5 weights -------------------------------------------------------------
list_metrics <- list()
for(ij in 1:arg3){
  print(ij)
  dcasted_representations <- dcast.data.table(list_res[[ij]][[3]], id+Label~representation, value.var = "scores")
  dcasted_original <- dcast.data.table(list_res[[ij]][[4]], id+Label~representation, value.var = "scores") 
  dcasted_representations[, Original:= dcasted_original$Original]

  norm_dcasted_representations <- dcasted_representations[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SD = 3:24]
  norm_dcasted_representations[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
  
  
  DT_list <- list()
  for(i in 1:30){
    sample_representations <- norm_dcasted_representations[, .SD, .SDcols = sample(x = 1:21, size = 5, replace = F)]
    setnames(sample_representations, old = names(sample_representations), 
             new = c("representation_1", "representation_2", "representation_3","representation_4", "representation_5"))
    sample_representations[, Original:=norm_dcasted_representations$Original]
    sample_representations[, weighted_new := (0.5 * Original) +(0.1 * representation_1) + (0.1 * representation_2) + (0.1 * representation_3)+ (0.1 * representation_4)+ (0.1 * representation_5)]
    sample_representations[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
    DT_list[[i]] <- sample_representations[, .SD, .SDcols = 6:9]
    rm(sample_representations)
  }
  
  
  DT_all <- rbindlist(DT_list)
  mean_DT <- DT_all[, mean(weighted_new), by = id]
  mean_DT[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]

  # Calculate evaluation metrics
  Original_scoresDT <- as.data.table(norm_dcasted_representations[, Original])
  scoresDT <- as.data.table(bind_cols(c(Original_scoresDT, mean_DT)))
  setnames(x = scoresDT, old = "V1", "Original")
  setnames(x = scoresDT, old = "V11", "Weighted")
  original_eval <- scoresDT[, HMeasure(true.class = Label, scores = Original)]
  original_evalDT <- as.data.table(original_eval$metrics)
  original_metrics <- original_evalDT[1, .(H, Gini, AUC, Spec.Sens95, Sens.Spec95)]
  original_metrics[, Representation:= "Original"]
  
  weighted_eval <- scoresDT[, HMeasure(true.class = Label, scores = Weighted)]
  weighted_evalDT <- as.data.table(weighted_eval$metrics)
  wighted_metrics <- weighted_evalDT[1, .(H, Gini, AUC, Spec.Sens95, Sens.Spec95)]
  wighted_metrics[, Representation:= "Multiple_Representations"]
  
  metricsDT <- rbindlist(list(original_metrics, wighted_metrics))
  metricsDT[, Iteration:= ij]
  list_metrics[[ij]] <- metricsDT
}

DT_metrics_05 <- rbindlist(list_metrics)
DT_metrics_05[, Ensemble:= "Weighted_50%"]

# H dataset
H_DT_05 <- melt.data.table(DT_metrics_05, id.vars = c("Representation", "Ensemble"), measure.vars = "H")
original_H_DT_05 <- H_DT_05[Representation == "Original"]
H_DT_05[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_H_DT_05$value))/sd(original_H_DT_05$value)]
# Gini dataset
Gini_DT_05 <- melt.data.table(DT_metrics_05, id.vars = c("Representation", "Ensemble"), measure.vars = "Gini")
original_Gini_DT_05 <- Gini_DT_05[Representation == "Original"]
Gini_DT_05[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Gini_DT_05$value))/sd(original_Gini_DT_05$value)]
# AUC dataset
AUC_DT_05 <- melt.data.table(DT_metrics_05, id.vars = c("Representation", "Ensemble"), measure.vars = "AUC")
original_AUC_DT_05 <- AUC_DT_05[Representation == "Original"]
AUC_DT_05[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_AUC_DT_05$value))/sd(original_AUC_DT_05$value)]
# Specificity dataset
Spec_Sens95_DT_05 <- melt.data.table(DT_metrics_05, id.vars = c("Representation", "Ensemble"), measure.vars = "Spec.Sens95")
original_Spec_Sens95_DT_05 <- Spec_Sens95_DT_05[Representation == "Original"]
Spec_Sens95_DT_05[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Spec_Sens95_DT_05$value))/sd(original_Spec_Sens95_DT_05$value)]
# Sensitivity dataset
Sens_Spec95_DT_05 <- melt.data.table(DT_metrics_05, id.vars = c("Representation", "Ensemble"), measure.vars = "Sens.Spec95")
original_Sens_Spec95_DT_05 <- Sens_Spec95_DT_05[Representation == "Original"]
Sens_Spec95_DT_05[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Sens_Spec95_DT_05$value))/sd(original_Sens_Spec95_DT_05$value)]



# 0.6 weights -------------------------------------------------------------
list_metrics <- list()
for(ij in 1:arg3){
  print(ij)
  dcasted_representations <- dcast.data.table(list_res[[ij]][[3]], id+Label~representation, value.var = "scores")
  dcasted_original <- dcast.data.table(list_res[[ij]][[4]], id+Label~representation, value.var = "scores") 
  dcasted_representations[, Original:= dcasted_original$Original]
  
  norm_dcasted_representations <- dcasted_representations[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SD = 3:24]
  norm_dcasted_representations[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
  
  
  DT_list <- list()
  for(i in 1:30){
    sample_representations <- norm_dcasted_representations[, .SD, .SDcols = sample(x = 1:21, size = 4, replace = F)]
    setnames(sample_representations, old = names(sample_representations), 
             new = c("representation_1", "representation_2", "representation_3","representation_4"))
    sample_representations[, Original:=norm_dcasted_representations$Original]
    sample_representations[, weighted_new := (0.5 * Original) +(0.1 * representation_1) + (0.1 * representation_2) + (0.1 * representation_3)+ (0.1 * representation_4)]
    sample_representations[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
    DT_list[[i]] <- sample_representations[, .SD, .SDcols = 5:8]
    rm(sample_representations)
  }
  
  
  DT_all <- rbindlist(DT_list)
  mean_DT <- DT_all[, mean(weighted_new), by = id]
  mean_DT[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
  
  # Calculate evaluation metrics
  Original_scoresDT <- as.data.table(norm_dcasted_representations[, Original])
  scoresDT <- as.data.table(bind_cols(c(Original_scoresDT, mean_DT)))
  setnames(x = scoresDT, old = "V1", "Original")
  setnames(x = scoresDT, old = "V11", "Weighted")
  original_eval <- scoresDT[, HMeasure(true.class = Label, scores = Original)]
  original_evalDT <- as.data.table(original_eval$metrics)
  original_metrics <- original_evalDT[1, .(H, Gini, AUC, Spec.Sens95, Sens.Spec95)]
  original_metrics[, Representation:= "Original"]
  
  weighted_eval <- scoresDT[, HMeasure(true.class = Label, scores = Weighted)]
  weighted_evalDT <- as.data.table(weighted_eval$metrics)
  wighted_metrics <- weighted_evalDT[1, .(H, Gini, AUC, Spec.Sens95, Sens.Spec95)]
  wighted_metrics[, Representation:= "Multiple_Representations"]
  
  metricsDT <- rbindlist(list(original_metrics, wighted_metrics))
  metricsDT[, Iteration:= ij]
  list_metrics[[ij]] <- metricsDT
}

DT_metrics_06 <- rbindlist(list_metrics)
DT_metrics_06[, Ensemble:= "Weighted_60%"]

# H dataset
H_DT_06 <- melt.data.table(DT_metrics_06, id.vars = c("Representation", "Ensemble"), measure.vars = "H")
original_H_DT_06 <- H_DT_06[Representation == "Original"]
H_DT_06[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_H_DT_06$value))/sd(original_H_DT_06$value)]
# Gini dataset
Gini_DT_06 <- melt.data.table(DT_metrics_06, id.vars = c("Representation", "Ensemble"), measure.vars = "Gini")
original_Gini_DT_06 <- Gini_DT_06[Representation == "Original"]
Gini_DT_06[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Gini_DT_06$value))/sd(original_Gini_DT_06$value)]
# AUC dataset
AUC_DT_06 <- melt.data.table(DT_metrics_06, id.vars = c("Representation", "Ensemble"), measure.vars = "AUC")
original_AUC_DT_06 <- AUC_DT_06[Representation == "Original"]
AUC_DT_06[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_AUC_DT_06$value))/sd(original_AUC_DT_06$value)]
# Specificity dataset
Spec_Sens95_DT_06 <- melt.data.table(DT_metrics_06, id.vars = c("Representation", "Ensemble"), measure.vars = "Spec.Sens95")
original_Spec_Sens95_DT_06 <- Spec_Sens95_DT_06[Representation == "Original"]
Spec_Sens95_DT_06[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Spec_Sens95_DT_06$value))/sd(original_Spec_Sens95_DT_06$value)]
# Sensitivity dataset
Sens_Spec95_DT_06 <- melt.data.table(DT_metrics_06, id.vars = c("Representation", "Ensemble"), measure.vars = "Sens.Spec95")
original_Sens_Spec95_DT_06 <- Sens_Spec95_DT_06[Representation == "Original"]
Sens_Spec95_DT_06[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Sens_Spec95_DT_06$value))/sd(original_Sens_Spec95_DT_06$value)]





# 0.7 weights -------------------------------------------------------------

list_metrics <- list()
for(ij in 1:arg3){
  print(ij)
  dcasted_representations <- dcast.data.table(list_res[[ij]][[3]], id+Label~representation, value.var = "scores")
  dcasted_original <- dcast.data.table(list_res[[ij]][[4]], id+Label~representation, value.var = "scores") 
  dcasted_representations[, Original:= dcasted_original$Original]
  
  norm_dcasted_representations <- dcasted_representations[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SD = 3:24]
  norm_dcasted_representations[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
  
  
  DT_list <- list()
  for(i in 1:30){
    sample_representations <- norm_dcasted_representations[, .SD, .SDcols = sample(x = 1:21, size = 3, replace = F)]
    setnames(sample_representations, old = names(sample_representations), 
             new = c("representation_1", "representation_2", "representation_3"))
    sample_representations[, Original:=norm_dcasted_representations$Original]
    sample_representations[, weighted_new := (0.5 * Original) +(0.1 * representation_1) + (0.1 * representation_2) + (0.1 * representation_3)]
    sample_representations[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
    DT_list[[i]] <- sample_representations[, .SD, .SDcols = 4:7]
    rm(sample_representations)
  }
  
  
  DT_all <- rbindlist(DT_list)
  mean_DT <- DT_all[, mean(weighted_new), by = id]
  mean_DT[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
  
  # Calculate evaluation metrics
  Original_scoresDT <- as.data.table(norm_dcasted_representations[, Original])
  scoresDT <- as.data.table(bind_cols(c(Original_scoresDT, mean_DT)))
  setnames(x = scoresDT, old = "V1", "Original")
  setnames(x = scoresDT, old = "V11", "Weighted")
  original_eval <- scoresDT[, HMeasure(true.class = Label, scores = Original)]
  original_evalDT <- as.data.table(original_eval$metrics)
  original_metrics <- original_evalDT[1, .(H, Gini, AUC, Spec.Sens95, Sens.Spec95)]
  original_metrics[, Representation:= "Original"]
  
  weighted_eval <- scoresDT[, HMeasure(true.class = Label, scores = Weighted)]
  weighted_evalDT <- as.data.table(weighted_eval$metrics)
  wighted_metrics <- weighted_evalDT[1, .(H, Gini, AUC, Spec.Sens95, Sens.Spec95)]
  wighted_metrics[, Representation:= "Multiple_Representations"]
  
  metricsDT <- rbindlist(list(original_metrics, wighted_metrics))
  metricsDT[, Iteration:= ij]
  list_metrics[[ij]] <- metricsDT
}

DT_metrics_07 <- rbindlist(list_metrics)
DT_metrics_07[, Ensemble:= "Weighted_70%"]

# H dataset
H_DT_07 <- melt.data.table(DT_metrics_07, id.vars = c("Representation", "Ensemble"), measure.vars = "H")
original_H_DT_07 <- H_DT_07[Representation == "Original"]
H_DT_07[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_H_DT_07$value))/sd(original_H_DT_07$value)]
# Gini dataset
Gini_DT_07 <- melt.data.table(DT_metrics_07, id.vars = c("Representation", "Ensemble"), measure.vars = "Gini")
original_Gini_DT_07 <- Gini_DT_07[Representation == "Original"]
Gini_DT_07[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Gini_DT_07$value))/sd(original_Gini_DT_07$value)]
# AUC dataset
AUC_DT_07 <- melt.data.table(DT_metrics_07, id.vars = c("Representation", "Ensemble"), measure.vars = "AUC")
original_AUC_DT_07 <- AUC_DT_07[Representation == "Original"]
AUC_DT_07[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_AUC_DT_07$value))/sd(original_AUC_DT_07$value)]
# Specificity dataset
Spec_Sens95_DT_07 <- melt.data.table(DT_metrics_07, id.vars = c("Representation", "Ensemble"), measure.vars = "Spec.Sens95")
original_Spec_Sens95_DT_07 <- Spec_Sens95_DT_07[Representation == "Original"]
Spec_Sens95_DT_07[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Spec_Sens95_DT_07$value))/sd(original_Spec_Sens95_DT_07$value)]
# Sensitivity dataset
Sens_Spec95_DT_07 <- melt.data.table(DT_metrics_07, id.vars = c("Representation", "Ensemble"), measure.vars = "Sens.Spec95")
original_Sens_Spec95_DT_07 <- Sens_Spec95_DT_07[Representation == "Original"]
Sens_Spec95_DT_07[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Sens_Spec95_DT_07$value))/sd(original_Sens_Spec95_DT_07$value)]



# Only - Multiple Representations  ----------------------------------------

list_metrics <- list()
for(ij in 1:arg3){
  print(ij)
  dcasted_representations <- dcast.data.table(list_res[[ij]][[3]], id+Label~representation, value.var = "scores")
  dcasted_original <- dcast.data.table(list_res[[ij]][[4]], id+Label~representation, value.var = "scores") 
  dcasted_representations[, Original:= dcasted_original$Original]
  
  norm_dcasted_representations <- dcasted_representations[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SD = 3:23]
  average_representationsDT <- as.data.table(rowMeans(norm_dcasted_representations))
  average_representationsDT[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
  average_representationsDT[, Original:= dcasted_original$Original]
  
  
  # Calculate evaluation metrics

  original_eval <- average_representationsDT[, HMeasure(true.class = Label, scores = Original)]
  original_evalDT <- as.data.table(original_eval$metrics)
  original_metrics <- original_evalDT[1, .(H, Gini, AUC, Spec.Sens95, Sens.Spec95)]
  original_metrics[, Representation:= "Original"]
  
  weighted_eval <- average_representationsDT[, HMeasure(true.class = Label, scores = V1)]
  weighted_evalDT <- as.data.table(weighted_eval$metrics)
  wighted_metrics <- weighted_evalDT[1, .(H, Gini, AUC, Spec.Sens95, Sens.Spec95)]
  wighted_metrics[, Representation:= "Multiple_Representations"]
  
  metricsDT <- rbindlist(list(original_metrics, wighted_metrics))
  metricsDT[, Iteration:= ij]
  list_metrics[[ij]] <- metricsDT
}

DT_metrics_Representations_ensemble <- rbindlist(list_metrics)
DT_metrics_Representations_ensemble[, Ensemble:= "Average Representations"]

# H dataset
H_DT_ensemble <- melt.data.table(DT_metrics_Representations_ensemble, id.vars = c("Representation", "Ensemble"), measure.vars = "H")
original_H_DT_ensemble <- H_DT_ensemble[Representation == "Original"]
H_DT_ensemble[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_H_DT_ensemble$value))/sd(original_H_DT_ensemble$value)]
# Gini dataset
Gini_DT_ensemble <- melt.data.table(DT_metrics_Representations_ensemble, id.vars = c("Representation", "Ensemble"), measure.vars = "Gini")
original_Gini_DT_ensemble <- Gini_DT_ensemble[Representation == "Original"]
Gini_DT_ensemble[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Gini_DT_ensemble$value))/sd(original_Gini_DT_ensemble$value)]
# AUC dataset
AUC_DT_ensemble <- melt.data.table(DT_metrics_Representations_ensemble, id.vars = c("Representation", "Ensemble"), measure.vars = "AUC")
original_AUC_DT_ensemble <- AUC_DT_ensemble[Representation == "Original"]
AUC_DT_ensemble[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_AUC_DT_ensemble$value))/sd(original_AUC_DT_ensemble$value)]
# Specificity dataset
Spec_Sens95_DT_ensemble <- melt.data.table(DT_metrics_Representations_ensemble, id.vars = c("Representation", "Ensemble"), measure.vars = "Spec.Sens95")
original_Spec_Sens95_DT_ensemble <- Spec_Sens95_DT_ensemble[Representation == "Original"]
Spec_Sens95_DT_ensemble[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Spec_Sens95_DT_ensemble$value))/sd(original_Spec_Sens95_DT_ensemble$value)]
# Sensitivity dataset
Sens_Spec95_DT_ensemble <- melt.data.table(DT_metrics_Representations_ensemble, id.vars = c("Representation", "Ensemble"), measure.vars = "Sens.Spec95")
original_Sens_Spec95_DT_ensemble <- Sens_Spec95_DT_ensemble[Representation == "Original"]
Sens_Spec95_DT_ensemble[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Sens_Spec95_DT_ensemble$value))/sd(original_Sens_Spec95_DT_ensemble$value)]




# 50% Original + 50% §Ensemble of Representations --------------------

list_metrics <- list()
for(ij in 1:arg3){
  print(ij)
  dcasted_representations <- dcast.data.table(list_res[[ij]][[3]], id+Label~representation, value.var = "scores")
  dcasted_original <- dcast.data.table(list_res[[ij]][[4]], id+Label~representation, value.var = "scores") 
  dcasted_representations[, Original:= dcasted_original$Original]
  
  norm_dcasted_representations <- dcasted_representations[, lapply(.SD, function(x) (x - mean(x))/sd(x)), .SD = 3:23]
  average_representationsDT <- as.data.table(rowMeans(norm_dcasted_representations))
  average_representationsDT[, Original:= dcasted_original$Original]
  average_ensembleDT <- as.data.table(rowMeans(average_representationsDT))
  average_ensembleDT[, `:=` (id = dcasted_representations$id, Label = dcasted_representations$Label)]
  average_ensembleDT[, Original:= dcasted_original$Original]
  
  # Calculate evaluation metrics
  
  original_eval <- average_ensembleDT[, HMeasure(true.class = Label, scores = Original)]
  original_evalDT <- as.data.table(original_eval$metrics)
  original_metrics <- original_evalDT[1, .(H, Gini, AUC, Spec.Sens95, Sens.Spec95)]
  original_metrics[, Representation:= "Original"]
  
  weighted_eval <- average_ensembleDT[, HMeasure(true.class = Label, scores = V1)]
  weighted_evalDT <- as.data.table(weighted_eval$metrics)
  wighted_metrics <- weighted_evalDT[1, .(H, Gini, AUC, Spec.Sens95, Sens.Spec95)]
  wighted_metrics[, Representation:= "Multiple_Representations"]
  
  metricsDT <- rbindlist(list(original_metrics, wighted_metrics))
  metricsDT[, Iteration:= ij]
  list_metrics[[ij]] <- metricsDT
}

DT_metrics_ensemble <- rbindlist(list_metrics)
DT_metrics_ensemble[, Ensemble:= "Average Ensemble"]

# H dataset
H_DT_average_ensemble <- melt.data.table(DT_metrics_ensemble, id.vars = c("Representation", "Ensemble"), measure.vars = "H")
original_H_DT_average_ensemble <- H_DT_average_ensemble[Representation == "Original"]
H_DT_average_ensemble[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_H_DT_average_ensemble$value))/sd(original_H_DT_average_ensemble$value)]
# Gini dataset
Gini_DT_average_ensemble <- melt.data.table(DT_metrics_ensemble, id.vars = c("Representation", "Ensemble"), measure.vars = "Gini")
original_Gini_DT_average_ensemble <- Gini_DT_average_ensemble[Representation == "Original"]
Gini_DT_average_ensemble[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Gini_DT_average_ensemble$value))/sd(original_Gini_DT_average_ensemble$value)]
# AUC dataset
AUC_DT_average_ensemble <- melt.data.table(DT_metrics_ensemble, id.vars = c("Representation", "Ensemble"), measure.vars = "AUC")
original_AUC_DT_average_ensemble <- AUC_DT_average_ensemble[Representation == "Original"]
AUC_DT_average_ensemble[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_AUC_DT_average_ensemble$value))/sd(original_AUC_DT_average_ensemble$value)]
# Specificity dataset
Spec_Sens95_DT_average_ensemble <- melt.data.table(DT_metrics_ensemble, id.vars = c("Representation", "Ensemble"), measure.vars = "Spec.Sens95")
original_Spec_Sens95_DT_average_ensemble <- Spec_Sens95_DT_average_ensemble[Representation == "Original"]
Spec_Sens95_DT_average_ensemble[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Spec_Sens95_DT_average_ensemble$value))/sd(original_Spec_Sens95_DT_average_ensemble$value)]
# Sensitivity dataset
Sens_Spec95_DT_average_ensemble <- melt.data.table(DT_metrics_ensemble, id.vars = c("Representation", "Ensemble"), measure.vars = "Sens.Spec95")
original_Sens_Spec95_DT_average_ensemble <- Sens_Spec95_DT_average_ensemble[Representation == "Original"]
Sens_Spec95_DT_average_ensemble[Representation == "Multiple_Representations", standardized_value:= (value - mean(original_Sens_Spec95_DT_average_ensemble$value))/sd(original_Sens_Spec95_DT_average_ensemble$value)]






# combine all DTs ---------------------------------------------------------

HmeasureDT <- rbindlist(list(H_DT_05, H_DT_06, H_DT_07, H_DT_ensemble, H_DT_average_ensemble))
HmeasureDT[is.na(standardized_value), standardized_value:=0]
HmeasureDT1 <- rbindlist(list(HmeasureDT[Representation != "Original"], 
                              HmeasureDT[Representation == "Original" & Ensemble == "Weighted_50%"]))
HmeasureDT1[Representation == "Original", Ensemble:= "None"]

GiniDT <- rbindlist(list(Gini_DT_05, Gini_DT_06, Gini_DT_07, Gini_DT_ensemble, Gini_DT_average_ensemble))
GiniDT[is.na(standardized_value), standardized_value:=0]
GiniDT1 <- rbindlist(list(GiniDT[Representation != "Original"], 
                          GiniDT[Representation == "Original" & Ensemble == "Weighted_50%"]))
GiniDT1[Representation == "Original", Ensemble:= "None"]


AUCDT <- rbindlist(list(AUC_DT_05, AUC_DT_06, AUC_DT_07, AUC_DT_ensemble, AUC_DT_average_ensemble))
AUCDT[is.na(standardized_value), standardized_value:=0]
AUCDT1 <- rbindlist(list(AUCDT[Representation != "Original"], 
                        AUCDT[Representation == "Original" & Ensemble == "Weighted_50%"]))
AUCDT1[Representation == "Original", Ensemble:= "None"]


Spec_Sens95_DT <- rbindlist(list(Spec_Sens95_DT_05, Spec_Sens95_DT_06, Spec_Sens95_DT_07, Spec_Sens95_DT_ensemble, Spec_Sens95_DT_average_ensemble))
Spec_Sens95_DT[is.na(standardized_value), standardized_value:=0]
Spec_Sens95_DT1 <- rbindlist(list(Spec_Sens95_DT[Representation != "Original"], 
                                  Spec_Sens95_DT[Representation == "Original" & Ensemble == "Weighted_50%"]))
Spec_Sens95_DT1[Representation == "Original", Ensemble:= "None"]


Sens_Spec95_DT <- rbindlist(list(Sens_Spec95_DT_05, Sens_Spec95_DT_07, Sens_Spec95_DT_07, Sens_Spec95_DT_ensemble, Sens_Spec95_DT_average_ensemble))
Sens_Spec95_DT[is.na(standardized_value), standardized_value:=0]
Sens_Spec95_DT1 <- rbindlist(list(Sens_Spec95_DT[Representation != "Original"], 
                                  Sens_Spec95_DT[Representation == "Original" & Ensemble == "Weighted_50%"]))
Sens_Spec95_DT1[Representation == "Original", Ensemble:= "None"]


all_metricsDT <- rbindlist(list(HmeasureDT1, GiniDT1, AUCDT1, Spec_Sens95_DT1, Sens_Spec95_DT1))



# fwrite(all_metricsDT, paste0(final_path_to_save, "figures/",  
#                                   arg2, "/", arg1, "_OCSVM_21_Multiple_Repres_allMetrics", arg3,"_iters.csv"))


fwrite(all_metricsDT, paste0(final_path_to_save, "figures/",  
                             arg2, "/", arg1, "_iForest_21_Multiple_Repres_allMetrics", arg3,"_iters.csv"))










