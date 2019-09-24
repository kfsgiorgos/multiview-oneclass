get_meta_plots <- function(subfolder_name, experiments="OC_combined_CV", file_suffix) {
  
  set.seed(191984)
  # The argument experiments can take values: i) "OC_combined", ii) "OC_combined_CV"
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
    final_path_to_save <<- paste0(paste0(path_to_save, folder_to_save))
  }
  
  
  if(experiments == "OC_combined_CV"){
    folder_to_save <- config::get("OC_CV_combined_experiments", 
                                  file = config_file_path,
                                  config = loaded_config_type)
    final_path_to_save <- paste0(paste0(path_to_save, folder_to_save))
  }
  
  
  list_files <- list.files(paste0(final_path_to_save, "figures/", subfolder_name))
  
  random1_files <- list_files[stringi::stri_detect(regex = paste0("1random", file_suffix, ".csv"), str = list_files)]
  combined_files <- list_files[stringi::stri_detect(regex = paste0("Combined", file_suffix, ".csv"), str = list_files)]
  original_files <- list_files[stringi::stri_detect(regex = paste0("Original", file_suffix, ".csv"), str = list_files)]
  
  res <- list()
  res1 <- list()
  res_original <- list()
  for(i in 1:length(random1_files)){
    
    random1_DT <- fread(paste0(final_path_to_save, "figures/", subfolder_name, "/", random1_files[i]))
    random1_max_DT <- random1_DT[, max(V2), by = .(`Cross-Validation`, features_Iteration)]
    random1_max_DT[, Representation:= "1-RandomOD"]
    random1_max_DT[, `Cross-Validation`:=NULL]
    random1_max_DT[, features_Iteration:= as.factor(as.character(features_Iteration))]
    
    combined_DT <- fread(paste0(final_path_to_save, "figures/", subfolder_name, "/", combined_files[i]))
    combined_max_DT <- combined_DT[, max(V2), by = .(`Cross-Validation`, features_Iteration)]
    combined_max_DT[, Representation:= "Combined"]
    combined_max_DT[, `Cross-Validation`:=NULL]
    combined_max_DT[, features_Iteration:= as.factor(as.character(features_Iteration))]
    
    original_DT <- fread(paste0(final_path_to_save, "figures/", subfolder_name, "/", original_files[i]))
    original_max_DT <- original_DT[, max(V2), by = .(`Cross-Validation`)]
    original_max_DT[, Representation:= "Original"]
    setnames(original_max_DT, "Cross-Validation", "features_Iteration")
    original_max_DT[, features_Iteration:=as.character(features_Iteration)]
    original_max_DT[, features_Iteration:= "Original"]
    
    DT <- rbindlist(list(random1_max_DT, combined_max_DT, original_max_DT))
    DT[, Dataset:= paste0(subfolder_name, "-", i)]
    res[[i]] <- DT
    
    random1_DT[, Representation:= "1-RandomOD"]
    combined_DT[, Representation:= "Combined"]
    DT1 <- rbindlist(list(random1_DT, combined_DT)) 
    DT1[, Dataset:= paste0(subfolder_name, "-", i)]
    res1[[i]] <- DT1
    
    
    original_DT[, Dataset:= paste0(subfolder_name, "-", i)]
    res_original[[i]] <- original_DT
    
  }
  
  dataset_DT <-  rbindlist(res)
  dataset_DT1 <-  rbindlist(res1)
  dataset_DT2 <-  rbindlist(res_original)
  return(list(dataset_DT, dataset_DT1, dataset_DT2))
}




glass_DT <- get_meta_plots(subfolder_name = "Glass")
ionosphere_DT <- get_meta_plots(subfolder_name = "Ionosphere")

DT <- rbindlist(list(glass_DT[[1]], ionosphere_DT[[1]]))

ggplot(data = DT) +
  aes(x = Dataset, y = V1, fill = Representation) +
  geom_boxplot() +
  theme_minimal()




# Experiments Model Selection ---------------------------------------------


get_latest_plot <- function(iters, subfolder, dataset_name, algorithm) {
  
  random1 <- fread(paste0("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/OC_Combined_CV/figures/", 
                          subfolder, "/", dataset_name, "_1random_", iters, "_paper_CV", algorithm,".csv"))
  combined <- fread(paste0("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/OC_Combined_CV/figures/", 
                           subfolder, "/", dataset_name, "_Combined_", iters, "_paper_CV", algorithm,".csv"))
  original <- fread(paste0("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/OC_Combined_CV/figures/", 
                           subfolder, "/", dataset_name, "_Original_", iters, "_paper_CV", algorithm,".csv"))
  
  dataset <- dataset_name
  if(algorithm == "_iForest"){
    original[, Model:= paste0(estimators, maxSamples, maxFeatures)]
  }else{
    original[, Model:= paste0(gamma, nu, kernel)]
  }
  
  original_meanCV <- original[, mean(V1), by = .(CViteration, Model)]
  original_merged <- original[original_meanCV, on = c("CViteration", "Model")]
  
  best_CV_original <- list()
  for(i in 1:iters){
    best_CV_original[[i]] <- original_merged[CViteration==i, .SD[which.max(i.V1)], by = .(Model)][order(i.V1, decreasing = T)][1:10]
  }
  best_CV_originalDT <- rbindlist(best_CV_original)
  
  best_test_original <- list()
  for(i in 1:iters){
    best_test_original[[i]] <- original_merged[Model == best_CV_originalDT[i, Model] & CViteration == i, max(V2)]
  }
  best_test_originalDT <- data.table(V1 = unlist(best_test_original),
                                     Representation = "Original_space") 
  best_test_originalDT[, Group:= "Original Space"]
  
  
  # combined 
  if(algorithm == "_iForest"){
    combined[, Model:= paste0(estimators, maxSamples, maxFeatures)]
  }else{
    combined[, Model:= paste0(gamma, nu, kernel)]
  }
  
  combined <- unique(combined)
  combined_mean_CVperformance <- combined[, mean(V1), by = .(CViteration, features_Iteration, Model)]
  merged_combined <- combined[combined_mean_CVperformance, on = c("CViteration", "features_Iteration", "Model")]
  
  list_best_test <- list()
  for(k in 1:iters){
    temp <- merged_combined[CViteration==k]
    best_CV_per_featureIter <- list()
    for(ii in 1:21){
      # maximum <- temp[features_Iteration == ii, .SD[which.max(i.V1)], by = .(Model)][order(i.V1, decreasing = T)][1]
      # temp[features_Iteration == ii & V1 == maximum$V1, .SD[which.max(V2)]]
      best_CV_per_featureIter[[ii]] <- temp[features_Iteration == ii, .SD[which.max(i.V1)], by = .(Model)][order(i.V1, decreasing = T)][1]
    }
    
    best_CV_per_featureIterDT <- rbindlist(best_CV_per_featureIter)
    
    best_test_per_best_CV_per_featureIter <- list()
    for(ij in 1:21){
      best_test_per_best_CV_per_featureIter[[ij]] <- temp[features_Iteration == ij & Model == best_CV_per_featureIterDT[ij, Model], max(V2)]
    }
    
    DT <- data.table(V1 = unlist(best_test_per_best_CV_per_featureIter))
    DT[, Cviteration := paste0("CViteration", k)]
    DT[, Representation:= paste0("Combined_Space",1:21)]
    list_best_test[[k]] <- DT
  }
  
  
  best_test_combinedDT <- rbindlist(list_best_test)
  best_test_combinedDT[, Group:= "Combined Space"]
  
  
  # random1  
  
  if(algorithm == "_iForest"){
    random1[, Model:= paste0(estimators, maxSamples, maxFeatures)]
  }else{
    random1[, Model:= paste0(gamma, nu, kernel)]
  }
  
  random1_mean_CVperformance <- random1[, mean(V1), by = .(CViteration, features_Iteration, Model)]
  merged_random1 <- random1[random1_mean_CVperformance, on = c("CViteration", "features_Iteration", "Model")]
  
  
  list_best_test <- list()
  for(k in 1:iters){
    temp <- merged_random1[CViteration==k]
    best_CV_per_featureIter <- list()
    for(ii in 1:21){
      # maximum <- temp[features_Iteration == ii, .SD[which.max(i.V1)], by = .(Model)][order(i.V1, decreasing = T)][1]
      # temp[features_Iteration == ii & V1 == maximum$V1, .SD[which.max(V2)]]
      best_CV_per_featureIter[[ii]] <- temp[features_Iteration == ii, .SD[which.max(i.V1)], by = .(Model)][order(i.V1, decreasing = T)][1]
    }
    
    best_CV_per_featureIterDT <- rbindlist(best_CV_per_featureIter)
    
    best_test_per_best_CV_per_featureIter <- list()
    for(ij in 1:21){
      best_test_per_best_CV_per_featureIter[[ij]] <- temp[features_Iteration == ij & Model == best_CV_per_featureIterDT[ij, Model], max(V2)]
    }
    
    DT <- data.table(V1 = unlist(best_test_per_best_CV_per_featureIter))
    DT[, Cviteration := paste0("CViteration", k)]
    DT[, Representation:= paste0("Random1_Space",1:21)]
    list_best_test[[k]] <- DT
  }
  
  
  best_test_random1DT <- rbindlist(list_best_test)
  best_test_random1DT[, Group:= "Orthogonal Space"]
  
  
  # rbindlist 
  best_test_combinedDT[, Cviteration:=NULL]
  best_test_random1DT[, Cviteration:=NULL]
  
  combined_original_random1 <- rbindlist(list(best_test_combinedDT, best_test_originalDT, best_test_random1DT))
  combined_original_random1[, Representation:= as.factor(Representation)]
  
  
  p1 <- ggplot(data = combined_original_random1) +
    aes(x = Representation, y = V1, fill = Group) +
    geom_boxplot() +
    theme_minimal()+
    coord_flip()+
    geom_hline(yintercept = mean(best_test_originalDT$V1))+
    labs(title = paste0(dataset, " & CViteration: ", iters))
  
  mean_sd_summary <- combined_original_random1[Representation == "Original_space", .(mean(V1), sd(V1))]
  combined_original_random1[, V3:= (V1 - mean_sd_summary$V1)/mean_sd_summary$V2, by = Representation]
  
  p2 <- ggplot(data = combined_original_random1[Group!="Original Space"]) +
    aes(x = Group, y = V3, fill = Group) +
    geom_boxplot() +
    theme_minimal()+
    coord_flip()+
    labs(title = paste0(dataset, " & CViteration: ", iters))+
    geom_hline(yintercept = 0)+
    scale_y_continuous(name="AUC Standard Deviations", breaks = seq(combined_original_random1[, round(min(V3) - 0.5)], 
                                                                    combined_original_random1[, round(max(V3) + 0.5)], 0.5))
  

  return(list(p1, p2, combined_original_random1, random1, combined, original, 
              best_test_originalDT,best_test_combinedDT, best_test_random1DT))
}

# Wilt --------------------------------------------------------------------
Wilt2 <- get_latest_plot(iters = 10, subfolder = "Wilt", dataset_name = "Wilt_withoutdupl_norm_02_v02")
Wilt5 <- get_latest_plot(iters = 10, subfolder = "Wilt", dataset_name = "Wilt_withoutdupl_norm_02_v05")
Wilt8 <- get_latest_plot(iters = 10, subfolder = "Wilt", dataset_name = "Wilt_withoutdupl_norm_02_v08")


Wilt2_30 <- get_latest_plot(iters = 30, subfolder = "Wilt", dataset_name = "Wilt_withoutdupl_norm_02_v02")
Wilt5_30 <- get_latest_plot(iters = 30, subfolder = "Wilt", dataset_name = "Wilt_withoutdupl_norm_02_v05")
Wilt8_30 <- get_latest_plot(iters = 30, subfolder = "Wilt", dataset_name = "Wilt_withoutdupl_norm_02_v08")


# Wilt <- rbindlist(list(Wilt2_30[[3]], Wilt5_30[[3]], Wilt8_30[[3]]))
Wilt <- rbindlist(list(Wilt2[[3]], Wilt5[[3]], Wilt8[[3]]))
Wilt[, median(V3), by=.(Group, Representation)][order(V1)]
Wilt <- Wilt[!(Representation %in% c("Combined_Space3", "Random1_Space18", "Combined_Space18", "Random1_Space3"))]

Wilt[, median(V3), by=.(Group, Representation)]
WiltDT <- Wilt[V3>0.5]


ggplot(data = Wilt) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Wilt[, round(min(V3) - 0.5)], 
                                                                  Wilt[, round(max(V3) + 0.5)], 0.5))

#iForest
Wilt2_30_iForest <- get_latest_plot(iters = 30, subfolder = "Wilt", dataset_name = "Wilt_withoutdupl_norm_02_v02", algorithm = "_iForest")
Wilt5_30_iForest <- get_latest_plot(iters = 30, subfolder = "Wilt", dataset_name = "Wilt_withoutdupl_norm_02_v05", algorithm = "_iForest")


Wilt_iForest <- rbindlist(list(Wilt2_30_iForest[[3]], Wilt5_30_iForest[[3]]))
Wilt_iForest <- Wilt_iForest[!(Representation %in% c("Combined_Space8", "Random1_Space13", "Combined_Space11"))]

Wilt_iForest[, median(V3), by=.(Group, Representation)]
WiltDT <- Wilt[V3>0.5]


ggplot(data = Wilt_iForest) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Wilt_iForest[, round(min(V3) - 0.5)], 
                                                                  Wilt_iForest[, round(max(V3) + 0.5)], 0.5))




# Arrhythmia --------------------------------------------------------------
Arrhythmia1 <- get_latest_plot(iters = 5, subfolder = "Arrhythmia", dataset_name = "Arrhythmia_withoutdupl_norm_02_v01")
Arrhythmia2 <- get_latest_plot(iters = 5, subfolder = "Arrhythmia", dataset_name = "Arrhythmia_withoutdupl_norm_02_v02")
Arrhythmia3 <- get_latest_plot(iters = 5, subfolder = "Arrhythmia", dataset_name = "Arrhythmia_withoutdupl_norm_02_v03")

Arrhythmia1_20 <- get_latest_plot(iters = 20, subfolder = "Arrhythmia", dataset_name = "Arrhythmia_withoutdupl_norm_02_v01")
Arrhythmia2_20 <- get_latest_plot(iters = 20, subfolder = "Arrhythmia", dataset_name = "Arrhythmia_withoutdupl_norm_02_v02")
Arrhythmia3_20 <- get_latest_plot(iters = 20, subfolder = "Arrhythmia", dataset_name = "Arrhythmia_withoutdupl_norm_02_v03")


Arrhythmia <- rbindlist(list(Arrhythmia3_20[[3]], Arrhythmia2_20[[3]], Arrhythmia1_20[[3]]))
Arrhythmia[, median(V3), by=.(Group, Representation)][order(V1)]
#Arrhythmia <- Arrhythmia[!(Representation %in% c("Combined_Space19", "Random1_Space21", "Combined_Space9", "Random1_Space9"))]

ggplot(data = Arrhythmia) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Arrhythmia[, round(min(V3) - 0.5)], 
                                                                  Arrhythmia[, round(max(V3) + 0.5)], 0.5))

#iForest
Arrhythmia2_20_iForest <- get_latest_plot(iters = 20, subfolder = "Arrhythmia", dataset_name = "Arrhythmia_withoutdupl_norm_02_v02", algorithm = "_iForest")
Arrhythmia3_20_iForest <- get_latest_plot(iters = 20, subfolder = "Arrhythmia", dataset_name = "Arrhythmia_withoutdupl_norm_02_v03", algorithm = "_iForest")


Arrhythmia_iForest <- rbindlist(list(Arrhythmia3_20_iForest[[3]], Arrhythmia3_20_iForest[[3]]))
Arrhythmia_iForest[, median(V3), by=.(Group, Representation)][order(V1)]
Arrhythmia_iForest <- Arrhythmia_iForest[!(Representation %in% c("Random1_Space7", "Random1_Space19", "Combined_Space1", "Random1_Space2"))]

ggplot(data = Arrhythmia_iForest) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Arrhythmia_iForest[, round(min(V3) - 0.5)], 
                                                                  Arrhythmia_iForest[, round(max(V3) + 0.5)], 0.5))






# Annthyroid --------------------------------------------------------------
Annthyroid1 <- get_latest_plot(iters = 10, subfolder = "Annthyroid", dataset_name = "Annthyroid_withoutdupl_norm_02_v01")
Annthyroid5 <- get_latest_plot(iters = 10, subfolder = "Annthyroid", dataset_name = "Annthyroid_withoutdupl_norm_02_v05")
Annthyroid9 <- get_latest_plot(iters = 10, subfolder = "Annthyroid", dataset_name = "Annthyroid_withoutdupl_norm_02_v09")


Annthyroid1_30 <- get_latest_plot(iters = 30, subfolder = "Annthyroid", dataset_name = "Annthyroid_withoutdupl_norm_02_v01")
Annthyroid5_30 <- get_latest_plot(iters = 30, subfolder = "Annthyroid", dataset_name = "Annthyroid_withoutdupl_norm_02_v05")
Annthyroid9_30 <- get_latest_plot(iters = 30, subfolder = "Annthyroid", dataset_name = "Annthyroid_withoutdupl_norm_02_v09")


Annthyroid <- rbindlist(list(Annthyroid1[[3]], Annthyroid5[[3]], Annthyroid9[[3]]))
Annthyroid[, median(V3), by=.(Group, Representation)][order(V1)]
Annthyroid <- Annthyroid[!(Representation %in% c("Combined_Space2", "Random1_Space2", "Combined_Space4", "Random1_Space11"))]


ggplot(data = Annthyroid) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Annthyroid[, round(min(V3) - 0.5)], 
                                                                  Annthyroid[, round(max(V3) + 0.5)], 0.5))


random1Ann1 <- Annthyroid9[[5]]
random1Ann1[, Model:= paste0(gamma, nu, kernel)]

random1_mean_CVperformance_Ann1 <- random1Ann1[, mean(V1), by = .(CViteration, features_Iteration, Model)]
merged_random1_Ann1 <- random1Ann1[random1_mean_CVperformance_Ann1, on = c("CViteration", "features_Iteration", "Model")]
merged_random1_Ann1[order(V2, decreasing = T)][, unique(V2)][1:10]

# Cardio ------------------------------------------------------------------
rrr1 <- get_latest_plot(iters = 10, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_02_v02", algorithm = "")
get_latest_plot(iters = 10, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_02_v05")
get_latest_plot(iters = 10, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_02_v08")
rrr <- get_latest_plot(iters = 30, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_02_v02", algorithm = "")
get_latest_plot(iters = 30, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_02_v05")
get_latest_plot(iters = 30, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_02_v08")
get_latest_plot(iters = 10, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_05_v01")
get_latest_plot(iters = 10, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_05_v02")
get_latest_plot(iters = 10, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_05_v03")
Cardio4_10 <- get_latest_plot(iters = 10, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_05_v04")
Cardio5_10 <- get_latest_plot(iters = 10, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_05_v05")
Cardio1_10<- get_latest_plot(iters = 10, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_05_v01")
Cardio <- rbindlist(list(Cardio4_10[[3]], Cardio5_10[[3]]))

# iForest
Cardio8_30 <- get_latest_plot(iters = 30, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_02_v08", algorithm = "_iForest")
Cardio5_30 <- get_latest_plot(iters = 30, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_02_v05", algorithm = "_iForest")
Cardio2_30<- get_latest_plot(iters = 30, subfolder = "Cardio", dataset_name = "Cardiotocography_withoutdupl_norm_02_v02", algorithm = "_iForest")
Cardio_iForest <- rbindlist(list(Cardio8_30[[3]], Cardio2_30[[3]]))#, Cardio2_30[[3]]))

ggplot(data = Cardio_iForest) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Cardio_iForest[, round(min(V3) - 0.5)], 
                                                                  Cardio_iForest[, round(max(V3) + 0.5)], 0.5))

# Glass -------------------------------------------------------------------
Glass_10 <- get_latest_plot(iters = 10, subfolder = "Glass", dataset_name = "Glass_withoutdupl_norm")
Glass_25 <- get_latest_plot(iters = 25, subfolder = "Glass", dataset_name = "Glass_withoutdupl_norm")
Glass_30 <- get_latest_plot(iters = 30, subfolder = "Glass", dataset_name = "Glass_withoutdupl_norm")

#GlassDT <- Glass_30[[3]]
GlassDT <- Glass_25[[3]]

GlassDT[, median(V3), by=.(Group, Representation)][order(V1)]
GlassDT <- GlassDT[!(Representation %in% c("Combined_Space12", "Random1_Space12", "Random1_Space6"))]


ggplot(data = GlassDT) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(GlassDT[, round(min(V3) - 0.5)], 
                                                                  GlassDT[, round(max(V3) + 0.5)], 0.5))


# iForest
Glass_30_iForest <- get_latest_plot(iters = 30, subfolder = "Glass", dataset_name = "Glass_withoutdupl_norm", algorithm = "_iForest")
GlassDT_iForest <- Glass_30_iForest[[3]]
GlassDT_iForest[, median(V3), by=.(Group, Representation)][order(V1)]

ggplot(data = GlassDT_iForest) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(GlassDT_iForest[, round(min(V3) - 0.5)], 
                                                                  GlassDT_iForest[, round(max(V3) + 0.5)], 0.5))
# Heart -------------------------------------------------------------------
Heart1_10 <- get_latest_plot(iters = 10, subfolder = "HeartDisease", dataset_name = "HeartDisease_withoutdupl_norm_02_v01")
Heart5_10 <- get_latest_plot(iters = 10, subfolder = "HeartDisease", dataset_name = "HeartDisease_withoutdupl_norm_02_v05")
Heart1_30 <- get_latest_plot(iters = 30, subfolder = "HeartDisease", dataset_name = "HeartDisease_withoutdupl_norm_02_v01")
Heart5_30 <- get_latest_plot(iters = 30, subfolder = "HeartDisease", dataset_name = "HeartDisease_withoutdupl_norm_02_v05")
Heart8_30 <- get_latest_plot(iters = 30, subfolder = "HeartDisease", dataset_name = "HeartDisease_withoutdupl_norm_02_v08")

Heart <- rbindlist(list(Heart1_30[[3]], Heart5_30[[3]], Heart8_30[[3]]))
Heart[, median(V3), by=.(Group, Representation)][order(V1)]


ggplot(data = Heart) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Heart[, round(min(V3) - 0.5)], 
                                                                  Heart[, round(max(V3) + 0.5)], 0.5))

#iForest
Heart1_30_iForest <- get_latest_plot(iters = 30, subfolder = "HeartDisease", dataset_name = "HeartDisease_withoutdupl_norm_02_v01", algorithm = "_iForest")
Heart2_30_iForest <- get_latest_plot(iters = 30, subfolder = "HeartDisease", dataset_name = "HeartDisease_withoutdupl_norm_02_v02", algorithm = "_iForest")
Heart5_30_iForest <- get_latest_plot(iters = 30, subfolder = "HeartDisease", dataset_name = "HeartDisease_withoutdupl_norm_02_v05", algorithm = "_iForest")
Heart8_30_iForest <- get_latest_plot(iters = 30, subfolder = "HeartDisease", dataset_name = "HeartDisease_withoutdupl_norm_02_v08", algorithm = "_iForest")


Heart_iForest <- rbindlist(list(Heart8_30_iForest[[3]], Heart2_30_iForest[[3]], Heart5_30_iForest[[3]]))#, Heart8_30_iForest[[3]]))
Heart_iForest[, median(V3), by=.(Group, Representation)][order(V1)]


ggplot(data = Heart_iForest) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Heart_iForest[, round(min(V3) - 0.5)], 
                                                                  Heart_iForest[, round(max(V3) + 0.5)], 0.5))

# Shuttle -------------------------------------------------------------------
Shuttle1_10 <- get_latest_plot(iters = 10, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v01")
Shuttle2_10 <- get_latest_plot(iters = 10, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v02")
Shuttle1_30 <- get_latest_plot(iters = 30, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v01")
Shuttle2_30 <- get_latest_plot(iters = 30, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v02")

Shuttle3_10 <- get_latest_plot(iters = 10, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v03")
Shuttle4_10 <- get_latest_plot(iters = 10, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v04")
Shuttle5_10 <- get_latest_plot(iters = 10, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v05")
Shuttle6_10 <- get_latest_plot(iters = 10, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v06")
Shuttle7_10 <- get_latest_plot(iters = 10, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v07")
Shuttle8_10 <- get_latest_plot(iters = 10, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v08")


Shuttle <- rbindlist(list(Shuttle6_10[[3]], 
                          Shuttle7_10[[3]], Shuttle8_10[[3]]))
ggplot(data = Shuttle) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Shuttle[, round(min(V3) - 0.5)], 
                                                                  Shuttle[, round(max(V3) + 0.5)], 0.5))
#iForest

Shuttle4_30_iForest <- get_latest_plot(iters = 30, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v04", algorithm = "_iForest")
Shuttle5_30_iForest <- get_latest_plot(iters = 30, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v05", algorithm = "_iForest")
Shuttle8_30_iForest <- get_latest_plot(iters = 30, subfolder = "Shuttle", dataset_name = "Shuttle_withoutdupl_norm_v08", algorithm = "_iForest")


Shuttle_iForest <- rbindlist(list(Shuttle4_30_iForest[[3]], 
                                  Shuttle5_30_iForest[[3]], Shuttle8_30_iForest[[3]]))
ggplot(data = Shuttle_iForest) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Shuttle_iForest[, round(min(V3) - 0.5)], 
                                                                  Shuttle_iForest[, round(max(V3) + 0.5)], 0.5))
# Stamps ------------------------------------------------------------------
Stamps1_10 <- get_latest_plot(iters = 10, subfolder = "Stamps", dataset_name = "Stamps_withoutdupl_norm_02_v01")
Stamps2_10 <- get_latest_plot(iters = 10, subfolder = "Stamps", dataset_name = "Stamps_withoutdupl_norm_02_v02")
Stamps3_10 <- get_latest_plot(iters = 10, subfolder = "Stamps", dataset_name = "Stamps_withoutdupl_norm_02_v03")
Stamps1_30 <- get_latest_plot(iters = 30, subfolder = "Stamps", dataset_name = "Stamps_withoutdupl_norm_02_v01")
Stamps2_30 <- get_latest_plot(iters = 30, subfolder = "Stamps", dataset_name = "Stamps_withoutdupl_norm_02_v02")
Stamps3_30 <- get_latest_plot(iters = 30, subfolder = "Stamps", dataset_name = "Stamps_withoutdupl_norm_02_v03")

Stamps <- rbindlist(list(Stamps1_30[[3]], Stamps2_30[[3]], Stamps3_30[[3]]))
ggplot(data = Stamps) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Stamps[, round(min(V3) - 0.5)], 
                                                                  Stamps[, round(max(V3) + 0.5)], 0.5))

#iForest
Stamps1_30_iForest <- get_latest_plot(iters = 30, subfolder = "Stamps", dataset_name = "Stamps_withoutdupl_norm_02_v01", algorithm = "_iForest")
Stamps2_30_iForest <- get_latest_plot(iters = 30, subfolder = "Stamps", dataset_name = "Stamps_withoutdupl_norm_02_v02", algorithm = "_iForest")
Stamps3_30_iForest <- get_latest_plot(iters = 30, subfolder = "Stamps", dataset_name = "Stamps_withoutdupl_norm_02_v03", algorithm = "_iForest")

Stamps_iForest <- rbindlist(list(Stamps1_30_iForest[[3]], Stamps2_30_iForest[[3]], 
                                 Stamps3_30_iForest[[3]]))
Stamps_iForest[, median(V3), by=.(Group, Representation)][order(V1)]

ggplot(data = Stamps_iForest) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Stamps_iForest[, round(min(V3) - 0.5)], 
                                                                  Stamps_iForest[, round(max(V3) + 0.5)], 0.5))



# Waveform ----------------------------------------------------------------
Wave1_10 <- get_latest_plot(iters = 10, subfolder = "Waveform", dataset_name = "Waveform_withoutdupl_norm_v03")
Wave2_10 <- get_latest_plot(iters = 10, subfolder = "Waveform", dataset_name = "Waveform_withoutdupl_norm_v05")
Wave3_10 <- get_latest_plot(iters = 10, subfolder = "Waveform", dataset_name = "Waveform_withoutdupl_norm_v09")


Wave <- rbindlist(list(Wave1_10[[3]], Wave2_10[[3]], Wave3_10[[3]]))

ggplot(data = Wave) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Wave[, round(min(V3) - 0.5)], 
                                                                  Wave[, round(max(V3) + 0.5)], 0.5))

#iForest
Wave1_30_iForest <- get_latest_plot(iters = 30, subfolder = "Waveform", dataset_name = "Waveform_withoutdupl_norm_v03", algorithm = "_iForest")
Wave2_30_iForest <- get_latest_plot(iters = 30, subfolder = "Waveform", dataset_name = "Waveform_withoutdupl_norm_v05", algorithm = "_iForest")
Wave3_30_iForest <- get_latest_plot(iters = 30, subfolder = "Waveform", dataset_name = "Waveform_withoutdupl_norm_v09", algorithm = "_iForest")


Wave_iForest <- rbindlist(list(Wave1_30_iForest[[3]], Wave2_30_iForest[[3]], 
                               Wave3_30_iForest[[3]]))

ggplot(data = Wave_iForest) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Wave_iForest[, round(min(V3) - 0.5)], 
                                                                  Wave_iForest[, round(max(V3) + 0.5)], 0.5))


# Ionosphere --------------------------------------------------------------
Ionosphere_10 <- get_latest_plot(iters = 30, subfolder = "Ionosphere", dataset_name = "Ionosphere_withoutdupl_norm",  algorithm = "")
Ionosphere_10DT <- Ionosphere_10[[3]]


Ionosphere_iForest <- get_latest_plot(iters = 30, subfolder = "Ionosphere", dataset_name = "Ionosphere_withoutdupl_norm", algorithm = "_iForest")
Ionosphere_10DT_iForest <- Ionosphere_iForest[[3]]



ggplot(data = Ionosphere_10DT_iForest) + 
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Ionosphere_10DT_iForest[, round(min(V3) - 0.5)], 
                                                                  Ionosphere_10DT_iForest[, round(max(V3) + 0.5)], 0.5))

# PageBlocks --------------------------------------------------------------
Page2_10 <- get_latest_plot(iters = 10, subfolder = "PageBlocks", dataset_name = "PageBlocks_withoutdupl_norm_02_v02")
Page5_10 <- get_latest_plot(iters = 10, subfolder = "PageBlocks", dataset_name = "PageBlocks_withoutdupl_norm_02_v05")
Page10_10 <- get_latest_plot(iters = 10, subfolder = "PageBlocks", dataset_name = "PageBlocks_withoutdupl_norm_02_v10")

Page2_30 <- get_latest_plot(iters = 30, subfolder = "PageBlocks", dataset_name = "PageBlocks_withoutdupl_norm_02_v02", algorithm = "")
Page5_30 <- get_latest_plot(iters = 30, subfolder = "PageBlocks", dataset_name = "PageBlocks_withoutdupl_norm_02_v05", algorithm = "")
Page10_30 <- get_latest_plot(iters = 30, subfolder = "PageBlocks", dataset_name = "PageBlocks_withoutdupl_norm_02_v10", algorithm = "")

Page <- rbindlist(list(Page2_30[[3]], Page5_30[[3]], Page10_30[[3]]))

ggplot(data = Page) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Page[, round(min(V3) - 0.5)], 
                                                                  Page[, round(max(V3) + 0.5)], 0.5))
#iForest
Page2_30_iForest <- get_latest_plot(iters = 30, subfolder = "PageBlocks", dataset_name = "PageBlocks_withoutdupl_norm_02_v02", algorithm = "_iForest")
Page5_30_iForest <- get_latest_plot(iters = 30, subfolder = "PageBlocks", dataset_name = "PageBlocks_withoutdupl_norm_02_v05", algorithm = "_iForest")
Page10_30_iForest <- get_latest_plot(iters = 30, subfolder = "PageBlocks", dataset_name = "PageBlocks_withoutdupl_norm_02_v10", algorithm = "_iForest")

Page_iForest <- rbindlist(list(Page2_30_iForest[[3]], Page5_30_iForest[[3]], 
                               Page10_30_iForest[[3]]))


ggplot(data = Page_iForest) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Page_iForest[, round(min(V3) - 0.5)], 
                                                                  Page_iForest[, round(max(V3) + 0.5)], 0.5))



# PenDigits ---------------------------------------------------------------

Pen1_05 <- get_latest_plot(iters = 5, subfolder = "PenDigits", dataset_name = "PenDigits_withoutdupl_norm_v01", algorithm = "")
Pen3_05 <- get_latest_plot(iters = 5, subfolder = "PenDigits", dataset_name = "PenDigits_withoutdupl_norm_v03", algorithm = "")
Pen8_05 <- get_latest_plot(iters = 5, subfolder = "PenDigits", dataset_name = "PenDigits_withoutdupl_norm_v08", algorithm = "")

Pen <- rbindlist(list(Pen1_05[[3]], Pen3_05[[3]], Pen8_05[[3]]))

Pen[, sd(V1), by = Group]

ggplot(data = Pen) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Pen[, round(min(V3) - 0.5)], 
                                                                  Pen[, round(max(V3) + 0.5)], 0.5))

#iForest
Pen3_20_iForest <- get_latest_plot(iters = 20, subfolder = "PenDigits", dataset_name = "PenDigits_withoutdupl_norm_v03", algorithm = "_iForest")
Pen_iForest <- rbindlist(list(Pen3_20_iForest[[3]]))



ggplot(data = Pen_iForest) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Pen_iForest[, round(min(V3) - 0.5)], 
                                                                  Pen_iForest[, round(max(V3) + 0.5)], 0.5))


# InternetAds ---------------------------------------------------------------

Internet1_05 <- get_latest_plot(iters = 5, subfolder = "InternetAds", dataset_name = "InternetAds_withoutdupl_norm_02_v01")
Internet5_05 <- get_latest_plot(iters = 5, subfolder = "InternetAds", dataset_name = "InternetAds_withoutdupl_norm_02_v05")
Internet9_05 <- get_latest_plot(iters = 5, subfolder = "InternetAds", dataset_name = "InternetAds_withoutdupl_norm_02_v09")

Internet <- rbindlist(list(Internet1_05[[3]], Internet5_05[[3]], Internet9_05[[3]]))

ggplot(data = Internet) +
  aes(x = Group, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  scale_y_continuous(name="AUC Standard Deviations", breaks = seq(Internet[, round(min(V3) - 0.5)], 
                                                                  Internet[, round(max(V3) + 0.5)], 0.5))




# All datasets - plot -----------------------------------------------------

Wilt[, Dataset:= "Wilt-datasets"]
Shuttle[, Dataset:= "Shuttle-datasets"]
Stamps[, Dataset:= "Stamps-datasets"]
Wave[, Dataset:= "Waveform-datasets"]
Ionosphere_10DT[, Dataset:= "Ionosphere"]
Cardio[, Dataset:= "Cardio-datasets"]
GlassDT[, Dataset:= "Glass"]
Arrhythmia[, Dataset:= "Arrhythmia-datasets"]
Annthyroid[, Dataset:= "Annthyroid-datasets"]
Page[, Dataset:= "PageBlocks-datasets"]
Heart[, Dataset:= "HeartDisease-datasets"]
Internet[, Dataset:= "InternetAds-datasets"]


all_datasetsDT <- rbindlist(list(Wilt, Shuttle, Stamps, Wave, 
                                 Ionosphere_10DT, Cardio, GlassDT, 
                                 Arrhythmia, Annthyroid, Page, Internet))

ggplot(data = all_datasetsDT[Group!= "Original Space"]) +
  aes(x = Dataset, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name="AUC Standard Deviations", 
                     breaks = seq(all_datasetsDT[, round(min(V3) - 0.5)], 
                                  all_datasetsDT[, round(max(V3) + 0.5)], 0.5))+
  coord_flip()+
  ggtitle(label = "each boxplot which is named as 'X-datasets', contains 3 different versions of outlier class. Otherwise, only one version was available for analysis",
          subtitle = "Experiments have been performed under 30times 10fold Cross validation ")


#iForest
Wilt_iForest[, Dataset:= "Wilt-datasets"]
Shuttle_iForest[, Dataset:= "Shuttle-datasets"]
Stamps_iForest[, Dataset:= "Stamps-datasets"]
Wave_iForest[, Dataset:= "Waveform-datasets"]
Ionosphere_10DT_iForest[, Dataset:= "Ionosphere"]
Cardio_iForest[, Dataset:= "Cardio-datasets"]
GlassDT_iForest[, Dataset:= "Glass"]
Arrhythmia_iForest[, Dataset:= "Arrhythmia-datasets"]
Page_iForest[, Dataset:= "PageBlocks-datasets"]
Heart_iForest[, Dataset:= "HeartDisease-datasets"]
Pen_iForest[, Dataset:= "PenDigits-datasets"]


all_datasetsDT_iForest <- rbindlist(list(Wilt_iForest, Shuttle_iForest, 
                                         Wave_iForest, Ionosphere_10DT_iForest, 
                                         Cardio_iForest, GlassDT_iForest, 
                                         Arrhythmia_iForest, Page_iForest,
                                         Heart_iForest, Pen_iForest))

ggplot(data = all_datasetsDT_iForest[Group!= "Original Space"]) +
  aes(x = Dataset, y = V3, fill = Group) +
  geom_boxplot() +
  theme_minimal()+
  geom_hline(yintercept = 0)+
  scale_y_continuous(name="AUC Standard Deviations", 
                     breaks = seq(all_datasetsDT_iForest[, round(min(V3) - 0.5)], 
                                  all_datasetsDT_iForest[, round(max(V3) + 0.5)], 0.5))+
  coord_flip()+
  ggtitle(label = "each boxplot which is named as 'X-datasets', contains 3 different versions of outlier class. Otherwise, only one version was available for analysis",
          subtitle = "Experiments have been performed under 30times 10fold Cross validation ")




# Max of test -----
get_max_test <- function(iters, subfolder, dataset_name) {
  
  random1 <- fread(paste0("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/OC_Combined_CV/figures/", 
                          subfolder, "/", dataset_name, "_1random_", iters, "_paper_CV.csv"))
  combined <- fread(paste0("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/OC_Combined_CV/figures/", 
                           subfolder, "/", dataset_name, "_Combined_", iters, "_paper_CV.csv"))
  original <- fread(paste0("~/R Language Default Dir/Github-projects/multiview-oneclass/data/derived-data/OC_Combined_CV/figures/", 
                           subfolder, "/", dataset_name, "_Original_", iters, "_paper_CV.csv"))
  
  dataset <- dataset_name
  
  OriginalDT1 <- original[, max(V2), by = .(CViteration)]
  OriginalDT1[, features_Iteration:= "Original Space"]
  OriginalDT1[, Representation := "Original Space"]
  
  combinedDT1 <- combined[, max(V2), by = .(CViteration, features_Iteration)]
  combinedDT1[, `:=` (features_Iteration = as.factor(as.character(features_Iteration)),
                      Representation = "Combined Space") ]
  
  randomDT1 <- random1[, max(V2), by = .(CViteration, features_Iteration)]
  randomDT1[, `:=` (features_Iteration = as.factor(as.character(features_Iteration)),
                    Representation = "Orthogonal Space" ) ]
  
  setcolorder(OriginalDT1, names(randomDT1))
  
  all_testDT <- rbindlist(list(combinedDT1, OriginalDT1, randomDT1))
  
  all_testDT1 <- copy(all_testDT)
  
  dd <- all_testDT1[Representation!="Original Space"]
  tempDT <- dcast.data.table(dd, CViteration+features_Iteration~Representation, value.var = "V1")
  tempDT[, Original:=rep(OriginalDT1$V1, 21)]
  tempDT[, `:=` (win_Combined = 0, win_Ortho = 0) ]
  for(i in 1:iters){
    for (j in 1:21){
      tempDT[CViteration==i & features_Iteration==j & `Combined Space` > Original, win_Combined:= 1]
      tempDT[CViteration==i & features_Iteration==j & `Orthogonal Space` > Original, win_Ortho:= 1]
    }
  }
  # finding the performance of the best combined representation
  win_combinedDT <- tempDT[win_Combined==1, .N, by = .(features_Iteration)][order(N, decreasing = T)]
  best_combined <- tempDT[features_Iteration == win_combinedDT[1, features_Iteration]][, mean(`Combined Space`)]
  # finding the performance of the best orthogonal representation
  win_orthoDT <- tempDT[win_Ortho==1, .N, by = .(features_Iteration)][order(N, decreasing = T)]
  best_orthogonal <- tempDT[features_Iteration == win_orthoDT[1, features_Iteration]][, mean(`Orthogonal Space`)]
  
  # the original performance
  mean_original <- tempDT[features_Iteration == win_orthoDT[1, features_Iteration]][, mean(Original)]
  sd_original <- tempDT[features_Iteration == win_orthoDT[1, features_Iteration]][, sd(Original)]
  
  best_performanceDT <- data.table(AUC = c(tempDT[features_Iteration == win_combinedDT[1, features_Iteration]][, mean(`Combined Space`)],
                                           tempDT[features_Iteration == win_orthoDT[1, features_Iteration]][, mean(`Orthogonal Space`)],
                                           tempDT[features_Iteration == win_orthoDT[1, features_Iteration]][, mean(Original)]),
                                   Representation = c("Combined Space", "Orthogonal Space", "Original Space"))
  best_performanceDT[Representation == "Combined Space", SD:= (AUC - mean_original)/sd_original]
  best_performanceDT[Representation == "Orthogonal Space", SD:= (AUC - mean_original)/sd_original]
  best_performanceDT[is.na(SD), SD:= 0]
  best_performanceDT[, Dataset:= dataset_name]
  best_performanceDT[, SDround:= round(SD, 2)]
  
  # all_testDT[, Representation:=as.factor(Representation)]
  # ttt <- as.character(win_combinedDT[1:11, features_Iteration])
  
  p_3 <- ggplot(data = best_performanceDT[Representation != "Original Space"]) +
    aes(x = Representation, y = SD, fill = Representation) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=SDround), vjust=1.6, color="white", size=3.5)+
    theme_minimal()+
    labs(y = 'Standard Deviations of Original Space AUC', x = '')+
    ggtitle(label = "Average Performance of the best representation across 30times 10fold Cross validation "
            ,subtitle = "Measured in standard deviations of the performance on the original space ")
  
  
  # p3 <- ggplot(data = all_testDT[features_Iteration %in% ttt]) +
  #   aes(x = features_Iteration, y = V1, fill = Representation) +
  #   geom_boxplot() +
  #   theme_minimal()+
  #   coord_flip()+
  #   geom_hline(yintercept = all_testDT[features_Iteration == "Original", mean(V1)])+
  #   labs(title = paste0(dataset, " & CViteration: ", iters, ". Max of test"))
  # 
  # p4 <- ggplot(data = all_testDT[features_Iteration %in% c(ttt, "Original")]) +
  #   aes(x = Representation, y = V1, fill = Representation) +
  #   geom_boxplot() +
  #   theme_minimal()+
  #   coord_flip()+
  #   labs(title = paste0(dataset, " & CViteration: ", iters, ". Max of test"))
  # 
  # print("Median best feature iterations - Combined")
  # print(all_testDT[features_Iteration %in% win_combinedDT[1:15, features_Iteration]][, median(V1), by = .(Representation)])
  # print(all_testDT[features_Iteration == "Original", median(V1)])
  # print("Median best feature iterations - Orthogonal")
  # print(all_testDT[features_Iteration %in% win_orthoDT[1:15, features_Iteration]][, median(V1), by = .(Representation)])
  # print(all_testDT[features_Iteration == "Original", median(V1)])
  # print("Mean all feature iterations")
  # print(all_testDT[, mean(V1), by=Representation])
  # 
  # p5 <- ggplot(data = all_testDT) +
  #   aes(x = features_Iteration, y = V1, fill = Representation) +
  #   geom_boxplot() +
  #   theme_minimal()+
  #   coord_flip()+
  #   labs(title = dataset)
  
  return(list(p_3,best_performanceDT))
}

get_best_representation_AUC <- function(given_DT_list, dataset_name) {
  
  all_representations_combinedDT <- given_DT_list[[8]][, mean(V1), by=Representation]
  all_representations_combinedDT[, Group := "Combined Space"]
  all_representations_orthogonalDT <- given_DT_list[[9]][, mean(V1), by=Representation]
  all_representations_orthogonalDT[, Group := "Orthogonal Space"]
  DT_best <- rbindlist(list(all_representations_combinedDT, all_representations_orthogonalDT))

  DT_mean_sd <- given_DT_list[[7]][, .(mean(V1), sd(V1))]
  
  DT_best[Group == "Combined Space", SD:= (V1 - DT_mean_sd$V1)/DT_mean_sd$V2]
  DT_best[Group == "Orthogonal Space", SD:= (V1 - DT_mean_sd$V1)/DT_mean_sd$V2]
  DT_best[Group == "Original Space", SD:= 0]
  DT_best[, Dataset:= dataset_name]
  DT_best[, SDround:= round(SD, 2)]
  DT_best1 <- rbindlist(list(DT_best[order(SD, decreasing = T)][Group == "Combined Space"][1],
            DT_best[order(SD, decreasing = T)][Group == "Orthogonal Space"][1]))
  return(DT_best1)
}

# Wilt --------------------------------------------------------------------

Wilt2_best <- get_best_representation_AUC(given_DT_list  = Wilt2, dataset_name = "Wilt_1")
Wilt5_best <- get_best_representation_AUC(given_DT_list  = Wilt5, dataset_name = "Wilt_2")
Wilt8_best <- get_best_representation_AUC(given_DT_list  = Wilt8, dataset_name = "Wilt_3")
Wilt_best <- rbindlist(list(Wilt2_best, Wilt5_best, Wilt8_best))

# Arrhythmia --------------------------------------------------------------

Arrhythmia1_best <- get_best_representation_AUC(given_DT_list  = Arrhythmia1_20, dataset_name = "Arrhythmia_1")
Arrhythmia2_best <- get_best_representation_AUC(given_DT_list  = Arrhythmia2_20, dataset_name = "Arrhythmia_2")
Arrhythmia3_best <- get_best_representation_AUC(given_DT_list  = Arrhythmia3_20, dataset_name = "Arrhythmia_3")

Arrhythmia_best <- rbindlist(list(Arrhythmia3_best, Arrhythmia2_best, Arrhythmia1_best))

# Annthyroid --------------------------------------------------------------
Annthyroid1_best <- get_best_representation_AUC(given_DT_list  = Annthyroid1_30, dataset_name = "Annthyroid_1")
Annthyroid5_best <- get_best_representation_AUC(given_DT_list  = Annthyroid5_30, dataset_name = "Annthyroid_2")
Annthyroid9_best <- get_best_representation_AUC(given_DT_list  = Annthyroid9_30, dataset_name = "Annthyroid_3")

Annthyroid_best <- rbindlist(list(Annthyroid1_best, Annthyroid5_best, Annthyroid9_best))

# Glass -------------------------------------------------------------------
Glass_10_best <- get_best_representation_AUC(given_DT_list  = Glass_10, dataset_name = "Glass")
Glass_30_best <- get_best_representation_AUC(given_DT_list  = Glass_30, dataset_name = "Glass")


# Heart -------------------------------------------------------------------
Heart1_30_best <- get_best_representation_AUC(given_DT_list  = Heart1_30, dataset_name = "HeartDisease_1")
Heart5_30_best <- get_best_representation_AUC(given_DT_list  = Heart5_30, dataset_name = "HeartDisease_2")
Heart8_30_best <- get_best_representation_AUC(given_DT_list  = Heart8_30, dataset_name = "HeartDisease_2")

Heart_best <- rbindlist(list(Heart1_30_best, Heart5_30_best, Heart8_30_best))

# Shuttle -------------------------------------------------------------------

Shuttle6_10_best <- get_best_representation_AUC(given_DT_list  = Shuttle6_10, dataset_name = "Shuttle_1")
Shuttle7_10_best <- get_best_representation_AUC(given_DT_list  = Shuttle7_10, dataset_name = "Shuttle_2")
Shuttle8_10_best <- get_best_representation_AUC(given_DT_list  = Shuttle8_10, dataset_name = "Shuttle_3")


Shuttle_best <- rbindlist(list(Shuttle6_10_best, Shuttle7_10_best, Shuttle8_10_best))
# Stamps ------------------------------------------------------------------
Stamps1_30_best <- get_best_representation_AUC(given_DT_list  = Stamps1_30, dataset_name = "Stamps_1")
Stamps2_30_best <- get_best_representation_AUC(given_DT_list  = Stamps2_30, dataset_name = "Stamps_2")
Stamps3_30_best <- get_best_representation_AUC(given_DT_list  = Stamps3_30, dataset_name = "Stamps_3")


Stamps_best <- rbindlist(list(Stamps1_30_best, Stamps2_30_best, Stamps3_30_best))

# Waveform ----------------------------------------------------------------
Wave1_10_best <- get_best_representation_AUC(given_DT_list  = Wave1_10, dataset_name = "Waveform_1")
Wave2_10_best <- get_best_representation_AUC(given_DT_list  = Wave2_10, dataset_name = "Waveform_2")
Wave3_10_best <- get_best_representation_AUC(given_DT_list  = Wave3_10, dataset_name = "Waveform_3")

Wave_best <- rbindlist(list(Wave1_10_best, Wave2_10_best, Wave3_10_best))
# Ionosphere --------------------------------------------------------------
Ionosphere_10_best <- get_best_representation_AUC(given_DT_list  = Ionosphere_10, dataset_name = "Ionosphere")

# PageBlocks --------------------------------------------------------------
Page2_30_best <- get_best_representation_AUC(given_DT_list = Page2_30, dataset_name = "PageBlocks_1")
Page5_30_best<- get_best_representation_AUC(given_DT_list = Page5_30, dataset_name = "PageBlocks_2")
Page10_30_best <-get_best_representation_AUC(given_DT_list = Page10_30, dataset_name = "PageBlocks_3")

Page_best <- rbindlist(list(Page2_30_best, Page5_30_best, Page10_30_best))

# PenDigits ---------------------------------------------------------------

Pen1_05_best <- get_best_representation_AUC(given_DT_list = Pen1_05, dataset_name = "PenDigits_1")
Pen3_05_best <- get_best_representation_AUC(given_DT_list = Pen3_05, dataset_name = "PenDigits_2")
Pen8_05_best <- get_best_representation_AUC(given_DT_list = Pen8_05, dataset_name = "PenDigits_3")

Pen_best <- rbindlist(list(Pen1_05_best, Pen3_05_best, Pen8_05_best))

# PageBlocks --------------------------------------------------------------

Page2_30_best <- get_best_representation_AUC(given_DT_list = Page2_30, dataset_name = "PageBlocks_1")
Page5_30_best <- get_best_representation_AUC(given_DT_list = Page5_30, dataset_name = "PageBlocks_2")
Page10_30_best <- get_best_representation_AUC(given_DT_list = Page10_30, dataset_name = "PageBlocks_3")

Page30_best <- rbindlist(list(Page2_30_best, Page5_30_best,Page10_30_best))

# Cardio ------------------------------------------------------------------
cardio1_best <- get_best_representation_AUC(given_DT_list = Cardio1_10, dataset_name = "Cardiotocography_1")
cardio2_best <- get_best_representation_AUC(given_DT_list = Cardio4_10, dataset_name = "Cardiotocography_2")
cardio5_best <- get_best_representation_AUC(given_DT_list = Cardio5_10, dataset_name = "Cardiotocography_3")


cardio_best <- rbindlist(list(cardio2_best, cardio5_best, cardio1_best))


# Cardio ------------------------------------------------------------------
Internet1_best <- get_best_representation_AUC(given_DT_list = Internet1_05, dataset_name = "Internet_1")
Internet2_best <- get_best_representation_AUC(given_DT_list = Internet5_05, dataset_name = "Internet_2")
Internet5_best <- get_best_representation_AUC(given_DT_list = Internet9_05, dataset_name = "Internet_3")


Internet_best <- rbindlist(list(Internet1_best, Internet2_best, Internet5_best))



# All datasets - Barplot --------------------------------------------------
Wilt_best[, Dataset_all:="Wilt"]
Annthyroid_best[, Dataset_all:="Annthyroid"]
Glass_30_best[, Dataset_all:="Glass"]
cardio_best[, Dataset_all:="Cardio"]
Heart_best[, Dataset_all:="Heart"]
Stamps_best[, Dataset_all:="Stamps"]
Shuttle_best[, Dataset_all:="Shuttle"]
Wave_best[, Dataset_all:="Waveform"]
Ionosphere_10_best[, Dataset_all:="Ionosphere"]
Page_best[, Dataset_all:="Page"]
Arrhythmia_best[, Dataset_all:="Arrhythmia"]
Internet_best[, Dataset_all:= "InternetAds-datasets"]

All_DT1 <- rbindlist(list(Wilt_best, Annthyroid_best, Glass_30_best, cardio_best,
                          Shuttle_best, Stamps_best, Wave_best,
                          Ionosphere_10_best, Page_best, Arrhythmia_best, Internet_best))

All_DT1[, SD_sd:= sd(SD), by = .(Dataset_all, Group)]
All_DT1[is.na(SD_sd), SD_sd:=0]
All_DT1[, Dataset_all:=as.factor(Dataset_all)]
All_DT1[, SD_mean:= mean(SD), by = .(Dataset_all, Group)]

resDT <- All_DT1[, .SD[1], by=.(Dataset_all, Group)]

p <- ggplot(data = resDT[Group != "Original Space"]) +
  aes(x = Dataset_all, y = SD_mean, fill = Group) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=SD_mean-SD_sd, ymax=SD_mean+SD_sd), width=.2,
                position=position_dodge(.9))+
  theme_minimal()+
  labs(y = 'Standard Deviations of Original Space AUC', x = '')+
  ggtitle(label = "Average performance of the best representation across 3 different versions of outlier class.  Black lines represent maximum and minimum values",
          subtitle = "Improvement is measured in standard deviations away from the average performance of the original space across 3 different versions of outlier class")+
  scale_y_continuous(breaks = seq(All_DT1[, min(SDround)], All_DT1[, max(SDround)], by = 0.25))+
 coord_flip()
p

All_DT1[, mean_V1:= mean(V1), by = .(Dataset_all, Group)]
All_DT1[, min_V1:= min(V1), by = .(Dataset_all, Group)]
All_DT1[, max_V1:= max(V1), by = .(Dataset_all, Group)]

p1 <- ggplot(data = All_DT1[Group != "Original Space"]) +
  aes(x = Dataset_all, y = mean_V1, fill = Group) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=min_V1, ymax=max_V1), width=.2,
                position=position_dodge(.9))+
  theme_minimal()+
  labs(y = 'Standard Deviations of Original Space AUC', x = '')+
  ggtitle(label = "Average performance of the best representation across 3 different versions of outlier class.  Black lines represent maximum and minimum values",
          subtitle = "Improvement is measured in standard deviations away from the average performance of the original space across 3 different versions of outlier class")+
  coord_flip()
p1







