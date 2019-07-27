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


# subfolder_name <- "Annthyroid"
# subfolder_name <- "Arrhythmia"
# subfolder_name <- "Cardio"
# subfolder_name <- "Glass"
# subfolder_name <- "HeartDisease"
# subfolder_name <- "InternetAds"
# subfolder_name <- "PenDigits"
# subfolder_name <- "Pima"
# subfolder_name <- "Shuttle"
# subfolder_name <- "SpamBase"
# subfolder_name <- "Stamps"
# subfolder_name <- "Waveform"
# subfolder_name <- "WDBC"

glass_DT <- get_meta_plots(subfolder_name = "Glass")
ionosphere_DT <- get_meta_plots(subfolder_name = "Ionosphere")

DT <- rbindlist(list(glass_DT[[1]], ionosphere_DT[[1]]))

ggplot(data = DT) +
  aes(x = Dataset, y = V1, fill = Representation) +
  geom_boxplot() +
  theme_minimal()
