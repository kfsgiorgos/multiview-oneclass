get_meta_plots <- function(subfolder_name, experiments) {
  
  set.seed(191984)
  # The argument experiments can take values: i) "OC_combined", ii) 
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
  subfolder_name <- "WDBC"
  
  
  
  list_files <- list.files(paste0(final_path_to_save, "figures/", subfolder_name))
  AUC_ensemble_files <- list_files[stringi::stri_detect(regex = "AUCensemble.csv", str = list_files)]
  
  list_names <- list()
  for(i in 1:length(AUC_ensemble_files)){
    list_names[[i]] <- stringi::stri_split(AUC_ensemble_files[i], fixed = "_withoutdupl_norm")[[1]][1]
  }
  dataset_name <-  unique(as.vector(unlist(list_names)))
  
  
  
  listDT <- list()
  for(i in 1:length(AUC_ensemble_files)){
    tempDT <- fread(paste0(final_path_to_save, "figures/", subfolder_name, "/", AUC_ensemble_files[i]))
    
    if(dataset_name == "Glass"){
      tempDT[, Dataset:= paste0(dataset_name)]
      listDT[[i]] <- tempDT
    } else{
    split1 <- stringi::stri_split(str = AUC_ensemble_files[i], fixed = "norm_")[[1]][2]
    dataset_suffix <- stringi::stri_split(str = split1, fixed = "_mixedViewFeat_AUCensemble.csv")[[1]][1]
    
    tempDT[, Dataset:= paste0(dataset_name, "_", dataset_suffix)]
    listDT[[i]] <- tempDT
      
    }
    
  }
  
  DT <- rbindlist(listDT)
  DT[, Dataset_Features:= paste0(Dataset, "-OD", mixedViewFeat)]
  DT[, mixedViewFeat:= as.factor(mixedViewFeat)]
  DT[, Normal_Size:= factor(paste0(Normal_Size*100, "%"), levels = c("1%",  
                                                                     "5%",
                                                                     "10%", 
                                                                     "20%", 
                                                                     "30%",
                                                                     "50%",
                                                                     "70%",
                                                                     "80%"))]
  
  DT$Normal_Size_1 <- factor(DT$Normal_Size_1 , 
                                      levels = c("Training Normal class: 1%",
                                                 "Training Normal class: 5%",
                                                 "Training Normal class: 10%",
                                                 "Training Normal class: 20%",
                                                 "Training Normal class: 30%",
                                                 "Training Normal class: 50%",
                                                 "Training Normal class: 70%",
                                                 "Training Normal class: 80%" ))
  
  #esquisser()
  
  p1 <- ggplot(data = DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1]) +
    aes(x = Normal_Size_1, y = V2, fill = Dataset) +
    geom_boxplot() +
    theme_minimal()+
    geom_hline(yintercept = 0, color='red', size = 1.5) +
    labs(title = glue("Dataset: {dataset_name}"))
  p1
  ggsave(plot = p1, filename = paste0(final_path_to_save, "figures/",  
                                      subfolder_name, "/", dataset_name,
                                      "_1_mixedViewFeat.pdf"),
         width = 18, height = 9, units = "in", dpi = 300)
  
  
  p2 <- ggplot(data = DT[Representation == "Ensemble of Combined Space"]) +
    aes(x = Normal_Size, y = V2, fill = Dataset_Features) +
    geom_boxplot() +
    theme_linedraw()+
    facet_wrap(~mixedViewFeat)+
    geom_hline(yintercept = 0, color='red', size = 1.5) +
    labs(title = glue("Dataset: {dataset_name}"), x = "Portion of Normal data points")
  p2
  ggsave(plot = p2, filename = paste0(final_path_to_save, "figures/",  
                                      subfolder_name, "/", dataset_name,
                                      "_all_mixedViewFeat.pdf"),
         width = 18, height = 9, units = "in", dpi = 300)

  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.01, 
  #    .(mean(V2), max(V2)), by = Dataset][, mean(V2)]
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.05, 
  #    .(mean(V2), max(V2)), by = Dataset]
  # 
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.05, 
  #    .(mean(V2), max(V2)), by = Dataset][, mean(V2)]
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.05, 
  #    .(mean(V2), max(V2)), by = Dataset]
  # 
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.1, 
  #    .(mean(V2), max(V2)), by = Dataset][, mean(V2)]
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.1, 
  #    .(mean(V2), max(V2)), by = Dataset]
  # 
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.2, 
  #    .(mean(V2), max(V2)), by = Dataset][, mean(V2)]
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.2, 
  #    .(mean(V2), max(V2), quantile(V2)), by = Dataset]
  # 
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.3, 
  #    .(mean(V2), max(V2)), by = Dataset][, mean(V2)]
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.3, 
  #    .(mean(V2), max(V2)), by = Dataset]
  # 
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.5, 
  #    .(mean(V2), max(V2)), by = Dataset][, mean(V2)]
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.5, 
  #    .(mean(V2), max(V2)), by = Dataset]
  # 
  #   
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.7, 
  #    .(mean(V2), max(V2)), by = Dataset][, mean(V2)]
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.7, 
  #    .(mean(V2), max(V2)), by = Dataset]
  # 
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.8, 
  #    .(mean(V2), max(V2)), by = Dataset][, mean(V2)]
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.8, 
  #    mean(V2), by = Dataset][, mean(V1)]
  # DT[Representation == "Ensemble of Combined Space" & mixedViewFeat==1 & Normal_Size == 0.8, 
  #    .(mean(V2), max(V2)), by = Dataset]
  

  
  }