source("~/GitHub_projects/multiview-oneclass/src.R")
# source("~/R Language Default Dir/Github-projects/multiview-oneclass/src.R")
args <- commandArgs(TRUE)
arg1 <- args[1]
arg2 <- args[2]
arg3 <- args[3]

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



list_res <- list()
for( k in 1:as.numeric(arg3)){
  print(Sys.time())
  list_res[[k]] <- get_CV_experiments_paper_ensemble(datasetname = arg1, 
                                                     experiments = "OC_combined_CV", 
                                                     CViterations = 3)
  print(list_res[[k]])
}

ensembleDT <- data.table(V1 = unlist(list_res), Representation = "Ensemble-Multiple Represenations ")

fwrite(ensembleDT, paste0(final_path_to_save, "figures/",  
                            arg2, "/", arg1, "_OCSVM_Ensemble_Multiple_Repres_", arg3,"_iters.csv"))



