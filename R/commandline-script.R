# load packages & source scripts ------------------------------------------
setwd("~/GitHub_projects/multiview-oneclass/")
source("R/Functional/OCSVM-multiview.R")
source("R/Functional/construct-tabular-outlier.R")


# arff to csv -------------------------------------------------------------
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Annthyroid", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "yes")
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Annthyroid", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "no")
# 
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "InternetAds", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "yes")
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "InternetAds", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "no")
# 
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "PenDigits", dataset_pattern= "withoutdupl_norm_.*.arff", just_observe = "yes")
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "PenDigits", dataset_pattern= "withoutdupl_norm_.*.arff", just_observe = "no")
# 
# 
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "SpamBase", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "yes")
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "SpamBase", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "no")
# 
# 
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Stamps", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "yes")
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Stamps", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "no")
# 
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Waveform", dataset_pattern= "withoutdupl_norm_.*.arff", just_observe = "yes")
# Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Waveform", dataset_pattern= "withoutdupl_norm_.*.arff", just_observe = "no")



# Make tabular all the results.csv ----------------------------------------
fnames <- list.files("~/Downloads/DAMI_datasets/derived_data/", pattern = "results")
# fnames[1:30]
# Get_multiple_TabularOutlierScore(fnames[2])
# for(j in 3:30){
#   start1 <- Sys.time()
#   Get_multiple_TabularOutlierScore(fnames[j])
#   Sys.time() - start1
# }
# first_experiments_datasets <- fnames
first_experiments_datasets <- as.vector(readRDS("~/Downloads/DAMI_datasets/first_experiments_datasets.rds"))

# test dataset name: "Pima_withoutdupl_norm_02_v01"
list_names <- list()
for(i in 1:length(fnames)){
  list_names[[i]] <- stringi::stri_split(fnames[i], fixed = ".results.csv")[[1]][1]
}
all_names <-  as.vector(unlist(list_names))

first_experiments <- list()
for(i in 1:length(first_experiments_datasets)){
  first_experiments[[i]] <- stringi::stri_split(first_experiments_datasets[i], fixed = ".results.csv")[[1]][1]
}
first_experiments_names <-  as.vector(unlist(first_experiments))


experiments2_names <- base::setdiff(all_names, first_experiments_names)
experiments2_names <- readRDS("~/Downloads/DAMI_datasets/second_experiments_datasets.rds")

all_experiments <- c(first_experiments_names, experiments2_names)
all_experiments <- readRDS("~/Downloads/DAMI_datasets/all_experiments_datasets.rds")

latest_experiments <- setdiff(all_names, all_experiments)

args <- commandArgs(TRUE)
arg1 <- args[1]
arg2 <- args[2]
arg3 <- args[3]



if(arg1 == 1){
  for(j in arg2:arg3){
    print(first_experiments_datasets[j])
    run_unsupervised_multiview_per_dataset(datasetname = first_experiments_names[j])
  }
} else if(arg1 == 2 ){
  for(j in arg2:arg3){
    print(experiments2_names[j])
    # GetCsvFromArff(datasetname = experiments2_names[j])
    # print("arff to csv. DONE!")
    # Get_multiple_TabularOutlierScore(dataset_name = paste0(experiments2_names[2], ".results.csv"))
    # print("outliers tabular. DONE!")
    run_unsupervised_multiview_per_dataset(datasetname = experiments2_names[j])
  }
}

if(arg1 == 3){
  for(j in 1:10){
    print(latest_experiments[j])
    # GetCsvFromArff(datasetname = latest_experiments[j])
    # print("arff to csv. DONE!")
    # Get_multiple_TabularOutlierScore(dataset_name = paste0(latest_experiments[j], ".results.csv"))
    # print("outliers tabular. DONE!")
    run_unsupervised_multiview_per_dataset(datasetname = latest_experiments[j])
  }
}


