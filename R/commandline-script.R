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
fnames[1:30]
# Get_multiple_TabularOutlierScore(fnames[2])
# for(j in 3:30){
#   start1 <- Sys.time()
#   Get_multiple_TabularOutlierScore(fnames[j])
#   Sys.time() - start1
# }
first_experiments_datasets <- fnames
saveRDS(first_experiments_datasets, "~/Downloads/DAMI_datasets/first_experiments_datasets.rds")

# test dataset name: "Pima_withoutdupl_norm_02_v01"
list_names <- list()
for(i in 1:length(fnames)){
  list_names[[i]] <- stringi::stri_split(fnames[i], fixed = ".results.csv")[[1]][1]
}
all_names <-  unlist(list_names)
length(all_names)

args <- commandArgs(TRUE)
arg1 <- args[1]
arg2 <- args[2]


for(j in arg1:arg2){
  run_unsupervised_multiview_per_dataset(datasetname = all_names[j])
}


