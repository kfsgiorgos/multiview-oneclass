# arff to csv -------------------------------------------------------------
Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Annthyroid", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "yes")
Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Annthyroid", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "no")

Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "InternetAds", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "yes")
Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "InternetAds", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "no")

Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "PenDigits", dataset_pattern= "withoutdupl_norm_.*.arff", just_observe = "yes")
Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "PenDigits", dataset_pattern= "withoutdupl_norm_.*.arff", just_observe = "no")


Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "SpamBase", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "yes")
Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "SpamBase", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "no")


Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Stamps", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "yes")
Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Stamps", dataset_pattern= "withoutdupl_norm_02_.*.arff", just_observe = "no")

Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Waveform", dataset_pattern= "withoutdupl_norm_.*.arff", just_observe = "yes")
Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Waveform", dataset_pattern= "withoutdupl_norm_.*.arff", just_observe = "no")



# Make tabular all the results.csv ----------------------------------------
fnames <- list.files("~/Downloads/DAMI_datasets/derived_data/", pattern = "results")
fnames
Get_multiple_TabularOutlierScore(fnames[2])
for(j in 3:30){
  start1 <- Sys.time()
  Get_multiple_TabularOutlierScore(fnames[j])
  Sys.time() - start1
}


# test dataset name: "Pima_withoutdupl_norm_02_v01"
args <- commandArgs(TRUE)
arg1 <- args[1]

for( i in c(0.1, 0.2)){
  for (j in c(0.01, 0.05)){
    print(Sys.time())
    print("start")
    res <- run_unsupervised_multiview_multipletimes(datasetname = "Waveform_withoutdupl_norm_v01", 
                                                    percentage_OD = i, 
                                                    mixed_view_features = 1, 
                                                    Iter_outlier_features = 2, 
                                                    normal_size = j, 
                                                    Iters_normal_class = 3)
    print(Sys.time())
    print("end")
    
    }
}



# datasetname="Waveform_withoutdupl_norm_v01"
# percentage_OD=0.2
# mixed_view_features=1
# Iter_outlier_features=2
# normal_size=0.01
# Iters_normal_class=2

