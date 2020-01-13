GetTabularOutlierScore <- function(datasetname) {

  # This function read a XXXXXresults.csv from the DAMI repository and 
  # transforms it to a proper tabular csv file. Before we apply thid function, 
  # we have to unzip the XXXX.results.csv file
  

  # fnames <- list.files(paste0("~/Downloads/", datasetname,  "/"))
  # text.loaded <- readtext::readtext(paste0("data/downloaded-data/", datasetname, ".results.csv"))
  text.loaded <- readtext::readtext(paste0("~/Downloads/", datasetname, ".results.csv"))
  

  
  list.columns <- list()
  for (i in 1:dim(text.loaded)[1]) {
    DTtext <- as.data.table(text.loaded$text[i])
    DTtext1 <- as.data.table(str_split(DTtext, " "))
    if(i == 1){
      setnames(DTtext1, "V1", "Label")
      DT <- DTtext1[-1] 
      list.columns[[i]] <- DT
    } else{
      setnames(DTtext1, "V1", DTtext1$V1[1])
      DT <- DTtext1[-1] 
      list.columns[[i]] <- DT
    }
    DTtabular <- dplyr::bind_cols(list.columns)

    
    # fwrite(DTtabular, paste0("data/derived-data/", datasetname, ".results.csv"), nThread = 2)
    fwrite(DTtabular, paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".results.csv"), nThread = 20)

  }
}

# Examples:
# GetTabularOutlierScore(datasetname = "Ionosphere_withoutdupl_norm")
# GetTabularOutlierScore(datasetname = "Parkinson_withoutdupl_norm_10_v03")

# create csv from arff ----------------------------------------------------

GetCsvFromArff <- function(datasetname) {
  # This function works only when you have downloaded an .arff dataset 
  # in a spevific directory
  # DT <- as.data.table(foreign::read.arff(paste0("data/downloaded-data/", datasetname, ".arff")))
  DT<- as.data.table(foreign::read.arff(paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".arff")))
  setnames(DT, old = "outlier", "Label")
  # fwrite(DT, paste0("data/derived-data/", datasetname, ".csv"), nThread = 2)
  fwrite(DT, paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".csv"), nThread = 20)
  
}

# Examples
# GetCsvFromArff(datasetname = "Ionosphere_withoutdupl_norm")
# GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v01")



Get_multiple_CsvFromArff <- function(datasetspath, dataset_name, dataset_pattern, just_observe) {
  # We have create a directory which contains multiple folders. 
  # Each folder contain all the arff files. 

  # Objective: We want to select only the "withoutdupl_norm_*" files
  # Example: Get_multiple_CsvFromArff(datasetspath = "~/Downloads/DAMI_datasets/", dataset_name = "Annthyroid", dataset_pattern= "withoutdupl_norm_02_.*.arff")
  

  dataset_path_constructed <- paste0(datasetspath, dataset_name, "/")
  fnames <- list.files(dataset_path_constructed, pattern = dataset_pattern)
  
  if(just_observe == "yes"){
    print(fnames)
  } else{
    for(i in 1:length(fnames)){
      DT <- as.data.table(foreign::read.arff(paste0(dataset_path_constructed, fnames[[i]])))
      print(DT)
      names_tosave <- stringi::stri_split(str = fnames[[i]], fixed = ".arff")[[1]][1]
      fwrite(DT, paste0(dataset_path_constructed, names_tosave, ".csv"), nThread = 2)
    }
  }
  }


Get_multiple_TabularOutlierScore <- function(dataset_name) {
  # This functions works when you have all the results CSVs in one folder.
  # It reads all the results.csv that you have and makes each ot them in a tabular format
  
  text.loaded <- readtext::readtext(paste0("~/Downloads/DAMI_datasets/derived_data/", dataset_name))
    
  list.columns <- list()
  for (i in 1:dim(text.loaded)[1]) {
    DTtext <- as.data.table(text.loaded$text[i])
    DTtext1 <- as.data.table(str_split(DTtext, " "))
    if(i == 1){
      setnames(DTtext1, "V1", "Label")
      DT <- DTtext1[-1] 
      list.columns[[i]] <- DT
    } else{
      setnames(DTtext1, "V1", DTtext1$V1[1])
      DT <- DTtext1[-1] 
      list.columns[[i]] <- DT
    }
    DTtabular <- dplyr::bind_cols(list.columns)
    fwrite(DTtabular, paste0("~/Downloads/DAMI_datasets/derived_data/", dataset_name), nThread = 10)
  }
  }
    
    
    
