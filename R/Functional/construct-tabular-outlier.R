GetTabularOutlierScore <- function(datasetname) {

  
  # The dataset name must always have the ".results" suffix

  # fnames <- list.files(paste0("~/Downloads/", datasetname,  "/"))
  # text.loaded <- readtext::readtext(paste0("data/downloaded-data/", datasetname, ".csv"))
  text.loaded <- readtext::readtext(paste0("~/Downloads/", datasetname, ".csv"))
  # text.loaded <- readtext::readtext(paste0("data/downloaded-data/", datasetname, ".csv"))

  
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

    
    # fwrite(DTtabular, paste0("data/derived-data/", datasetname, ".csv"), nThread = 2)
    fwrite(DTtabular, paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".csv"), nThread = 20)

  }
}

# Examples:
# GetTabularOutlierScore(datasetname = "Ionosphere_withoutdupl_norm.results")
# GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v01.results")
# GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v09.results")
# GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_05_v07.results")
# GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v01.results")
# GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v04.results")
# GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v06.results")
# GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v07.results")
# GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v08.results")
# GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v09.results")
# GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v10.results")
# GetTabularOutlierScore(datasetname = "Cardiotocography_withoutdupl_norm_05_v01.results")
# GetTabularOutlierScore(datasetname = "Cardiotocography_withoutdupl_norm_05_v02.results")
# GetTabularOutlierScore(datasetname = "Cardiotocography_withoutdupl_norm_05_v03.results")
# GetTabularOutlierScore(datasetname = "Cardiotocography_withoutdupl_norm_05_v04.results")
# GetTabularOutlierScore(datasetname = "Cardiotocography_withoutdupl_norm_05_v05.results")

# GetTabularOutlierScore(datasetname = "Stamps_withoutdupl_norm_02_v06.results")
# GetTabularOutlierScore(datasetname = "Waveform_withoutdupl_norm_v01.results")
# GetTabularOutlierScore(datasetname = "Waveform_withoutdupl_norm_v02.results")
# GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v07.results")
# system.time({GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v01.results")})
# system.time({GetTabularOutlierScore(datasetname = "Arrhythmia_withoutdupl_norm_02_v01.results")})
# system.time({GetTabularOutlierScore(datasetname = "Arrhythmia_withoutdupl_norm_02_v02.results")})
# system.time({GetTabularOutlierScore(datasetname = "Arrhythmia_withoutdupl_norm_02_v03.results")})
# system.time({GetTabularOutlierScore(datasetname = "Cardiotocography_withoutdupl_norm_05_v03.results")})
# system.time({GetTabularOutlierScore(datasetname = "Cardiotocography_withoutdupl_norm_05_v08.results")})
# system.time({GetTabularOutlierScore(datasetname = "Glass_withoutdupl_norm.results")})
# system.time({GetTabularOutlierScore(datasetname = "Ionosphere_withoutdupl_norm.results")})
# system.time({GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v01.results")})
# system.time({GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v02.results")})
# system.time({GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v03.results")})
# system.time({GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v01.results")})
# system.time({GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v02.results")})
# system.time({GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v03.results")})
# system.time({GetTabularOutlierScore(datasetname = "Parkinson_withoutdupl_norm_05_v03.results")})
# system.time({GetTabularOutlierScore(datasetname = "KDDCup99_withoutdupl_catremoved.results")})
# system.time({GetTabularOutlierScore(datasetname = "ALOI_withoutdupl_norm.results")})
# system.time({GetTabularOutlierScore(datasetname = "HeartDisease_withoutdupl_norm_02_v08.results")})
# create csv from arff ----------------------------------------------------

GetCsvFromArff <- function(datasetname) {
  #This function works only when you have the arff data in one directory
  # DT <- as.data.table(foreign::read.arff(paste0("data/downloaded-data/", datasetname, ".arff")))
  DT<- as.data.table(foreign::read.arff(paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".arff")))
  setnames(DT, old = "outlier", "Label")
  #fwrite(DT, paste0("data/derived-data/", datasetname, ".csv"), nThread = 2)
  fwrite(DT, paste0("~/Downloads/DAMI_datasets/derived_data/", datasetname, ".csv"), nThread = 20)
  
}


# GetCsvFromArff(datasetname = "Ionosphere_withoutdupl_norm")
# GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v01")
# GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v02")
# GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v03")
# GetCsvFromArff(datasetname = "Shuttle_withoutdupl_norm_v01")
# GetCsvFromArff(datasetname = "Shuttle_withoutdupl_norm_v02")
# GetCsvFromArff(datasetname = "Arrhythmia_withoutdupl_norm_02_v01")
# GetCsvFromArff(datasetname = "Arrhythmia_withoutdupl_norm_02_v02")
# GetCsvFromArff(datasetname = "Arrhythmia_withoutdupl_norm_02_v03")
# GetCsvFromArff(datasetname = "Glass_withoutdupl_norm")
# GetCsvFromArff(datasetname = "Parkinson_withoutdupl_norm_05_v01")
# GetCsvFromArff(datasetname = "Parkinson_withoutdupl_norm_05_v02")
# GetCsvFromArff(datasetname = "Parkinson_withoutdupl_norm_05_v03")
# GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v03")
# GetCsvFromArff(datasetname = "Shuttle_withoutdupl_norm_v04")
# GetCsvFromArff(datasetname = "Stamps_withoutdupl_norm_02_v06")
# GetCsvFromArff(datasetname = "Waveform_withoutdupl_norm_v01")
# GetCsvFromArff(datasetname = "Waveform_withoutdupl_norm_v02")
# GetCsvFromArff(datasetname = "WDBC_withoutdupl_norm_v01")
# system.time({GetCsvFromArff(datasetname = "KDDCup99_withoutdupl_catremoved")})
# print(Sys.time())
# system.time({GetCsvFromArff(datasetname = "WDBC_withoutdupl_norm_v01")})
# system.time({GetCsvFromArff(datasetname = "Arrhythmia_withoutdupl_norm_02_v01")})
# system.time({GetCsvFromArff(datasetname = "Arrhythmia_withoutdupl_norm_02_v02")})
# system.time({GetCsvFromArff(datasetname = "Arrhythmia_withoutdupl_norm_02_v03")})
# system.time({GetCsvFromArff(datasetname = "Cardiotocography_withoutdupl_norm_05_v03")})
# system.time({GetCsvFromArff(datasetname = "Cardiotocography_withoutdupl_norm_05_v08")})
# system.time({GetCsvFromArff(datasetname = "Glass_withoutdupl_norm")})
# system.time({GetCsvFromArff(datasetname = "Ionosphere_withoutdupl_norm")})
# system.time({GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v01")})
# system.time({GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v02")})
# system.time({GetCsvFromArff(datasetname = "Pima_withoutdupl_norm_02_v03")})
# system.time({GetCsvFromArff(datasetname = "Shuttle_withoutdupl_norm_v01")})
# system.time({GetCsvFromArff(datasetname = "Shuttle_withoutdupl_norm_v02")})
# system.time({GetCsvFromArff(datasetname = "ALOI_withoutdupl_norm")})
# GetCsvFromArff(datasetname = "Shuttle_withoutdupl_norm_v06")
# GetCsvFromArff(datasetname = "Shuttle_withoutdupl_norm_v07")
# GetCsvFromArff(datasetname = "Shuttle_withoutdupl_norm_v08")
# GetCsvFromArff(datasetname = "Shuttle_withoutdupl_norm_v09")
# GetCsvFromArff(datasetname = "Shuttle_withoutdupl_norm_v10")
# GetCsvFromArff(datasetname = "Cardiotocography_withoutdupl_norm_05_v01")
# GetCsvFromArff(datasetname = "Cardiotocography_withoutdupl_norm_05_v02")
# GetCsvFromArff(datasetname = "Cardiotocography_withoutdupl_norm_05_v03")
# GetCsvFromArff(datasetname = "Cardiotocography_withoutdupl_norm_05_v04")
# GetCsvFromArff(datasetname = "Cardiotocography_withoutdupl_norm_05_v05")



# example: Get_multiple_CsvFromArff(datasetpath = "~/Downloads/ALOI/")
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
    
    
    
