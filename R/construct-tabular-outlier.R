setwd("~/R Language Default Dir/multiview-oneclass/")
source("R/load-packages.R")


GetTabularOutlierScore <- function(datasetname) {
  
  text.loaded <- readtext::readtext(paste0("data/downloaded-data/", datasetname, ".csv"))
  
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
    fwrite(DTtabular, paste0("data/derived-data/", datasetname, ".csv"), nThread = 2)
  }
  }

# GetTabularOutlierScore(datasetname = "Ionosphere_withoutdupl_norm.results")
# GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v01.results")
# GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_02_v09.results")
# GetTabularOutlierScore(datasetname = "Pima_withoutdupl_norm_05_v07.results")
# GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v01.results")
# GetTabularOutlierScore(datasetname = "Shuttle_withoutdupl_norm_v05.results")
# GetTabularOutlierScore(datasetname = "Stamps_withoutdupl_norm_02_v06.results")
# GetTabularOutlierScore(datasetname = "Waveform_withoutdupl_norm_v01.results")
# GetTabularOutlierScore(datasetname = "Waveform_withoutdupl_norm_v02.results")
# GetTabularOutlierScore(datasetname = "WDBC_withoutdupl_norm_v07.results")


tt <- (list.files(path = "data/derived-data/"))
for(i in tt){
  dim(fread(i))
}
       

  