# load packages & source scripts ------------------------------------------
setwd("~/R Language Default Dir/Github-projects/multiview-oneclass/")
source("R/Functional/OCSVM-multiview.R")



# test dataset name: "Pima_withoutdupl_norm_02_v01"
args <- commandArgs(TRUE)
arg1 <- args[1]

for( i in c(0.1, 0.2)){
  for (j in c(0.01, 0.05)){
    print(Sys.time())
    print("start")
    res <- run_unsupervised_multiview_multipletimes(datasetname = arg1, 
                                                    percentage_OD = i, 
                                                    mixed_view_features = 1, 
                                                    Iter_outlier_features = 2, 
                                                    normal_size = j, 
                                                    Iters_normal_class = 3)
    print(Sys.time())
    print("end")
    
    }
}



# datasetname="Pima_withoutdupl_norm_02_v01"
# percentage_OD=0.2
# mixed_view_features=1
# Iter_outlier_features=2
# normal_size=0.01
# Iters_normal_class=2

