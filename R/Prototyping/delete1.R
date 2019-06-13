# load packages & source scripts ------------------------------------------
setwd("~/GitHub_projects/multiview-oneclass/")
source("R/Functional/OCSVM-multiview.R")
source("R/Functional/construct-tabular-outlier.R")


fnames <- list.files("~/Downloads/DAMI_datasets/derived_data/", pattern = "results")
fnames[20]
# Get_multiple_TabularOutlierScore(fnames[1])

for(j in 31:length(fnames)){
  print("=========")
  print(j)
  start1 <- Sys.time()
  print(start1)
  Get_multiple_TabularOutlierScore(fnames[j])
  Sys.time() - start1
}


print(Sys.time())
ee <- run_unsupervised_multiview_multipletimes(datasetname = "InternetAds_withoutdupl_norm_02_v10", 
                                         percentage_OD = 0.2, 
                                         mixed_view_features = 1, 
                                         Iter_outlier_features = 2, 
                                         normal_size = 0.2, Iters_normal_class = 3)

temp <- ee[[2]]
temp[Representation == "Original-View", Iteration_Features:=1]
temp[, Iteration_Features:= as.factor(Iteration_Features)]
yintercept1 <- temp[Representation == "Original-View", mean(V1)]

dt<-temp[Normal_Iteration ==1& Iteration_Features==1]
dt1 <- dt[-.N]
comb1 <- combn(unique(dt1$Representation), m = 2)
KNN <- comb1[, 14]
dt1[Representation == KNN[1], V1] > dt1[Representation == KNN[2], V1]

KNN <- comb1[, 27] 
KNN <- comb1[, 39]
KNN <- comb1[, 50]
KNN <- comb1[, 60]
KNN <- comb1[, 69]
KNN <- comb1[, 77]
KNN <- comb1[, 84]
KNN <- comb1[, 90]
KNN <- comb1[, 95]
KNN <- comb1[, 99]
KNN <- comb1[, 102]
KNN <- comb1[, 104]
KNN <- comb1[, 105]


# esquisser()
p <- ggplot(data = temp) +
  aes(x = Representation, y = V1, fill = Iteration_Features) +
  geom_boxplot() +
  theme_minimal() +
  theme_minimal() + scale_y_continuous(breaks = seq(0.3, 1.0,0.05))+
  geom_hline(yintercept = yintercept1)


p  
# + 
#   
# +
#   labs(title = paste0(percentage_OD1 * 100,
#                       "% randomly selected features for each Representation Algo & ",
#                       normal_sample_size1 * 100, "% randomly selected Normal data (", Iter1 ," times)."),
#        subtitle = paste0("Dataset: ", datasetname1), y = "AUC")


ggsave(plot = p, filename = paste0("figures/sample_OD_many/", datasetname1, 
                                   "_normalsize_", normal_sample_size1, 
                                   "_percentageOD_", percentage_OD1, ".pdf"), 
       width = 14, height = 7, units = "in", dpi = 300)





ee[[2]][, mean(V1), by = c("Normal_Iteration", "Representation")][, max(V1), by =Representation]
ee[[2]][, mean(V1), by = c("Normal_Iteration", "Representation")][, mean(V1), by =Representation]