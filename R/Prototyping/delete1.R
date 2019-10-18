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



# 1 random experiments ----------------------------------------------------


system.time({t <- run_unsupervised_multiviem_1random(datasetname = "InternetAds_withoutdupl_norm_02_v10",
                                                     mixed_view_features = 1,
                                                     Iter_outlier_features = 10,
                                                     normal_size = 0.2,
                                                     Iters_normal_class = 50, percentage_OD = 1)})
temp22 <- t[[2]]
temp22[, Features_Iteration:= as.factor(Features_Iteration)]


p <- ggplot(data = temp22) +
  aes(x = Features_Iteration, y = V1, fill = Representation) +
  geom_boxplot() +
  theme_minimal() +
  theme_minimal() + scale_y_continuous(breaks = seq(0.3, 1.0,0.05)) +
  labs(y = "AUC")
p



temp33 <- t[[1]]
average_ense <- temp33[, mean(Scores), by = c("Representation", "Normal_Iteration", "id")]
Labels <- temp33[Normal_Iteration == 1 & Representation == "12-Scores-random", Label]
average_ense[, Label:= rep(Labels, 4)]
auc_res <- average_ense[, auc(Label, V1), by =  c("Representation", "Normal_Iteration")]

winner_me <- 0
winner_original <- 0
for(i in 1:10){
  hh <- auc_res[Normal_Iteration==i]
  if(hh[Representation == "12-Scores-random", V1] > hh[Representation == "Original-View", V1]){
    winner_me <- winner_me + 1
  } else{
    winner_original <- winner_original + 1
  }
}

p1 <- ggplot(data = auc_res) +
  aes(x = Representation, y = V1, fill = Representation) +
  geom_boxplot() +
  theme_minimal() +
  theme_minimal() + scale_y_continuous(breaks = seq(0.3, 1.0,0.05)) +
  theme(legend.position = "none") +
  labs(y= "AUC")
# +
#   labs(title = paste0("Ensmble multiple view vs Original view"),
#        subtitle = paste0("Dataset: "), y = "AUC") +


p1


# Change colnames of Parkinson --------------------------------------------
Parkinson_07 <- fread("data/derived-data/OC_Combined_CV/figures/Parkinson/Parkinson_withoutdupl_norm_05_v07.results.csv")
setnames(Parkinson_07, names(Parkinson_07)[2:10], paste0("KNN-00", 1:9))
setnames(Parkinson_07, names(Parkinson_07)[11:50], paste0("KNN-0", 10:49))

setnames(Parkinson_07, names(Parkinson_07)[51:59], paste0("KNNW-00", 1:9))
setnames(Parkinson_07, names(Parkinson_07)[60:99], paste0("KNNW-0", 10:49))

setnames(Parkinson_07, names(Parkinson_07)[(99+1):(99+9)], paste0("LOF-00", 1:9))
setnames(Parkinson_07, names(Parkinson_07)[(99+9+1):(99+9+40)], paste0("LOF-0", 10:49))


setnames(Parkinson_07, names(Parkinson_07)[(148+1):(148+9)], paste0("SimplifiedLOF-00", 1:9))
setnames(Parkinson_07, names(Parkinson_07)[(148+9+1):(148+9+40)], paste0("SimplifiedLOF-0", 10:49))

setnames(Parkinson_07, names(Parkinson_07)[(197+1):(197+9)], paste0("LoOP-00", 1:9))
setnames(Parkinson_07, names(Parkinson_07)[(197+9+1):(197+9+40)], paste0("LoOP-0", 10:49))

setnames(Parkinson_07, names(Parkinson_07)[(246+1):(246+8)], paste0("LDOF-00", 2:9))
setnames(Parkinson_07, names(Parkinson_07)[(246+8+1):(246+8+40)], paste0("LDOF-0", 10:49))


setnames(Parkinson_07, names(Parkinson_07)[(294+1):(294+9)], paste0("ODIN-00", 1:9))
setnames(Parkinson_07, names(Parkinson_07)[(294+9+1):(294+9+40)], paste0("ODIN-0", 10:49))


setnames(Parkinson_07, names(Parkinson_07)[(343+1):(343+7)], paste0("FastABOD-00", 3:9))
setnames(Parkinson_07, names(Parkinson_07)[(343+7+1):(343+7+40)], paste0("FastABOD-0", 10:49))


setnames(Parkinson_07, names(Parkinson_07)[(390+1):(390+8)], paste0("KDEOS-00", 2:9))
setnames(Parkinson_07, names(Parkinson_07)[(390+8+1):(390+8+40)], paste0("KDEOS-0", 10:49))

setnames(Parkinson_07, names(Parkinson_07)[(438+1):(438+9)], paste0("LDF-00", 1:9))
setnames(Parkinson_07, names(Parkinson_07)[(438+9+1):(438+9+40)], paste0("LDF-0", 10:49))

setnames(Parkinson_07, names(Parkinson_07)[(487+1):(487+9)], paste0("INFLO-00", 1:9))
setnames(Parkinson_07, names(Parkinson_07)[(487+9+1):(487+9+40)], paste0("INFLO-0", 10:49))


setnames(Parkinson_07, names(Parkinson_07)[(536+1):(536+9)], paste0("COF-00", 1:9))
setnames(Parkinson_07, names(Parkinson_07)[(536+9+1):(536+9+40)], paste0("COF-0", 10:49))
fwrite(Parkinson_07, "data/derived-data/OC_Combined_CV/figures/Parkinson/Parkinson_withoutdupl_norm_05_v07.results.csv")



# Parkinson v01 -----------------------------------------------------------

Parkinson_01 <- fread("data/derived-data/OC_Combined_CV/figures/Parkinson/Parkinson_withoutdupl_norm_05_v01.results.csv")
dim(Parkinson_01)
setnames(Parkinson_01, names(Parkinson_01)[2:10], paste0("KNN-00", 1:9))
setnames(Parkinson_01, names(Parkinson_01)[11:50], paste0("KNN-0", 10:49))

setnames(Parkinson_01, names(Parkinson_01)[51:59], paste0("KNNW-00", 1:9))
setnames(Parkinson_01, names(Parkinson_01)[60:99], paste0("KNNW-0", 10:49))

setnames(Parkinson_01, names(Parkinson_01)[(99+1):(99+9)], paste0("LOF-00", 1:9))
setnames(Parkinson_01, names(Parkinson_01)[(99+9+1):(99+9+40)], paste0("LOF-0", 10:49))


setnames(Parkinson_01, names(Parkinson_01)[(148+1):(148+9)], paste0("SimplifiedLOF-00", 1:9))
setnames(Parkinson_01, names(Parkinson_01)[(148+9+1):(148+9+40)], paste0("SimplifiedLOF-0", 10:49))

setnames(Parkinson_01, names(Parkinson_01)[(197+1):(197+9)], paste0("LoOP-00", 1:9))
setnames(Parkinson_01, names(Parkinson_01)[(197+9+1):(197+9+40)], paste0("LoOP-0", 10:49))

setnames(Parkinson_01, names(Parkinson_01)[(246+1):(246+8)], paste0("LDOF-00", 2:9))
setnames(Parkinson_01, names(Parkinson_01)[(246+8+1):(246+8+40)], paste0("LDOF-0", 10:49))


setnames(Parkinson_01, names(Parkinson_01)[(294+1):(294+9)], paste0("ODIN-00", 1:9))
setnames(Parkinson_01, names(Parkinson_01)[(294+9+1):(294+9+40)], paste0("ODIN-0", 10:49))


setnames(Parkinson_01, names(Parkinson_01)[(343+1):(343+7)], paste0("FastABOD-00", 3:9))
setnames(Parkinson_01, names(Parkinson_01)[(343+7+1):(343+7+40)], paste0("FastABOD-0", 10:49))


setnames(Parkinson_01, names(Parkinson_01)[(390+1):(390+8)], paste0("KDEOS-00", 2:9))
setnames(Parkinson_01, names(Parkinson_01)[(390+8+1):(390+8+40)], paste0("KDEOS-0", 10:49))

setnames(Parkinson_01, names(Parkinson_01)[(438+1):(438+9)], paste0("LDF-00", 1:9))
setnames(Parkinson_01, names(Parkinson_01)[(438+9+1):(438+9+40)], paste0("LDF-0", 10:49))

setnames(Parkinson_01, names(Parkinson_01)[(487+1):(487+9)], paste0("INFLO-00", 1:9))
setnames(Parkinson_01, names(Parkinson_01)[(487+9+1):(487+9+40)], paste0("INFLO-0", 10:49))


setnames(Parkinson_01, names(Parkinson_01)[(536+1):(536+9)], paste0("COF-00", 1:9))
setnames(Parkinson_01, names(Parkinson_01)[(536+9+1):(536+9+40)], paste0("COF-0", 10:49))
fwrite(Parkinson_01, "data/derived-data/OC_Combined_CV/figures/Parkinson/Parkinson_withoutdupl_norm_05_v01.results.csv")


















