
originalDT <- fread("data/derived-data/Pima_withoutdupl_norm_02_v01.csv")


one_randomOD <- list()
augmentedOD <- list()
for(i in 1:5){
  unsupervised_DTs1 <- create_unsupervised_view("Pima_withoutdupl_norm_02_v01", percentage_OD=1, mixed_view_features=1)
  tempDT <- unsupervised_DTs1$mixed_arthur
  tempDT1 <- dplyr::bind_cols(originalDT, tempDT)
  augmentedOD[[i]] <- tempDT1
  
  tempDT[, Label:= originalDT$Label]
  one_randomOD[[i]] <- tempDT
  }



UFS_1 <- copy(one_randomOD[[2]])
UFS_1[, Label:=NULL]
UFS_1_tsne <- tsne::tsne(X = UFS_1, k = 2)
UFS_1_tsne1 <- as.data.table(UFS_1_tsne)
UFS_1_tsne1[, Label:= originalDT$Label]
UFS_1_tsne1[, representation:= "UFS-RR"]



augmentedOD_1 <- copy(augmentedOD[[2]])
augmentedOD_1[, `:=` (Label = NULL, id = NULL)]
augmentedOD_1_tsne <- tsne::tsne(X = augmentedOD_1, k = 2)
augmentedOD_1_tsne1 <- as.data.table(augmentedOD_1_tsne)
augmentedOD_1_tsne1[, Label:= originalDT$Label]
augmentedOD_1_tsne1[, representation:= "AFS-RR"]



tempOriginal <- copy(originalDT)
tempOriginal[, `:=` (Label = NULL, id = NULL)]
tempOriginal_tsne <- tsne::tsne(X = tempOriginal, k = 2)
tempOriginal_tsne1 <- as.data.table(tempOriginal_tsne)
tempOriginal_tsne1[, Label:= originalDT$Label]
tempOriginal_tsne1[, representation:= "Original"]




All_DT <- rbindlist(list(UFS_1_tsne1,
                         augmentedOD_1_tsne1, 
                         tempOriginal_tsne1))
All_DT[, representation:= factor(representation, levels = c("Original", "UFS-RR", "AFS-RR"))]
setnames(x = All_DT, old = "Label", new = "Outlier")

DT <- rbindlist(list(All_DT))

#esquisse::esquisser()

All_DT[, unique(representation)]
All_DT[, Outlier:= as.factor(Outlier)]




#pima_All_DT <- copy(All_DT)
# Cardio_All_DT <- copy(All_DT)
# stamps_All_DT <- copy(All_DT)
# shuttle_All_DT <- copy(All_DT)

All_DT1 <- shuttle_All_DT

ggplot(All_DT1) +
  aes(x = V1, y = V2, colour = Outlier) +
  geom_point(size = 1L) +
  scale_color_hue() +
  theme_bw(base_size = 16) +
  facet_wrap(vars(representation))+
  scale_color_manual(values=c("#2ea906", "#c12200" ))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())




