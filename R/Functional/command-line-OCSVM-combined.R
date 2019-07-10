# Sys.setenv('R_MAX_VSIZE'=32000000000)
# Sys.getenv('R_MAX_VSIZE')

# Select the correct path for the src.R file
# source("~/GitHub_projects/multiview-oneclass/src.R")
# source("~/R Language Default Dir/Github-projects/multiview-oneclass/src.R")
args <- commandArgs(TRUE)
arg1 <- args[1]
arg2 <- args[2]



# arg1 <- "Shuttle_withoutdupl_norm_v01"
# arg2 <- "Shuttle_v01"
temp1 <- run_unsupervised_ensemble_per_dataset(datasetname = arg1,
                                                experiments = "OC_combined",
                                                input_mixed_view_features = 1,
                                                subfolder_name = arg2)

temp2 <- run_unsupervised_ensemble_per_dataset(datasetname = arg1, 
                                                experiments = "OC_combined",
                                                input_mixed_view_features = 2,
                                                subfolder_name = arg2)

temp3 <- run_unsupervised_ensemble_per_dataset(datasetname = arg1, 
                                                experiments = "OC_combined",
                                                input_mixed_view_features = 3,
                                                subfolder_name = arg2)


temp <- rbindlist(list(temp1, temp2, temp3))
temp[, mixedViewFeat:= as.factor(paste0(mixedViewFeat, "-OD parameters"))]
temp[, Normal_Size_1:= as.factor(Normal_Size_1)]
esquisser()
p4 <- ggplot(data = temp) +
  aes(x = mixedViewFeat, y = V2, fill = Representation) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(~Normal_Size_1, scale = "free") +
  geom_hline(yintercept = 0, color='red', size = 1.5) +
  #scale_y_continuous(breaks = seq(0.4, 0.9, 0.04))+
  labs(title = paste0("Ensemble on Combined-Spaces. Boxplots with different number of random selected Outlier Detection parameters and multiple Normal Class Training percentages."), 
     subtitle = paste0( "Dataset: ", arg1),
     y = "Standard Deviations of Mean AUC of Original-View", x = "")
p5 <- p4+theme(legend.position="top") + scale_fill_manual(values=c("#b1cb49", "#515f58"))
p5

ggsave(plot = p5, filename = paste0(final_path_to_save, "figures/",  
                                    arg2,"/",arg1, ".pdf"),
       width = 18, height = 10, units = "in", dpi = 300)



