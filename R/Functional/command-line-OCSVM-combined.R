# Sys.setenv('R_MAX_VSIZE'=32000000000)
# Sys.getenv('R_MAX_VSIZE')

# Select the correct path for the src.R file
# source("~/GitHub_projects/multiview-oneclass/src.R")
source("~/R Language Default Dir/Github-projects/multiview-oneclass/src.R")
args <- commandArgs(TRUE)
arg1 <- args[1]
arg2 <- args[2]
arg3 <- args[3]

# arg1 <- "Parkinson_withoutdupl_norm_05_v03"
# arg2 <- "Parkinson"
# res1 <- run_unsupervised_ensemble_per_dataset(datasetname = arg1,
#                                                 experiments = "OC_combined",
#                                                 input_mixed_view_features = 1,
#                                                 subfolder_name = arg2)
# gc()
# res2 <- run_unsupervised_ensemble_per_dataset(datasetname = arg1, 
#                                                 experiments = "OC_combined",
#                                                 input_mixed_view_features = 2,
#                                                 subfolder_name = arg2)
# gc()
# res3 <- run_unsupervised_ensemble_per_dataset(datasetname = arg1, 
#                                                 experiments = "OC_combined",
#                                                 input_mixed_view_features = 3,
#                                                 subfolder_name = arg2)
# 
# gc()
# res <- rbindlist(list(res1, res2, res3))
# res[, mixedViewFeat:= as.factor(paste0(mixedViewFeat, "-OD parameters"))]
# res[, Normal_Size_1:= as.factor(Normal_Size_1)]
# 
# p <- ggplot(data = res) +
#   aes(x = mixedViewFeat, y = V2, fill = Representation) +
#   geom_boxplot() +
#   theme_minimal() +
#   facet_wrap(~Normal_Size_1, scale = "free") +
#   geom_hline(yintercept = 0, color='red', size = 1.5) +
#   #scale_y_continuous(breaks = seq(0.4, 0.9, 0.04))+
#   labs(title = paste0("Ensemble on Combined-Spaces. Boxplots with different number of random selected Outlier Detection parameters and multiple Normal Class Training percentages."), 
#      subtitle = paste0( "Dataset: ", arg1),
#      y = "Standard Deviations of Mean AUC of Original-View", x = "")
# p1 <- p+theme(legend.position="top") + scale_fill_manual(values=c("#b1cb49", "#515f58"))
# p1
# 
# ggsave(plot = p1, filename = paste0(final_path_to_save, "figures/",  
#                                     arg2,"/",arg1, ".pdf"),
#        width = 18, height = 10, units = "in", dpi = 300)



get_CV_experiments(datasetname = arg1, subfolder_name = arg2, 
                   experiments = "OC_combined_CV", CViterations = arg3)


