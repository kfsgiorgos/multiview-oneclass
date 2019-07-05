# Sys.setenv('R_MAX_VSIZE'=32000000000)
# Sys.getenv('R_MAX_VSIZE')

args <- commandArgs(TRUE)
arg1 <- args[1]



temp1 <- run_unsupervised_multiview_per_dataset(datasetname = arg1,
                                                experiments = "OC_combined",
                                                input_mixed_view_features = 1)

temp2 <- run_unsupervised_multiview_per_dataset(datasetname = arg1, 
                                                experiments = "OC_combined",
                                                input_mixed_view_features = 2)

temp3 <- run_unsupervised_multiview_per_dataset(datasetname = arg1, 
                                                experiments = "OC_combined",
                                                input_mixed_view_features = 3)


temp <- rbindlist(list(temp1, temp2, temp3))
temp[, mixedViewFeat:= as.factor(paste0(mixedViewFeat, "-OD parameters"))]

p4 <- ggplot(data = temp) +
  aes(x = mixedViewFeat, y = V1, fill = Representation) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(vars(Normal_size)) + 
  scale_y_continuous(breaks = seq(0.4, 0.9, 0.04))+
  labs(title = paste0("Boxplot of AUC Combined-Spaces with different number of OD parameters and multiple Normal Class percentages."), 
     subtitle = paste0( "Dataset: ", arg1),
     y = "Standard Deviations of Mean AUC of Original-View", x = "")
p4 <- p4+theme(legend.position="top")
p4

ggsave(plot = p4, filename = paste0(final_path_to_save, "figures/",  
                                    arg1, ".pdf"),
       width = 12, height = 6, units = "in", dpi = 300)



