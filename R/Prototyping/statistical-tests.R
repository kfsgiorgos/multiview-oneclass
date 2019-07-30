# data(data_gh_2008)
# head(data.gh.2008)


get_plots_pvalues <- function(input_res_DT) {
  res_DT <- input_res_DT
  
  pvalues_median_list <- list()
  pvalues_quantiles_list <- list()
  list_quantilesDT <- list()
  list_medianDT <- list()
  i <- 0
  
  for(dataset_i in res_DT[[2]][, unique(Dataset)]){
    i <- i + 1
    mean_DT <- res_DT[[2]][Dataset == dataset_i][, .SD[sample(1:.N, 500, replace = F), max(V2)], by = .(`Cross-Validation`, Representation)]
    original_DT <<- res_DT[[3]][Dataset == dataset_i]
    original_DT[, .N, by = .(`Cross-Validation`)]
    original_DT1 <- original_DT[, .SD[sample(1:.N, 50, replace = F), max(V2)], by = .(`Cross-Validation`)]
    
    original_DT1[, Representation:= "Original"]
    setcolorder(original_DT1, neworder = c(1,3,2))
    
    combined_original <- rbindlist(list(mean_DT[Representation=="Combined"], original_DT1))
    combined <- combined_original[Representation=="Combined", V1]
    original <- combined_original[Representation=="Original", V1]
    
    random1_original <- rbindlist(list(mean_DT[Representation=="1-RandomOD"], original_DT1))
    random1 <- random1_original[Representation=="1-RandomOD", V1]
    
    DT_all <- rbindlist(list(combined_original, random1_original[Representation=="1-RandomOD"]))
    mean_sd <- DT_all[, .(Mean = mean(V1), SD = sd(V1)), by = Representation]
    
    DT_all[Representation == "Combined", sdAUC:= (V1 - mean_sd[Representation == "Original", Mean])/mean_sd[Representation == "Original", SD]]
    DT_all[Representation == "1-RandomOD", sdAUC:= (V1 - mean_sd[Representation == "Original", Mean])/mean_sd[Representation == "Original", SD]]
    DT_all[Representation == "Original", sdAUC:= 0]
    DT_all[, Representation:=as.factor(Representation)]
    DT_all[, Evaluation := "Median"]
    DT_all[, Dataset := dataset_i]
    list_medianDT[[i]] <- DT_all
    
    p <- ggplot(data = DT_all) +
      aes(x = Representation, y = V1, fill = Representation) +
      geom_boxplot() +
      theme_minimal()+
      labs(title = paste0("Evaluation: Median of all CVs. ", dataset_i), y = " AUC")+
      theme(legend.position = "none")
    print(p)
    
    
    
    p_sd <- ggplot(data = DT_all[Representation!="Original"]) +
      aes(x = Representation, y = sdAUC, fill = Representation) +
      geom_boxplot() +
      theme_minimal()+
      geom_hline(yintercept = 0)+
      labs(title = paste0("Evaluation: Median of all CVs. ", dataset_i), y = "Standard Deviations of Mean AUC of Original features")+
      theme(legend.position = "none")
    print(p_sd)
    
    pvalues_median_list[[i]] <- data.table(combined_original = wilcox.test(combined, original, paired = FALSE, alternative = "greater")["p.value"],
                                           random1_original = wilcox.test(random1, original, paired = FALSE, alternative = "greater")["p.value"],
                                           combined_random = wilcox.test(combined, random1, paired = FALSE, alternative = "greater")["p.value"],
                                           Dataset = dataset_i, Evaluation = "median")
    
    print(dataset_i)
  }
  
  # Alternative -------------------------------------------------------------
  i <- 0
  for(dataset_i in res_DT[[2]][, unique(Dataset)]){
    
    i <- i + 1
    temp <- res_DT[[2]][Dataset == dataset_i]
    quantiles <- temp[, .SD[1:.N, quantile(V2, probs = 0.99)], by = .(`Cross-Validation`, Representation, features_Iteration)]
    
    merged_1randomDT <- temp[quantiles, on = c("Cross-Validation", "Representation", "features_Iteration")]
    #merged_1randomDT[V1 >= i.V1][, .N, by = .(`Cross-Validation`, Representation, features_Iteration)]
    
    DT_hyper <- merged_1randomDT[V1 >= i.V1, .SD[sample(1:.N, 5, replace = T), mean(V2)], by = .(`Cross-Validation`, Representation, features_Iteration)]
    DT_hyper_final <- DT_hyper[, median(V1), by = .(`Cross-Validation`, Representation)]
    
    
    original_DT <- res_DT[[3]][Dataset == dataset_i]
    quantiles_original <- original_DT[, .SD[1:.N, quantile(V2, probs = 0.99)], by = .(`Cross-Validation`)]
    merged_originalDT <- original_DT[quantiles_original, on = c("Cross-Validation")]
    DT_hyper_original <- merged_originalDT[V1 >= i.V1, .SD[sample(1:.N, 5, replace = T), max(V2)], by = .(`Cross-Validation`)]
    DT_hyper_original[, Representation:="Original"]
    setcolorder(DT_hyper_original, c(1,3,2))
    
    
    
    combined_original_1 <- rbindlist(list(DT_hyper_final[Representation=="Combined"], DT_hyper_original))
    combined_new <- combined_original_1[Representation=="Combined", V1]
    original_new <- combined_original_1[Representation=="Original", V1]
    
    random1_original <- rbindlist(list(DT_hyper_final[Representation=="1-RandomOD"], DT_hyper_original))
    random1_new <- random1_original[Representation=="1-RandomOD", V1]
    
    DT_all1 <- rbindlist(list(combined_original_1, random1_original[Representation=="1-RandomOD"]))
    
    mean_sd1 <- DT_all1[, .(Mean = mean(V1), SD = sd(V1)), by = Representation]
    
    DT_all1[Representation == "Combined", sdAUC:= (V1 - mean_sd1[Representation == "Original", Mean])/mean_sd1[Representation == "Original", SD]]
    DT_all1[Representation == "1-RandomOD", sdAUC:= (V1 - mean_sd1[Representation == "Original", Mean])/mean_sd1[Representation == "Original", SD]]
    DT_all1[Representation == "Original", sdAUC:= 0]
    DT_all1[, Evaluation := "quantiles"]
    DT_all1[, Representation:=as.factor(Representation)]
    DT_all1[, Dataset := dataset_i]
    list_quantilesDT[[i]] <- DT_all1
    
    
    p_1 <- ggplot(data = DT_all1) +
      aes(x = Representation, y = V1, fill = Representation) +
      geom_boxplot() +
      theme_minimal()+
      labs(title = paste0("Evaluation: Quantiles", dataset_i), y = " AUC")+
      theme(legend.position = "none")
    print(p_1)
    
    DT_all1[, Representation:=as.factor(Representation)]
    
    p_sd_1 <- ggplot(data = DT_all1[Representation!="Original"]) +
      aes(x = Representation, y = sdAUC, fill = Representation) +
      geom_boxplot() +
      theme_minimal()+
      geom_hline(yintercept = 0)+
      labs(title = paste0("Evaluation: Quantiles ", dataset_i), y = "Standard Deviations of Mean AUC of Original features")+
      theme(legend.position = "none")
    
    print(p_sd_1)
    
    
    pvalues_quantiles_list[[i]] <- data.table(combined_original = wilcox.test(combined_new, original_new, paired = FALSE, alternative = "greater")["p.value"],
                                              random1_original = wilcox.test(random1_new, original_new, paired = FALSE, alternative = "greater")["p.value"],
                                              combined_random = wilcox.test(combined_new, random1_new, paired = FALSE, alternative = "greater")["p.value"],
                                              Dataset = dataset_i, Evaluation = "quantiles")
    
  }
  pvalues <- rbindlist(list(rbindlist(pvalues_median_list), rbindlist(pvalues_quantiles_list)))
  DTs <- rbindlist(list(rbindlist(list_medianDT), rbindlist(list_quantilesDT)))
  
  
  return(list(pvalues, DTs))
}


# 100CV -------------------------------------------------------------------
Glass_DT <- get_meta_plots(subfolder_name = "Glass", file_suffix = "_100CVnew")
glass_res <- get_plots_pvalues(input_res_DT = Glass_DT)
glass_res[[1]]

Ionosphere_DT <- get_meta_plots(subfolder_name = "Ionosphere", file_suffix = "_100CVnew")
ionosphere_res <- get_plots_pvalues(input_res_DT = Ionosphere_DT)
ionosphere_res[[1]]

Wave_DT <- get_meta_plots(subfolder_name = "Waveform", file_suffix = "_100CVnew")
wave_res <- get_plots_pvalues(input_res_DT = Wave_DT)
wave_res[[1]]

Stamps_DT <- get_meta_plots(subfolder_name = "Stamps", file_suffix = "_100CVnew")
stamps_res <- get_plots_pvalues(input_res_DT = Stamps_DT)
stamps_res[[1]]

Annthryroid_DT <- get_meta_plots(subfolder_name = "Annthyroid", file_suffix = "_100CVnew")
annthryroid_res <- get_plots_pvalues(input_res_DT = Annthryroid_DT)
annthryroid_res[[1]]

Shuttle_DT <- get_meta_plots(subfolder_name = "Shuttle", file_suffix = "_100CVnew")
shuttle_res <- get_plots_pvalues(input_res_DT = Shuttle_DT)
shuttle_res[[1]]


Pima_DT <- get_meta_plots(subfolder_name = "Pima", file_suffix = "_100CVnew")
pima_res <- get_plots_pvalues(input_res_DT = Pima_DT)
pima_res[[1]]

Cardio_DT <- get_meta_plots(subfolder_name = "Cardio", file_suffix = "_100CVnew")
cardio_res <- get_plots_pvalues(input_res_DT = Cardio_DT)
cardio_res[[1]]

HeartDisease_DT <- get_meta_plots(subfolder_name = "HeartDisease", file_suffix = "_100CVnew")
heart_res <- get_plots_pvalues(input_res_DT = HeartDisease_DT)
heart_res[[1]]


PenDigits_DT <- get_meta_plots(subfolder_name = "PenDigits", file_suffix = "")
pen_res <- get_plots_pvalues(input_res_DT = PenDigits_DT)
pen_res[[1]]

Internet_DT <- get_meta_plots(subfolder_name = "InternetAds", file_suffix = "")
internet_res <- get_plots_pvalues(input_res_DT = Internet_DT)
internet_res[[1]]

Arrhythmia_DT <- get_meta_plots(subfolder_name = "Arrhythmia", file_suffix = "_100CVnew")
arrhythmia_res <- get_plots_pvalues(input_res_DT = Arrhythmia_DT)
arrhythmia_res[[1]]



# 100CV - Normalized --------------------------------------------------------------

Glass_DT_100_normalized <- get_meta_plots(subfolder_name = "Glass", file_suffix = "_100CVnew_Normalized")
glass_res_100_norm <- get_plots_pvalues(input_res_DT = Glass_DT_100_normalized)
glass_res_100_norm[[1]]


Shuttle_DT_normalized <- get_meta_plots(subfolder_name = "Shuttle", file_suffix = "_100CVnew_Normalized")
shuttle_res_norm <- get_plots_pvalues(input_res_DT = Shuttle_DT_normalized)
shuttle_res_norm[[1]]

PageBlocks_DT_normalized <- get_meta_plots(subfolder_name = "PageBlocks", file_suffix = "_100CVnew_Normalized")
page_res_norm <- get_plots_pvalues(input_res_DT = PageBlocks_DT_normalized)
page_res_norm[[1]]


Stamps_DT_normalized <- get_meta_plots(subfolder_name = "Stamps", file_suffix = "_100CVnew_Normalized")
stamps_res_norm <- get_plots_pvalues(input_res_DT = Stamps_DT_normalized)
stamps_res_norm[[1]]

WDBC_DT_normalized <- get_meta_plots(subfolder_name = "WDBC", file_suffix = "_100CVnew_Normalized")
wdbc_res_norm <- get_plots_pvalues(input_res_DT = WDBC_DT_normalized)
wdbc_res_norm[[1]]


Wilt_DT_normalized <- get_meta_plots(subfolder_name = "Wilt", file_suffix = "_100CVnew_Normalized")
wilt_res_norm <- get_plots_pvalues(input_res_DT = Wilt_DT_normalized)
wilt_res_norm[[1]]

Ionosphere_DT_normalized <- get_meta_plots(subfolder_name = "Ionosphere", file_suffix = "_100CVnew_Normalized")
ionosphere_res_norm <- get_plots_pvalues(input_res_DT = Ionosphere_DT_normalized)
ionosphere_res_norm[[1]]


Pima_DT_normalized <- get_meta_plots(subfolder_name = "Pima", file_suffix = "_100CVnew_Normalized")
pima_res_norm <- get_plots_pvalues(input_res_DT = Pima_DT_normalized)
pima_res_norm[[1]]


Cardio_DT_normalized <- get_meta_plots(subfolder_name = "Cardio", file_suffix = "_100CVnew_Normalized")
cardio_res_norm <- get_plots_pvalues(input_res_DT = Cardio_DT_normalized)
cardio_res_norm[[1]]


# 300CV -------------------------------------------------------------------
file_suffix_300CV <- "_300CVnew"

Glass_DT_300 <- get_meta_plots(subfolder_name = "Glass", file_suffix = file_suffix_300CV)
glass_res_300 <- get_plots_pvalues(input_res_DT = Glass_DT_300)
glass_res_300[[1]]

Ionosphere_DT_300 <- get_meta_plots(subfolder_name = "Ionosphere", file_suffix = file_suffix_300CV)
ionosphere_res_300 <- get_plots_pvalues(input_res_DT = Ionosphere_DT_300)
ionosphere_res_300[[1]]

Wave_DT_300 <- get_meta_plots(subfolder_name = "Waveform", file_suffix = file_suffix_300CV)
wave_res_300 <- get_plots_pvalues(input_res_DT = Wave_DT_300)
wave_res_300[[1]]

Stamps_DT_300 <- get_meta_plots(subfolder_name = "Stamps", file_suffix = file_suffix_300CV)
stamps_res_300 <- get_plots_pvalues(input_res_DT = Stamps_DT_300)
stamps_res_300[[1]]

# Annthryroid_DT_300 <- get_meta_plots(subfolder_name = "Annthyroid", file_suffix = file_suffix_300CV)
# annthryroid_res_300 <- get_plots_pvalues(input_res_DT = Annthryroid_DT_300)
# annthryroid_res_300[[1]]


Shuttle_DT_300 <- get_meta_plots(subfolder_name = "Shuttle", file_suffix = file_suffix_300CV)
shuttle_res_300 <- get_plots_pvalues(input_res_DT = Shuttle_DT_300)
shuttle_res_300[[1]]


Pima_DT_300 <- get_meta_plots(subfolder_name = "Pima", file_suffix = file_suffix_300CV)
pima_res_300 <- get_plots_pvalues(input_res_DT = Pima_DT_300)
pima_res_300[[1]]

Cardio_DT_300 <- get_meta_plots(subfolder_name = "Cardio", file_suffix = file_suffix_300CV)
cardio_res_300 <- get_plots_pvalues(input_res_DT = Cardio_DT_300)
cardio_res_300[[1]]


#Arrhythmia_DT_300 <- get_meta_plots(subfolder_name = "Arrhythmia", file_suffix = file_suffix_300CV)


# 300CV - Normalized ------------------------------------------------------

file_suffix_300CV <- "_300CVnew_Normalized"

Glass_DT_300_normalized <- get_meta_plots(subfolder_name = "Glass", file_suffix = file_suffix_300CV)
glass_res_300_norm <- get_plots_pvalues(input_res_DT = Glass_DT_300_normalized)
glass_res_300_norm[[1]]


Stamps_DT_300_normalized <- get_meta_plots(subfolder_name = "Stamps", file_suffix = file_suffix_300CV)
stamps_res_300_norm <- get_plots_pvalues(input_res_DT = Stamps_DT_300_normalized)
stamps_res_300_norm[[1]]

# Annthryroid_DT_300 <- get_meta_plots(subfolder_name = "Annthyroid", file_suffix = file_suffix_300CV)
# annthryroid_res_300 <- get_plots_pvalues(input_res_DT = Annthryroid_DT_300)
# annthryroid_res_300[[1]]


Shuttle_DT_300_norm <- get_meta_plots(subfolder_name = "Shuttle", file_suffix = file_suffix_300CV)
shuttle_res_300_norm <- get_plots_pvalues(input_res_DT = Shuttle_DT_300_norm)
shuttle_res_300_norm[[1]]


Pima_DT_300 <- get_meta_plots(subfolder_name = "Pima", file_suffix = file_suffix_300CV)
pima_res_300 <- get_plots_pvalues(input_res_DT = Pima_DT_300)
pima_res_300[[1]]

Cardio_DT_300 <- get_meta_plots(subfolder_name = "Cardio", file_suffix = file_suffix_300CV)
cardio_res_300 <- get_plots_pvalues(input_res_DT = Cardio_DT_300)
cardio_res_300[[1]]


#Arrhythmia_DT_300 <- get_meta_plots(subfolder_name = "Arrhythmia", file_suffix = file_suffix_300CV)

# Aggregated results ------------------------------------------------------

DT <- rbindlist(list(glass_res[[2]][Evaluation == "quantiles"], 
                     shuttle_res[[2]][Evaluation == "quantiles"],
                     stamps_res[[2]][Evaluation == "quantiles"] ,
                     wave_res[[2]][Evaluation == "quantiles"] ,
                     ionosphere_res[[2]][Evaluation == "quantiles"],
                     heart_res[[2]][Evaluation == "quantiles"],
                     annthryroid_res[[2]][Evaluation == "quantiles"],
                     pima_res[[2]][Evaluation == "quantiles" ],
                     wilt_res_norm[[2]][Evaluation == "quantiles"],
                     page_res_norm[[2]][Evaluation == "quantiles"],
                     pen_res[[2]][Evaluation == "quantiles"],
                     internet_res[[2]][Evaluation == "quantiles"],
                     arrhythmia_res[[2]][Evaluation == "quantiles"]))


DT1 <- DT[Representation!="Original"]

p_1 <- ggplot(data = DT1) +
  aes(x = Dataset, y = sdAUC, fill = Representation) +
  geom_boxplot() +
  theme_minimal()+
  geom_hline(yintercept = 0)+
  coord_flip()
print(p_1)



shutle <- shuttle_res[[2]][Evaluation == "quantiles"]
shutle[, Dataset:="Shuttle"]

stamps <- stamps_res[[2]][Evaluation == "quantiles"]
stamps[, Dataset:="Stamps"]

wave <- wave_res[[2]][Evaluation == "quantiles"]
wave[, Dataset:="Waveform"]

annthryroid <- annthryroid_res[[2]][Evaluation == "quantiles"]
annthryroid[, Dataset:= "Annthryroid"]

pima <- pima_res[[2]][Evaluation == "quantiles"]
pima[, Dataset:= "Pima"]

wilt <- wilt_res_norm[[2]][Evaluation == "quantiles"]
wilt[,Dataset:= "Wilt"]

page <- page_res_norm[[2]][Evaluation == "quantiles"]
page[,Dataset:= "PageBlocks"]

inter <- internet_res[[2]][Evaluation == "quantiles"]
inter[,Dataset:= "InternetAds"]

arrhythmia <- arrhythmia_res[[2]][Evaluation == "quantiles"]
arrhythmia[, Dataset:= "Arrythmia"]


glass <- glass_res[[2]][Evaluation == "quantiles"]
glass[, Dataset:= "Glass"]

iono <- ionosphere_res[[2]][Evaluation == "quantiles"]
iono[,Dataset:="Ionosphere"]

heart <- heart_res[[2]][Evaluation == "quantiles"]
heart[,Dataset:="Heart"]

DT <- rbindlist(list(shutle, stamps, wave, annthryroid, pima, wilt, page, 
                     inter,arrhythmia,glass, iono, heart))


DT1 <- DT[Representation!="Original"]

DT1[!(Dataset %in% c("Pima", "PageBlocks", "Shuttle", "Arrythmia")), mean(sdAUC), by = Representation]

p_1 <- ggplot(data = DT1) +
  aes(x = Dataset, y = sdAUC, fill = Representation) +
  geom_boxplot() +
  theme_minimal()+
  geom_hline(yintercept = 0)+
  coord_flip()+
  scale_y_continuous(name="SD of AUC", breaks = seq(-10, 10, 0.5))
print(p_1)



# Optimistic Results ------------------------------------------------------

DT_1 <- rbindlist(list(glass_res[[2]][Evaluation == "Median"], 
                     shuttle_res[[2]][Evaluation == "Median"],
                     stamps_res[[2]][Evaluation == "Median"],
                     wave_res[[2]][Evaluation == "Median"],
                     ionosphere_res[[2]][Evaluation == "Median"],
                     heart_res[[2]][Evaluation == "Median"],
                     annthryroid_res[[2]][Evaluation == "Median"],
                     pima_res[[2]][Evaluation == "Median"],
                     wilt_res_norm[[2]][Evaluation == "Median"],
                     page_res_norm[[2]][Evaluation == "Median"],
                     pen_res[[2]][Evaluation == "Median"],
                     internet_res[[2]][Evaluation == "Median"],
                     arrhythmia_res[[2]][Evaluation == "Median"]))


DT1_1 <- DT_1[Representation!="Original"]

DT_hyper[, features_Iteration:=as.factor(features_Iteration)]

p_1_1 <- ggplot(data = DT_hyper) +
  aes(x = features_Iteration, y = V1, fill = Representation) +
  geom_boxplot() +
  theme_minimal()
print(p_1_1)




