  if("KNN" %in% DT[, unique(OD)]){
    KNNs <- DT[OD == "KNN", selected_columns]
    sampleKNNs <- sample(x = KNNs, size = percentage_OD * length(KNNs), replace = F)
    DToutliersKNNs <- DToutliers1[, .SD, .SDcols = sampleKNNs]
    sampleKNN <- sample(x = KNNs, size = mixed_view_features, replace = F)
  } else{
    sampleKNN <- NULL
  }


  if("KNNW" %in% DT[, unique(OD)]){
    KNNWs <- DT[OD == "KNNW", selected_columns]
    sampleKNNWs <- sample(x = KNNWs, size = percentage_OD * length(KNNWs), replace = F)
    DToutliersKNNWs <- DToutliers1[, .SD, .SDcols = sampleKNNWs]
    sampleKNNW <- sample(x = KNNWs, size = mixed_view_features, replace = F)
  } else{
    sampleKNNW <- NULL
  }



  if("LOF" %in% DT[, unique(OD)]){
    LOFs <- DT[OD == "LOF", selected_columns]
    sampleLOFs <- sample(x = LOFs, size = percentage_OD * length(LOFs), replace = F)
    DToutliersLOFs <- DToutliers1[, .SD, .SDcols = sampleLOFs]
    sampleLOF <- sample(x = LOFs, size = mixed_view_features, replace = F)
  } else{
    sampleLOF <- NULL
  }


  if("SimplifiedLOF" %in% DT[, unique(OD)]){
    SimplifiedLOFs <- DT[OD == "SimplifiedLOF", selected_columns]
    sampleSimplifiedLOFs <- sample(x = SimplifiedLOFs, size = percentage_OD * length(SimplifiedLOFs), replace = F)
    DToutliersSimplifiedLOFs <- DToutliers1[, .SD, .SDcols = sampleSimplifiedLOFs]
    sampleSimplifiedLOF <- sample(x = SimplifiedLOFs, size = mixed_view_features, replace = F)
  } else{
    sampleSimplifiedLOF <- NULL
  }



  if("LoOP" %in% DT[, unique(OD)]){
    LoOPs <- DT[OD == "LoOP", selected_columns]
    sampleLoOPs <- sample(x = LoOPs, size = percentage_OD * length(LoOPs), replace = F)
    DToutliersLoOPs <- DToutliers1[, .SD, .SDcols = sampleLoOPs]
    sampleLoOP <- sample(x = LoOPs, size = mixed_view_features, replace = F)
  } else{
    sampleLoOP <- NULL
  }


  if("LDOF" %in% DT[, unique(OD)]){
    LDOFs <- DT[OD == "LDOF", selected_columns]
    sampleLDOFs <- sample(x = LDOFs, size = percentage_OD * length(LDOFs), replace = F)
    DToutliersLDOFs <- DToutliers1[, .SD, .SDcols = sampleLDOFs]
    sampleLDOF <- sample(x = LDOFs, size = mixed_view_features, replace = F)
  } else{
    sampleLDOF <- NULL
  }


    if("ODIN" %in% DT[, unique(OD)]){
    ODINs <- DT[OD == "ODIN", selected_columns]
    sampleODINs <- sample(x = ODINs, size = percentage_OD * length(ODINs), replace = F)
    DToutliersODINs <- DToutliers1[, .SD, .SDcols = sampleODINs]
    sampleODIN <- sample(x = ODINs, size = mixed_view_features, replace = F)
  } else{
    sampleODIN <- NULL
  }

 if("FastABOD" %in% DT[, unique(OD)]){
    FastABODs <- DT[OD == "FastABOD", selected_columns]
    sampleFastABODs <- sample(x = FastABODs, size = percentage_OD * length(FastABODs), replace = F)
    DToutliersFastABODs <- DToutliers1[, .SD, .SDcols = sampleFastABODs]
    sampleFastABOD <- sample(x = FastABODs, size = mixed_view_features, replace = F)
  } else{
    sampleFastABOD <- NULL
  }
  
   if("KDEOS" %in% DT[, unique(OD)]){
    KDEOSs <- DT[OD == "KDEOS", selected_columns]
    sampleKDEOSs <- sample(x = KDEOSs, size = percentage_OD * length(KDEOSs), replace = F)
    DToutliersKDEOSs <- DToutliers1[, .SD, .SDcols = sampleKDEOSs]
    sampleKDEOS <- sample(x = KDEOSs, size = mixed_view_features, replace = F)
  } else{
    sampleKDEOS <- NULL
  }


 if("LDF" %in% DT[, unique(OD)]){
    LDFs <- DT[OD == "LDF", selected_columns]
    sampleLDFs <- sample(x = LDFs, size = percentage_OD * length(LDFs), replace = F)
    DToutliersLDFs <- DToutliers1[, .SD, .SDcols = sampleLDFs]
    sampleLDF <- sample(x = LDFs, size = mixed_view_features, replace = F)
  } else{
    sampleLDF <- NULL
  }

 if("INFLO" %in% DT[, unique(OD)]){
    INFLOs <- DT[OD == "INFLO", selected_columns]
    sampleINFLOs <- sample(x = INFLOs, size = percentage_OD * length(INFLOs), replace = F)
    DToutliersINFLOs <- DToutliers1[, .SD, .SDcols = sampleINFLOs]
    sampleINFLO <- sample(x = INFLOs, size = mixed_view_features, replace = F)
  } else{
    sampleINFLO <- NULL
  }



 if("COF" %in% DT[, unique(OD)]){
    COFs <- DT[OD == "COF", selected_columns]
    sampleCOFs <- sample(x = COFs, size = percentage_OD * length(COFs), replace = F)
    DToutliersCOFs <- DToutliers1[, .SD, .SDcols = sampleCOFs]
    sampleCOF <- sample(x = COFs, size = mixed_view_features, replace = F)
  } else{
    sampleCOF <- NULL
  }
