####################################################################################################################################
####################################################################################################################################
### This script contains a variety of functions that are utilized in obtaining and assessing fairness corrected thresholds in    ###
### all contexts.  The particular functions are the following:                                                                   ###
###   1) FindPreFCthresh_fcn                                                                                                     ###
###   2) Compute_Fairness_Measures                                                                                               ###
###   3) Compute_Predictive_Performance_Measure                                                                                  ###
###   4) Assign_Risk_Scores                                                                                                      ###
###   5) Optimize_Fair_Thresholds_fcn                                                                                            ###
###   6) ParOptimThresh_fcn                                                                                                      ###
###   7) ParTuningGridAssessment_fcn                                                                                             ###
###   8) SubSamples_Measures_fcn                                                                                                 ###
####################################################################################################################################
####################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------###
###                                          Function 1: FindPreFCthresh_fcn                                                     ###                                         
###------------------------------------------------------------------------------------------------------------------------------###
####################################################################################################################################
####################################################################################################################################
### FindPreFCthresh_fcn -- This function calculates the pre-fairness corrected thresholds for a given subsample/sample of data.  ###
###------------------------------------------------------------------------------------------------------------------------------###
### PredProbVec -- The vector of predicted probabilities.                                                                        ###
### Method -- the method of identifying the pre-fairness corrected threshold.  There are two possibilities:                      ###
###           a) When Method = 1 (the default), the pre-fairness corrected threshold will correspond to a specified              ###
###                              (via PreProbPerc) percentile of the predicted probabilities in PredProbVec.                     ###
###           b) When Method = 2, the pre-fairness corrected threshold will correspond to the mean of the predicted probailities ###
###                               in PredProbVec.                                                                                ###
###           c) When Method = 3, the pre-fairness corrected threshold will correspond to the PreProbPerc percentile of          ###
###                               predicted probabilities among the set of predicted probabilities that are LESS THAN the mean   ###
###                               predicted probability.                                                                         ###
###                               (So this threshold is the "p-th percentile" of predicted probabilities less than the mean      ###
###                                predicted probability.)                                                                       ###
###           d) When Method = 4, the pre-fairness corrected threshold will correspond to the PreProbPerc percentile of          ###
###                               predicted probabilities among the set of predicted probabilities that are GREATER THAN the     ###
###                               mean predicted probability.                                                                    ###
###                               (So this threshold is the "p-th percentile" of predicted probabilities greater than the mean   ###
###                                predicted probability.)                                                                       ###
### PreProbPerc -- If Method = 1, this argument specifies the desired percentile of the predicted probabilities in PredProbVec   ###
###                to be used as the pre-fairness corrected threshold. The default is 0.50.                                      ###
###             -- If Method = 3, this argument specifies the desired percentile of the predicted probabilities among the set of ###
###                predicted probabilities that are LESS THAN the mean predicted probability.                                    ###
###                (So this specifies the "p-th percentile" of predicted probabilities less than the mean predicted probability.)###
###             -- If Method = 4, this argument specifies the desired percentile of the predicted probabilities among the set of ###
###                predicted probabilities that are GREATER THAN the mean predicted probability.                                 ###
###                (So this specifies the "p-th percentile" of predicted probabilities greater than the mean predicted           ###
###                probability.)                                                                                                 ###
###------------------------------------------------------------------------------------------------------------------------------###
### The function outputs a scalar which is the desired pre-fairness corrected threshold.                                         ###
####################################################################################################################################
####################################################################################################################################
FindPreFCthresh_fcn <- function(PredProbVec, Method = 1, PreProbPerc = 0.50){
  if(Method == 1){
    threshVal <- quantile(PredProbVec, probs = PreProbPerc)
  }
  if(Method == 2){
    threshVal <- mean(PredProbVec)
  }
  if(Method == 3){
    MeanPredProb <- mean(PredProbVec)
    threshVal <- quantile(PredProbVec[which(PredProbVec < MeanPredProb)], probs = PreProbPerc)
  }
  if(Method == 4){
    MeanPredProb <- mean(PredProbVec)
    threshVal <- quantile(PredProbVec[which(PredProbVec > MeanPredProb)], probs = PreProbPerc)
  }
  return(threshVal)
}
####################################################################################################################################
####################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------###
###                                        Function 2: Compute_Fairness_Measures                                                 ###                                         
###------------------------------------------------------------------------------------------------------------------------------###
### Compute_Fairness_Measures --> Computes any one of 9 specified algorithmic fairness measures for a confusion matrix for a set ###
###                               of risk-classification thresholds.                                                             ###
###------------------------------------------------------------------------------------------------------------------------------###
### Function Inputs:                                                                                                             ###
### 1) Thresholds - A vector of thresholds for each level of the protected attribute for each threshold.  This is a vector, as   ###
###                 opposed to a matrix since the optimizer used to find the post-fairness-corrected thresholds needs a vector   ###
###                 specification.                                                                                               ###
### 2) DataSet - The sample/subsample for which the specified fairness measures will be computed.  Consists of three columns:    ###
###              Outcome (0/1), Protected Attribute Level, and predicted probability.  Columns must be in this order.            ###
### 3) ProtAtrLevels - The levels of the protected attribute.                                                                    ###
### 4) NumThresholds - The number of thresholds.                                                                                 ###
### 5) ThresholdNames - The names of the various thresholds (e.g. Low-Risk, Average-Risk, and High-Risk).                        ###
### 6) FairnessType - The definition of algorithmic fairness to be calculated.  The 9 options are the following, with            ###
###                   corresponding definitions given in one or both of the following manuscripts.                               ### 
###                   a) Alexamdra Chouldechova. "Fair prediction with disparate impact: A study of bias in recidivism           ###
###                      prediction instruments". arXiv:1703.00056v1 [stat.AP] 28 Feb 2017.                                      ###
###                   b) Verma and Rubin.  "Fairness Definitions Explained". 2018 ACM/IEEE International Workshop on Software    ###
###                      Fairness.                                                                                               ###
###                   1) ERB - Error Rate Balance                                                                                ###
###                   2) CUAE - Conditional Use Accuracy Equality                                                                ###
###                   3) PP - Predictive Parity                                                                                  ###
###                   4) SP - Statistical Parity                                                                                 ###
###                   5) TE - Treatment Equality                                                                                 ###
###                   6) PE - Predictive Equality                                                                                ###
###                   7) EO - Equal Opportunity                                                                                  ###
###                   8) OAE - Overall Accuracy Equality                                                                         ###
###                   9) CAL - Calibration                                                                                       ###
### 7) ProtAttrSubAudit - Indicates whether or not the function is being called on to calculate fairness measures for a protected###
###                       attribute as part of a subaudit, which is to say that the fairness of a protected attribute is being   ###
###                       assessed when that protected attribute was not part of the protected attribute for which the fairness  ###
###                       correction procedure was applied.  The default setting is FALSE.                                       ###
### 8) FullDataSet.SubAudit - If ProtAttrSubAudit is set to TRUE, this is a data set containing the same columns as DataSet, plus###
###                           additional columns necessary to carry out the subaudit.  The default setting is NULL.              ###
###------------------------------------------------------------------------------------------------------------------------------###
####################################################################################################################################
####################################################################################################################################
Compute_Fairness_Measures <- function(Thresholds, DataSet, ProtAtrLevels, NumThresholds, ThresholdNames, FairnessType,
                                      ProtAttrSubAudit = FALSE, FullDataSet.SubAudit = NULL){
  ### Convert Thresholds vector into a matrix in which the rows correspond to the different thresholds and the columns           ###    
  ### correspond to the different levels of the protected attribute.                                                             ###
  if(ProtAttrSubAudit == TRUE){
    ThreshMat <- matrix(Thresholds, NumThresholds, length(levels(FullDataSet.SubAudit$Actual.PA)), byrow = T)
    colnames(ThreshMat) <- levels(FullDataSet.SubAudit$Actual.PA)
    rownames(ThreshMat) <- ThresholdNames
  }else{
    ThreshMat <- matrix(Thresholds, NumThresholds, length(ProtAtrLevels), byrow = T)
    colnames(ThreshMat) <- ProtAtrLevels
    rownames(ThreshMat) <- ThresholdNames
  }
  ###--------------------###
  ### Error Rate Balance ###
  ###--------------------###
  if(FairnessType == "ERB"){
    Fairness_Metric_Info <- list()
    ERBvec <- data.frame(Threshold = ThresholdNames, ERB = rep(NA, NumThresholds))
    for(k in 1:NumThresholds){
      if(ProtAttrSubAudit == TRUE){
        FullDataSet.SubAudit.k <- FullDataSet.SubAudit
        ThreshVec <- ThreshMat[k, ]
        FullDataSet.SubAudit.k$THRESH <- ThreshVec[match(FullDataSet.SubAudit$Actual.PA, levels(FullDataSet.SubAudit$Actual.PA))]
        DataSet.k <- FullDataSet.SubAudit.k[,which(colnames(FullDataSet.SubAudit.k) %in% c(colnames(DataSet), "THRESH"))]
      }else{
        DataSet.k <- DataSet
        ThreshVec <- ThreshMat[k, ]
        DataSet.k$THRESH <- ThreshVec[match(DataSet.k$Protected_Attribute, ProtAtrLevels)]
      }
      DataSet.k$FalseNeg <- (DataSet.k$m_pred < DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 1)
      DataSet.k$FalsePos <- (DataSet.k$m_pred >= DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 0)
      ErrorRatesPAlev.k <- DataSet.k %>% group_by(Protected_Attribute) %>% summarize(FNR = sum(FalseNeg)/sum(OUTCOME == 1), FPR = sum(FalsePos)/sum(OUTCOME == 0))
      Fairness_Metric_Info[[k]] <- ErrorRatesPAlev.k
      names(Fairness_Metric_Info)[k] <- str_c(ThresholdNames[k], "_Threshold")
      ERBvec$ERB[k] <- min(min(ErrorRatesPAlev.k$FNR)/max(ErrorRatesPAlev.k$FNR), min(ErrorRatesPAlev.k$FPR)/max(ErrorRatesPAlev.k$FPR))
    }
    Fairness_Metric_Info[[(NumThresholds + 1)]] <- ERBvec
    names(Fairness_Metric_Info)[(NumThresholds + 1)] <- "Error_Rate_Balance"
  }
  ###-----------------------------------###
  ### Conditional Use Accuracy Equality ###
  ###-----------------------------------###
  if(FairnessType == "CUAE"){
    Fairness_Metric_Info <- list()
    CUAEvec <- data.frame(Threshold = ThresholdNames, CUAE = rep(NA, NumThresholds))
    for(k in 1:NumThresholds){
      if(ProtAttrSubAudit == TRUE){
        FullDataSet.SubAudit.k <- FullDataSet.SubAudit
        ThreshVec <- ThreshMat[k, ]
        FullDataSet.SubAudit.k$THRESH <- ThreshVec[match(FullDataSet.SubAudit$Actual.PA, levels(FullDataSet.SubAudit$Actual.PA))]
        DataSet.k <- FullDataSet.SubAudit.k[,which(colnames(FullDataSet.SubAudit.k) %in% c(colnames(DataSet), "THRESH"))]
      }else{
        DataSet.k <- DataSet
        ThreshVec <- ThreshMat[k, ]
        DataSet.k$THRESH <- ThreshVec[match(DataSet.k$Protected_Attribute, ProtAtrLevels)]
      }
      DataSet.k$TruePos <- (DataSet.k$m_pred >= DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 1)
      DataSet.k$TrueNeg <- (DataSet.k$m_pred < DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 0)
      CorrectPredsRatesPAlev.k <- DataSet.k %>% group_by(Protected_Attribute) %>% summarize(PPV = sum(TruePos)/sum(m_pred >= THRESH), 
                                                                                     NPV = sum(TrueNeg)/sum(m_pred < THRESH))
      Fairness_Metric_Info[[k]] <- CorrectPredsRatesPAlev.k
      names(Fairness_Metric_Info)[k] <- str_c(ThresholdNames[k], "_Threshold")
      CUAEvec$CUAE[k] <- min(min(CorrectPredsRatesPAlev.k$PPV)/max(CorrectPredsRatesPAlev.k$PPV), min(CorrectPredsRatesPAlev.k$NPV)/max(CorrectPredsRatesPAlev.k$NPV))
    }
    Fairness_Metric_Info[[(NumThresholds + 1)]] <- CUAEvec
    names(Fairness_Metric_Info)[(NumThresholds + 1)] <- "Conditional_Use_Accuracy_Equality"
  }
  ###-------------------###
  ### Predictive Parity ###
  ###-------------------###
  if(FairnessType == "PP"){
    Fairness_Metric_Info <- list()
    PPvec <- data.frame(Threshold = ThresholdNames, PP = rep(NA, NumThresholds))
    for(k in 1:NumThresholds){
      if(ProtAttrSubAudit == TRUE){
        FullDataSet.SubAudit.k <- FullDataSet.SubAudit
        ThreshVec <- ThreshMat[k, ]
        FullDataSet.SubAudit.k$THRESH <- ThreshVec[match(FullDataSet.SubAudit$Actual.PA, levels(FullDataSet.SubAudit$Actual.PA))]
        DataSet.k <- FullDataSet.SubAudit.k[,which(colnames(FullDataSet.SubAudit.k) %in% c(colnames(DataSet), "THRESH"))]
      }else{
        DataSet.k <- DataSet
        ThreshVec <- ThreshMat[k, ]
        DataSet.k$THRESH <- ThreshVec[match(DataSet.k$Protected_Attribute, ProtAtrLevels)]
      }
      DataSet.k$TruePos <- (DataSet.k$m_pred >= DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 1)
      PPVsPAlev.k <- DataSet.k %>% group_by(Protected_Attribute) %>% summarize(PPV = sum(TruePos)/sum(m_pred >= THRESH))
      Fairness_Metric_Info[[k]] <- PPVsPAlev.k
      names(Fairness_Metric_Info)[k] <- str_c(ThresholdNames[k], "_Threshold")
      PPvec$PP[k] <- min(PPVsPAlev.k$PPV)/max(PPVsPAlev.k$PPV)
    }
    Fairness_Metric_Info[[(NumThresholds + 1)]] <- PPvec
    names(Fairness_Metric_Info)[(NumThresholds + 1)] <- "Predictive_Parity"
  }
  ###--------------------###
  ### Statistical Parity ###
  ###--------------------###
  if(FairnessType == "SP"){
    Fairness_Metric_Info <- list()
    SPvec <- data.frame(Threshold = ThresholdNames, SP = rep(NA, NumThresholds))
    for(k in 1:NumThresholds){
      if(ProtAttrSubAudit == TRUE){
        FullDataSet.SubAudit.k <- FullDataSet.SubAudit
        ThreshVec <- ThreshMat[k, ]
        FullDataSet.SubAudit.k$THRESH <- ThreshVec[match(FullDataSet.SubAudit$Actual.PA, levels(FullDataSet.SubAudit$Actual.PA))]
        DataSet.k <- FullDataSet.SubAudit.k[,which(colnames(FullDataSet.SubAudit.k) %in% c(colnames(DataSet), "THRESH"))]
      }else{
        DataSet.k <- DataSet
        ThreshVec <- ThreshMat[k, ]
        DataSet.k$THRESH <- ThreshVec[match(DataSet.k$Protected_Attribute, ProtAtrLevels)]
      }
      DataSet.k$PredPos <- (DataSet.k$m_pred >= DataSet.k$THRESH)
      PredPosPAlev.k <- DataSet.k %>% group_by(Protected_Attribute) %>% summarize(PropPredPos = sum(PredPos)/n())
      Fairness_Metric_Info[[k]] <- PredPosPAlev.k
      names(Fairness_Metric_Info)[k] <- str_c(ThresholdNames[k], "_Threshold")
      SPvec$SP[k] <- min(PredPosPAlev.k$PropPredPos)/max(PredPosPAlev.k$PropPredPos)
    }
    Fairness_Metric_Info[[(NumThresholds + 1)]] <- SPvec
    names(Fairness_Metric_Info)[(NumThresholds + 1)] <- "Statistical_Parity"
  }
  ###--------------------###
  ### Treatment Equality ###
  ###--------------------###
  if(FairnessType == "TE"){
    Fairness_Metric_Info <- list()
    TEvec <- data.frame(Threshold = ThresholdNames, TE = rep(NA, NumThresholds))
    for(k in 1:NumThresholds){
      if(ProtAttrSubAudit == TRUE){
        FullDataSet.SubAudit.k <- FullDataSet.SubAudit
        ThreshVec <- ThreshMat[k, ]
        FullDataSet.SubAudit.k$THRESH <- ThreshVec[match(FullDataSet.SubAudit$Actual.PA, levels(FullDataSet.SubAudit$Actual.PA))]
        DataSet.k <- FullDataSet.SubAudit.k[,which(colnames(FullDataSet.SubAudit.k) %in% c(colnames(DataSet), "THRESH"))]
      }else{
        DataSet.k <- DataSet
        ThreshVec <- ThreshMat[k, ]
        DataSet.k$THRESH <- ThreshVec[match(DataSet.k$Protected_Attribute, ProtAtrLevels)]
      }
      DataSet.k$FalseNeg <- (DataSet.k$m_pred < DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 1)
      DataSet.k$FalsePos <- (DataSet.k$m_pred >= DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 0)
      TreatEqualPAlev.k <- DataSet.k %>% group_by(Protected_Attribute) %>% summarize(FNcount = sum(FalseNeg), FPcount = sum(FalsePos),  TE = sum(FalseNeg)/sum(FalsePos))
      Fairness_Metric_Info[[k]] <- TreatEqualPAlev.k
      names(Fairness_Metric_Info)[k] <- str_c(ThresholdNames[k], "_Threshold")
      TEvec$TE[k] <- min(TreatEqualPAlev.k$TE)/max(TreatEqualPAlev.k$TE)
    }
    Fairness_Metric_Info[[(NumThresholds + 1)]] <- TEvec
    names(Fairness_Metric_Info)[(NumThresholds + 1)] <- "Treatment_Equality"
  }
  ###--------------------------------------------------------------###
  ### Predictive Equality (i.e. False Positive Error Rate Balance) ###
  ###--------------------------------------------------------------###
  if(FairnessType == "PE"){
    Fairness_Metric_Info <- list()
    PEvec <- data.frame(Threshold = ThresholdNames, PE = rep(NA, NumThresholds))
    for(k in 1:NumThresholds){
      if(ProtAttrSubAudit == TRUE){
        FullDataSet.SubAudit.k <- FullDataSet.SubAudit
        ThreshVec <- ThreshMat[k, ]
        FullDataSet.SubAudit.k$THRESH <- ThreshVec[match(FullDataSet.SubAudit$Actual.PA, levels(FullDataSet.SubAudit$Actual.PA))]
        DataSet.k <- FullDataSet.SubAudit.k[,which(colnames(FullDataSet.SubAudit.k) %in% c(colnames(DataSet), "THRESH"))]
      }else{
        DataSet.k <- DataSet
        ThreshVec <- ThreshMat[k, ]
        DataSet.k$THRESH <- ThreshVec[match(DataSet.k$Protected_Attribute, ProtAtrLevels)]
      }
      DataSet.k$FalsePos <- (DataSet.k$m_pred >= DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 0)
      FPRpalev.k <- DataSet.k %>% group_by(Protected_Attribute) %>% summarize(FPR = sum(FalsePos)/sum(OUTCOME == 0))
      Fairness_Metric_Info[[k]] <- FPRpalev.k
      names(Fairness_Metric_Info)[k] <- str_c(ThresholdNames[k], "_Threshold")
      PEvec$PE[k] <- min(FPRpalev.k$FPR)/max(FPRpalev.k$FPR)
    }
    Fairness_Metric_Info[[(NumThresholds + 1)]] <- PEvec
    names(Fairness_Metric_Info)[(NumThresholds + 1)] <- "Predictive_Equality"
  }
  ###------------------------------------------------------------###
  ### Equal Opportunity (i.e. False Negative Error Rate Balance) ###
  ###------------------------------------------------------------###
  if(FairnessType == "EO"){
    Fairness_Metric_Info <- list()
    EOvec <- data.frame(Threshold = ThresholdNames, EO = rep(NA, NumThresholds))
    for(k in 1:NumThresholds){
      if(ProtAttrSubAudit == TRUE){
        FullDataSet.SubAudit.k <- FullDataSet.SubAudit
        ThreshVec <- ThreshMat[k, ]
        FullDataSet.SubAudit.k$THRESH <- ThreshVec[match(FullDataSet.SubAudit$Actual.PA, levels(FullDataSet.SubAudit$Actual.PA))]
        DataSet.k <- FullDataSet.SubAudit.k[,which(colnames(FullDataSet.SubAudit.k) %in% c(colnames(DataSet), "THRESH"))]
      }else{
        DataSet.k <- DataSet
        ThreshVec <- ThreshMat[k, ]
        DataSet.k$THRESH <- ThreshVec[match(DataSet.k$Protected_Attribute, ProtAtrLevels)]
      }
      DataSet.k$FalseNeg <- (DataSet.k$m_pred < DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 1)
      FNRpalev.k <- DataSet.k %>% group_by(Protected_Attribute) %>% summarize(FNR = sum(FalseNeg)/sum(OUTCOME == 1))
      Fairness_Metric_Info[[k]] <- FNRpalev.k
      names(Fairness_Metric_Info)[k] <- str_c(ThresholdNames[k], "_Threshold")
      EOvec$EO[k] <- min(FNRpalev.k$FNR)/max(FNRpalev.k$FNR)
    }
    Fairness_Metric_Info[[(NumThresholds + 1)]] <- EOvec
    names(Fairness_Metric_Info)[(NumThresholds + 1)] <- "Equal_Opportunity"
  }
  ###---------------------------###
  ### Overall Accuracy Equality ###
  ###---------------------------###
  if(FairnessType == "OAE"){
    Fairness_Metric_Info <- list()
    OAEvec <- data.frame(Threshold = ThresholdNames, OAE = rep(NA, NumThresholds))
    for(k in 1:NumThresholds){
      if(ProtAttrSubAudit == TRUE){
        FullDataSet.SubAudit.k <- FullDataSet.SubAudit
        ThreshVec <- ThreshMat[k, ]
        FullDataSet.SubAudit.k$THRESH <- ThreshVec[match(FullDataSet.SubAudit$Actual.PA, levels(FullDataSet.SubAudit$Actual.PA))]
        DataSet.k <- FullDataSet.SubAudit.k[,which(colnames(FullDataSet.SubAudit.k) %in% c(colnames(DataSet), "THRESH"))]
      }else{
        DataSet.k <- DataSet
        ThreshVec <- ThreshMat[k, ]
        DataSet.k$THRESH <- ThreshVec[match(DataSet.k$Protected_Attribute, ProtAtrLevels)]
      }
      DataSet.k$TrueNeg <- (DataSet.k$m_pred < DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 0)
      DataSet.k$TruePos <- (DataSet.k$m_pred >= DataSet.k$THRESH) &  (DataSet.k$OUTCOME == 1)
      AccRatesPAlev.k <- DataSet.k %>% group_by(Protected_Attribute) %>% summarize(ACC = sum(TrueNeg + TruePos)/n())
      Fairness_Metric_Info[[k]] <- AccRatesPAlev.k
      names(Fairness_Metric_Info)[k] <- str_c(ThresholdNames[k], "_Threshold")
      OAEvec$OAE[k] <- min(AccRatesPAlev.k$ACC)/max(AccRatesPAlev.k$ACC)
    }
    Fairness_Metric_Info[[(NumThresholds + 1)]] <- OAEvec
    names(Fairness_Metric_Info)[(NumThresholds + 1)] <- "Overall_Accuracy_Equality"
  }
  ###-------------###
  ### Calibration ###
  ###-------------###
  if(FairnessType == "CAL"){
    Fairness_Metric_Info <- list()
    CALvec <- data.frame(Score = str_c(rep("S", (NumThresholds + 1)), 1:(NumThresholds + 1)), CAL = rep(NA, (NumThresholds + 1)))
    if(ProtAttrSubAudit == TRUE){
      FullDataSet.SubAudit.Thresh <- FullDataSet.SubAudit
    }else{DataSet.Thresh <- DataSet}
    for(k in 1:NumThresholds){
      ThreshVec <- ThreshMat[k, ]
      if(ProtAttrSubAudit == TRUE){
        FullDataSet.SubAudit.Thresh$THRESH <- ThreshVec[match(FullDataSet.SubAudit.Thresh$Actual.PA, levels(FullDataSet.SubAudit.Thresh$Actual.PA))]
        colnames(FullDataSet.SubAudit.Thresh)[ncol(FullDataSet.SubAudit.Thresh)] <- str_c("THRESH_", k)
        FullDataSet.SubAudit.Thresh$LESS <- FullDataSet.SubAudit.Thresh$m_pred < FullDataSet.SubAudit.Thresh[, ncol(FullDataSet.SubAudit.Thresh)]
        colnames(FullDataSet.SubAudit.Thresh)[ncol(FullDataSet.SubAudit.Thresh)] <- str_c("LESS_THRESH_", k)
        DataSet.Thresh <- FullDataSet.SubAudit.Thresh[,-which(colnames(FullDataSet.SubAudit.Thresh) %in% c("Actual.PA", "RiskPreFC", "RiskPostFC"))]
      }else{
        DataSet.Thresh$THRESH <- ThreshVec[match(DataSet.Thresh$Protected_Attribute, ProtAtrLevels)]
        colnames(DataSet.Thresh)[ncol(DataSet.Thresh)] <- str_c("THRESH_", k)
        DataSet.Thresh$LESS <- DataSet.Thresh$m_pred < DataSet.Thresh[, ncol(DataSet.Thresh)]
        colnames(DataSet.Thresh)[ncol(DataSet.Thresh)] <- str_c("LESS_THRESH_", k)
      }
    }
    RelColLocs <- which(str_detect(colnames(DataSet.Thresh), pattern = "LESS_THRESH_") == TRUE)
    if(length(RelColLocs) == 1){
      DataSet.Thresh$NumLESS <- rowSums(matrix(DataSet.Thresh[, RelColLocs], nrow = dim(DataSet.Thresh)[1], ncol = length(RelColLocs)))
    }else{DataSet.Thresh$NumLESS <- rowSums(DataSet.Thresh[, RelColLocs])}
    DataSet.Thresh$RiskScore <- (NumThresholds + 1) - DataSet.Thresh$NumLESS
    CALbyPAlev <- DataSet.Thresh %>% group_by(RiskScore, Protected_Attribute) %>% summarize(CAL = sum(OUTCOME == 1)/n())
    Fairness_Metric_Info[[1]] <- CALbyPAlev
    CALbyScore <- CALbyPAlev %>% group_by(RiskScore) %>% summarize(CAL = min(CAL)/max(CAL))
    CALvec$CAL <- CALbyScore$CAL
    Fairness_Metric_Info[[2]] <- CALvec
    names(Fairness_Metric_Info)[2] <- "Calibration"
  }
  return(Fairness_Metric_Info)
}
####################################################################################################################################
####################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------###
###                                   Function 3: Compute_Predictive_Performance_Measures                                        ###                                         
###------------------------------------------------------------------------------------------------------------------------------###
### Compute_Predictive_Performance_Measure --> Computes each of the eight possible predictive performance measures of a          ###
###                                            confusion matrix, along with overall accuracy for a set of risk-classification    ###
###                                            thresholds.                                                                       ###
###------------------------------------------------------------------------------------------------------------------------------###
### Function Inputs:                                                                                                             ###
### 1) Thresholds - A vector of thresholds for each level of the protected attribute for each threshold.  This is a vector, as   ###
###                 opposed to a matrix since the optimizer used to find the post-fairness-corrected thresholds needs a vector   ###
###                 specification.                                                                                               ###
### 2) DataSet - The sample/subsample for which the predictive performance measures will be computed.  Consists of three columns:###
###              Outcome (0/1), Protected Attribute Level, and predicted probability.  Columns must be in this order.            ###
### 3) ProtAtrLevels - The levels of the protected attribute.                                                                    ###
### 4) NumThresholds - The number of thresholds.                                                                                 ###
### 5) ThresholdNames - The names of the various thresholds (e.g. Low-Risk, Average-Risk, and High-Risk).                        ###
### 6) ProtAttrSubAudit - Indicates whether or not the function is being called on to calculate accuracy measures for a protected###
###                       attribute as part of a subaudit, which is to say that the fairness of a protected attribute is being   ###
###                       assessed when that protected attribute was not part of the protected attribute for which the fairness  ###
###                       correction procedure was applied.  The default setting is FALSE.                                       ###
### 7) FullDataSet.SubAudit - If ProtAttrSubAudit is set to TRUE, this is a data set containing the same columns as DataSet, plus###
###                           additional columns necessary to carry out the subaudit.  The default setting is NULL.              ###
###------------------------------------------------------------------------------------------------------------------------------###
####################################################################################################################################
####################################################################################################################################
Compute_Predictive_Performance_Measures <- function(Thresholds, DataSet, ProtAtrLevels, NumThresholds, ThresholdNames,
                                                    ProtAttrSubAudit = FALSE, FullDataSet.SubAudit = NULL){
  ### Convert Thresholds vector into a matrix in which the rows correspond to the different thresholds and the columns           ###    
  ### correspond to the different levels of the protected attribute.                                                             ###
  if(ProtAttrSubAudit == TRUE){
    ThreshMat <- matrix(Thresholds, NumThresholds, length(levels(FullDataSet.SubAudit$Actual.PA)), byrow = T)
    colnames(ThreshMat) <- levels(FullDataSet.SubAudit$Actual.PA)
    rownames(ThreshMat) <- ThresholdNames
  }else{
    ThreshMat <- matrix(Thresholds, NumThresholds, length(ProtAtrLevels), byrow = T)
    colnames(ThreshMat) <- ProtAtrLevels
    rownames(ThreshMat) <- ThresholdNames
  }
  ### Create the object to return -- a data frame in which the rows represent the 8 different predictive performance measures    ###
  ### and the columns correspond to the different thresholds.                                                                    ###
  PredPerfDF <- as.data.frame(matrix(NA, 9 , NumThresholds))
  colnames(PredPerfDF) <- ThresholdNames
  rownames(PredPerfDF) <- c("FNR", "TPR", "FPR", "TNR", "PPV", "FDR", "NPV", "FOR", "ACC")
  ### Loop through each threshold, calculating the 8 predictive performance measures.                                            ###
  for(k in 1:NumThresholds){
    if(ProtAttrSubAudit == TRUE){
      FullDataSet.SubAudit.k <- FullDataSet.SubAudit
      ThreshVec <- ThreshMat[k, ]
      FullDataSet.SubAudit.k$THRESH <- ThreshVec[match(FullDataSet.SubAudit$Actual.PA, levels(FullDataSet.SubAudit$Actual.PA))]
      DataSet.k <- FullDataSet.SubAudit.k[,which(colnames(FullDataSet.SubAudit.k) %in% c(colnames(DataSet), "THRESH"))]
    }else{
      DataSet.k <- DataSet
      ThreshVec <- ThreshMat[k, ]
      DataSet.k$THRESH <- ThreshVec[match(DataSet.k$Protected_Attribute, ProtAtrLevels)]
    }
    PredPerfDF[1, k] <- sum((DataSet.k$m_pred < DataSet.k$THRESH) & (DataSet.k$OUTCOME == 1))/sum(DataSet.k$OUTCOME == 1)
    PredPerfDF[2, k] <- sum((DataSet.k$m_pred >= DataSet.k$THRESH) & (DataSet.k$OUTCOME == 1))/sum(DataSet.k$OUTCOME == 1)
    PredPerfDF[3, k] <- sum((DataSet.k$m_pred >= DataSet.k$THRESH) & (DataSet.k$OUTCOME == 0))/sum(DataSet.k$OUTCOME == 0)
    PredPerfDF[4, k] <- sum((DataSet.k$m_pred < DataSet.k$THRESH) & (DataSet.k$OUTCOME == 0))/sum(DataSet.k$OUTCOME == 0)
    PredPerfDF[5, k] <- sum((DataSet.k$m_pred >= DataSet.k$THRESH) & (DataSet.k$OUTCOME == 1))/sum((DataSet.k$m_pred >= DataSet.k$THRESH))
    PredPerfDF[6, k] <- sum((DataSet.k$m_pred >= DataSet.k$THRESH) & (DataSet.k$OUTCOME == 0))/sum((DataSet.k$m_pred >= DataSet.k$THRESH))
    PredPerfDF[7, k] <- sum((DataSet.k$m_pred < DataSet.k$THRESH) & (DataSet.k$OUTCOME == 0))/sum((DataSet.k$m_pred < DataSet.k$THRESH))
    PredPerfDF[8, k] <- sum((DataSet.k$m_pred < DataSet.k$THRESH) & (DataSet.k$OUTCOME == 1))/sum((DataSet.k$m_pred < DataSet.k$THRESH))
    PredPerfDF[9, k] <- (sum((DataSet.k$m_pred < DataSet.k$THRESH) & (DataSet.k$OUTCOME == 0)) + sum((DataSet.k$m_pred >= DataSet.k$THRESH) & (DataSet.k$OUTCOME == 1)))/nrow(DataSet)
  }
  return(PredPerfDF)
}
####################################################################################################################################
####################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------###
###                                             Function 4: Assign_Risk_Scores                                                   ###                                         
###------------------------------------------------------------------------------------------------------------------------------###
### Assign_Risk_Scores --> Determines the risk score for a specified group of predicted "probabilities" for a given set of       ###
###                        thresholds.                                                                                           ###
###------------------------------------------------------------------------------------------------------------------------------###
### Function Inputs:                                                                                                             ###
### 1) Thresholds - A vector of thresholds for each level of the protected attribute for each threshold.  This is a vector, as   ###
###                 opposed to a matrix since the optimizer used to find the post-fairness-corrected thresholds needs a vector   ###
###                 specification.                                                                                               ###
### 2) DataSet - The sample/subsample for which the risk scores will be assigned  Consists of three columns: Outcome (0/1),      ###
###              Protected Attribute Level, and predicted probability.  Columns must be in this order.                           ###
### 3) ProtAtrLevels - The levels of the protected attribute.                                                                    ###
### 4) NumThresholds - The number of thresholds.                                                                                 ###
### 5) ThresholdNames - The names of the various thresholds (e.g. Low-Risk, Average-Risk, and High-Risk).                        ###
### 6) ProtAttrSubAudit - Indicates whether or not the function is being called on to assign risk scores for a protected         ###
###                       attribute as part of a subaudit, which is to say that the fairness of a protected attribute is being   ###
###                       assessed when that protected attribute was not part of the protected attribute for which the fairness  ###
###                       correction procedure was applied.  The default setting is FALSE.                                       ###
### 7) FullDataSet.SubAudit - If ProtAttrSubAudit is set to TRUE, this is a data set containing the same columns as DataSet, plus###
###                           additional columns necessary to carry out the subaudit.  The default setting is NULL.              ###
###------------------------------------------------------------------------------------------------------------------------------###
####################################################################################################################################
####################################################################################################################################
Assign_Risk_Scores <- function(Thresholds, DataSet, ProtAtrLevels, NumThresholds, ThresholdNames,
                               ProtAttrSubAudit = FALSE, FullDataSet.SubAudit = NULL){
  ### Convert Thresholds vector into a matrix in which the rows correspond to the different thresholds and the columns           ###    
  ### correspond to the different levels of the protected attribute.                                                             ###
  if(ProtAttrSubAudit == TRUE){
    ThreshMat <- matrix(Thresholds, NumThresholds, length(levels(FullDataSet.SubAudit$Actual.PA)), byrow = T)
    colnames(ThreshMat) <- levels(FullDataSet.SubAudit$Actual.PA)
    rownames(ThreshMat) <- ThresholdNames
  }else{
    ThreshMat <- matrix(Thresholds, NumThresholds, length(ProtAtrLevels), byrow = T)
    colnames(ThreshMat) <- ProtAtrLevels
    rownames(ThreshMat) <- ThresholdNames
  }
  ### Append Columns to DataSet which correspond to appropriate protected attribute level specific thresholds.                   ###
  for(k in 1:NumThresholds){
    ThreshVec <- ThreshMat[k, ]
    if(ProtAttrSubAudit == TRUE){
      FullDataSet.SubAudit$THRESH <- ThreshVec[match(FullDataSet.SubAudit$Actual.PA, levels(FullDataSet.SubAudit$Actual.PA))]
      colnames(FullDataSet.SubAudit)[ncol(FullDataSet.SubAudit)] <- str_c(ThresholdNames[k], "_THRESH")
      DataSet <- FullDataSet.SubAudit[,-which(colnames(FullDataSet.SubAudit) == "Actual.PA")]
      
      
    }else{
      DataSet$THRESH <- ThreshVec[match(DataSet$Protected_Attribute, ProtAtrLevels)]
      colnames(DataSet)[ncol(DataSet)] <- str_c(ThresholdNames[k], "_THRESH")
    }
  }
  if(NumThresholds == 1){
    DataSet$RiskScore <- (NumThresholds + 1) - (DataSet$m_pred < DataSet[, 4:ncol(DataSet)])
  }else{
    DataSet$RiskScore <- (NumThresholds + 1) - rowSums(DataSet$m_pred < DataSet[, 4:ncol(DataSet)])
  }
  return(DataSet$RiskScore)
}
####################################################################################################################################
####################################################################################################################################
###------------------------------------------------------------------------------------------------------------------------------###
###                                       Function 5: Optimize_Fair_Thresholds_fcn                                               ###                                         
###------------------------------------------------------------------------------------------------------------------------------###
### Optimize_Fair_Thresholds_fcn --> "Optimizing" this function provides the post-fairness-corrected thresholds for a given      ###
###                                  sample/subsample of observations.  The function has a penalty weight/coefficient which is   ###
###                                  multiplied by the proportion of scores that change.  The penalty is parameterized so that it###
###                                  ranges smoothly between o and 1 with 0 indicating that there is no limit on the fraction of ###
###                                  scores that can change and 1 indicating that no scores can change.  Additionally, there are ###
###                                  built in constraints, one of which is optional; see function inputs below for details       ###
###                                  surrounding the one optional constraint - Constraint.Cover.                                 ###
###------------------------------------------------------------------------------------------------------------------------------###
### Function Inputs:                                                                                                             ###
### 1) ThresholdsAll - A vector of thresholds for each level of the protected attribute for each threshold.  This is a vector,   ###
###                    as opposed to a matrix since the optimizer used to find the post-fairness-corrected thresholds needs a    ###
###                    vector specification.                                                                                     ###
### 2) DataSET - The sample/subsample for which the post-fairness-corrected thresholds are to be found. Consists of three        ###
###              columns: Outcome (0/1), Protected Attribute Level, and predicted probability.  Columns must be in this order.   ###
### 3) ProtATRlevels - The levels of the protected attribute.                                                                    ###
### 4) PREfcThresholds - The pre-fairness-corrected thresholds for the given sample/subsample (i.e. DataSET).                    ###
### 5) NumTHRESH - The number of thresholds.                                                                                     ###
### 6) THRESHnames - The names of the various thresholds (e.g. Low-Risk, Average-Risk, and High-Risk).                           ###
### 7) FairnessTYPE - The definition of algorithmic fairness to be "corrected" for.  The 9 options are the following, with       ###
###                   corresponding definitions given in one or both of the following manuscripts.                               ### 
###                   a) Alexamdra Chouldechova. "Fair prediction with disparate impact: A study of bias in recidivism           ###
###                      prediction instruments". arXiv:1703.00056v1 [stat.AP] 28 Feb 2017.                                      ###
###                   b) Verma and Rubin.  "Fairness Definitions Explained". 2018 ACM/IEEE International Workshop on Software    ###
###                      Fairness.                                                                                               ###
###                   1) ERB - Error Rate Balance                                                                                ###
###                   2) CUAE - Conditional Use Accuracy Equality                                                                ###
###                   3) PP - Predictive Parity                                                                                  ###
###                   4) SP - Statistical Parity                                                                                 ###
###                   5) TE - Treatment Equality                                                                                 ###
###                   6) PE - Predictive Equality                                                                                ###
###                   7) EO - Equal Opportunity                                                                                  ###
###                   8) OAE - Overall Accuracy Equality                                                                         ###
###                   9) CAL - Calibration                                                                                       ###
### 8) PropDeltaScoresPenalty.Weight - The weight (between 0 and 1) given to the penalty term associated with the proportion of  ###
###                                    scores that change when using ThresholdsAll vs. PREfcThresholds.  When this value is 0    ###
###                                    it corresponds to finding the greatest fairness with no regard to the proportion of       ###
###                                    scores that change.  The bigger this value gets, the greater the cost for changing scores.###
###                                    A value of 1 will force the thresholds to not change at all since this weight is the      ###
###                                    complement of the weight given to the fairness value for the thresholds.                  ### 
###                                    This penalty term should serve as a tuning parameter.                                     ###
### 9) Optimizer.Approach - The approach being utilized to optimize over the sets of thresholds.  There are two options:         ###
###                         a) "Simultaneous", indicates that the optimizer tries to find the optimal set of PostFC thresholds   ###
###                                           across all thresholds (e.g. LR, AR, and HR) at once. THIS APPROACH SHOULD ONLY BE  ###
###                                           USED WHEN CALIBRATION IS THE SELECTED DEFINITION OF ALGORITHMIC FAIRNESS.          ###
###                         b) "Sequential" indicates that the optimizer tries to find the optimal set of PostFC thresholds one  ###
###                                         threshold at a time (e.g. LR -> AR -> HR).  THIS APPROACH SHOULD BE USED WHEN THE    ###
###                                         SELECTED DEFINITION OF ALGORITHMIC FAIRNESS IS NOT CALIBRATION.                      ###
### 10) Optimizer.Type - The optimization technique utilized.  There are two options:                                            ###
###                      a) "optim" uses the base optim function with the default "Nelder-Mead".                                 ###
###                      b) "GenSA" uses the GenSA function within the GenSA package to perform simulated annealing.  This       ###
###                         technique can be extremely computationally expensive (see below for a hyperparameter related to this ###
###                         technique), but can be useful if the solution space is complex with many local optima and optim is   ###
###                         potentially coming up with a really suboptimal solution.                                             ###
### 11) Sequential.LB - The lower bounds for the thresholds if Optimizer.Type = "Sequential".  So if, for example, the Average   ###
###                     Risk threshold is being optimized, then the Sequential.LB is the set of PostFC thresholds obtained for   ###
###                     Low Risk threshold.  When Optimizer.Type = "Simultaneous", this function argument is not required/       ###
###                     relevant; hence, the default for Sequential.LB is NA.                                                    ###
### 11) Sequential.UB - The upper bounds for the thresholds if Optimizer.Type = "Sequential".  So if, for example, the Average   ###
###                     Risk threshold is being optimized, then the Sequential.UB is the set of PreFC thresholds obtained for    ###
###                     High Risk threshold.  When Optimizer.Type = "Simultaneous", this function argument is not required/      ###
###                     relevant; hence, the default for Sequential.UB is NA.                                                    ###
### 12) Constraint.Cover - Optional constraint specifying whether viable solutions must "cover" the pre-FC thresholds (e.g. the  ###
###                        minimum PostFC low-risk threshold across the levels of the protected attribute is smaller than the    ###
###                        PreFC low-risk threshold AND the maximum PostFC low-risk threshold across the levels of the protected ###
###                        attribute is greater than the PreFC low-risk threshold).  Default setting is TRUE.                    ###
###------------------------------------------------------------------------------------------------------------------------------###
####################################################################################################################################
####################################################################################################################################
Optimize_Fair_Thresholds_fcn <- function(ThresholdsAll, DataSET, ProtATRlevels, PREfcThresholds, NumTHRESH, THRESH_names, 
                                         FairnessTYPE, PropDeltaScoresPenalty.Weight, Optimize.Approach, Optimize.Type, 
                                         Sequential.LB = NA, Sequential.UB = NA, Constraint.Cover = TRUE){
  ### Create an alternative format for ThresholdsAll that is needed below.                                                       ###
  ThreshMat <- matrix(ThresholdsAll, NumTHRESH, length(ProtATRlevels), byrow = T)
  ### Calculate specified fairness metric at each threshold/score.                                                               ###
  if(FairnessTYPE != "CAL"){
    FairnessValues <- rep(NA, NumTHRESH)
    Fairness_Info <- Compute_Fairness_Measures(Thresholds = ThresholdsAll, DataSet = DataSET, ProtAtrLevels = ProtATRlevels, 
                                          NumThresholds = NumTHRESH, ThresholdNames = THRESH_names, FairnessType = FairnessTYPE)
    FairnessValues <- Fairness_Info[[(NumTHRESH + 1)]][, 2]
  }else{
    FairnessValues <- rep(NA, (NumTHRESH + 1))
    Fairness_Info <- Compute_Fairness_Measures(Thresholds = ThresholdsAll, DataSet = DataSET, ProtAtrLevels = ProtATRlevels, 
                                               NumThresholds = NumTHRESH, ThresholdNames = THRESH_names, FairnessType = FairnessTYPE)
    FairnessValues <- Fairness_Info[[2]][, 2]
  }
  ### Determine if there is a constraint violation for "disorderly" thresholds (e.g. the low-risk threshold is higher than the   ###
  ### average-risk threshold).  Only applicable is Optimize.Approach = "Simultaneous".                                           ###
  DisorderlyThresholds <- 0
  if(Optimize.Approach == "Simultaneous"){
    for(j in 1:length(ProtATRlevels)){
      DisorderlyThresholds <- max(DisorderlyThresholds, (sum(ThreshMat[, j] == sort(ThreshMat[, j])) != NumTHRESH))
    }
    DisorderlyThresholds.Constraint <- DisorderlyThresholds
  }else{DisorderlyThresholds.Constraint <- 0}
  ### Determine if there is a constraint violation for thresholds being impossible; i.e. less than 0 or greater than 1. ###
  if(Optimize.Approach == "Simultaneous"){
    ImpossibleThresholds.Constraint <- sum(ThresholdsAll < 0 | ThresholdsAll > 1)
  }else{
    ImpossibleThresholds.Constraint <- sum(ThresholdsAll < Sequential.LB | ThresholdsAll > Sequential.UB)
  }
  ### Determine if there is a constraint violation for "non-covering" thresholds (e.g. the PostFC low-risk threshold for each protected attribute ###
  ### level is larger/smaller than its PreFC threshold counterpart).  Only applicable is optional constraint, Constraint.Cover, is set equal to TRUE. ###
  
  if(Constraint.Cover == TRUE){
    ThreshMat.RowMin <- apply(ThreshMat, 1, min)
    ThreshMat.RowMax <- apply(ThreshMat, 1, max)
    if(PropDeltaScoresPenalty.Weight != 1){
      CoverMat <- apply(cbind(unique(PREfcThresholds) > ThreshMat.RowMin, unique(PREfcThresholds) < ThreshMat.RowMax), 1, sum)
    }else{CoverMat <- apply(cbind(unique(PREfcThresholds) >= ThreshMat.RowMin, unique(PREfcThresholds) <= ThreshMat.RowMax), 1, sum)
    }
    if(sum(CoverMat < 2) > 0){NonCoveringThresholds <- 1}else{NonCoveringThresholds <- 0}
  }else{NonCoveringThresholds <- 0}
  ### Determine the proportion of risk scores that change between the pre-fairness-corrected risk scores and the proposed post-  ###
  ### fairness-corrected risk scores.                                                                                            ###
  PreFC.RiskScores <- Assign_Risk_Scores(Thresholds = PREfcThresholds, DataSet = DataSET, ProtAtrLevels = ProtATRlevels, 
                                         NumThresholds = NumTHRESH, ThresholdNames = THRESH_names)
  THRESH.RiskScores <- Assign_Risk_Scores(Thresholds = ThresholdsAll, DataSet = DataSET, ProtAtrLevels = ProtATRlevels, 
                                          NumThresholds = NumTHRESH, ThresholdNames = THRESH_names)  
  PropDeltaScores <- sum(THRESH.RiskScores != PreFC.RiskScores)/length(THRESH.RiskScores)
  ### Calculate the value to be minimized by optimizer.  The idea is to find the least unfair set of thresholds that change as   ###
  ### few of risk scores as possible.                                                                                            ###
  w <- PropDeltaScoresPenalty.Weight
  ValToMinimize <- ((1 - w) * sum(1 - FairnessValues)) + (w * PropDeltaScores) + 
    1000 * DisorderlyThresholds.Constraint + 1000 * ImpossibleThresholds.Constraint + 1000 * NonCoveringThresholds
  return(ValToMinimize)
}
###################################################################################################################################
#---------------------------------------------------------------------------------------------------------------------------------#
###                                                 Function 6: ParOptimThresh_fcn                                              ###                                         
#---------------------------------------------------------------------------------------------------------------------------------#
###################################################################################################################################
###################################################################################################################################
### ParOptimThresh_fcn -- This function serves to parallelize the following process: for a random subsample/resample find the   ###
### post-fairness-corrected thresholds by optimizing (via the base-R optim or the GenSA simulated annealing appraoch in the     ###
### GenSA package) the function Optimize_Fair_Thresholds_fcn.                                                                   ###
###-----------------------------------------------------------------------------------------------------------------------------###
### Function Inputs:                                                                                                            ###
### 1) i - this is simply the index for the parallelizing process.                                                              ###
### 2) DataToSS - The data set from which the resample/subsample is taken. This data set consists of at least 3 columns:        ###
###                a) Outcome (0/1; named "OUTCOME")                                                                            ###
###                b) Protected Attribute Level (named "Protected_Attribute")                                                   ###
###                c) predicted probability (named "m_pred)                                                                     ###
###               Any additional columns are necessary for obtaining relevant resamples/subsamples.  Note that the columns must ###
###               be in this order with these names.                                                                            ###
### 3) ProtAttrLevs - The levels of the protected attribute.                                                                    ###
### 4) NumberTHRESH - The number of thresholds.                                                                                 ###
### 5) THRESHnames - The names of the various thresholds (e.g. Low-Risk, Average-Risk, and High-Risk).                          ###
### 6) FAIRmetric - The definition of algorithmic fairness to be "corrected" for.  The 9 options are the following, with        ###
###                 corresponding definitions given in one or both of the following manuscripts.                                ### 
###                 a) Alexamdra Chouldechova. "Fair prediction with disparate impact: A study of bias in recidivism            ###
###                    prediction instruments". arXiv:1703.00056v1 [stat.AP] 28 Feb 2017.                                       ###
###                 b) Verma and Rubin.  "Fairness Definitions Explained". 2018 ACM/IEEE International Workshop on Software     ###
###                    Fairness.                                                                                                ###
###                 1) ERB - Error Rate Balance                                                                                 ###
###                 2) CUAE - Conditional Use Accuracy Equality                                                                 ###
###                 3) PP - Predictive Parity                                                                                   ###
###                 4) SP - Statistical Parity                                                                                  ###
###                 5) TE - Treatment Equality                                                                                  ###
###                 6) PE - Predictive Equality                                                                                 ###
###                 7) EO - Equal Opportunity                                                                                   ###
###                 8) OAE - Overall Accuracy Equality                                                                          ###
###                 9) CAL - Calibration                                                                                        ###
### 7) propSCOREchangePENALTY.wt - The weight (between 0 and 1) given to the penalty term associated with the proportion of     ###
###                                scores that change when using ThresholdsAll vs. PREfcThresholds.  When this value is 0       ###
###                                it corresponds to finding the greatest fairness with no regard to the proportion of          ###
###                                scores that change.  The bigger this value gets, the greater the cost for changing scores.   ###
###                                A value of 1 will force the thresholds to not change at all since this weight is the         ###
###                                complement of the weight given to the fairness value for the thresholds.                     ### 
###                                This penalty term should serve as a tuning parameter.                                        ### 
### 8) PreFC.Thresh.Inputs - a (Number of Threshold) by 2 data frame in which the first column indicates the Method parameter   ###
###                          of the FindPreFCthresh_fcn and the second column indicates the PreProbPerc parameter of the        ###
###                          FindPreFCthresh_fcn.                                                                               ###
### 9) Optimizer.Approach - The approach being utilized to optimize over the sets of thresholds.  There are two options:        ###
###                         a) "Simultaneous", indicates that the optimizer tries to find the optimal set of PostFC thresholds  ###
###                                           across all thresholds (e.g. LR, AR, and HR) at once. THIS APPROACH SHOULD ONLY BE ###
###                                           USED WHEN CALIBRATION IS THE SELECTED DEFINITION OF ALGORITHMIC FAIRNESS.         ###
###                         b) "Sequential" indicates that the optimizer tries to find the optimal set of PostFC thresholds one ###
###                                         threshold at a time (e.g. LR -> AR -> HR).  THIS APPROACH SHOULD BE USED WHEN THE   ###
###                                         SELECTED DEFINITION OF ALGORITHMIC FAIRNESS IS NOT CALIBRATION.                     ###
### 10) Optimizer.Type - The optimization technique utilized.  There are two options:                                           ###
###                      a) "optim" uses the base optim function with the default "Nelder-Mead".                                ###
###                      b) "GenSA" uses the GenSA function within the GenSA package to perform simulated annealing.  This      ###
###                         technique can be extremely computationally expensive (see below for a hyperparameter related to this###
###                         technique), but can be useful if the solution space is complex with many local optima and optim is  ###
###                         potentially coming up with a really suboptimal solution.                                            ###
### 11) GenSA.MaxRunTime - This is a hyperparameter setting for the "GenSA" technique which dictates how long (in seconds) the  ###
###                        user is willing to wait to obtain a solution.  If Optimizer.Approach = "Simultaneous", then this     ###
###                        function input is a scalar.  This default setting is that this input is a scalar with value 30       ###
###                        seconds, though for more complex problems, this likely should be increased.                          ###
###                        When Optimizer.Approach = "Sequential", then this function argument is a vector of length equal to   ###
###                        the number of thresholds; this always different specified maximum search times for the different     ###
###                        thresholds. Also note that the GenSA function has additional alternative control hyperparameters     ###
###                        which may be better to explore with complex problems than the max.time hyperparameter, but this keeps###
###                        it relatively simple.                                                                                ###
### 12) Constraint.COVER - Optional constraint specifying whether viable solutions must "cover" the pre-FC thresholds (e.g. the ###
###                        minimum PostFC low-risk threshold across the levels of the protected attribute is smaller than the   ###
###                        PreFC low-risk threshold AND the maximum PostFC low-risk threshold across the levels of the protected###
###                        attribute is greater than the PreFC low-risk threshold).  Default setting is TRUE.                   ###
###-----------------------------------------------------------------------------------------------------------------------------###
### The function outputs the pre- and post-fairness-corrected thresholds for the given subsample.                               ###
###################################################################################################################################
###################################################################################################################################
ParOptimThresh_fcn <- function(i, DataToSS, ProtAttrLevs, NumberTHRESH, THRESHnames, FAIRmetric, propSCOREchangePENALTY.wt, 
                               PreFC.Thresh.Inputs, Optimizer.Approach, Optimizer.Type, GenSA.MaxRunTime = 30, 
                               Constraint.COVER = TRUE){
  ### Some Stop Statements for function argument miss-specification. ###
  if(FAIRmetric != "CAL" & Optimizer.Approach == "Simultaneous"){stop("When the selected definition of algorithmic fairness is not CAL (i.e. Calibration), the selected option for Optimizer.Approach should be Sequential, not Simultaneous.", call. = FALSE)}
  if(FAIRmetric == "CAL" & Optimizer.Approach == "Sequential"){stop("When the selected definition of algorithmic fairness is CAL (i.e. Calibration), the selected option for Optimizer.Approach should be Simultaneous, not Sequential.", call. = FALSE)}
  if(Optimizer.Approach == "Simultaneous" & Optimizer.Type == "GenSA" & length(GenSA.MaxRunTime) != 1){stop("When the optimizer type is GenSA and the optimizer approach is Simultaneous, GenSA.MaxRunTime should be a vector of length one", call. = FALSE)}
  if(Optimizer.Approach == "Sequential" & Optimizer.Type == "GenSA" & length(GenSA.MaxRunTime) != NumberTHRESH){stop("When the optimizer type is GenSA and the optimizer approach is Sequential, GenSA.MaxRunTime should be a vector of length NumberTHRESH.", call. = FALSE)}
  SubSamp <- Application_Specific_Resampling_fcn(data_set = DataToSS)
  PreFCthresh <- c()
  for(k2 in 1:NumberTHRESH){
    PreFCthresh.k2 <- FindPreFCthresh_fcn(PredProbVec = SubSamp$m_pred, Method = PreFC.Thresh.Inputs$Method[k2], PreProbPerc = PreFC.Thresh.Inputs$PreProbPerc[k2])
    PreFCthresh <- c(PreFCthresh, rep(as.vector(PreFCthresh.k2), length(ProtAttrLevs)))
  }
  PreFCthreshMat <- matrix(PreFCthresh, nrow = NumberTHRESH, ncol = length(ProtAttrLevs), byrow = TRUE)
  if(Optimizer.Approach == "Simultaneous"){
    if(Optimizer.Type == "optim"){
      Output.Optimizer <- optim(par = PreFCthresh, fn = Optimize_Fair_Thresholds_fcn, DataSET = SubSamp, ProtATRlevels = ProtAttrLevs, 
                                PREfcThresholds = PreFCthresh, NumTHRESH = NumberTHRESH, THRESH_names = THRESHnames, FairnessTYPE = FAIRmetric, 
                                PropDeltaScoresPenalty.Weight = propSCOREchangePENALTY.wt, Optimize.Approach = "Simultaneous", 
                                Optimize.Type = "optim", Sequential.LB = NA, Sequential.UB = NA, 
                                Constraint.Cover = Constraint.COVER)
      PostFCthresholds <- unlist(Output.Optimizer$par)
    }else{ # Optimizer.Type == "GenSA"
      LOWER.bounds <- rep(0, length(ProtAttrLevs))
      UPPER.bounds <- rep(1, length(ProtAttrLevs))
      if(NumberTHRESH > 1){
        LOWER.bounds <- c(LOWER.bounds, PreFCthresh[1:(length(PreFCthresh) - (length(ProtAttrLevs)))])
        UPPER.bounds <- c(PreFCthresh[(length(ProtAttrLevs) + 1):length(PreFCthresh)], UPPER.bounds)
      }
      Output.Optimizer <- GenSA(par = PreFCthresh, fn = Optimize_Fair_Thresholds_fcn, lower = LOWER.bounds, 
                                upper = UPPER.bounds, DataSET = SubSamp, ProtATRlevels = ProtAttrLevs, 
                                PREfcThresholds = PreFCthresh, NumTHRESH = NumberTHRESH, THRESH_names = THRESHnames, 
                                FairnessTYPE = FAIRmetric, PropDeltaScoresPenalty.Weight = propSCOREchangePENALTY.wt,
                                Optimize.Approach = "Simultaneous", Optimize.Type = "GenSA", Sequential.LB = NA, 
                                Sequential.UB = NA, control = list(smooth = FALSE, max.time = GenSA.MaxRunTime), 
                                Constraint.Cover = Constraint.COVER)
      PostFCthresholds <- unlist(Output.Optimizer["par"])
    }
  }else{ # Optimizer.Approach == "Sequential"
    PostFCthresholdsMat <- matrix(NA, NumberTHRESH, length(ProtAttrLevs))
    if(Optimizer.Type == "optim"){
      LOWER.bounds <- rep(0, length(ProtAttrLevs))
      if(NumberTHRESH == 1){
        UPPER.bounds <- rep(1, length(ProtAttrLevs))
      }else{
        UPPER.bounds <- c(PreFCthreshMat[2, ])
      }
      Output.Optimizer <- optim(par = PreFCthreshMat[1,], fn = Optimize_Fair_Thresholds_fcn, DataSET = SubSamp, ProtATRlevels = ProtAttrLevs, 
                                PREfcThresholds = PreFCthreshMat[1,], NumTHRESH = 1, THRESH_names = THRESHnames[1], FairnessTYPE = FAIRmetric, 
                                PropDeltaScoresPenalty.Weight = propSCOREchangePENALTY.wt,  Optimize.Approach = "Sequential", 
                                Optimize.Type = "optim", Sequential.LB = LOWER.bounds, Sequential.UB = UPPER.bounds, Constraint.Cover = Constraint.COVER)
      PostFCthresholdsMat[1, ] <- unlist(Output.Optimizer$par)
      if(NumberTHRESH > 1){
        for(j7 in 2:NumberTHRESH){
          LOWER.bounds <- PostFCthresholdsMat[(j7 - 1),]
          if(j7 < NumberTHRESH){
            UPPER.bounds <- PreFCthreshMat[(j7 + 1),]
          }else{UPPER.bounds <- rep(1, length(ProtAttrLevs))}
          Output.Optimizer <- optim(par = PreFCthreshMat[j7,], fn = Optimize_Fair_Thresholds_fcn, DataSET = SubSamp, ProtATRlevels = ProtAttrLevs, 
                                    PREfcThresholds = PreFCthreshMat[j7,], NumTHRESH = 1, THRESH_names = THRESHnames[j7], FairnessTYPE = FAIRmetric, 
                                    PropDeltaScoresPenalty.Weight = propSCOREchangePENALTY.wt,  Optimize.Approach = "Sequential", 
                                    Optimize.Type = "optim", Sequential.LB = LOWER.bounds, Sequential.UB = UPPER.bounds, 
                                    Constraint.Cover = Constraint.COVER)
          PostFCthresholdsMat[j7, ] <- unlist(Output.Optimizer$par)
        }
      }
    }else{ # Optimizer.Type == "GenSA"
      LOWER.bounds <- rep(0, length(ProtAttrLevs))
      
      if(NumberTHRESH == 1){
        UPPER.bounds <- rep(1, length(ProtAttrLevs))
      }else{
        UPPER.bounds <- c(PreFCthreshMat[2, ])
      }
      Output.Optimizer <- GenSA(par = PreFCthreshMat[1,], fn = Optimize_Fair_Thresholds_fcn, lower = LOWER.bounds, 
                                upper = UPPER.bounds, DataSET = SubSamp, ProtATRlevels = ProtAttrLevs, 
                                PREfcThresholds = PreFCthreshMat[1,], NumTHRESH = 1, THRESH_names = THRESHnames[1], 
                                FairnessTYPE = FAIRmetric, PropDeltaScoresPenalty.Weight = propSCOREchangePENALTY.wt,
                                Optimize.Approach = "Sequential", Optimize.Type = "GenSA", Sequential.LB = LOWER.bounds, 
                                Sequential.UB = UPPER.bounds, control = list(smooth = FALSE, max.time = GenSA.MaxRunTime[1]), 
                                Constraint.Cover = Constraint.COVER)
      PostFCthresholdsMat[1, ] <- unlist(Output.Optimizer["par"])
      if(NumberTHRESH > 1){
        for(j7 in 2:NumberTHRESH){
          LOWER.bounds <- PostFCthresholdsMat[(j7 - 1),]
          if(j7 < NumberTHRESH){
            UPPER.bounds <- PreFCthreshMat[(j7 + 1),]
          }else{UPPER.bounds <- rep(1, length(ProtAttrLevs))}
          Output.Optimizer <- GenSA(par = PreFCthreshMat[j7,], fn = Optimize_Fair_Thresholds_fcn, lower = LOWER.bounds, 
                                    upper = UPPER.bounds, DataSET = SubSamp, ProtATRlevels = ProtAttrLevs, 
                                    PREfcThresholds = PreFCthreshMat[j7,], NumTHRESH = 1, THRESH_names = THRESHnames[j7], 
                                    FairnessTYPE = FAIRmetric, PropDeltaScoresPenalty.Weight = propSCOREchangePENALTY.wt,
                                    Optimize.Approach = "Sequential", Optimize.Type = "GenSA", Sequential.LB = LOWER.bounds, 
                                    Sequential.UB = UPPER.bounds, control = list(smooth = FALSE, max.time = GenSA.MaxRunTime[j7]), 
                                    Constraint.Cover = Constraint.COVER)
          PostFCthresholdsMat[j7, ] <- unlist(Output.Optimizer["par"])
        }
      }
    }
    PostFCthresholds <- as.vector(t(PostFCthresholdsMat))
  }
  ThreshMat <- rbind(PreFCthresh, PostFCthresholds)
  colnames(ThreshMat) <- str_c(str_c(rep(THRESHnames, each = length(ProtAttrLevs)), "_"), rep(ProtAttrLevs, NumberTHRESH))
  ThreshMat <- cbind(ThreshMat, rep(i, 2))
  colnames(ThreshMat)[ncol(ThreshMat)] <- "SubSampID"
  ThreshDF <- as.data.frame(ThreshMat)
  ThreshDF$ThreshType <- c("PreFC", "PostFC")
  ThreshDF$OptimizerType <- rep(Optimizer.Type, 2)
  ThreshDF$OptimizerApproach <- rep(Optimizer.Approach, 2)
  ThreshDF$OptimizerProcess <- str_c(str_c(ThreshDF$OptimizerType, "_"), ThreshDF$OptimizerApproach)
  ThreshDF$PenaltyWeight <- rep(propSCOREchangePENALTY.wt, 2)
  return(ThreshDF)
}
###################################################################################################################################
#---------------------------------------------------------------------------------------------------------------------------------#
###                                           Function 7: ParTuningGridAssessment_fcn                                           ###                                         
#---------------------------------------------------------------------------------------------------------------------------------#
###################################################################################################################################
###################################################################################################################################
### ParTuningGridAssessment_fcn -- This function serves to parallelize the following process: for a given set of (pre- and)     ###
### post-fairness-corrected thresholds obtained for a given penalty weight within the grid of penalty weight specifications for ###
### ParOptimThresh_fcn and Optimize_Fair_Thresholds_fcn, take a large number of resamples/subsamples and calculate the average  ###
### value of each predictive performance and algorithmic fairness measure across all such subsamples.                           ###
###-----------------------------------------------------------------------------------------------------------------------------###
### Function Inputs:                                                                                                            ###
### 1) i - this is simply the index for the parallelizing process.                                                              ###
### 2) randSeedNumber - the random seed to enable reproduction of results.                                                      ###
### 3) NumSS - The number of random subsamples to utilize in obtaining the average results.                                     ###
### 4) DataToSS - The data set from which the resample/subsample is taken. This data set consists of at least 3 columns:        ###
###                a) Outcome (0/1; named "OUTCOME")                                                                            ###
###                b) Protected Attribute Level (named "Protected_Attribute")                                                   ###
###                c) predicted probability (named "m_pred)                                                                     ###
###               Any additional columns are necessary for obtaining relevant resamples/subsamples.  Note that the columns must ###
###               be in this order with these names.                                                                            ###
### 5) ProtAttrLevs - The levels of the protected attribute.                                                                    ###
### 6) NumberTHRESH - The number of thresholds.                                                                                 ###
### 7) THRESHnames - The names of the various thresholds (e.g. Low-Risk, Average-Risk, and High-Risk).                          ###
### 8) FAIRmetric - The definition of algorithmic fairness to be "corrected" for.  The 9 options are the following, with        ###
###                 corresponding definitions given in one or both of the following manuscripts.                                ### 
###                 a) Alexamdra Chouldechova. "Fair prediction with disparate impact: A study of bias in recidivism            ###
###                    prediction instruments". arXiv:1703.00056v1 [stat.AP] 28 Feb 2017.                                       ###
###                 b) Verma and Rubin.  "Fairness Definitions Explained". 2018 ACM/IEEE International Workshop on Software     ###
###                    Fairness.                                                                                                ###
###                 1) ERB - Error Rate Balance                                                                                 ###
###                 2) CUAE - Conditional Use Accuracy Equality                                                                 ###
###                 3) PP - Predictive Parity                                                                                   ###
###                 4) SP - Statistical Parity                                                                                  ###
###                 5) TE - Treatment Equality                                                                                  ###
###                 6) PE - Predictive Equality                                                                                 ###
###                 7) EO - Equal Opportunity                                                                                   ###
###                 8) OAE - Overall Accuracy Equality                                                                          ###
###                 9) CAL - Calibration                                                                                        ###
### 9) OptimizerMix - Indicates whether the set of thresholds is a mix across different optimization processes (e.g. sequential ###
###                   optimization for AverageRisk and HighRisk Thresholds and sequential GenSA for LowRisk Thresholds).  The   ###
###                   default for this argument is FALSE.                                                                       ###
###-----------------------------------------------------------------------------------------------------------------------------###
### The function outputs the average values and sd's of performance (predictive and fairness) across all subsamples, for each   ###
### threshold (i.e., low-risk, average-risk, and high-risk), for both the pre- and post-fairness-corrected thresholds.          ###
### Additionally, it outputs (in a duplicative manner) the subsample aggregated PreFC and PostFC thresholds that were used in   ###
### obtaining the performance and fairness vaules, along with the average minimum value of the optimized objective function.    ###
###################################################################################################################################
###################################################################################################################################
ParTuningGridAssessment_fcn <- function(i, randSeedNumber, NumSS, DataToSS, ProtAttrLevs, NumberTHRESH, THRESHnames, FAIRmetric,
                                        OptimizerMix = FALSE){
  Thresholds <- ThreshStorageList.Grid[[i]]
  propSCOREchangePENALTY.wt <- PropScoreChangePenaltyWeight.TuningGrid[i]
  ### Find subsample aggregated PreFC and PostFC thresholds. ###
  SSpreFCthresh <- Thresholds %>% filter(ThreshType == "PreFC")
  PreFCthresh <- colMeans(SSpreFCthresh[, 1:(NumberTHRESH * length(ProtAttrLevs))])
  SSpostFCthresh <- Thresholds %>% filter(ThreshType == "PostFC")
  PostFCthresh <- colMeans(SSpostFCthresh[, 1:(NumberTHRESH * length(ProtAttrLevs))])
  ### Across specified number of subsamples, compute the average fairness and predictive performance measures. ###
  ### Additionally, determine the proportion of scores that change between the PreFC and PostFC thresholds. ###
  set.seed(randSeedNumber)
  PerformanceResultsSS <- data.frame()
  PropDeltaScoresSS <- data.frame()
  if(FAIRmetric == "CAL"){
    FairnessResultsSS <- data.frame()
  }
  for(j in 1:NumSS){
    SubSamp.j <- Application_Specific_Resampling_fcn(data_set = DataToSS)
    ### Calculate proportion of scores that change between PreFC and PostFC thresholds for the jth subsample. ###
    RiskScoresPreFC.j <- Assign_Risk_Scores(Thresholds = PreFCthresh, DataSet = SubSamp.j, ProtAtrLevels = ProtAttrLevs, 
                                            NumThresholds = NumberTHRESH, ThresholdNames = THRESHnames)
    RiskScoresPostFC.j <- Assign_Risk_Scores(Thresholds = PostFCthresh, DataSet = SubSamp.j, ProtAtrLevels = ProtAttrLevs, 
                                             NumThresholds = NumberTHRESH, ThresholdNames = THRESHnames)  
    PropDeltaScores.j <- sum(RiskScoresPostFC.j != RiskScoresPreFC.j)/length(RiskScoresPostFC.j)
    PropDeltaScoresDF.j <- data.frame(PropDeltaScores = PropDeltaScores.j)
    ### PreFC -- Calculate fairness and predictive performance measures for the jth subsample. ###
    PPresultsPreFC.j <- Compute_Predictive_Performance_Measures(Thresholds = PreFCthresh, DataSet = SubSamp.j, ProtAtrLevels = ProtAttrLevs, NumThresholds = NumberTHRESH, 
                                                                ThresholdNames = THRESHnames)
    PPresultsPreFC.j$ThreshType <- "PreFC"
    PPresultsPreFC.j$SubSampID <- j
    PPresultsPreFC.j$PerformanceMeasure <- rownames(PPresultsPreFC.j)
    PPresultsPreFC.j <- PPresultsPreFC.j %>% gather(THRESHnames, key = Threshold, value = PerformanceValue)
    FairnessResultsPreFC.j <- Compute_Fairness_Measures(Thresholds = PreFCthresh, DataSet = SubSamp.j, ProtAtrLevels = ProtAttrLevs, NumThresholds = NumberTHRESH, 
                                                        ThresholdNames = THRESHnames, FairnessType = FAIRmetric)
    if(FAIRmetric == "ERB"){
      FairnessResultsPreFC.j <- FairnessResultsPreFC.j$Error_Rate_Balance
    }
    if(FAIRmetric == "CUAE"){
      FairnessResultsPreFC.j <- FairnessResultsPreFC.j$Conditional_Use_Accuracy_Equality
    }
    if(FAIRmetric == "PP"){
      FairnessResultsPreFC.j <- FairnessResultsPreFC.j$Predictive_Parity
    }
    if(FAIRmetric == "SP"){
      FairnessResultsPreFC.j <- FairnessResultsPreFC.j$Statistical_Parity
    }
    if(FAIRmetric == "TE"){
      FairnessResultsPreFC.j <- FairnessResultsPreFC.j$Treatment_Equality
    }
    if(FAIRmetric == "PE"){
      FairnessResultsPreFC.j <- FairnessResultsPreFC.j$Predictive_Equality
    }
    if(FAIRmetric == "EO"){
      FairnessResultsPreFC.j <- FairnessResultsPreFC.j$Equal_Opportunity
    }
    if(FAIRmetric == "OAE"){
      FairnessResultsPreFC.j <- FairnessResultsPreFC.j$Overall_Accuracy_Equality
    }
    if(FAIRmetric == "CAL"){
      FairnessResultsPreFC.j <- FairnessResultsPreFC.j$Calibration
    }
    FairnessResultsPreFC.j$ThreshType <- "PreFC"
    FairnessResultsPreFC.j$SubSampID <- j
    FairnessResultsPreFC.j <- FairnessResultsPreFC.j %>% gather(FAIRmetric, key = PerformanceMeasure, value = PerformanceValue)
    if(FAIRmetric != "CAL"){
      PerformanceResultsPreFC.j <- bind_rows(PPresultsPreFC.j, FairnessResultsPreFC.j)
    }
    ### PostFC -- Calculate fairness and predictive performance measures for the jth subsample. ###
    PPresultsPostFC.j <- Compute_Predictive_Performance_Measures(Thresholds = PostFCthresh, DataSet = SubSamp.j, ProtAtrLevels = ProtAttrLevs, NumThresholds = NumberTHRESH, 
                                                                 ThresholdNames = THRESHnames)
    PPresultsPostFC.j$ThreshType <- "PostFC"
    PPresultsPostFC.j$SubSampID <- j
    PPresultsPostFC.j$PerformanceMeasure <- rownames(PPresultsPostFC.j)
    PPresultsPostFC.j <- PPresultsPostFC.j %>% gather(THRESHnames, key = Threshold, value = PerformanceValue)
    FairnessResultsPostFC.j <- Compute_Fairness_Measures(Thresholds = PostFCthresh, DataSet = SubSamp.j, ProtAtrLevels = ProtAttrLevs, NumThresholds = NumberTHRESH, 
                                                         ThresholdNames = THRESHnames, FairnessType = FAIRmetric)
    if(FAIRmetric == "ERB"){
      FairnessResultsPostFC.j <- FairnessResultsPostFC.j$Error_Rate_Balance
    }
    if(FAIRmetric == "CUAE"){
      FairnessResultsPostFC.j <- FairnessResultsPostFC.j$Conditional_Use_Accuracy_Equality
    }
    if(FAIRmetric == "PP"){
      FairnessResultsPostFC.j <- FairnessResultsPostFC.j$Predictive_Parity
    }
    if(FAIRmetric == "SP"){
      FairnessResultsPostFC.j <- FairnessResultsPostFC.j$Statistical_Parity
    }
    if(FAIRmetric == "TE"){
      FairnessResultsPostFC.j <- FairnessResultsPostFC.j$Treatment_Equality
    }
    if(FAIRmetric == "PE"){
      FairnessResultsPostFC.j <- FairnessResultsPostFC.j$Predictive_Equality
    }
    if(FAIRmetric == "EO"){
      FairnessResultsPostFC.j <- FairnessResultsPostFC.j$Equal_Opportunity
    }
    if(FAIRmetric == "OAE"){
      FairnessResultsPostFC.j <- FairnessResultsPostFC.j$Overall_Accuracy_Equality
    }
    if(FAIRmetric == "CAL"){
      FairnessResultsPostFC.j <- FairnessResultsPostFC.j$Calibration
    }
    FairnessResultsPostFC.j$ThreshType <- "PostFC"
    FairnessResultsPostFC.j$SubSampID <- j
    FairnessResultsPostFC.j <- FairnessResultsPostFC.j %>% gather(FAIRmetric, key = PerformanceMeasure, value = PerformanceValue)
    if(FAIRmetric != "CAL"){
      PerformanceResultsPostFC.j <- bind_rows(PPresultsPostFC.j, FairnessResultsPostFC.j)
    }
    ### Combine Results. ###
    if(FAIRmetric != "CAL"){
      PerformanceResults.j <- bind_rows(PerformanceResultsPreFC.j, PerformanceResultsPostFC.j)
      PerformanceResultsSS <- bind_rows(PerformanceResultsSS, PerformanceResults.j)
      PropDeltaScoresSS <- bind_rows(PropDeltaScoresSS, PropDeltaScoresDF.j)
    }else{
      PerformanceResults.j <- bind_rows(PPresultsPreFC.j, PPresultsPostFC.j)
      PerformanceResultsSS <- bind_rows(PerformanceResultsSS, PerformanceResults.j)
      FairnessResults.j <- bind_rows(FairnessResultsPreFC.j, FairnessResultsPostFC.j)
      FairnessResultsSS <- bind_rows(FairnessResultsSS, FairnessResults.j)
      PropDeltaScoresSS <- bind_rows(PropDeltaScoresSS, PropDeltaScoresDF.j)
    }
  } 
  ### Calculate average values and standard deviations of performance (predictive and fairness) across all subsamples, for each threshold. ###
  Results_All <- PerformanceResultsSS %>% group_by(ThreshType, Threshold, PerformanceMeasure) %>% summarize(AvgPerformanceValue = mean(PerformanceValue),
                                                                                                            SDperformanceValue = sd(PerformanceValue))
  Results_All$AvgPropDeltaScores <- mean(PropDeltaScoresSS$PropDeltaScores)
  Results_All$AvgPropDeltaScores[which(Results_All$ThreshType == "PreFC")] <- 0
  Results_All$TuningGridID <- i
  Results_All$PenaltyWeight <- propSCOREchangePENALTY.wt
  if(FAIRmetric != "CAL"){
    if(OptimizerMix == FALSE){
      Results_All$OptimizerType <- unique(Thresholds$OptimizerType)
      Results_All$OptimizerApproach <- unique(Thresholds$OptimizerApproach)
      Results_All$OptimizerProcess <- str_c(str_c(Results_All$OptimizerType, "_"), Results_All$OptimizerApproach)
    }else{
      for(j3 in 1:NumberTHRESH){
        colLoc.j3 <- which(colnames(Thresholds) == str_c(str_c("OptimizerProcess", "_"), THRESHnames[j3]))
        Results_All$PlaceHolder <- unique(Thresholds[, colLoc.j3])
        colnames(Results_All)[which(colnames(Results_All) == "PlaceHolder")] <- str_c(str_c("OptimizerProcess", "_"), THRESHnames[j3])
      }
    }
    ### Calculate and Append Average Minimum Objective Function Value. ###
    ObjFunVal.Info <- Results_All %>% filter(PerformanceMeasure == FAIRmetric) %>% group_by(ThreshType) %>% 
      summarise(AvgObjFcnVal = ((1 - sum(PenaltyWeight)/NumberTHRESH) * sum((1 - AvgPerformanceValue)) + sum(PenaltyWeight*AvgPropDeltaScores)/NumberTHRESH),  
                WEIGHT = sum(PenaltyWeight)/NumberTHRESH)
    Results_All$AvgObjFcnVal <- rep(ObjFunVal.Info$AvgObjFcnVal, each = (10*NumberTHRESH))
    ### Append the PreFC and PostFC thresholds. ###
    PreFCthresh.Mat <- matrix(PreFCthresh, nrow = NumberTHRESH, ncol = length(ProtAttrLevs), byrow = TRUE)
    colnames(PreFCthresh.Mat) <- ProtAttrLevs
    PostFCthresh.Mat <- matrix(PostFCthresh, nrow = NumberTHRESH, ncol = length(ProtAttrLevs), byrow = TRUE)
    colnames(PostFCthresh.Mat) <- ProtAttrLevs
    AddThreshNames <- str_c("SubSampAggThreshold.", ProtAttrLevs)
    for(k6 in 1:length(ProtAttrLevs)){
      Results_All$var.k6 <- NA
      colnames(Results_All)[which(colnames(Results_All) == "var.k6")] <- AddThreshNames[k6]
    }
    REL.COL.LOCS <- which(colnames(Results_All) %in% AddThreshNames)
    for(k5 in 1:NumberTHRESH){
      REL.Row.PreFC.locs.k5 <- which(Results_All$ThreshType == "PreFC" & Results_All$Threshold == THRESHnames[k5])
      Results_All[REL.Row.PreFC.locs.k5, REL.COL.LOCS] <- rep(PreFCthresh.Mat[k5, ], each = 10)
      REL.Row.PostFC.locs.k5 <- which(Results_All$ThreshType == "PostFC" & Results_All$Threshold == THRESHnames[k5])
      Results_All[REL.Row.PostFC.locs.k5, REL.COL.LOCS] <- rep(PostFCthresh.Mat[k5, ], each = 10)
    }
  }else{
    ### Append the PreFC and PostFC thresholds. ###
    PreFCthresh.Mat <- matrix(PreFCthresh, nrow = NumberTHRESH, ncol = length(ProtAttrLevs), byrow = TRUE)
    colnames(PreFCthresh.Mat) <- ProtAttrLevs
    PostFCthresh.Mat <- matrix(PostFCthresh, nrow = NumberTHRESH, ncol = length(ProtAttrLevs), byrow = TRUE)
    colnames(PostFCthresh.Mat) <- ProtAttrLevs
    AddThreshNames <- str_c("SubSampAggThreshold.", ProtAttrLevs)
    for(k6 in 1:length(ProtAttrLevs)){
      Results_All$var.k6 <- NA
      colnames(Results_All)[which(colnames(Results_All) == "var.k6")] <- AddThreshNames[k6]
    }
    REL.COL.LOCS <- which(colnames(Results_All) %in% AddThreshNames)
    for(k5 in 1:NumberTHRESH){
      REL.Row.PreFC.locs.k5 <- which(Results_All$ThreshType == "PreFC" & Results_All$Threshold == THRESHnames[k5])
      Results_All[REL.Row.PreFC.locs.k5, REL.COL.LOCS] <- rep(PreFCthresh.Mat[k5, ], each = 9)
      REL.Row.PostFC.locs.k5 <- which(Results_All$ThreshType == "PostFC" & Results_All$Threshold == THRESHnames[k5])
      Results_All[REL.Row.PostFC.locs.k5, REL.COL.LOCS] <- rep(PostFCthresh.Mat[k5, ], each = 9)
    }
    FairnessResults_All <- FairnessResultsSS %>% group_by(ThreshType, Score) %>% summarize(AvgPerformanceValue = mean(PerformanceValue),
                                                                                           SDperformanceValue = sd(PerformanceValue))
    FairnessResults_All$AvgPropDeltaScores <- mean(PropDeltaScoresSS$PropDeltaScores)
    FairnessResults_All$AvgPropDeltaScores[which(FairnessResults_All$ThreshType == "PreFC")] <- 0
    FairnessResults_All$TuningGridID <- i
    FairnessResults_All$PenaltyWeight <- propSCOREchangePENALTY.wt
    ### Calculate and Append Average Minimum Objective Function Value. ###
    ObjFunVal.Info <- FairnessResults_All %>% group_by(ThreshType) %>% 
      summarise(AvgObjFcnVal = ((1 - sum(PenaltyWeight)/(NumberTHRESH + 1)) * sum((1 - AvgPerformanceValue)) + sum(PenaltyWeight*AvgPropDeltaScores)/(NumberTHRESH + 1)),  
                WEIGHT = sum(PenaltyWeight)/(NumberTHRESH + 1))
    FairnessResults_All$AvgObjFcnVal <- rep(ObjFunVal.Info$AvgObjFcnVal, each = (NumberTHRESH + 1))
    Results_All <- list(Results_All, FairnessResults_All)
  }
  return(Results_All)
}
################################################################################################################################################
#----------------------------------------------------------------------------------------------------------------------------------------------#
###                                                 Function 8: SubSamples_Measures_fcn                                                      ###                                         
#----------------------------------------------------------------------------------------------------------------------------------------------#
################################################################################################################################################
################################################################################################################################################
### SubSamples_Measures_fcn -- For an arbitrary protected attribute, and an arbitrary number of thresholds, this function calculates various ###
### algorithmic fairness, predictive performance, and other relevant measures across a specified number of subsamples.  Such information can ###
### be used to 1) audit the initial thresholds (before any fairness-correction procedure is applied) 2) compare the pre- and post-fairness-  ###
### corrected thresholds or 3) to audit the change in fairness, and other measures, between the the pre- and post-fairness-corrected         ###
### thresholds for a separate feature that was not a part of the specified protected attribute.                                              ###
###------------------------------------------------------------------------------------------------------------------------------------------###
### DataSet.Compare -- The data set from which the resample/subsample is taken. This data set consists of at least 3 columns:                ###
###                a) Outcome (0/1; named "OUTCOME")                                                                                         ###
###                b) Protected Attribute Level (named "Protected_Attribute")                                                                ###
###                c) predicted probability (named "m_pred)                                                                                  ###
###               Any additional columns are necessary for obtaining relevant resamples/subsamples.  Note that the columns must              ###
###               be in this order with these names.                                                                                         ###
### PA.Compare -- The name of the protected attribute for which algorithmic fairness is being audited.                                       ###
### NumSubSamp.Compare -- The number of subsamples to utilize in obtaining the various measures.                                             ###
### Thresh.PreFC -- The T x P matrix of pre-fairness-corrected thresholds; T denotes the number of thresholds while P denotes the number of  ###
###                 levels of the PA.                                                                                                        ###
### Thresh.PostFC -- The T x P matrix of post-fairness-corrected thresholds; T denotes the number of thresholds while P denotes the number   ###
###                  of levels of the PA.                                                                                                    ###
### ThreshNames.Compare -- The "names" given to the various thresholds (e.g. LowRisk, AverageRisk, HighRisk); a vector of the names.         ###
### PA.Audit -- Indicates whether an initial audit of the (pre-fairness-corrected) thresholds is being performed as an initial exploration.  ###
###             The default setting is FALSE.                                                                                                ###
### PA.SubAudit -- Indicates whether a post-hoc analysis is being performed across a feature that was not part of the protected attribute    ###
###                used in finding the post-fairness-corrected thresholds.  The default setting is FALSE.                                    ###
###------------------------------------------------------------------------------------------------------------------------------------------###
### The function outputs a list in which each element of the list is a data frame consisting of the following information:                   ###
###        1) Unduplicated Protected Attribute Distribution.                                                                                 ###
###        2) Outcome Distribution Overall and by Protected Attribute Level For All Subsamples.                                              ###
###        3) Risk Score Distribution Overall and by Protected Attribute Level For All Subsamples.                                           ###
###        4) AUC by Protected Attribute Level For All Subsamples.                                                                           ###
###        5) Confusion Matrix Distribution Overall and by Protected Attribute Level For All Subsamples.                                     ###
###        6) Statistical Fairness Measures For All Subsamples.                                                                              ###
###        7) Statistical Calibration Fairness Measure For All Subsamples.                                                                   ###
###        8) Predictive Performance Measures Overall and by Protected Attribute Level For All Subsamples.                                   ###
###        9) Risk Score Change Distribution Overall and by Protected Attribute Level For All Subsamples.                                    ###
################################################################################################################################################
################################################################################################################################################
SubSamples_Measures_fcn <- function(DataSet.Compare, PA.Compare, NumSubSamp.Compare, Thresh.PreFC, Thresh.PostFC, ThreshNames.Compare, 
                                    PA.Audit = FALSE, PA.SubAudit = FALSE){
  ###################################################################################################################################
  ###################################################################################################################################
  ###                                          PART 1 -- Organize and Initialize.                                                 ###
  ###-----------------------------------------------------------------------------------------------------------------------------###
  ### 1) Create duplicate of DataSet.Compare for use within function.  Additionally, determine the column location of the PA,     ###
  ###    along with the specific levels of the PA and the corresponding number of levels for the PA.                              ###
  ### 2) Identify the number of thresholds.  Additionally, transpose the matrix of pre-fairness-corrected, as well as the matrix  ###
  ###    of post-fairness-corrected thresholds.  These matrices are needed for some of the other functions this script calls on.  ###
  ### 3) Assign risk scores to each observational unit based on the corresponding predicted probability and the specified         ###
  ###    thresholds, both for the PreFC thresholds and the PostFC thresholds.  Additionally, identify the levels of the risk      ###
  ###    scoring system as well as the number of possible risk scores.                                                            ###
  ### 4) Using a single subsample, determine the distribution of the Protected Attribute (PA); note that this will neither change ###
  ###    from subsample to subsample nor between the two sets of thresholds.                                                      ###
  ### 5) Initialize storage bins and reference information for PART 2.                                                            ###
  ###################################################################################################################################
  ###################################################################################################################################
  #-------------#
  ### Step 1. ###
  #-------------#
  Data.compare <- DataSet.Compare
  PAcolLoc <- which(colnames(Data.compare) == eval(PA.Compare))
  PAlevels.compare <- levels(unlist(Data.compare[, PAcolLoc]))
  NumPAlevels.compare <- length(PAlevels.compare)
  PA.Compare <- sym(PA.Compare) ### For use in dyplr function below; particularly the group_by function.
  #-------------#
  ### Step 2. ###
  #-------------#
  NumThresh.compare <- length(ThreshNames.Compare)
  ThreshPA.PreFC <- t(Thresh.PreFC)
  ThreshPA.PostFC <- t(Thresh.PostFC)
  #-------------#
  ### Step 3. ###
  #-------------#
  if(PA.SubAudit == TRUE){
    Reorg.Data.compare <- data.frame(OUTCOME = Data.compare$OUTCOME, Actual.PA = Data.compare$Actual.PA, m_pred = Data.compare$m_pred)
    Reorg.Data.compare <- Reorg.Data.compare %>% rename(Protected_Attribute = Actual.PA)
    FCedPAlevels <- levels(Reorg.Data.compare$Protected_Attribute)
    Score.PreFC <- Assign_Risk_Scores(Thresholds = as.vector(ThreshPA.PreFC), 
                                      DataSet = Reorg.Data.compare, 
                                      ProtAtrLevels = FCedPAlevels, NumThresholds = NumThresh.compare, ThresholdNames = ThreshNames.Compare)
    Data.compare$RiskPreFC <- as.factor(Score.PreFC)
    Score.PostFC <- Assign_Risk_Scores(Thresholds = as.vector(ThreshPA.PostFC), 
                                       DataSet = Reorg.Data.compare, 
                                       ProtAtrLevels = FCedPAlevels, NumThresholds = NumThresh.compare, ThresholdNames = ThreshNames.Compare)
    Data.compare$RiskPostFC <- as.factor(Score.PostFC)
    NumRiskScores.Compare <- NumThresh.compare + 1
    RiskScoreLevels.Compare <- levels(Data.compare$RiskPreFC)
  }else{
    if(PA.Audit == TRUE){
      Score.PreFC <- Assign_Risk_Scores(Thresholds = as.vector(ThreshPA.PreFC), 
                                        DataSet = Data.compare[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                        ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, ThresholdNames = ThreshNames.Compare)
      Data.compare$RiskPreFC <- as.factor(Score.PreFC)
      Score.PostFC <- Assign_Risk_Scores(Thresholds = as.vector(ThreshPA.PreFC), 
                                         DataSet = Data.compare[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                         ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, ThresholdNames = ThreshNames.Compare)
      Data.compare$RiskPostFC <- as.factor(Score.PostFC)
      NumRiskScores.Compare <- NumThresh.compare + 1
      RiskScoreLevels.Compare <- levels(Data.compare$RiskPreFC)
    }else{
      Score.PreFC <- Assign_Risk_Scores(Thresholds = as.vector(ThreshPA.PreFC), 
                                        DataSet = Data.compare[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                        ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, ThresholdNames = ThreshNames.Compare)
      Data.compare$RiskPreFC <- as.factor(Score.PreFC)
      Score.PostFC <- Assign_Risk_Scores(Thresholds = as.vector(ThreshPA.PostFC), 
                                         DataSet = Data.compare[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                         ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, ThresholdNames = ThreshNames.Compare)
      Data.compare$RiskPostFC <- as.factor(Score.PostFC)
      NumRiskScores.Compare <- NumThresh.compare + 1
      RiskScoreLevels.Compare <- levels(Data.compare$RiskPreFC)
    }
  }
  #------------#
  ### Step 4 ###
  #------------#
  Distbn.PA <- as.data.frame(matrix(NA, NumPAlevels.compare, 3))
  colnames(Distbn.PA) <-  c(PA.Compare, "Proportion", "Count")
  Distbn.PA[, 1] <- PAlevels.compare
  Data.compare.PAdistnOnly <- Data.compare
  colnames(Data.compare.PAdistnOnly)[4] <- "Person_ID"
  Distbn.PA$Count <- c(unlist(Data.compare.PAdistnOnly %>% group_by(Person_ID) %>% sample_n(1) %>% ungroup() %>% group_by(!!PA.Compare) %>% summarize(n = n()) %>%
                                complete(!!PA.Compare, fill = list(n = 0)) %>% ungroup() %>% select(n)))
  Distbn.PA$Proportion <- c(unlist(Data.compare.PAdistnOnly %>% group_by(Person_ID) %>% sample_n(1) %>% ungroup() %>% group_by(!!PA.Compare) %>% summarize(n = n()) %>% 
                                     complete(!!PA.Compare, fill = list(n = 0)) %>% mutate(Total = sum(n)) %>% mutate(Distbn = n/Total) %>% ungroup() %>% select(Distbn)))
  #------------#
  ### Step 5 ###
  #------------#
  CondDist.Outcome.PA <- data.frame()
  CondDist.RiskScore.PA <- data.frame()
  AUC.PA <- data.frame()
  Conf.Matrix.PA <- data.frame()
  FairMetrics <- data.frame()
  FairMetricAbrv <- c("ERB", "CUAE", "PP", "SP", "TE", "PE", "EO", "OAE")
  CAL.metric <- data.frame()
  Predictive.Performance.Metrics.PA <- data.frame()
  Predictive.Performance.Metrics.PA <- data.frame() 
  ChangeCount.PA <- data.frame()
  ScoreChangeOptions <- expand.grid(RiskScoreLevels.Compare, RiskScoreLevels.Compare)
  OutputToReturn.List <- list()
  ###################################################################################################################################
  ###################################################################################################################################
  ###                   PART 2 -- Loop through subsamples computing measures for each such subsample.                             ###
  ###-----------------------------------------------------------------------------------------------------------------------------###
  ### 1) Take a random subsample.                                                                                                 ###
  ### 2) For the given subsample, compute and store the observed distribution of the outcome by level of PA.                      ###
  ###    Additionally, find the distribution of the outcome overall.                                                              ###
  ### 3) For the given subsample, compute and store the conditional distribution of assigned risk score by level of PA for both   ###
  ###    sets of thresholds.                                                                                                      ###
  ### 4) For the given subsample, compute and store the AUC for each level of the PA.                                             ###
  ### 5) For the given subsample, compute and store the distribution of counts (TN, FN, TP, FP) making up the confusion matrix    ###
  ###    for each threshold and for each level of the PA.  Do this for both sets of thresholds.                                   ### 
  ### 6) For the given subsample, compute and store the eight measures of statistical fairness (excludes Calibration, given its   ###
  ###    fundamentally different nature) for each threshold for both sets of thresholds.  These eight measures are the following: ###
  ###      a) Error Rate Balance (ERB)                                                                                            ###
  ###      b) Conditional Use Accuracy Equality (CUAE)                                                                            ###
  ###      c) Predictive Parity (PP)                                                                                              ###
  ###      d) Statistical Parity (SP)                                                                                             ###
  ###      e) Treatment Equality (TE)                                                                                             ###
  ###      f) Predictive Equality (PE)                                                                                            ###
  ###      g) Equal Opportunity (EO)                                                                                              ###
  ###      h) Overall Accuracy Equality (OAE)                                                                                     ###
  ### 7) For the given subsample, compute and store the Calibration (CAL) for each risk score for both sets of thresholds.        ###
  ### 8) For the given subsample, for each threshold and for each level of the PA, across both sets of thresholds, compute and    ###
  ###    store the various predictive performance measures (i.e. ACC, PPV, NPV, TPR, TNR, FPR, FNR, FDR, FOR).                    ###
  ###    Additionally, for both sets of thresholds and for each threshold, compute the overall (i.e. not broken down by PA level) ###
  ###    value for each of the various predictive performance metrics (i.e. ACC, PPV, NPV, TPR, TNR, FPR, FNR, FDR, FOR).         ###
  ### 9) For the given subsample, compute the proportion of scores that changed between the pre- and post-fairness corrected      ###
  ###    thresholds.  Among those that changed, determine what proportion were of each possible type of change (e.g. S1 -> S2).   ###
  ###    Additionally, repeat the above for each level of the PA.                                                                 ###
  ### 10) Append each constructed data frame to the list that is to be outputted by this function.                                ###
  ### 11) Return the list to be outputted by this function.                                                                       ###
  ###################################################################################################################################
  ###################################################################################################################################
  for(i in 1:NumSubSamp.Compare){
    print(str_c("Subsample_", i))
    #-------------#
    ### Step 1. ###
    #-------------#
    Filtered.compare <- Application_Specific_Resampling_fcn(data_set = Data.compare, For__SubSamples_Measures_fcn = TRUE)
    #-------------#
    ### Step 2. ###
    #-------------#
    CondDist.Outcome.PA.i <- Filtered.compare %>% group_by(!!PA.Compare, OUTCOME) %>% summarize(n = n()) %>% mutate(PA_Level_n = sum(n)) %>% 
      mutate(PA_Level_Distribution = n/PA_Level_n) %>% mutate(SubSample_ID = i) %>% rename(PA_Level = !!PA.Compare)
    CondDist.Outcome.Ovrl.i <- Filtered.compare %>% group_by(OUTCOME) %>% summarize(n = n()) %>% mutate(PA_Level_n = sum(n)) %>% 
      mutate(PA_Level_Distribution = n/PA_Level_n) %>% mutate(PA_Level = "Overall", SubSample_ID = i)
    CondDist.Outcome.PA.i <- bind_rows(CondDist.Outcome.PA.i, CondDist.Outcome.Ovrl.i)
    CondDist.Outcome.PA <- bind_rows(CondDist.Outcome.PA, CondDist.Outcome.PA.i)
    #-------------#
    ### Step 3. ###
    #-------------#
    CondDist.RiskScore.PA.PreFC.i <- Filtered.compare %>% group_by(!!PA.Compare, RiskPreFC) %>% summarize(n = n()) %>% 
      mutate(PA_Level_n = sum(n)) %>% mutate(Risk_Score_Distribution = n/PA_Level_n) %>% 
      ungroup() %>% complete(!!PA.Compare, RiskPreFC, fill = list(Risk_Score_Distribution = 0)) %>% mutate(ThreshType = "PreFC", SubSample_ID = i) %>%
      rename(RiskScore = RiskPreFC) %>% rename(PA_Level = !!PA.Compare)
    CondDist.RiskScore.Ovrl.PreFC.i <- Filtered.compare %>% group_by(RiskPreFC) %>% summarize(n = n()) %>% 
      mutate(PA_Level_n = sum(n)) %>% mutate(Risk_Score_Distribution = n/PA_Level_n) %>% 
      mutate(ThreshType = "PreFC", SubSample_ID = i, PA_Level = "Overall") %>%
      rename(RiskScore = RiskPreFC)
    CondDist.RiskScore.PA.PreFC.i <- bind_rows(CondDist.RiskScore.PA.PreFC.i, CondDist.RiskScore.Ovrl.PreFC.i)
    if(PA.Audit == FALSE){
      CondDist.RiskScore.PA.PostFC.i <- Filtered.compare %>% group_by(!!PA.Compare, RiskPostFC) %>% summarize(n = n()) %>% 
        mutate(PA_Level_n = sum(n)) %>% mutate(Risk_Score_Distribution = n/PA_Level_n) %>% 
        ungroup() %>% complete(!!PA.Compare, RiskPostFC, fill = list(Risk_Score_Distribution = 0)) %>% mutate(ThreshType = "PostFC", SubSample_ID = i) %>%
        rename(RiskScore = RiskPostFC)%>% rename(PA_Level = !!PA.Compare)
      CondDist.RiskScore.Ovrl.PostFC.i <- Filtered.compare %>% group_by(RiskPostFC) %>% summarize(n = n()) %>% 
        mutate(PA_Level_n = sum(n)) %>% mutate(Risk_Score_Distribution = n/PA_Level_n) %>% 
        mutate(ThreshType = "PostFC", SubSample_ID = i, PA_Level = "Overall") %>%
        rename(RiskScore = RiskPostFC)
      CondDist.RiskScore.PA.PostFC.i <- bind_rows(CondDist.RiskScore.PA.PostFC.i, CondDist.RiskScore.Ovrl.PostFC.i)
      CondDist.RiskScore.PA.PreFC.i <- bind_rows(CondDist.RiskScore.PA.PreFC.i, CondDist.RiskScore.PA.PostFC.i)
    }
    CondDist.RiskScore.PA <- bind_rows(CondDist.RiskScore.PA, CondDist.RiskScore.PA.PreFC.i)
    #-------------#
    ### Step 4. ###
    #-------------#
    AUC.PA.i <- Filtered.compare %>% group_by(!!PA.Compare) %>% summarize(PA_Level_AUC = auc(OUTCOME, m_pred)) %>% mutate(SubSample_ID = i)
    AUC.PA <- bind_rows(AUC.PA, AUC.PA.i)
    #-------------#
    ### Step 5. ###
    #-------------#
    Conf.Matrix.i <- data.frame()
    for(j3 in 1:NumThresh.compare){
      ### PreFC ###
      Conf.Matrix.PreFC.j3.i <- data.frame()
      Conf.Matrix.PA.j3.i <- Filtered.compare %>% mutate(True_Negative = ((RiskPreFC %in% RiskScoreLevels.Compare[1:j3]) & OUTCOME == 0), 
                                                         False_Negative = ((RiskPreFC %in% RiskScoreLevels.Compare[1:j3]) & OUTCOME == 1),
                                                         True_Positive = ((RiskPreFC %in% RiskScoreLevels.Compare[(j3 + 1):NumRiskScores.Compare]) & OUTCOME == 1),
                                                         False_Positive = ((RiskPreFC %in% RiskScoreLevels.Compare[(j3 + 1):NumRiskScores.Compare]) & OUTCOME == 0)) %>%
        group_by(!!PA.Compare) %>% summarize(TN = sum(True_Negative), 
                                             FN = sum(False_Negative), 
                                             TP = sum(True_Positive), 
                                             FP = sum(False_Positive)) %>%
        mutate(TNprop = TN/(TN + FN + TP + FP), FNprop = FN/(TN + FN + TP + FP), TPprop = TP/(TN + FN + TP + FP), FPprop = FP/(TN + FN + TP + FP)) %>%
        mutate(PA_Level = PAlevels.compare)
      Conf.Matrix.Counts.PA.j3.i <- Conf.Matrix.PA.j3.i %>% select(TN, FN, TP, FP, PA_Level) %>% gather("TN", "FN", "TP", "FP", key = Cell_Type, value = "Count")
      Conf.Matrix.Prop.PA.j3.i <- Conf.Matrix.PA.j3.i %>% select(TNprop, FNprop, TPprop, FPprop, PA_Level) %>% 
        gather("TNprop", "FNprop", "TPprop", "FPprop", key = Cell_Type, value = "Proportion") %>% select(Proportion)
      Conf.Matrix.PA.j3.i <- bind_cols(Conf.Matrix.Counts.PA.j3.i, Conf.Matrix.Prop.PA.j3.i)
      Conf.Matrix.PA.j3.i <- Conf.Matrix.PA.j3.i %>% select(PA_Level, Cell_Type, Count, Proportion)
      Conf.Matrix.Ovrl.j3.i <- Filtered.compare %>% mutate(True_Negative = ((RiskPreFC %in% RiskScoreLevels.Compare[1:j3]) & OUTCOME == 0), 
                                                           False_Negative = ((RiskPreFC %in% RiskScoreLevels.Compare[1:j3]) & OUTCOME == 1),
                                                           True_Positive = ((RiskPreFC %in% RiskScoreLevels.Compare[(j3 + 1):NumRiskScores.Compare]) & OUTCOME == 1),
                                                           False_Positive = ((RiskPreFC %in% RiskScoreLevels.Compare[(j3 + 1):NumRiskScores.Compare]) & OUTCOME == 0)) %>% 
        summarize(TN = sum(True_Negative), 
                  FN = sum(False_Negative), 
                  TP = sum(True_Positive), 
                  FP = sum(False_Positive)) %>%
        mutate(TNprop = TN/(TN + FN + TP + FP), FNprop = FN/(TN + FN + TP + FP), TPprop = TP/(TN + FN + TP + FP), FPprop = FP/(TN + FN + TP + FP)) %>%
        mutate(PA_Level = "Overall")
      Conf.Matrix.Counts.Ovrl.j3.i <- Conf.Matrix.Ovrl.j3.i %>% select(TN, FN, TP, FP, PA_Level) %>% gather("TN", "FN", "TP", "FP", key = Cell_Type, value = "Count")
      Conf.Matrix.Prop.Ovrl.j3.i <- Conf.Matrix.Ovrl.j3.i %>% select(TNprop, FNprop, TPprop, FPprop, PA_Level) %>% 
        gather("TNprop", "FNprop", "TPprop", "FPprop", key = Cell_Type, value = "Proportion") %>% select(Proportion)
      Conf.Matrix.Ovrl.j3.i <- bind_cols(Conf.Matrix.Counts.Ovrl.j3.i, Conf.Matrix.Prop.Ovrl.j3.i)
      Conf.Matrix.PreFC.j3.i <- bind_rows(Conf.Matrix.PA.j3.i, Conf.Matrix.Ovrl.j3.i)
      Conf.Matrix.PreFC.j3.i <- Conf.Matrix.PreFC.j3.i %>% arrange(PA_Level) %>% mutate(ThreshType = "PreFC", Threshold = ThreshNames.Compare[j3], SubSample_ID = i)
      ### PostFC ###
      if(PA.Audit == FALSE){
        Conf.Matrix.PostFC.j3.i <- data.frame()
        Conf.Matrix.PA.j3.i <- Filtered.compare %>% mutate(True_Negative = ((RiskPostFC %in% RiskScoreLevels.Compare[1:j3]) & OUTCOME == 0), 
                                                           False_Negative = ((RiskPostFC %in% RiskScoreLevels.Compare[1:j3]) & OUTCOME == 1),
                                                           True_Positive = ((RiskPostFC %in% RiskScoreLevels.Compare[(j3 + 1):NumRiskScores.Compare]) & OUTCOME == 1),
                                                           False_Positive = ((RiskPostFC %in% RiskScoreLevels.Compare[(j3 + 1):NumRiskScores.Compare]) & OUTCOME == 0)) %>%
          group_by(!!PA.Compare) %>% summarize(TN = sum(True_Negative), 
                                               FN = sum(False_Negative), 
                                               TP = sum(True_Positive), 
                                               FP = sum(False_Positive)) %>%
          mutate(TNprop = TN/(TN + FN + TP + FP), FNprop = FN/(TN + FN + TP + FP), TPprop = TP/(TN + FN + TP + FP), FPprop = FP/(TN + FN + TP + FP)) %>%
          mutate(PA_Level = PAlevels.compare)
        Conf.Matrix.Counts.PA.j3.i <- Conf.Matrix.PA.j3.i %>% select(TN, FN, TP, FP, PA_Level) %>% gather("TN", "FN", "TP", "FP", key = Cell_Type, value = "Count")
        Conf.Matrix.Prop.PA.j3.i <- Conf.Matrix.PA.j3.i %>% select(TNprop, FNprop, TPprop, FPprop, PA_Level) %>% 
          gather("TNprop", "FNprop", "TPprop", "FPprop", key = Cell_Type, value = "Proportion") %>% select(Proportion)
        Conf.Matrix.PA.j3.i <- bind_cols(Conf.Matrix.Counts.PA.j3.i, Conf.Matrix.Prop.PA.j3.i)
        Conf.Matrix.PA.j3.i <- Conf.Matrix.PA.j3.i %>% select(PA_Level, Cell_Type, Count, Proportion)
        Conf.Matrix.Ovrl.j3.i <- Filtered.compare %>% mutate(True_Negative = ((RiskPostFC %in% RiskScoreLevels.Compare[1:j3]) & OUTCOME == 0), 
                                                             False_Negative = ((RiskPostFC %in% RiskScoreLevels.Compare[1:j3]) & OUTCOME == 1),
                                                             True_Positive = ((RiskPostFC %in% RiskScoreLevels.Compare[(j3 + 1):NumRiskScores.Compare]) & OUTCOME == 1),
                                                             False_Positive = ((RiskPostFC %in% RiskScoreLevels.Compare[(j3 + 1):NumRiskScores.Compare]) & OUTCOME == 0)) %>% 
          summarize(TN = sum(True_Negative), 
                    FN = sum(False_Negative), 
                    TP = sum(True_Positive), 
                    FP = sum(False_Positive)) %>%
          mutate(TNprop = TN/(TN + FN + TP + FP), FNprop = FN/(TN + FN + TP + FP), TPprop = TP/(TN + FN + TP + FP), FPprop = FP/(TN + FN + TP + FP)) %>%
          mutate(PA_Level = "Overall")
        Conf.Matrix.Counts.Ovrl.j3.i <- Conf.Matrix.Ovrl.j3.i %>% select(TN, FN, TP, FP, PA_Level) %>% gather("TN", "FN", "TP", "FP", key = Cell_Type, value = "Count")
        Conf.Matrix.Prop.Ovrl.j3.i <- Conf.Matrix.Ovrl.j3.i %>% select(TNprop, FNprop, TPprop, FPprop, PA_Level) %>% 
          gather("TNprop", "FNprop", "TPprop", "FPprop", key = Cell_Type, value = "Proportion") %>% select(Proportion)
        Conf.Matrix.Ovrl.j3.i <- bind_cols(Conf.Matrix.Counts.Ovrl.j3.i, Conf.Matrix.Prop.Ovrl.j3.i)
        Conf.Matrix.PostFC.j3.i <- bind_rows(Conf.Matrix.PA.j3.i, Conf.Matrix.Ovrl.j3.i)
        Conf.Matrix.PostFC.j3.i <- Conf.Matrix.PostFC.j3.i %>% arrange(PA_Level) %>% mutate(ThreshType = "PostFC", Threshold = ThreshNames.Compare[j3], SubSample_ID = i)
        Conf.Matrix.PreFC.j3.i <- bind_rows(Conf.Matrix.PreFC.j3.i, Conf.Matrix.PostFC.j3.i)
      }
      Conf.Matrix.i <- bind_rows(Conf.Matrix.i, Conf.Matrix.PreFC.j3.i)
    }
    Conf.Matrix.PA <- bind_rows(Conf.Matrix.PA, Conf.Matrix.i)
    #-------------#
    ### Step 6. ###
    #-------------#
    FairMetrics.i <- data.frame()
    for(j7 in 1:length(FairMetricAbrv)){
      ### PreFC ###
      FairMetrics.FullInfo.PreFC.j7.i <- Compute_Fairness_Measures(Thresholds = as.vector(ThreshPA.PreFC), 
                                                                   DataSet = Filtered.compare[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                                                   ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, 
                                                                   ThresholdNames = ThreshNames.Compare, FairnessType = FairMetricAbrv[j7], 
                                                                   ProtAttrSubAudit = PA.SubAudit, FullDataSet.SubAudit = Filtered.compare)
      FairMetrics.PreFC.j7.i <- FairMetrics.FullInfo.PreFC.j7.i[[length(FairMetrics.FullInfo.PreFC.j7.i)]]
      colnames(FairMetrics.PreFC.j7.i)[ncol(FairMetrics.PreFC.j7.i)] <- FairMetricAbrv[j7]
      FairMetrics.PreFC.j7.i <- FairMetrics.PreFC.j7.i %>% mutate(Fairness_Metric = FairMetricAbrv[j7]) %>% rename(Fairness_Value = FairMetricAbrv[j7]) %>%
        mutate(ThresholdType = "PreFC", SubSample_ID = i)
      ### PostFC ###
      if(PA.Audit == FALSE){
        FairMetrics.FullInfo.PostFC.j7.i <- Compute_Fairness_Measures(Thresholds = as.vector(ThreshPA.PostFC), 
                                                                      DataSet = Filtered.compare[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                                                      ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, 
                                                                      ThresholdNames = ThreshNames.Compare, FairnessType = FairMetricAbrv[j7], 
                                                                      ProtAttrSubAudit = PA.SubAudit, FullDataSet.SubAudit = Filtered.compare)
        FairMetrics.PostFC.j7.i <- FairMetrics.FullInfo.PostFC.j7.i[[length(FairMetrics.FullInfo.PostFC.j7.i)]]
        colnames(FairMetrics.PostFC.j7.i)[ncol(FairMetrics.PostFC.j7.i)] <- FairMetricAbrv[j7]
        FairMetrics.PostFC.j7.i <- FairMetrics.PostFC.j7.i %>% mutate(Fairness_Metric = FairMetricAbrv[j7]) %>% rename(Fairness_Value = FairMetricAbrv[j7]) %>%
          mutate(ThresholdType = "PostFC", SubSample_ID = i)
        FairMetrics.PreFC.j7.i <- bind_rows(FairMetrics.PreFC.j7.i, FairMetrics.PostFC.j7.i)
      }
      FairMetrics.i <- bind_rows(FairMetrics.i, FairMetrics.PreFC.j7.i) 
    }
    FairMetrics <- bind_rows(FairMetrics, FairMetrics.i)
    #-------------#
    ### Step 7. ###
    #-------------#
    CAL.metric.PreFC.i <- Compute_Fairness_Measures(Thresholds = as.vector(ThreshPA.PreFC), 
                                                    DataSet = Filtered.compare[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                                    ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, 
                                                    ThresholdNames = ThreshNames.Compare, FairnessType = "CAL", 
                                                    ProtAttrSubAudit = PA.SubAudit, FullDataSet.SubAudit = Filtered.compare)$Calibration
    CAL.metric.PreFC.i <- CAL.metric.PreFC.i %>% mutate(Fairness_Metric = "CAL") %>% rename(Fairness_Value = "CAL") %>%
      mutate(ThresholdType = "PreFC", SubSample_ID = i)
    if(PA.Audit == FALSE){
      CAL.metric.PostFC.i <- Compute_Fairness_Measures(Thresholds = as.vector(ThreshPA.PostFC), 
                                                       DataSet = Filtered.compare[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                                       ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, 
                                                       ThresholdNames = ThreshNames.Compare, FairnessType = "CAL", 
                                                       ProtAttrSubAudit = PA.SubAudit, FullDataSet.SubAudit = Filtered.compare)$Calibration
      CAL.metric.PostFC.i <- CAL.metric.PostFC.i %>% mutate(Fairness_Metric = "CAL") %>% rename(Fairness_Value = "CAL") %>%
        mutate(ThresholdType = "PostFC", SubSample_ID = i)
      CAL.metric.PreFC.i <- bind_rows(CAL.metric.PreFC.i, CAL.metric.PostFC.i)
    }
    CAL.metric <- bind_rows(CAL.metric, CAL.metric.PreFC.i)
    #-------------#
    ### Step 8. ###
    #-------------#
    PredPerfMetrics.PreFC.i <- data.frame()
    ### PreFC - Overall  ###
    PredPerfMetricsPreFC.Ovrl.i <- Compute_Predictive_Performance_Measures(Thresholds = as.vector(ThreshPA.PreFC), 
                                                                           DataSet = Filtered.compare[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                                                           ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, 
                                                                           ThresholdNames = ThreshNames.Compare, 
                                                                           ProtAttrSubAudit = PA.SubAudit, FullDataSet.SubAudit = Filtered.compare)
    PredPerfMetricsPreFC.Ovrl.i$PerformanceMeasure <- rownames(PredPerfMetricsPreFC.Ovrl.i)
    PredPerfMetricsPreFC.Ovrl.i <- gather(PredPerfMetricsPreFC.Ovrl.i, ThreshNames.Compare, key = "Threshold", value = PerformanceValue)
    PredPerfMetricsPreFC.Ovrl.i <- PredPerfMetricsPreFC.Ovrl.i %>% mutate(PA_Level = "Overall", ThreshType = "PreFC", SubSampleID = i)
    ### PreFC - PA Level ###
    PredPerfMetricsPreFC.ByPA.Combined.i <- data.frame()
    for(j5 in 1:NumPAlevels.compare){
      Filtered.compare.PAlevel.j5 <- Filtered.compare %>% filter(!!PA.Compare == PAlevels.compare[[j5]])
      PredPerfMetricsPreFC.ByPA.i <- Compute_Predictive_Performance_Measures(Thresholds = as.vector(ThreshPA.PreFC), 
                                                                             DataSet = Filtered.compare.PAlevel.j5[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                                                             ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, 
                                                                             ThresholdNames = ThreshNames.Compare, 
                                                                             ProtAttrSubAudit = PA.SubAudit, FullDataSet.SubAudit = Filtered.compare.PAlevel.j5)
      PredPerfMetricsPreFC.ByPA.i$PerformanceMeasure <- rownames(PredPerfMetricsPreFC.ByPA.i)
      PredPerfMetricsPreFC.ByPA.i <- gather(PredPerfMetricsPreFC.ByPA.i, ThreshNames.Compare, key = "Threshold", value = PerformanceValue)
      PredPerfMetricsPreFC.ByPA.i <- PredPerfMetricsPreFC.ByPA.i %>% mutate(PA_Level = PAlevels.compare[j5], ThreshType = "PreFC", SubSampleID = i)
      PredPerfMetricsPreFC.ByPA.Combined.i <- bind_rows(PredPerfMetricsPreFC.ByPA.Combined.i, PredPerfMetricsPreFC.ByPA.i)
    }
    PredPerfMetrics.PreFC.i <- bind_rows(PredPerfMetricsPreFC.Ovrl.i, PredPerfMetricsPreFC.ByPA.Combined.i)
    ### PostFC
    if(PA.Audit == FALSE){
      ### Overall ###
      PredPerfMetricsPostFC.Ovrl.i <- Compute_Predictive_Performance_Measures(Thresholds = as.vector(ThreshPA.PostFC), 
                                                                              DataSet = Filtered.compare[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                                                              ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, 
                                                                              ThresholdNames = ThreshNames.Compare, 
                                                                              ProtAttrSubAudit = PA.SubAudit, FullDataSet.SubAudit = Filtered.compare)
      PredPerfMetricsPostFC.Ovrl.i$PerformanceMeasure <- rownames(PredPerfMetricsPostFC.Ovrl.i)
      PredPerfMetricsPostFC.Ovrl.i <- gather(PredPerfMetricsPostFC.Ovrl.i, ThreshNames.Compare, key = "Threshold", value = PerformanceValue)
      PredPerfMetricsPostFC.Ovrl.i <- PredPerfMetricsPostFC.Ovrl.i %>% mutate(PA_Level = "Overall", ThreshType = "PostFC", SubSampleID = i)
      ### PA Level ###
      PredPerfMetricsPostFC.ByPA.Combined.i <- data.frame()
      for(j5 in 1:NumPAlevels.compare){
        Filtered.compare.PAlevel.j5 <- Filtered.compare %>% filter(!!PA.Compare == PAlevels.compare[[j5]])
        PredPerfMetricsPostFC.ByPA.i <- Compute_Predictive_Performance_Measures(Thresholds = as.vector(ThreshPA.PostFC), 
                                                                                DataSet = Filtered.compare.PAlevel.j5[,which(colnames(Data.compare) %in% c("OUTCOME", PA.Compare, "m_pred"))], 
                                                                                ProtAtrLevels = PAlevels.compare, NumThresholds = NumThresh.compare, 
                                                                                ThresholdNames = ThreshNames.Compare, 
                                                                                ProtAttrSubAudit = PA.SubAudit, FullDataSet.SubAudit = Filtered.compare.PAlevel.j5)
        PredPerfMetricsPostFC.ByPA.i$PerformanceMeasure <- rownames(PredPerfMetricsPostFC.ByPA.i)
        PredPerfMetricsPostFC.ByPA.i <- gather(PredPerfMetricsPostFC.ByPA.i, ThreshNames.Compare, key = "Threshold", value = PerformanceValue)
        PredPerfMetricsPostFC.ByPA.i <- PredPerfMetricsPostFC.ByPA.i %>% mutate(PA_Level = PAlevels.compare[j5], ThreshType = "PostFC", SubSampleID = i)
        PredPerfMetricsPostFC.ByPA.Combined.i <- bind_rows(PredPerfMetricsPostFC.ByPA.Combined.i, PredPerfMetricsPostFC.ByPA.i)
      }
      PredPerfMetrics.PostFC.i <- bind_rows(PredPerfMetricsPostFC.Ovrl.i, PredPerfMetricsPostFC.ByPA.Combined.i)
      PredPerfMetrics.PreFC.i <- bind_rows(PredPerfMetrics.PreFC.i, PredPerfMetrics.PostFC.i)
    }
    Predictive.Performance.Metrics.PA <- bind_rows(Predictive.Performance.Metrics.PA, PredPerfMetrics.PreFC.i)
    #-------------#
    ### Step 9. ###
    #-------------#
    if(PA.Audit == FALSE){
      ChangeCount.i <- data.frame()
      ChangeCount.Overl.i <- data.frame()
      ChangeCount.PA.i <- data.frame()
      ### Overall ###
      for(j7 in 1:nrow(ScoreChangeOptions)){
        ChangeCount.j7.i <- Filtered.compare %>% filter(RiskPreFC == ScoreChangeOptions[j7, 1] &  RiskPostFC == ScoreChangeOptions[j7, 2]) %>% summarize(Change_Count = n())
        ChangeCount.j7.i <- ChangeCount.j7.i %>% mutate(PreFC_RiskScore = ScoreChangeOptions[j7, 1], PostFC_RiskScore = ScoreChangeOptions[j7, 2], 
                                                        PA_Level = "Overall", SubSample_ID = i)
        ChangeCount.Overl.i <- bind_rows(ChangeCount.Overl.i, ChangeCount.j7.i)
      }
      ChangeCount.Overl.i <- ChangeCount.Overl.i %>% arrange(PreFC_RiskScore)
      ### By PA Level ###
      for(j7 in 1:nrow(ScoreChangeOptions)){
        ChangeCount.PA.j7.i <- Filtered.compare %>% group_by(!!PA.Compare) %>% 
          filter(RiskPreFC == ScoreChangeOptions[j7, 1] &  RiskPostFC == ScoreChangeOptions[j7, 2]) %>% 
          summarize(Change_Count = n())
        if(nrow(ChangeCount.PA.j7.i) != NumPAlevels.compare){
          ChangeCount.PA.j7.i <- Filtered.compare %>% group_by(!!PA.Compare) %>% 
            filter(RiskPreFC == ScoreChangeOptions[j7, 1] &  RiskPostFC == ScoreChangeOptions[j7, 2]) %>% 
            summarize(Change_Count = n()) %>% ungroup() %>% complete(!!PA.Compare, fill = list(Change_Count = 0)) %>%
            drop_na()
        }
        ChangeCount.PA.j7.i <- ChangeCount.PA.j7.i %>% mutate(PreFC_RiskScore = ScoreChangeOptions[j7, 1], PostFC_RiskScore = ScoreChangeOptions[j7, 2], 
                                                              PA_Level = !!PA.Compare, SubSample_ID = i) %>%
          select(Change_Count, PreFC_RiskScore, PostFC_RiskScore, PA_Level, SubSample_ID)
        ChangeCount.PA.i <- bind_rows(ChangeCount.PA.i, ChangeCount.PA.j7.i)
      }
      ChangeCount.PA.i <- ChangeCount.PA.i %>% arrange(PreFC_RiskScore)
      ChangeCount.i <- bind_rows(ChangeCount.Overl.i, ChangeCount.PA.i)
      ChangeCount.PA <- bind_rows(ChangeCount.PA, ChangeCount.i)
    }
  }
  #--------------#
  ### Step 10. ###
  #--------------#
  OutputToReturn.List[[1]] <- Distbn.PA
  names(OutputToReturn.List)[[1]] <- "Unduplicated_Protected_Attribute_Distribution"
  OutputToReturn.List[[2]] <- CondDist.Outcome.PA
  names(OutputToReturn.List)[[2]] <- "Outcome_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
  OutputToReturn.List[[3]] <- CondDist.RiskScore.PA
  names(OutputToReturn.List)[[3]] <- "Risk_Score_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
  OutputToReturn.List[[4]] <- AUC.PA
  names(OutputToReturn.List)[[4]] <- "AUC_by_Protected_Attribute_Level__For_All_Subsamples"
  OutputToReturn.List[[5]] <- Conf.Matrix.PA
  names(OutputToReturn.List)[[5]] <- "Confusion_Matrix_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
  OutputToReturn.List[[6]] <- FairMetrics
  names(OutputToReturn.List)[[6]] <- "Statistical_Fairness_Measures__For_All_Subsamples"
  OutputToReturn.List[[7]] <- CAL.metric
  names(OutputToReturn.List)[[7]] <- "Statistical_Calibration_Fairness_Measure__For_All_Subsamples"
  OutputToReturn.List[[8]] <- Predictive.Performance.Metrics.PA
  names(OutputToReturn.List)[[8]] <- "Predictive_Performance_Measures_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
  OutputToReturn.List[[9]] <- ChangeCount.PA
  names(OutputToReturn.List)[[9]] <- "Risk_Score_Change_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
  #--------------#
  ### Step 11. ###
  #--------------#
  return(OutputToReturn.List)
}