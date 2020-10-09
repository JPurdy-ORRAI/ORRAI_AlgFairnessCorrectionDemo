#############################################################################################################################################
#############################################################################################################################################
### Key Notes: Demo Driver script for obtaining fairness corrected thresholds for the Salary Data under the following situation.          ###
###     1) Predicted probability of an individual making over 50k.                                                                        ###
###     2) Threshold Types --> Low Risk, Average Risk, and High Risk.                                                                     ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### This script ultimately carries out each of the following five objectives:                                                             ###
###     a) Create Reduced Data Set for Demo Script and Fit a Simple Logistic Mode, for illustrative purposes, to Fairness Correct.        ###
###     b) Audit algorithm fairness of "uncorrected" model.                                                                               ###
###     c) Obtain (pre- and) post-fairness-corrected threshold values; this includes obtaining such threshold values across a grid of     ###
###        penalty weights (on the proportion of risk scores that change) for the objective function, and then selecting the "best"       ###
###        penalty weight for which the corresponding set of Post-FC threshold values will ultimately be utilized.                        ###
###       (*) Note that within this step the option exists to utilize the GenSA optimizer, rather that the default optim optimizer, though###
###           this functionality, at this point, exists more for exploration than anything else.                                          ###
###     d) Audit PreFC vs. PostFC threshold values obtained in object (c).                                                                ###
###     e) Replicate Graphics and Tables of Algorithmic Fairness Sections of Reunification Manuscript.                                    ###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
### Additional Script Information:                                                                                                        ###
###                                                                                                                                       ###
### This script takes the predicted probabilities of individuals earning more than 50k annually and adjusts each respective               ###
### decision/classification threshold value across the various levels of the protected attribute to obtain fairness corrected             ###
### decisions/classifications.  Aggregation (i.e. average) of bootstrap samples is implemented to obtain more "robust" estimates of the   ###
### fairness corrected threshold values.                                                                                                  ###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
#############################################################################################################################################
### MUST FIRST RUN Fairness_Functions_Package script. ###
library(tidyverse)
library(parallel)
library(stringr)
library(ggplot2)
library(forcats)
library(ModelMetrics)
library(tidyr)
library(RColorBrewer)
library(ROCR)
library(janitor)
library(GenSA)
library(rlist)
library(gridExtra)
library(ggpubr)
### Establish Resampling Approach.  This function is used throughough the Fairness_Functions_Package.R script. ###
Application_Specific_Resampling_fcn <- function(data_set, For__SubSamples_Measures_fcn = FALSE){
  #
  # This function should default output a N x 3 data frame, where N is the number of resampled rows and the three columns are the
  # following (necessarily in the listed order):
  #    a) Outcome (0/1)         
  #    b) Protected Attribute Level                                                                       
  #    c) Predicted Probability                                                                                                     
  # When not the default use (when For__SubSamples_Measures_fcn = TRUE), the function should additionally output additional columns,
  # corresponding to the PreFC and PostFC risk scores for the re-sample, which are columns of data_set.                           
  if(For__SubSamples_Measures_fcn == FALSE){
    ReSample.Data <- data_set %>% sample_frac(1.0, replace = TRUE) %>% select(OUTCOME, Protected_Attribute, m_pred)
  }else{
    ReSample.Data <- data_set %>% sample_frac(1.0, replace = TRUE) %>% select(-person_ID)
  }
  return(ReSample.Data)
}
#############################################################################################################################################
#############################################################################################################################################
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###             Objective (a) -- Create Reduced Data Set for Demo Script and Fit a Simple Logistic Model to Fairness Correct.             ###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
#############################################################################################################################################
#############################################################################################################################################
###                                   Part 1 -- Initial Set-up and Data Preparation                                                       ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Set working directory and load salary data; both the training and test sets.  Set column names.                                    ###
### 2) Modify data set to fields relevant for analysis.                                                                                   ###
###    Recode outcome variable (salary) to 0/1 (0 is <=50k, 1 is >50k) and convert to factor.                                             ###
### 3) For ease of illustration, reduce Race variable to three levels.                                                                    ###
###     a) Determine the Salary (>50K) disparity across the 5 levels/groups of Race.  Based on this information and the number of people  ###
###        within each level/group of Race, identify three logical groupings to proceed with.                                             ###
###     b) Create a recoded Race variable based on the three identified groupings in step (1) above.                                      ###
###     c) Verify that the salary disparity among the new groupings makes sense based on step (1) results.                                ###
###     d) Recode the Race variable within the Test set based on the three identified groupings in step (1).                              ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 1. ###
#-------------#
setwd("Identify desired directory here")
AdultData.Train <- read_csv("AdultData.Train.txt", col_names = FALSE)
colnames(AdultData.Train) <- c("age","workClass","fnlwgt","educ","educNum","maritalStatus","Occupation","Relationship","Race","Sex",
                               "CapitalGain","capitalLoss","HrsWeek","Country","Salary")
AdultData.Test <- read_csv("AdultData.Test.txt", col_names = FALSE)
colnames(AdultData.Test) <- c("age","workClass","fnlwgt","educ","educNum","maritalStatus","Occupation","Relationship","Race","Sex",
                              "CapitalGain","capitalLoss","HrsWeek","Country","Salary")
#-------------#
### Step 2. ###
#-------------#
AdultDataReduced.Train <- AdultData.Train %>% select(Salary, age, educNum, Race) 
AdultDataReduced.Train$Salary <-  as.factor(recode(AdultDataReduced.Train$Salary, "<=50K" = '0', ">50K" = '1'))
AdultDataReduced.Test <- AdultData.Test %>% select(Salary, age, educNum, Race)
AdultDataReduced.Test$Salary <-  as.factor(recode(AdultDataReduced.Test$Salary, "<=50K." = '0', ">50K." = '1'))
#-------------------------#
### Step 3 -- part (a). ###
#-------------------------#
SalaryDistByRace.Train <- AdultDataReduced.Train %>% group_by(Race) %>% 
  summarize(SampSize = length(Salary), Prevalence = sum(Salary == "1")/length(Salary)) %>%
  mutate(Disparity = Prevalence/max(Prevalence))
#-------------------------#
### Step 3 -- part (b). ###
#-------------------------#
AdultDataReduced.Train$RaceCatDummy <- as.factor(ifelse(AdultDataReduced.Train$Race == "Amer-Indian-Eskimo", "ANO", 
                                                        ifelse(AdultDataReduced.Train$Race == "Other", "ANO", 
                                                               ifelse(AdultDataReduced.Train$Race == "Asian-Pac-Islander", "APW", 
                                                                      ifelse(AdultDataReduced.Train$Race == "White", "APW", "BLK")))))
#-------------------------#
### Step 3 -- part (c). ###
#-------------------------#
SalaryDistByRaceRecode.Train <- AdultDataReduced.Train %>% group_by(RaceCatDummy) %>% 
  summarize(SampSize = length(Salary), Prevalence = sum(Salary == "1")/length(Salary)) %>%
  mutate(Disparity = Prevalence/max(Prevalence))
#-------------------------#
### Step 3 -- part (d). ###
#-------------------------#
AdultDataReduced.Test$RaceCatDummy <- as.factor(ifelse(AdultDataReduced.Test$Race == "Amer-Indian-Eskimo", "ANO", 
                                                       ifelse(AdultDataReduced.Test$Race == "Other", "ANO", 
                                                              ifelse(AdultDataReduced.Test$Race == "Asian-Pac-Islander", "APW", 
                                                                     ifelse(AdultDataReduced.Test$Race == "White", "APW", "BLK")))))
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
###                          Part 2 -- Fit a simple logistic regression model and obtain test set predictions.                            ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Fit a simple logistic regression model to the training data.                                                                       ###
### 2) Obtain predicted probabilities for the test set data.                                                                              ###
### 3) Create data set necessary for fairness-correction procedure training; this data set will consist only of three fields: Salary      ###
###    (renamed OUTCOME), RaceCatDummy (renamed Protected_Attribute), and fitted.values(Model.Train) (renamed as m_pred).                 ###
### 4) Create data set necessary for fairness-correction procedure testing; this data set will consist only of three fields: Salary       ###
###    (renamed OUTCOME), RaceCatDummy (renamed Protected_Attribute), and m_predTest (renamed as m_pred).                                 ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 2. ###
#-------------#
Model.Train <- glm(Salary ~ age + educNum + RaceCatDummy, family = binomial, AdultDataReduced.Train)
summary(Model.Train)
#-------------#
### Step 2. ###
#-------------#
m_predTest <- predict(Model.Train, AdultDataReduced.Test, type = "response")
#-------------#
### Step 3. ###
#-------------#
FairnessDataSet <- data.frame(OUTCOME = AdultDataReduced.Train$Salary, 
                              Protected_Attribute = AdultDataReduced.Train$RaceCatDummy, 
                              m_pred = as.vector(fitted.values(Model.Train)),
                              person_ID = 1:nrow(AdultDataReduced.Train))
RelevantFieldNames <- colnames(FairnessDataSet)
View(FairnessDataSet)
#-------------#
### Step 4. ###
#-------------#
FairnessDataSet.Test <- data.frame(OUTCOME = AdultDataReduced.Test$Salary, 
                                   Protected_Attribute = AdultDataReduced.Test$RaceCatDummy, 
                                   m_pred = as.vector(m_predTest),
                                   person_ID = 1:nrow(AdultDataReduced.Test))
#############################################################################################################################################
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###                                Objective (b) -- Auditing of algorithm fairness of "uncorrected" model.                                ###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
###                                                Part 1 -- Audit Preparation                                                            ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### This part of the code consists of the following step:                                                                                 ###
### 1) Using aggregated bootstrapping, find the initial, pre-fairness corrected, threshold values.                                        ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 1. ###
#-------------#
FairnessDataSet.Audit <- FairnessDataSet
NumLevels.PAtoAudit <- length(levels(FairnessDataSet.Audit$Protected_Attribute))
NumSubSamps <- 200
ThreshStorage <- data.frame(SubSamp = 1:NumSubSamps, LR_thresh = rep(NA, NumSubSamps), AR_thresh = rep(NA, NumSubSamps), HR_thresh = rep(NA, NumSubSamps))
NumThresholds <- ncol(ThreshStorage) - 1
for(k in 1:NumSubSamps){
  print(k)
  SumSamp.k <- Application_Specific_Resampling_fcn(data_set = FairnessDataSet.Audit)
  ThreshStorage$AR_thresh[k] <- FindPreFCthresh_fcn(PredProbVec = SumSamp.k$m_pred, Method = 2)
  ThreshStorage$LR_thresh[k] <- FindPreFCthresh_fcn(PredProbVec = SumSamp.k$m_pred, Method = 3, PreProbPerc = 0.50)
  ThreshStorage$HR_thresh[k] <- FindPreFCthresh_fcn(PredProbVec = SumSamp.k$m_pred, Method = 4, PreProbPerc = 0.50)
}
ThreshToAudit.Init <- colMeans(ThreshStorage[, 2:ncol(ThreshStorage)])
ThreshToAudit <- rep(ThreshToAudit.Init, each = length(levels(FairnessDataSet.Audit$Protected_Attribute)))
#############################################################################################################################################
#############################################################################################################################################
###                            Part 2 -- Obtain Quantitative Measures Tied to Algorithmic Fairness Audit.                                 ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Specify input arguments for SubSamples_Measures_fcn function in the Fairness_Functions_Packages script.                            ###
### 2) Using the SubSamples_Measures_fcn function in the Fairness_Functions_Packages script, obtain metrics for each subsample.  The      ###
###    returned list consists of the following data frames:                                                                               ###
###        a) Unduplicated Protected Attribute Distribution.                                                                              ###
###        b) Outcome Distribution Overall and by Protected Attribute Level For All Subsamples.                                           ###
###        c) Risk Score Distribution Overall and by Protected Attribute Level For All Subsamples.                                        ###
###        d) AUC by Protected Attribute Level For All Subsamples.                                                                        ###
###        e) Confusion Matrix Distribution Overall and by Protected Attribute Level For All Subsamples.                                  ###
###        f) Statistical Fairness Measures For All Subsamples.                                                                           ###
###        g) Statistical Calibration Fairness Measure For All Subsamples.                                                                ###
###        h) Predictive Performance Measures Overall and by Protected Attribute Level For All Subsamples.                                ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 1. ###
#-------------#
set.seed(8417)
NumSubSamples <- 200
ThreshNames <- c("LR_thresh", "AR_thresh", "HR_thresh")
#-------------#
### Step 2. ###
#-------------#
CompareOutput <- SubSamples_Measures_fcn(DataSet.Compare = FairnessDataSet.Audit, PA.Compare = "Protected_Attribute", NumSubSamp.Compare = NumSubSamples,
                                         Thresh.PreFC = ThreshToAudit, Thresh.PostFC = NA, ThreshNames.Compare = ThreshNames, PA.Audit = TRUE, 
                                         PA.SubAudit = FALSE)
#############################################################################################################################################
#############################################################################################################################################
###           Part 3 -- Calculate and Visualize Various Fairness, Predictive Performance, and Other Relevant Measures.                    ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### If needed, this part of the code will also inform how to combine existing levels of the protected attribute.  In particular, if the   ###
### fairness metrics are too variable across subsamples (Step 2), this indicates that fairness cannot be accurately measured for the PA   ###
### as currently defined.  Hence, the distribution of the current protected attribute (Step 3), as well as the distribution of the        ###
### average (across all subsamples) outcome prevalence for each of the current PA levels, can be used to combine current PA levels that   ###
### have too few individuals identifying within.                                                                                          ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Distribution of protected attribute.                                                                                               ###
### 2) Distribution of outcome variable overall, and by protected attribute level.                                                        ###
### 3) Distribution of risk scores overall, and by protected attribute level.                                                             ###
### 4) AUC by protected attribute level.                                                                                                  ###
### 5) Statistical fairness measures (excluding Calibration, given its inherently different nature).                                      ###
### 6) Calibration (the statistical fairness measure).                                                                                    ###
### 7) Predictive performance measures, including AUC.                                                                                    ###
### 8) Distribution of confusion matrix overall, and by protected atrtibute level.                                                        ###
### 9) Distribution of change in risk scores (between PreFC and PostFC) overall, and by protected attribute level.                        ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 1. ###
#-------------#
PAdistSubSample <- CompareOutput$"Unduplicated_Protected_Attribute_Distribution"
ggplot(data = PAdistSubSample,
       mapping = aes(x = Protected_Attribute, y = Proportion, fill = Protected_Attribute)) +
  geom_col() +
  ggtitle("Distribution of Protected Attribute") +
  xlab("Protected Attribute Level") +
  ylab("Proportion") + 
  geom_text(aes(label = Count), vjust = -0.5) + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 2. ###
#-------------#
OtcmDistSubSample <- CompareOutput$"Outcome_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
ggplot(data = OtcmDistSubSample %>% filter(OUTCOME == 1) %>% group_by(PA_Level) %>% 
         summarize(AvgPrevalence = mean(PA_Level_Distribution), SDprev = sd(PA_Level_Distribution), AvgCount = mean(n), SDcount = sd(n)),
       mapping = aes(x = PA_Level, y = AvgPrevalence, fill = PA_Level)) +
  geom_col() +
  ggtitle("Prevalence of Outcome") +
  xlab("Protected Attribute Level") +
  ylab("Proportion") + 
  geom_text(aes(label = round(AvgCount)), vjust = -0.5) + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 3. ###
#-------------#
RiskScoreDistSubSamp <- CompareOutput$"Risk_Score_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
ggplot(data = RiskScoreDistSubSamp %>% group_by(PA_Level, RiskScore) %>% 
         summarize(AvgCount = mean(n), SDcount = sd(n), AvgRSdist = mean(Risk_Score_Distribution), SDrsDist = sd(Risk_Score_Distribution)),
       mapping = aes(x = PA_Level, y = AvgRSdist, fill = PA_Level)) + 
  geom_col(position = "dodge") +
  facet_wrap(~ RiskScore) +
  ggtitle("Average Risk Score Distribution by Protected Attribute") +
  xlab("Protected Attribute Level") +
  ylab("Proportion") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 4. ###
#-------------#
AUCsubSamp <- CompareOutput$"AUC_by_Protected_Attribute_Level__For_All_Subsamples"
ggplot(data = AUCsubSamp %>% group_by(Protected_Attribute) %>% summarize(AvgAUC = mean(PA_Level_AUC), SDauc = sd(PA_Level_AUC)),
       mapping = aes(x = Protected_Attribute, y = AvgAUC, fill = Protected_Attribute)) +
  geom_col() +
  ggtitle("AUC by Protected Attribute Level") +
  xlab("Protected Attribute Level") +
  ylab("AUC") + 
  ylim(0, 1) +
  geom_text(aes(label = round(AvgAUC, 2)), vjust = -0.5) + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 5. ###
#-------------#
FairnessSubSamp <- CompareOutput$"Statistical_Fairness_Measures__For_All_Subsamples"
###
Fairness.Summary <- FairnessSubSamp %>% group_by(Threshold, Fairness_Metric) %>% 
  summarize(Avg_Value = mean(Fairness_Value), SD_Value = sd(Fairness_Value)) %>% arrange(Fairness_Metric, Threshold)
ggplot(data = Fairness.Summary,
       mapping = aes(x = Fairness_Metric, y = Avg_Value, fill = Fairness_Metric)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~ Threshold, nrow = 3) + 
  ggtitle("Average Fairness Value") +
  xlab("Fairness Measure") +
  ylab("Average Fairness Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 6. ###
#-------------#
FairCalSubSamp <- CompareOutput$"Statistical_Calibration_Fairness_Measure__For_All_Subsamples"
###
FairCAL.Summary <- FairCalSubSamp %>% group_by(Score, Fairness_Metric) %>% 
  summarize(Avg_Value = mean(Fairness_Value), SD_Value = sd(Fairness_Value)) %>% arrange(Fairness_Metric, Score)
ggplot(data = FairCAL.Summary,
       mapping = aes(x = Fairness_Metric, y = Avg_Value, fill = Fairness_Metric)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~ Score, nrow = 1) + 
  ggtitle("Average Calibration Fairness Value") +
  xlab("Fairness Measure") +
  ylab("Average Calibration Fairness Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 7. ###
#-------------#
PredPerfSubSamp <- CompareOutput$"Predictive_Performance_Measures_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
###
PredPerf.Summary <- PredPerfSubSamp %>% group_by(PerformanceMeasure, Threshold, PA_Level) %>%
  summarize(Avg_Value = mean(PerformanceValue), SD_Value = sd(PerformanceValue)) %>% arrange(Threshold, PerformanceMeasure)
ggplot(data = PredPerf.Summary %>% 
         filter(PerformanceMeasure == "ACC" | PerformanceMeasure == "FNR" | PerformanceMeasure == "FPR" | PerformanceMeasure == "NPV" | PerformanceMeasure == "PPV") , 
       mapping = aes(x = PA_Level, y = Avg_Value, fill = PA_Level)) + 
  geom_col(position = "dodge") + 
  facet_wrap(Threshold ~ PerformanceMeasure, nrow = 3) + 
  ggtitle("Average Predictive Performance Values") +
  xlab("Protected Attribute Level") +
  ylab("Average Predictive Performance Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 8. ###
#-------------#
ConfMatSubSample <- CompareOutput$"Confusion_Matrix_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
ggplot(data = ConfMatSubSample %>% group_by(PA_Level, Cell_Type, Threshold) %>% 
         summarize(AvgCount = mean(Count), SDcount = sd(Count), AvgProp = mean(Proportion), SDprop = sd(Proportion)),
       mapping = aes(x = PA_Level, y = AvgProp, fill = PA_Level)) +
  geom_col(position = "dodge") +
  facet_wrap(Cell_Type ~ Threshold, nrow = 4) +
  ggtitle("Average Confusion Matrix Distribution Across Protected Attribute Levels") +
  xlab("Protected Attribute Level") +
  ylab("Proportion") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#############################################################################################################################################
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
### Objective (c) -- Obtaining (pre- and) post-fairness-corrected threshold values; this includes obtaining such thresholds across a grid ###
###                  of penalty weights (on the proportion of risk scores that change) for the objective function, and then selecting the ###
###                  "best" penalty weight for which the corresponding set of Post-FC threshold values will ultimately be utilized.       ###
###                   (*) Note that within this step the option exists to utilize the GenSA optimizer, rather that the default optim      ###
###                       optimizer, though this functionality, at this point, exists more for exploration than anything else.            ###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
###                     Part 1 -- Obtain Pre- and Post-Fairness-Corrected Threshold Values Across Tuning Grid.                            ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Initialize arguments and storage bins for various tuning parameter specifications.                                                 ###
### 2) Utilize parallel processing to obtain pre- and post-fairness-corrected thresholds.                                                 ###
### 3) Save the obtained thresholds across all tuning grid points as an rds file.                                                         ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 1. ###
#-------------#
NumSubSamps <- 200
PAlevs <- levels(FairnessDataSet$Protected_Attribute)
NumThresh <- 3
ThreshNames <- c("Low", "Average", "High")
FairnessMetric <- "ERB"
PreFCthresh.INPUTS <- data.frame(Method = c(3, 2, 4), PreProbPerc = c(0.50, NA, 0.50)) # See FindPreFCthresh_fcn comments in Fairness_Functions_Package.R for details of inputs here.
ThreshStorageList.Grid <- list()
PropScoreChangePenaltyWeight.TuningGrid <- seq(0, 1, 0.01)
OptimizerAPPROACH <- "Sequential"
CONSTRAINT.cover <- TRUE
OptimizerTYPE <- "optim"            
GenSA.MAXrunTIME <- NA
#-------------#
### Step 2. ###
#-------------#
pStartOvrl <- Sys.time()
for(k in 1:length(PropScoreChangePenaltyWeight.TuningGrid)){
  weight.k <- PropScoreChangePenaltyWeight.TuningGrid[k]
  cl <- makeCluster(detectCores() - 1) ### Send an instance of R to each core on the local machine.  The detectCores() function detects the number of phyiscal cores. ###
  clusterSetRNGStream(cl = cl, iseed = 1710) ### Set random number generator stream for cluster. ###
  clusterExport(cl, varlist = list("FindPreFCthresh_fcn", "Compute_Fairness_Measures", "Compute_Predictive_Performance_Measures",
                                   "Assign_Risk_Scores", "Optimize_Fair_Thresholds_fcn", "Application_Specific_Resampling_fcn")) ### Send initial state of objects (e.g. functions) and any other objects to each utilized core. ###
  clusterEvalQ(cl, c(library(tidyverse), library(stringr), library(GenSA))) ### Load required packages in R on each utilized core. ###
  ParallelResults <- do.call(rbind, parLapply(cl, 1:NumSubSamps, fun = ParOptimThresh_fcn, DataToSS = FairnessDataSet, ProtAttrLevs = PAlevs, 
                                              NumberTHRESH = NumThresh, THRESHnames = ThreshNames, FAIRmetric = FairnessMetric, 
                                              propSCOREchangePENALTY.wt = weight.k, PreFC.Thresh.Inputs = PreFCthresh.INPUTS,
                                              Optimizer.Approach = OptimizerAPPROACH, Optimizer.Type = OptimizerTYPE, 
                                              GenSA.MaxRunTime = GenSA.MAXrunTIME, Constraint.COVER = CONSTRAINT.cover))
  stopCluster(cl)
  ThreshStorageList.Grid[[k]] <- ParallelResults
  print(k)
}
pStopOvrl <- Sys.time()
pStopOvrl - pStartOvrl
#-------------#
### Step 3. ###
#-------------#
#saveRDS(ThreshStorageList.Grid, file = paste0("TuningGrid_SubSample_Thresholds_50Avg50_000_100_001_Grid_", Sys.Date(),".rds")) 
#############################################################################################################################################
#############################################################################################################################################
###                 Part 2 -- Assess Results/Performance Across Thresholds and Various Penalty Weights of Tuning Grid.                    ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Initialize arguments for assessing various specifications identified in the tuning grid.                                           ###
### 2) Utilizing parallel processing, for each set of PreFC and PostFC thresholds (one for each grid point), take a large number of       ###
###    subsamples and compute the average measures (fairness, predictive performance, the distance between the pre- and post-fc           ###
###    thresholds, and the average proportion of scores which change) across the subsamples.                                              ###
### 3) Organize and save the obtained results across all tuning grid points as an rds file.                                               ###
### 4) Create visual which provides a sense of whether the penalty term behaves in the anticipated manner.                                ###
### 5) Create visuals, for a specified threshold, conveying relevant relationships between the penalty weight, the proportion of changing ###
###    scores, the average fairness and predictive performance measures, and the location of the subsample aggregated thresholds.         ###
### 6) Create visuals, across all thresholds simultaneously, conveying the relationship between the average performance measures (i.e     ###
###    predictive performance measures and fairness) and the proportion of changing scores and the penalty weight.                        ###
### 7) Create visual pertaining to amount of change in predictive performance measures at each threshold for each penalty weight.         ### 
### 8) Create visuals pertaining to the proportional change in predictive performance measures and algorithmic fairness at each threshold,###
###    for each penalty weight.                                                                                                           ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 1. ###
#-------------#
NumSUBsamps <- 200
#-------------#
### Step 2. ###
#-------------#
pStartOvrl <- Sys.time()
cl <- makeCluster(detectCores() - 1) ### Send an instance of R to each core on the local machine.  The detectCores() function detects the number of phyiscal cores. ###
clusterSetRNGStream(cl = cl) ### Set random number generator stream for cluster. ###
clusterExport(cl, varlist = list("Compute_Fairness_Measures", "Compute_Predictive_Performance_Measures", "Assign_Risk_Scores",
                                 "ThreshStorageList.Grid", "PropScoreChangePenaltyWeight.TuningGrid", "Application_Specific_Resampling_fcn")) ### Send initial state of objects (e.g. functions) and any other objects to each utilized core. ###
clusterEvalQ(cl, c(library(tidyverse), library(stringr))) ### Load required packages in R on each utilized core. ###
ParallelResults.TuningAssess <- do.call(rbind, parLapply(cl, 1:length(ThreshStorageList.Grid), fun = ParTuningGridAssessment_fcn, randSeedNumber = 1123, 
                                                         NumSS = NumSUBsamps, DataToSS = FairnessDataSet, ProtAttrLevs = PAlevs, NumberTHRESH = NumThresh, 
                                                         THRESHnames = ThreshNames, FAIRmetric = FairnessMetric))
if(FairnessMetric == "CAL"){
  ParallelResults.TuningAssess.PredPerf <- list.stack(ParallelResults.TuningAssess[,1])
  ParallelResults.TuningAssess.CAL <- list.stack(ParallelResults.TuningAssess[,2])
  ParallelResults.TuningAssess <- list(ParallelResults.TuningAssess.PredPerf, ParallelResults.TuningAssess.CAL)
}
stopCluster(cl)
pStopOvrl <- Sys.time()
pStopOvrl - pStartOvrl
#-------------#
### Step 3. ###
#-------------#
#saveRDS(ParallelResults.TuningAssess, file = paste0("TuningGrid_AggSubSampleThresholds_Results_50Avg50_000_100_001_Grid_", Sys.Date(),".rds")) 
#-------------#
### Step 4. ###
#-------------#
ggplot(data = ParallelResults.TuningAssess,
       mapping = aes(x = PenaltyWeight, y = AvgPropDeltaScores)) +
  geom_point(mapping = aes(colour = ThreshType)) +
  ggtitle("Optimization Process Behavior") +
  xlab("Penalty Weight on Proportion of Changing Risk Scores") +
  ylab("Average Proportion of Changing Risk Scores")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
ggplot(data = ParallelResults.TuningAssess %>% filter(PerformanceMeasure == "ERB"),
       mapping = aes(x = PenaltyWeight, y = AvgObjFcnVal)) +
  geom_point(mapping = aes(colour = ThreshType)) +
  ggtitle("Average Objective Function Value vs. Penalty Weight") +
  xlab("Penalty Weight on Proportion of Changing Risk Scores") +
  ylab("Average Objective Function Value")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#------------#
### Step 5 ###
#------------#
Specified.Thresh.Name <- ThreshNames[1] # ThreshNames[1] for "Low Threshold";ThreshNames[2] for "Average Threshold"; ThreshNames[3] for "High Threshold"
ggplot(data = ParallelResults.TuningAssess %>% 
         filter(Threshold == Specified.Thresh.Name, PerformanceMeasure == "ERB" | PerformanceMeasure == "NPV" | PerformanceMeasure == "FNR" | PerformanceMeasure == "FPR" | PerformanceMeasure == "PPV" | PerformanceMeasure == "ACC"),
       mapping = aes(x = PenaltyWeight, y = AvgPerformanceValue)) +
  geom_point(mapping = aes(colour = ThreshType)) +
  facet_grid(~ PerformanceMeasure) +
  ggtitle(str_c(Specified.Thresh.Name , "Threshold")) +
  xlab("Penalty Weight on Proportion of Changing Risk Scores") +
  ylab("Average Performance Measure")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
ggplot(data = ParallelResults.TuningAssess %>% 
         filter(Threshold == Specified.Thresh.Name, PerformanceMeasure == "ERB" | PerformanceMeasure == "NPV" | PerformanceMeasure == "FNR" | PerformanceMeasure == "FPR" | PerformanceMeasure == "PPV" | PerformanceMeasure == "ACC"),
       mapping = aes(x = AvgPropDeltaScores, y = AvgPerformanceValue)) +
  geom_point(mapping = aes(colour = ThreshType)) +
  facet_grid(~ PerformanceMeasure) +
  ggtitle(str_c(Specified.Thresh.Name , "Threshold")) +
  xlab("Proportion of Changing Risk Scores") +
  ylab("Average Performance Measure")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
ggplot(data = ParallelResults.TuningAssess %>% filter(Threshold == Specified.Thresh.Name, PerformanceMeasure == "ERB") %>% 
         rename(ANO = SubSampAggThreshold.ANO, APW = SubSampAggThreshold.APW, BLK = SubSampAggThreshold.BLK) %>%
         gather('ANO', 'APW', 'BLK', key = "PA.Level", value = "ThresholdValue"),
       mapping = aes(x = PenaltyWeight, y = ThresholdValue)) +
  geom_point(mapping = aes(colour = ThreshType), size = 4) +
  facet_grid(~ PA.Level) + 
  theme(strip.text.x = element_text(size = 26, colour = "black", angle = 0)) +
  ggtitle(str_c(Specified.Thresh.Name , "Threshold")) +
  xlab("Penalty Weight on Proportion of Changing Risk Scores") +
  ylab("Threshold Value")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 28, vjust = 0.5, angle = 90, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 28, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 36, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 36, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 36, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 28, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 48, angle = 0, face = "plain"),
        legend.key.size = unit(3,"points"))
#------------#
### Step 6 ###
#------------#
ggplot(data = ParallelResults.TuningAssess %>% filter(PerformanceMeasure == "ERB"),
       mapping = aes(x = PenaltyWeight, y = AvgPerformanceValue)) +
  geom_point(mapping = aes(colour = ThreshType)) +
  facet_grid(Threshold ~ PerformanceMeasure) +
  ggtitle("All Thresholds") +
  xlab("Penalty Weight on Proportion of Changing Risk Scores") +
  ylab("Average ERB")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
ggplot(data = ParallelResults.TuningAssess %>% filter(PerformanceMeasure == "ERB"),
       mapping = aes(x = AvgPropDeltaScores, y = AvgPerformanceValue)) +
  geom_point(mapping = aes(colour = ThreshType)) +
  facet_grid(Threshold ~ PerformanceMeasure) +
  ggtitle("All Thresholds") +
  xlab("Proportion of Changing Risk Scores") +
  ylab("Average Performance Measure")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#------------#
### Step 7 ###
#------------#
DiffInfo <- ParallelResults.TuningAssess %>% select(ThreshType, Threshold, PerformanceMeasure, AvgPerformanceValue, TuningGridID) %>%
  group_by(TuningGridID, Threshold, PerformanceMeasure) %>% summarize(PerfValue_Increase = diff(AvgPerformanceValue)*(-1))
PostOnlyInfo <- ParallelResults.TuningAssess %>% filter(ThreshType == "PostFC")
ParResultsChange.TuningAssess.Best <- left_join(PostOnlyInfo, DiffInfo, by = c("TuningGridID", "Threshold", "PerformanceMeasure"))
ParResultsChange.TuningAssess.Best <- ParResultsChange.TuningAssess.Best %>% select(Threshold, PerformanceMeasure, PenaltyWeight, AvgPropDeltaScores, PerfValue_Increase, TuningGridID)
ggplot(data = ParResultsChange.TuningAssess.Best %>% 
         filter(PerformanceMeasure == "NPV" | PerformanceMeasure == "FNR" | PerformanceMeasure == "FPR" | PerformanceMeasure == "ACC" | PerformanceMeasure == "PPV"),
       mapping = aes(x = PenaltyWeight, y = PerfValue_Increase)) +
  geom_point(mapping = aes(colour = Threshold)) +
  facet_grid(~ PerformanceMeasure) +
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) + 
  ggtitle("Change in Average Performance Measure (PostFC - PreFC)") +
  xlab("Penalty Weight on Proportion of Changing Risk Scores") +
  ylab("Increace in Average Performance Measure")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 8. ###
#-------------#
PropChangeInfo <- ParallelResults.TuningAssess %>% select(ThreshType, Threshold, PerformanceMeasure, AvgPerformanceValue, TuningGridID) %>% 
  group_by(TuningGridID, Threshold, PerformanceMeasure) %>% summarize(PropChange_Value = AvgPerformanceValue[ThreshType == "PostFC"]/AvgPerformanceValue[ThreshType == "PreFC"])
ParResultsPropChange.TuningAssess.Best <- left_join(PostOnlyInfo, PropChangeInfo, by = c("TuningGridID", "Threshold", "PerformanceMeasure"))
ParResultsPropChange.TuningAssess.Best <- ParResultsPropChange.TuningAssess.Best %>% select(Threshold, PerformanceMeasure, PenaltyWeight, AvgPropDeltaScores, PropChange_Value, TuningGridID)
ggplot(data = ParResultsPropChange.TuningAssess.Best %>% 
         filter(PerformanceMeasure == "NPV" | PerformanceMeasure == "FNR" | PerformanceMeasure == "FPR" | PerformanceMeasure == "ACC" | PerformanceMeasure == "PPV"),
       mapping = aes(x = PenaltyWeight, y = PropChange_Value)) +
  geom_point(mapping = aes(colour = Threshold)) +
  geom_hline(yintercept = 1, linetype = 1, color = "black", size = 1) + 
  facet_grid(~ PerformanceMeasure) +
  ggtitle("Proportional Change in Average Performance Measure (PostFC/PreFC)") +
  xlab("Penalty Weight on Proportion of Changing Risk Scores") +
  ylab("Proportional Change in Average Performance Measure")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
ggplot(data = ParResultsPropChange.TuningAssess.Best %>% 
         filter(PerformanceMeasure == "ERB"),
       mapping = aes(x = PenaltyWeight, y = PropChange_Value)) +
  geom_point(mapping = aes(colour = Threshold)) +
  geom_hline(yintercept = 1, linetype = 1, color = "black", size = 1) +
  ggtitle("Proportional Change in Average ERB (PostFC/PreFC)") +
  xlab("Penalty Weight on Proportion of Changing Risk Scores") +
  ylab("Proportional change in Average ERB")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#############################################################################################################################################
#############################################################################################################################################
###                      Part 3 -- Select Thresholds to Move to Implementation and Save Relevant Information.                             ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Identify optimal threshold set, corresponding value of penalty weight (penalty weight on the proportion of scores that change), and###
###    other relevant information for each threshold (e.g. Low, Average, and High).                                                       ###
###    DEFAULT: Identify the threshold values that result is maximum Average ERB.  If there are "ties", then the thresholds resulting in  ###
###             the smallest variability in ERB across subsamples are selected.  If there still remain "ties", then thresholds            ###
###             corresponding to the largest penalty weight, w, are selected since larger values of w approximately correspond to smaller ###
###             losses in accuracy.                                                                                                       ### 
### 2) Save the thresholds identifed in step 1, along with some other relevant information.                                               ###
### 3) Isolate, consolidate, and save the various subsample thresholds corresponding to the optimal set of penalty weights across the     ###
###    thresholds.                                                                                                                        ###
### 4) Save the subsample thresholds combined in step 3.                                                                                  ###
### 5) Calculate the subsample aggregated PreFC and PostFC thresholds from step 3 and verify that they match the subsample aggregated     ###
###    PostFC thresholds from Step 1.  Assuming they match, save the resulting subsample aggregated thresholds.                           ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 1. ###
#-------------#
FairnessCutOffs <- ParallelResults.TuningAssess %>% filter(PerformanceMeasure == "ERB") %>% group_by(Threshold) %>% 
  summarize(FairnessQuantiles = quantile(AvgPerformanceValue, probs = 0.0))
FairnessCutOffs <- left_join(ParallelResults.TuningAssess, FairnessCutOffs, by = c("Threshold"))
FairnessCutOffs.Filtered <- FairnessCutOffs %>% filter(PerformanceMeasure == "ERB", AvgPerformanceValue > FairnessQuantiles, AvgPropDeltaScores < 0.99)
ggplot(data = FairnessCutOffs.Filtered,
       mapping = aes(x = AvgPropDeltaScores, y = AvgPerformanceValue)) +
  geom_point(mapping = aes(colour = Threshold, shape = ThreshType)) +
  #geom_point(mapping = aes(colour = Threshold, shape = ThreshType, size = SDperformanceValue)) +
  ggtitle("All Thresholds") +
  xlab("Proportion of Changing Risk Scores") +
  ylab("Average ERB")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
ggplot(data = FairnessCutOffs.Filtered,
       mapping = aes(x = PenaltyWeight, y = AvgPerformanceValue)) +
  geom_point(mapping = aes(colour = Threshold, shape = ThreshType)) +
  #geom_point(mapping = aes(colour = Threshold, shape = ThreshType, size = SDperformanceValue)) +
  ggtitle("All Thresholds") +
  xlab("Penalty Weight on Proportion of Changing Risk Scores") +
  ylab("Average ERB")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
ThreshToImplement.Info <- FairnessCutOffs.Filtered %>% group_by(Threshold) %>% filter(AvgPerformanceValue == max(AvgPerformanceValue)) %>%
  filter(SDperformanceValue == min(SDperformanceValue)) %>% filter(PenaltyWeight == max(PenaltyWeight))
ThreshToImplement.Info <- ThreshToImplement.Info %>% select(-FairnessQuantiles)
#-------------#
### Step 2. ###
#-------------#
#saveRDS(ThreshToImplement.Info, file = paste0("Info_FinalPostFCthresholds_50Avg50_", Sys.Date(),".rds")) 
#-------------#
### Step 3. ###
#-------------#
ConstantInfoAcrossThresh <- ThreshStorageList.Grid[[1]][, which(colnames(ThreshStorageList.Grid[[1]]) %in% c("SubSampID", "ThreshType", "OptimizerProcess"))]
PenaltyWeightColLoc <- which(colnames(ThreshStorageList.Grid[[1]]) %in% c("PenaltyWeight"))
PenaltyWeightDF <- as.data.frame(matrix(NA, nrow = nrow(ThreshStorageList.Grid[[1]]), ncol = NumThresh))
colnames(PenaltyWeightDF) <- str_c("PenaltyWeight_", ThreshNames)
for(k in 1:NumThresh){
  ThreshToImplInfo.k <- ThreshToImplement.Info %>% select(Threshold, TuningGridID, PenaltyWeight) %>% filter(Threshold == ThreshNames[k])
  RelColLocs.k <- which(str_detect(colnames(ThreshStorageList.Grid[[1]]), ThreshNames[k]) == TRUE)[1:length(PAlevs)]
  PenaltyWeightDF[, k] <- ThreshStorageList.Grid[[unlist(ThreshToImplInfo.k$TuningGridID)]][, PenaltyWeightColLoc]
  ThreshStorageSS.Final.k <- ThreshStorageList.Grid[[unlist(ThreshToImplInfo.k$TuningGridID)]][, RelColLocs.k]
  if(k == 1){
    ThreshStorageSS.Final <- ThreshStorageSS.Final.k
  }else{ThreshStorageSS.Final <- cbind(ThreshStorageSS.Final, ThreshStorageSS.Final.k)} 
}
ThreshStorageSS.Final <- cbind(cbind(ThreshStorageSS.Final, PenaltyWeightDF), ConstantInfoAcrossThresh)
#-------------#
### Step 4. ###
#-------------#
#saveRDS(ThreshStorageSS.Final, file = paste0("Final_SubSampThresh_50Avg50_", Sys.Date(),".rds")) 
#-------------#
### Step 5. ###
#-------------#
### Aggregated Pre-FC Thresholds. ###
PreFCssThresh <- ThreshStorageSS.Final %>% filter(ThreshType == "PreFC")
AggSubSampThresholds_PreFC <- colMeans(PreFCssThresh[, 1:(NumThresh * length(PAlevs))])
AggSubSampThresholds_PreFC <- matrix(unlist(AggSubSampThresholds_PreFC), NumThresh, length(PAlevs), byrow = TRUE)
colnames(AggSubSampThresholds_PreFC) <- PAlevs
rownames(AggSubSampThresholds_PreFC) <- ThreshNames
### Aggregated Post-FC Thresholds. ###
PostFCssThresh <- ThreshStorageSS.Final %>% filter(ThreshType == "PostFC")
AggSubSampThresholds_PostFC <- colMeans(PostFCssThresh[, 1:(NumThresh * length(PAlevs))])
AggSubSampThresholds_PostFC <- matrix(unlist(AggSubSampThresholds_PostFC), NumThresh, length(PAlevs), byrow = TRUE)
colnames(AggSubSampThresholds_PostFC) <- PAlevs
rownames(AggSubSampThresholds_PostFC) <- ThreshNames
###
ThreshTesterMatrix <- as.matrix(ThreshToImplement.Info[, -which(colnames(ThreshToImplement.Info) %in% c("ThreshType", "Threshold", "PerformanceMeasure", "AvgPerformanceValue", "SDperformanceValue", "AvgPropDeltaScores", "TuningGridID", "PenaltyWeight", "OptimizerType", "OptimizerApproach", "OptimizerProcess", "AvgObjFcnVal"))])
ThreshTesterMatrix.Reorg <- matrix(NA, nrow(ThreshTesterMatrix), ncol(ThreshTesterMatrix))
colnames(ThreshTesterMatrix.Reorg) <- colnames(ThreshTesterMatrix)
rownames(ThreshTesterMatrix.Reorg) <- ThreshNames
for(k in 1:NumThresh){
  ThreshTesterMatrix.Reorg[k, ] <- ThreshTesterMatrix[which(ThreshToImplement.Info$Threshold == ThreshNames[k]), ]
}   ### Verify that the pulled subsample thresholds produce the same subsample aggregated (i.e. PostFC) thresholds.  This next line of code should return 0 if correct. ###
sum(ThreshTesterMatrix.Reorg - AggSubSampThresholds_PostFC)
### Now combine pre- and post-fairness corrected thresholds into a list for storage. ###
AggThresholdsSubSampleList <- list(AggSubSampThresholds_PreFC, AggSubSampThresholds_PostFC)
names(AggThresholdsSubSampleList) <- c("Pre-FC", "Post-FC")
AggThresholdsSubSampleList.rds <- AggThresholdsSubSampleList
#saveRDS(AggThresholdsSubSampleList.rds, file = paste0("Final_SubSampAggThresholds_50Avg50_", Sys.Date(),".rds")) 
#############################################################################################################################################
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###                     Objective (d) -- Post Auditing of PreFC vs. PostFC Threshold Values using the Test Set.                           ###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
###                                        Part 1 -- Load libraries and data                                                              ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Load and put in an appropriate format the pre- and post-fairness corrected thresholds corresponding to the fitted model being      ###
###    assessed.                                                                                                                          ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 4. ###
#-------------#
AggSubSampThresholds_PreFC <- AggThresholdsSubSampleList[[1]]
AggSubSampThresholds_PostFC <- AggThresholdsSubSampleList[[2]]
#############################################################################################################################################
#############################################################################################################################################
### Part 2 -- Obtain Quantitative Measures Comparing Algorithmic Fairness and Predictive Performance between Pre- and Post-FC Thresholds. ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Specify input arguments for SubSamples_Measures_fcn function in the Fairness_Functions_Packages script.                            ###
### 2) Using the SubSamples_Measures_fcn function in the Fairness_Functions_Packages script, obtain metrics for each subsample.  The      ###
###    returned list consists of the following data frames:                                                                               ###
###        a) Unduplicated Protected Attribute Distribution.                                                                              ###
###        b) Outcome Distribution Overall and by Protected Attribute Level For All Subsamples.                                           ###
###        c) Risk Score Distribution Overall and by Protected Attribute Level For All Subsamples.                                        ###
###        d) AUC by Protected Attribute Level For All Subsamples.                                                                        ###
###        e) Confusion Matrix Distribution Overall and by Protected Attribute Level For All Subsamples.                                  ###
###        f) Statistical Fairness Measures For All Subsamples.                                                                           ###
###        g) Statistical Calibration Fairness Measure For All Subsamples.                                                                ###
###        h) Predictive Performance Measures Overall and by Protected Attribute Level For All Subsamples.                                ###
###        i) Risk Score Change Distribution Overall and by Protected Attribute Level For All Subsamples.                                 ###
### 3) Save output of step 2.                                                                                                             ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 1. ###
#-------------#
NumSubSamples <- 200
ThreshNames <- c("LR_thresh", "AR_thresh", "HR_thresh")
NumSubSamp.Compare <- NumSubSamples
#-------------#
### Step 2. ###
#-------------#
set.seed(112358)
CompareOutput <- SubSamples_Measures_fcn(DataSet.Compare = FairnessDataSet.Test, PA.Compare = "Protected_Attribute", NumSubSamp.Compare = NumSubSamples,
                                         Thresh.PreFC = AggSubSampThresholds_PreFC, Thresh.PostFC = AggSubSampThresholds_PostFC, 
                                         ThreshNames.Compare = ThreshNames, 
                                         PA.Audit = FALSE, PA.SubAudit = FALSE)
#-------------#
### Step 3. ###
#-------------#
#saveRDS(CompareOutput, file = paste0("PostFC_Thresholds_Audit_Results_50Avg50_", Sys.Date(),".rds")) 
#############################################################################################################################################
#############################################################################################################################################
###           Part 3 -- Calculate and Visualize Various Fairness, Predictive Performance, and Other Relevant Measures.                    ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Distribution of protected attribute.                                                                                               ###
### 2) Distribution of outcome variable overall, and by protected attribute level.                                                        ###
### 3) Distribution of risk scores overall, and by protected attribute level.                                                             ###
### 4) AUC by protected attribute level.                                                                                                  ###
### 5) Statistical fairness measures (excluding Calibration, given its inherently different nature).                                      ###
### 6) Calibration (the statistical fairness measure).                                                                                    ###
### 7) Predictive performance measures, including AUC.                                                                                    ###
### 8) Distribution of confusion matrix overall, and by protected atrtibute level.                                                        ###
### 9) Distribution of change in risk scores (between PreFC and PostFC) overall, and by protected attribute level.                        ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 1. ###
#-------------#
PAdistSubSample <- CompareOutput$"Unduplicated_Protected_Attribute_Distribution"
ggplot(data = PAdistSubSample,
       mapping = aes(x = Protected_Attribute, y = Proportion, fill = Protected_Attribute)) +
  geom_col() +
  ggtitle("Distribution of Protected Attribute") +
  xlab("Protected Attribute Level") +
  ylab("Proportion") + 
  geom_text(aes(label = Count), vjust = -0.5) + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 2. ###
#-------------#
OtcmDistSubSample <- CompareOutput$"Outcome_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
ggplot(data = OtcmDistSubSample %>% filter(OUTCOME == 1) %>% group_by(PA_Level) %>% 
         summarize(AvgPrevalence = mean(PA_Level_Distribution), SDprev = sd(PA_Level_Distribution), AvgCount = mean(n), SDcount = sd(n)),
       mapping = aes(x = PA_Level, y = AvgPrevalence, fill = PA_Level)) +
  geom_col() +
  ggtitle("Prevalence of Outcome") +
  xlab("Protected Attribute Level") +
  ylab("Proportion") + 
  geom_text(aes(label = round(AvgCount)), vjust = -0.5) + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 3. ###
#-------------#
RiskScoreDistSubSamp <- CompareOutput$"Risk_Score_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
ggplot(data = RiskScoreDistSubSamp %>% group_by(PA_Level, ThreshType, RiskScore) %>% 
         summarize(AvgCount = mean(n), SDcount = sd(n), AvgRSdist = mean(Risk_Score_Distribution), SDrsDist = sd(Risk_Score_Distribution)),
       mapping = aes(x = PA_Level, y = AvgRSdist, group = ThreshType, fill = ThreshType)) + 
  geom_col(position = "dodge") +
  facet_wrap(~ RiskScore) +
  ggtitle("Average Risk Score Distribution by Threshold Type \n (Pre- vs. Post-Fairness-Corrected)") +
  xlab("Protected Attribute Level") +
  ylab("Proportion") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 4. ###
#-------------#
AUCsubSamp <- CompareOutput$"AUC_by_Protected_Attribute_Level__For_All_Subsamples"
ggplot(data = AUCsubSamp %>% group_by(Protected_Attribute) %>% summarize(AvgAUC = mean(PA_Level_AUC), SDauc = sd(PA_Level_AUC)),
       mapping = aes(x = Protected_Attribute, y = AvgAUC, fill = Protected_Attribute)) +
  geom_col() +
  ggtitle("AUC by Protected Attribute Level") +
  xlab("Protected Attribute Level") +
  ylab("AUC") + 
  ylim(0, 1) +
  geom_text(aes(label = round(AvgAUC, 2)), vjust = -0.5) + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 5. ###
#-------------#
FairnessSubSamp <- CompareOutput$"Statistical_Fairness_Measures__For_All_Subsamples"
###
ggplot(data = FairnessSubSamp %>% spread(key = ThresholdType, value = Fairness_Value),
       mapping = aes(x = PreFC, y = PostFC)) +
  geom_point(aes(col = Threshold)) +
  xlim(0, 1) + 
  ylim(0, 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "blue", size = 1) +
  facet_wrap(~ Fairness_Metric, nrow = 2) + 
  ggtitle("Post-Fairness-Corrected vs. Pre-Fairness-Corrected \n Distribution of Fairness Values") +
  xlab("Pre-Fairness-Corrected Value") +
  ylab("Post-Fairness-Corrected Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
###
Fairness.Summary <- FairnessSubSamp %>% group_by(Threshold, Fairness_Metric, ThresholdType) %>% 
  summarize(Avg_Value = mean(Fairness_Value), SD_Value = sd(Fairness_Value)) %>% arrange(Fairness_Metric, Threshold)
ggplot(data = Fairness.Summary,
       mapping = aes(x = Fairness_Metric, y = Avg_Value, group = ThresholdType, fill = ThresholdType)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~ Threshold, nrow = 3) + 
  ggtitle("Average Fairness Value by Threshold Type \n (Pre- vs. Post-Fairness-Corrected)") +
  xlab("Fairness Measure") +
  ylab("Average Fairness Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
Fairness.Summary %>% filter(Fairness_Metric == "ERB") %>% arrange(ThresholdType)
###
ggplot(data = FairnessSubSamp %>% filter(Fairness_Metric == "ERB") %>%
         spread(key = ThresholdType, value = Fairness_Value) %>% mutate(ERB_Increase = PostFC-PreFC),
       mapping = aes(x = ERB_Increase)) +
  geom_histogram(fill = "grey", color = "black") +
  facet_grid( ~ Threshold) +
  theme(strip.text.x = element_text(size = 20, colour = "black", angle = 0)) +
  geom_vline(xintercept = 0, col = "blue", size = 2) +
  xlab("Increase in Error Rate Balance") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"))
#-------------#
### Step 6. ###
#-------------#
FairCalSubSamp <- CompareOutput$"Statistical_Calibration_Fairness_Measure__For_All_Subsamples"
###
ggplot(data = FairCalSubSamp %>% spread(key = ThresholdType, value = Fairness_Value),
       mapping = aes(x = PreFC, y = PostFC)) +
  geom_point(aes(col = Score)) +
  xlim(0, 1) + 
  ylim(0, 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "blue", size = 1) +
  ggtitle("Post-Fairness-Corrected vs. Pre-Fairness-Corrected Distribution \n of Calibration Fairness Values") +
  xlab("Pre-Fairness-Corrected Value") +
  ylab("Post-Fairness-Corrected Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
###
FairCAL.Summary <- FairCalSubSamp %>% group_by(Score, Fairness_Metric, ThresholdType) %>% 
  summarize(Avg_Value = mean(Fairness_Value), SD_Value = sd(Fairness_Value)) %>% arrange(Fairness_Metric, Score)
ggplot(data = FairCAL.Summary,
       mapping = aes(x = Fairness_Metric, y = Avg_Value, group = ThresholdType, fill = ThresholdType)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~ Score, nrow = 1) + 
  ggtitle("Average Calibration Fairness Value by Threshold Type \n (Pre- vs. Post-Fairness-Corrected)") +
  xlab("Fairness Measure") +
  ylab("Average Calibration Fairness Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 7. ###
#-------------#
PredPerfSubSamp <- CompareOutput$"Predictive_Performance_Measures_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
###
ggplot(data = PredPerfSubSamp %>% spread(key = ThreshType, value = PerformanceValue) %>%
         filter(PerformanceMeasure == "ACC" | PerformanceMeasure == "FNR" | PerformanceMeasure == "FPR" | PerformanceMeasure == "NPV" | PerformanceMeasure == "PPV"),
       mapping = aes(x = PreFC, y = PostFC, shape = Threshold)) +
  geom_point(aes(col = PA_Level)) +
  xlim(0, 1) + 
  ylim(0, 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "blue", size = 1) +
  facet_wrap(Threshold ~PerformanceMeasure, nrow = 3) + 
  ggtitle("Post-Fairness-Corrected vs. Pre-Fairness-Corrected Distribution \n of Predictive Performance Values") +
  xlab("Pre-Fairness-Corrected Predictive Performance Value") +
  ylab("Post-Fairness-Corrected Predictive Performance Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
###
PredPerf.Summary <- PredPerfSubSamp %>% group_by(PerformanceMeasure, Threshold, PA_Level, ThreshType) %>%
  summarize(Avg_Value = mean(PerformanceValue), SD_Value = sd(PerformanceValue)) %>% arrange(Threshold, PerformanceMeasure)
ggplot(data = PredPerf.Summary %>% 
         filter(PerformanceMeasure == "ACC" | PerformanceMeasure == "FNR" | PerformanceMeasure == "FPR" | PerformanceMeasure == "NPV" | PerformanceMeasure == "PPV") , 
       mapping = aes(x = PA_Level, y = Avg_Value, group = ThreshType, fill = ThreshType)) + 
  geom_col(position = "dodge") + 
  facet_wrap(Threshold ~ PerformanceMeasure, nrow = 3) + 
  ggtitle("Average Predictive Performance Values by Threshold Type \n (Pre- vs. Post-Fairness-Corrected)") +
  xlab("Protected Attribute Level") +
  ylab("Average Predictive Performance Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
PredPerf.Summary.LR <- PredPerf.Summary %>% filter(Threshold == "LR_thresh") %>% select(PerformanceMeasure, PA_Level, ThreshType, Avg_Value) %>%
  spread(key = PerformanceMeasure, value = Avg_Value) %>% select(PA_Level, ThreshType, ACC, FNR, FPR, NPV, PPV) %>% arrange(ThreshType)
PredPerf.Summary.LR
PredPerf.Summary.LR %>% filter(PA_Level == "Overall")
PredPerf.Summary.AR <- PredPerf.Summary %>% filter(Threshold == "AR_thresh") %>% select(PerformanceMeasure, PA_Level, ThreshType, Avg_Value) %>%
  spread(key = PerformanceMeasure, value = Avg_Value) %>% select(PA_Level, ThreshType, ACC, FNR, FPR, NPV, PPV) %>% arrange(ThreshType)
PredPerf.Summary.AR
PredPerf.Summary.AR %>% filter(PA_Level == "Overall")
PredPerf.Summary.HR <- PredPerf.Summary %>% filter(Threshold == "HR_thresh") %>% select(PerformanceMeasure, PA_Level, ThreshType, Avg_Value) %>%
  spread(key = PerformanceMeasure, value = Avg_Value) %>% select(PA_Level, ThreshType, ACC, FNR, FPR, NPV, PPV) %>% arrange(ThreshType)
PredPerf.Summary.HR
PredPerf.Summary.HR %>% filter(PA_Level == "Overall")
#-------------#
### Step 8. ###
#-------------#
ConfMatSubSample <- CompareOutput$"Confusion_Matrix_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
ggplot(data = ConfMatSubSample %>% group_by(PA_Level, Cell_Type, ThreshType, Threshold) %>% 
         summarize(AvgCount = mean(Count), SDcount = sd(Count), AvgProp = mean(Proportion), SDprop = sd(Proportion)),
       mapping = aes(x = PA_Level, y = AvgProp, group = ThreshType, fill = ThreshType)) +
  geom_col(position = "dodge") +
  facet_wrap(Cell_Type ~ Threshold, nrow = 4) +
  ggtitle("Average Confusion Matrix Distribution Across Protected Attribute Levels \n by Threshold Type (Pre- vs. Post-Fairness-Corrected)") +
  xlab("Protected Attribute Level") +
  ylab("Proportion") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
#-------------#
### Step 9. ###
#-------------#
RiskScoreChangeSubSamp <- CompareOutput$"Risk_Score_Change_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
ggplot(data = RiskScoreChangeSubSamp %>% group_by(PreFC_RiskScore, PostFC_RiskScore, PA_Level) %>%
         summarize(AvgChangeCount = mean(Change_Count)) %>% arrange(PA_Level) %>% 
         mutate(NewRS.PreFC = str_c("PreFC - ", PreFC_RiskScore), NewRS.PostFC = str_c("PostFC - ", PostFC_RiskScore)),
       mapping = aes(x = PA_Level, y = AvgChangeCount, fill = PA_Level)) +
  geom_col(position = "dodge") +
  facet_grid(NewRS.PreFC ~ NewRS.PostFC) +
  geom_text(aes(label = round(AvgChangeCount)), vjust = -0.5) + 
  ylim(0, 6200) + 
  ggtitle("Average Distribution of Change in Risk Scores \n (Pre- to Post-Fairness-Corrected) by Protected Attribute Levels") +
  xlab("Protected Attribute Level") +
  ylab("Average Count") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
###
RiskScoreChange.Summary <- RiskScoreChangeSubSamp %>% group_by(PA_Level, PreFC_RiskScore, PostFC_RiskScore) %>% 
  summarize(AvgChange_Count = mean(Change_Count)) %>% 
  mutate(AvgProportionChange_ByPreFCscore = AvgChange_Count/sum(AvgChange_Count)) %>% ungroup() %>% 
  group_by(PA_Level) %>% mutate(AvgProportionChange_Overall = AvgChange_Count/sum(AvgChange_Count))
ggplot(data = RiskScoreChange.Summary %>% 
         mutate(NewRS.PreFC = str_c("PreFC - ", PreFC_RiskScore), NewRS.PostFC = str_c("PostFC - ", PostFC_RiskScore)),
       mapping = aes(x = PA_Level, y = AvgProportionChange_ByPreFCscore, fill = PA_Level)) +
  geom_col(position = "dodge") +
  facet_grid(NewRS.PreFC ~ NewRS.PostFC) +
  geom_text(aes(label = round(AvgProportionChange_ByPreFCscore, 2)), vjust = -0.5) + 
  ylim(0, 1.1) + 
  ggtitle("Average Distribution of Change in Risk Scores \n (Pre- to Post-Fairness-Corrected) by Protected Attribute Levels") +
  xlab("Protected Attribute Level") +
  ylab("Average Proportion") + 
  theme(axis.text.x = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 15, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 12, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 25, angle = 0, face = "plain"))
RiskScoreChange.Summary %>% filter(PA_Level == "Overall") %>% filter(PreFC_RiskScore ==  PostFC_RiskScore) %>% summarize(AvgPropChange.Overall = 1 - sum(AvgProportionChange_Overall))
#############################################################################################################################################
#############################################################################################################################################
###                Part 4 -- Calculate and Visualize Some Additional Summaries Based on a Single Random Subsample.                        ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Generate a single random subsample.                                                                                                ###
### 2) Create "impact" plot.                                                                                                              ###
### 3) Create ROC curves by PA level (and overall).                                                                                       ###
### 4) Generate calibration-style table for both the pre- and post-fairness-corrected thresholds.                                         ###
#############################################################################################################################################
#############################################################################################################################################
#------------#
### Step 1 ###
#------------#
FairnessDataSetFiltered <- Application_Specific_Resampling_fcn(data_set = FairnessDataSet.Test)
FairnessDataSetFiltered <- FairnessDataSetFiltered[which(colnames(FairnessDataSet.Test) %in% c("OUTCOME", "Protected_Attribute", "m_pred"))]
#------------#
### Step 2 ###
#------------#
GraphicFairnessDataSet.RiskScore <- Assign_Risk_Scores(Thresholds = as.vector(t(AggSubSampThresholds_PostFC)), DataSet = FairnessDataSetFiltered, 
                                                       ProtAtrLevels = levels(FairnessDataSetFiltered$Protected_Attribute), 
                                                       NumThresholds = length(ThreshNames), ThresholdNames = ThreshNames)
GraphicFairnessDataSet <- FairnessDataSetFiltered
GraphicFairnessDataSet$RiskScore <- GraphicFairnessDataSet.RiskScore
GraphThresholds.Baseline <- data.frame(rep("Pre-FairnessCorrected", (length(levels(FairnessDataSetFiltered$Protected_Attribute))*length(ThreshNames))),
                                       as.vector(AggSubSampThresholds_PreFC), 
                                       rep(colnames(AggSubSampThresholds_PreFC), each = length(ThreshNames)), 
                                       rep(rownames(AggSubSampThresholds_PreFC), length(levels(FairnessDataSetFiltered$Protected_Attribute))))
colnames(GraphThresholds.Baseline) <- c("Type", "ThresholdValue", "ProtectedAttributeLevel", "ThresholdLocation")
GraphThresholds.FC <- data.frame(rep("Post-FairnessCorrected", (length(levels(FairnessDataSetFiltered$Protected_Attribute))*length(ThreshNames))),
                                 as.vector(AggSubSampThresholds_PostFC), 
                                 rep(colnames(AggSubSampThresholds_PostFC),each = length(ThreshNames)), 
                                 rep(rownames(AggSubSampThresholds_PostFC), length(levels(FairnessDataSetFiltered$Protected_Attribute))))
colnames(GraphThresholds.FC) <- c("Type", "ThresholdValue", "ProtectedAttributeLevel", "ThresholdLocation")
GraphThresholds <- as_tibble(rbind(GraphThresholds.Baseline, GraphThresholds.FC))
p.Reentry <- ggplot(data = GraphicFairnessDataSet) +
  aes(x = Protected_Attribute, y = m_pred, color = Protected_Attribute, size = 2) +
  facet_grid(. ~ RiskScore) +
  geom_jitter(width = .4, alpha = .2) +
  geom_segment(data = GraphThresholds %>% filter(Type == "Pre-FairnessCorrected"), 
               aes(x = -Inf, xend = Inf, y = ThresholdValue, yend = ThresholdValue),
               color = "maroon", size = 2.5) +
  labs(x = "Protected Attribute Level by Assigned Risk Score",
       y = "Probability of Success") +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", colour = "black", size = 30),
        axis.text.x = element_text(size = 26)) + 
  theme(axis.title.y = element_text(face="bold", colour = "black", size = 30), 
        axis.text.y = element_text(size = 26)) +
  theme(strip.text.x = element_text(size = 30, colour = "black")) +
  theme(plot.title = element_text(face="bold", colour = "black", size = 20, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", colour = "black", size = 18, hjust = 0.5)) + 
  theme(plot.caption = element_text(colour = "black", size = 15)) + 
  theme(legend.position = "none")
p.Reentry
#------------#
### Step 3 ###
#------------#
FiltPredsList.PA <- list()
FiltOutcmList.PA <- list()
for(k in 1:length(levels(FairnessDataSetFiltered$Protected_Attribute))){
  PAlev.k <- levels(FairnessDataSetFiltered$Protected_Attribute)[k]
  FiltPredsList.PA[[k]] <- unlist(GraphicFairnessDataSet[which(GraphicFairnessDataSet$Protected_Attribute == PAlev.k), which(colnames(GraphicFairnessDataSet) %in% c("m_pred"))])
  FiltOutcmList.PA[[k]] <- unlist(GraphicFairnessDataSet[which(GraphicFairnessDataSet$Protected_Attribute == PAlev.k), which(colnames(GraphicFairnessDataSet) %in% c("OUTCOME"))])
}
pred.PA <- prediction(FiltPredsList.PA, FiltOutcmList.PA)
rocs.PA <- performance(pred.PA, "tpr", "fpr")
plot(rocs.PA, col = as.list(1:length(levels(FairnessDataSetFiltered$Protected_Attribute))), 
     main = "Protected Attribute Level Specific ROC Curves", lwd = 2.0)
legend(x = "bottomright", legend = levels(FairnessDataSetFiltered$Protected_Attribute), 
       fill = 1:length(levels(FairnessDataSetFiltered$Protected_Attribute)), bty = "n")
abline(a = 0, b = 1, col = "grey", lwd = 2.0)
#------------#
### Step 4 ###
#------------#
   ### PreFC. ###
CalTableSS.PreFC <- FairnessDataSetFiltered
CalTableSS.PreFC$RiskScore.PreFC <- Assign_Risk_Scores(Thresholds = as.vector(t(AggSubSampThresholds_PreFC)), DataSet = FairnessDataSetFiltered, 
                                                       ProtAtrLevels = levels(FairnessDataSetFiltered$Protected_Attribute), 
                                                       NumThresholds = length(ThreshNames), ThresholdNames = ThreshNames)
TableResultsSS.PreFC <- tabyl(CalTableSS.PreFC, var1 = RiskScore.PreFC, var2 = OUTCOME)
colnames(TableResultsSS.PreFC) <- c("RiskScore.PreFC", "Outcome_0", "Outcome_1")
TableResultsSS.PreFC <- TableResultsSS.PreFC %>% group_by(RiskScore.PreFC) %>% mutate(Count = sum(Outcome_0, Outcome_1))
Cal.TableResultsSS.PreFC <- TableResultsSS.PreFC %>% group_by(RiskScore.PreFC) %>% summarize(Percent = round((Count/nrow(CalTableSS.PreFC)) * 100, 0), Number = Count, Success.Percent = round((Outcome_1/Count) * 100, 1), RelativeRisk = Success.Percent/(sum(CalTableSS.PreFC$OUTCOME == 1)/nrow(CalTableSS.PreFC) * 100))
Cal.TableResultsSS.PreFC$RelativeRisk[Cal.TableResultsSS.PreFC$RelativeRisk < 1] <- -1/Cal.TableResultsSS.PreFC$RelativeRisk[Cal.TableResultsSS.PreFC$RelativeRisk < 1]
PreFC.CalTable <- Cal.TableResultsSS.PreFC
   ### PostFC. ###
CalTableSS.PostFC <- FairnessDataSetFiltered
CalTableSS.PostFC$RiskScore.PostFC <- Assign_Risk_Scores(Thresholds = as.vector(t(AggSubSampThresholds_PostFC)), DataSet = FairnessDataSetFiltered, 
                                                         ProtAtrLevels = levels(FairnessDataSetFiltered$Protected_Attribute), 
                                                         NumThresholds = length(ThreshNames), ThresholdNames = ThreshNames)
TableResultsSS.PostFC <- tabyl(CalTableSS.PostFC, var1 = RiskScore.PostFC, var2 = OUTCOME)
colnames(TableResultsSS.PostFC) <- c("RiskScore.PostFC", "Outcome_0", "Outcome_1")
TableResultsSS.PostFC <- TableResultsSS.PostFC %>% group_by(RiskScore.PostFC) %>% mutate(Count = sum(Outcome_0, Outcome_1))
Cal.TableResultsSS.PostFC <- TableResultsSS.PostFC %>% group_by(RiskScore.PostFC) %>% summarize(Percent = round((Count/nrow(CalTableSS.PostFC)) * 100, 0), Number = Count, Success.Percent = round((Outcome_1/Count) * 100, 1), RelativeRisk = Success.Percent/(sum(CalTableSS.PostFC$OUTCOME == 1)/nrow(CalTableSS.PostFC) * 100))
Cal.TableResultsSS.PostFC$RelativeRisk[Cal.TableResultsSS.PostFC$RelativeRisk < 1] <- -1/Cal.TableResultsSS.PostFC$RelativeRisk[Cal.TableResultsSS.PostFC$RelativeRisk < 1]
PostFC.CalTable <- Cal.TableResultsSS.PostFC
PreFC.CalTable
PostFC.CalTable
#############################################################################################################################################
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###             Objective (e) -- Replicate Graphics and Tables of Algorithmic Fairness Sections of Reunification Manuscript.              ###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
###---------------------------------------------------------------------------------------------------------------------------------------###
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
###                 Part 1 -- (Re-)Create Analogous Tables and Figures in Paper, Loading Various Results as Needed.                       ###
###                           NOTE: The numbers given to tables and figures below does not necessarily line up with                       ###
###                                 those of the manuscript.                                                                              ###
###---------------------------------------------------------------------------------------------------------------------------------------###
### 1) Rename ThreshStorageSS.Final to Thresh.OptTunParam.                                                                                ###
### 2) First Figure: PAdist_PrevByPAlevel_1800x700.jpeg                                                                                   ###
### 3) First Table: Protected Attribute Level Specific FPR's and FNR's for a single random subsample, as well as all corresponding        ###
###                 pairwise error rate ratios.                                                                                           ###
### 4) Second Figure: LowRiskThresholds_AllPenaltyWeights_1600x600.jpeg                                                                   ###
### 5) Third Figure: AvgERBvsPropChangingScores_1650x1800.jpeg                                                                            ###
### 6) Second Table: Pre- and Post-Fairness-Corrected Thresholds.                                                                         ###
### 7) Overall Percent of Changing Risk Scores in transitioning between pre- and post-fairness-corrected threshold values.                ###
### 8) Fourth Figure: ImpactPlot_1800x600.jpeg                                                                                            ###
### 9) Third Table: Average and Standard Deviations of ERB for the pre- and post-fairness-corrected threshold values.                     ###
### 10) Fifth Figure: ERBchangeDistribution_1000x300.jpeg                                                                                 ###
### 11) Fourth Table: Average Predictive Performance Measures (ACC, FNR, FPR, NPV, PPV) for the pre- and post-fairness-corrected          ###
###                   threhsold values.                                                                                                   ###
### 12) Sixth Figure: AllFairnessDefsPlots1200x1200.jpeg                                                                                  ###
#############################################################################################################################################
#############################################################################################################################################
#-------------#
### Step 1. ###
#-------------#
Thresh.OptTunParam <- ThreshStorageSS.Final
#-------------#
### Step 2. ###
#-------------#
PAdistSubSample <- CompareOutput$"Unduplicated_Protected_Attribute_Distribution"
Figure.1a <- ggplot(data = PAdistSubSample, 
                    mapping = aes(x = Protected_Attribute, y = Proportion)) + 
  geom_col() +
  ggtitle("") +
  xlab("Protected Attribute Level") +
  ylab("Proportion") + 
  #geom_text(aes(label = Count), vjust = -0.5, size = 6.25) + 
  theme(axis.text.x = element_text(color = "grey20", size = 24, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 24, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 30, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 30, angle = 90, face = "plain"))
OtcmDistSubSample <- CompareOutput$"Outcome_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
Figure.1b <-  ggplot(data = OtcmDistSubSample %>% filter(OUTCOME == 1 & PA_Level != "Overall") %>% group_by(PA_Level) %>% 
                       summarize(AvgPrevalence = mean(PA_Level_Distribution), SDprev = sd(PA_Level_Distribution), AvgCount = mean(n), SDcount = sd(n)), 
                     mapping = aes(x = PA_Level, y = AvgPrevalence)) + 
  geom_col() +
  ggtitle("") +
  xlab("Protected Attribute Level") +
  ylab("Proportion") + 
  #geom_text(aes(label = round(AvgCount)), vjust = -0.5, size = 6.25) + 
  theme(axis.text.x = element_text(color = "grey20", size = 24, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 24, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 30, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 30, angle = 90, face = "plain"))
grid.arrange(Figure.1a, Figure.1b, ncol = 1)
#-------------#
### Step 3. ###
#-------------#
set.seed(38)
ThreshNames <- c("LR_thresh", "AR_thresh", "HR_thresh")
NumberThresh <- length(ThreshNames)
ReferenceInfo <- CompareOutput$Unduplicated_Protected_Attribute_Distribution
PA.levs <- ReferenceInfo$Protected_Attribute
NumPA.levs <- length(PA.levs)
randomSS <- sample(unique(Thresh.OptTunParam$SubSampID), 1)
Thresh.OptTunParam.SS <- Thresh.OptTunParam %>% filter(SubSampID == randomSS)
PreFCssThresh.Paper <- Thresh.OptTunParam.SS %>% filter(ThreshType == "PreFC")
PreFCssThresh.Paper <- PreFCssThresh.Paper[, 1:(NumberThresh * NumPA.levs)]
AggSubSampThresholds_PreFC.Paper <- matrix(unlist(PreFCssThresh.Paper), NumberThresh, NumPA.levs, byrow = TRUE)
colnames(AggSubSampThresholds_PreFC.Paper) <- PA.levs
rownames(AggSubSampThresholds_PreFC.Paper) <- ThreshNames
PostFCssThresh.Paper <- Thresh.OptTunParam.SS %>% filter(ThreshType == "PostFC")
PostFCssThresh.Paper <- PostFCssThresh.Paper[, 1:(NumberThresh * NumPA.levs)]
AggSubSampThresholds_PostFC.Paper <- matrix(unlist(PostFCssThresh.Paper), NumberThresh, NumPA.levs, byrow = TRUE)
colnames(AggSubSampThresholds_PostFC.Paper) <- PA.levs
rownames(AggSubSampThresholds_PostFC.Paper) <- ThreshNames
CompareOutput.Paper <- SubSamples_Measures_fcn(DataSet.Compare = FairnessDataSet, PA.Compare = "Protected_Attribute", NumSubSamp.Compare = 1, 
                                               Thresh.PreFC = AggSubSampThresholds_PreFC.Paper, Thresh.PostFC = AggSubSampThresholds_PostFC.Paper, 
                                               ThreshNames.Compare = ThreshNames, 
                                               PA.Audit = FALSE, PA.SubAudit = FALSE)
PPmeasures.SS <- CompareOutput.Paper$Predictive_Performance_Measures_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples
randomSS <- sample(unique(PPmeasures.SS$SubSampleID), 1)
   ###
ErrorRate.PreFC.AR <- PPmeasures.SS %>% filter(PerformanceMeasure == "FNR" | PerformanceMeasure == "FPR", SubSampleID == randomSS, 
                                               Threshold == "AR_thresh", PA_Level != "Overall", ThreshType == "PreFC") %>%
  arrange(PerformanceMeasure)
   ###
PairWise.Combns <- combn(PA.levs, 2)
Table.1b <- matrix(NA, NumPA.levs, NumPA.levs)
rownames(Table.1b) <- PA.levs
colnames(Table.1b) <- PA.levs
for(k in 1:choose(NumPA.levs, 2)){
  rowLoc.k <- which(PA.levs == PairWise.Combns[1, k])
  colLoc.k <- which(PA.levs == PairWise.Combns[2, k])
  Table.1b[rowLoc.k, colLoc.k] <- unlist(ErrorRate.PreFC.AR %>% filter(PerformanceMeasure == "FNR", PA_Level %in% PairWise.Combns[, k]) %>% 
                                          summarize(PairWiseRatio = min(PerformanceValue)/max(PerformanceValue)))
  Table.1b[colLoc.k, rowLoc.k] <- unlist(ErrorRate.PreFC.AR %>% filter(PerformanceMeasure == "FPR", PA_Level %in% PairWise.Combns[, k]) %>% 
                                          summarize(PairWiseRatio = min(PerformanceValue)/max(PerformanceValue)))
}
Table.1a <- ErrorRate.PreFC.AR %>% select(PerformanceMeasure, PerformanceValue, PA_Level)
Table.1b <- round(Table.1b, 2) # Upper triangle represents pairwise FNR ratios, while lower triangle represents pairwise FPR ratios.
Table.1a
Table.1b
#-------------#
### Step 4. ###
#-------------#
ParallelResults.TuningAssess$ThreshType <- factor(ParallelResults.TuningAssess$ThreshType, levels = c("PreFC", "PostFC"))
ParallelResults.TuningAssess$ThreshType <- fct_relevel(ParallelResults.TuningAssess$ThreshType, "PreFC", "PostFC")
ParallelResults.TuningAssess$ThreshType <- recode(ParallelResults.TuningAssess$ThreshType, PreFC = "Pre-Fairness Correction", PostFC = "Post-Fairness Correction")
Figure.2 <- ggplot(data = ParallelResults.TuningAssess %>% filter(Threshold == "Low", PerformanceMeasure == "ERB") %>% 
         rename(ANO = SubSampAggThreshold.ANO, APW = SubSampAggThreshold.APW, BLK = SubSampAggThreshold.BLK) %>%
         gather('ANO', 'APW', 'BLK', key = "PA.Level", value = "ThresholdValue"),
       mapping = aes(x = PenaltyWeight, y = ThresholdValue)) +
  geom_point(mapping = aes(colour = ThreshType), size = 4) +
  facet_grid(~ PA.Level) + 
  theme(strip.text.x = element_text(size = 26, colour = "black", angle = 0)) +
  ggtitle("") +
  xlab("Penalty Weight (w)") +
  ylab("Threshold Value")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 28, vjust = 0.5, angle = 90, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 28, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 36, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 36, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 22, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        plot.title = element_text(color = "grey20", size = 48, angle = 0, face = "plain"),
        legend.key.size = unit(3,"points"))
ggarrange(Figure.2, ncol = 1, nrow = 1, common.legend = TRUE, legend = "top")
#-------------#
### Step 5. ###
#-------------#
ParallelResults.TuningAssess$Threshold <- fct_relevel(ParallelResults.TuningAssess$Threshold, "Low", "Average", "High")
ParallelResults.TuningAssess$Threshold <- recode(ParallelResults.TuningAssess$Threshold, LowRisk = "Low-Risk Threshold", AverageRisk = "Average-Risk Threshold", HighRisk = "High-Risk Threshold")
Seq_Even <- seq(0, 0.99, 0.04)
Seq_All <- seq(0, 1, 0.01)
Figure.3aV2 <- ggplot(data = ParallelResults.TuningAssess %>% filter(PerformanceMeasure == "ERB") %>%
                        filter(case_when(ThreshType == "Pre-Fairness Correction" ~ PenaltyWeight %in% Seq_Even,
                                         ThreshType == "Post-Fairness Correction" ~ PenaltyWeight %in% Seq_All)),
                    mapping = aes(x = PenaltyWeight, y = AvgPerformanceValue)) +
  geom_point(mapping = aes(colour = Threshold, shape = ThreshType), size = 3.0, show.legend = TRUE) +
  #facet_grid(Threshold ~ PerformanceMeasure) +
  theme(strip.text.x = element_blank()) +
  theme(strip.text.y = element_text(size = 38, colour = "black", angle = 90)) +
  ggtitle("") +
  xlab("Penalty Weight (w)") +
  ylab("Average ERB")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 16, vjust = 0.5, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 24, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 24, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 16, angle = 0, face = "plain"))
Seq_AllBut1 <- seq(0, 0.99, 0.01)
Figure.3bV2 <- ggplot(data = ParallelResults.TuningAssess %>% filter(PerformanceMeasure == "ERB")%>%
                        filter(case_when(ThreshType == "Pre-Fairness Correction" ~ PenaltyWeight %in% Seq_All,
                                         ThreshType == "Post-Fairness Correction" ~ PenaltyWeight %in% Seq_AllBut1)),
                    mapping = aes(x = AvgPropDeltaScores, y = AvgPerformanceValue)) +
  geom_point(mapping = aes(colour = Threshold, shape = ThreshType), size = 3.0, show.legend = TRUE) +
  #facet_grid(Threshold ~ PerformanceMeasure) +
  theme(strip.text.x = element_blank()) +
  theme(strip.text.y = element_text(size = 38, colour = "black", angle = 90)) +
  ggtitle("") +
  xlab("Proportion of Changing Risk Scores") +
  ylab("Average ERB")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 16, vjust = 0.5, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 24, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 24, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 16, angle = 0, face = "plain"))
ggarrange(Figure.3aV2, Figure.3bV2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
#-------------#
### Step 6. ###
#-------------#
Table.2 <- matrix(NA, NumPA.levs, (NumberThresh * 2))
rownames(Table.2) <- PA.levs
colnames(Table.2) <- c("LR-PreFC", "LR-PostFC", "AR-PreFC", "AR-PostFC", "HR-PreFC", "HR-PostFC")
Table.2[, c(2, 4, 6)] <- t(AggSubSampThresholds_PostFC)
Table.2[, c(1, 3, 5)] <- t(AggSubSampThresholds_PreFC)
Table.2 <- round(Table.2, 3)
Table.2
#-------------#
### Step 7. ###
#-------------#
RiskScoreChangeSubSamp <- CompareOutput$"Risk_Score_Change_Distribution_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
RiskScoreChange.Summary <- RiskScoreChangeSubSamp %>% group_by(PA_Level, PreFC_RiskScore, PostFC_RiskScore) %>% 
  summarize(AvgChange_Count = mean(Change_Count)) %>% 
  mutate(AvgProportionChange_ByPreFCscore = AvgChange_Count/sum(AvgChange_Count)) %>% ungroup() %>% 
  group_by(PA_Level) %>% mutate(AvgProportionChange_Overall = AvgChange_Count/sum(AvgChange_Count))
RiskScoreChange.Summary %>% filter(PA_Level == "Overall") %>% filter(PreFC_RiskScore ==  PostFC_RiskScore) %>% summarize(AvgPropChange.Overall = 1 - sum(AvgProportionChange_Overall))
#-------------#
### Step 8. ###
#-------------#
FairnessDataSetFiltered <- Application_Specific_Resampling_fcn(data_set = FairnessDataSet)
FairnessDataSetFiltered <- FairnessDataSetFiltered[which(colnames(FairnessDataSet) %in% c("OUTCOME", "Protected_Attribute", "m_pred"))]
GraphicFairnessDataSet.RiskScore <- Assign_Risk_Scores(Thresholds = as.vector(t(AggSubSampThresholds_PostFC)), DataSet = FairnessDataSetFiltered, 
                                                       ProtAtrLevels = levels(FairnessDataSetFiltered$Protected_Attribute), 
                                                       NumThresholds = length(ThreshNames), ThresholdNames = ThreshNames)
GraphicFairnessDataSet <- FairnessDataSetFiltered
GraphicFairnessDataSet$RiskScore <- GraphicFairnessDataSet.RiskScore
GraphThresholds.Baseline <- data.frame(rep("Pre-FairnessCorrected", (length(levels(FairnessDataSetFiltered$Protected_Attribute))*length(ThreshNames))),
                                       as.vector(AggSubSampThresholds_PreFC), 
                                       rep(colnames(AggSubSampThresholds_PreFC), each = length(ThreshNames)), 
                                       rep(rownames(AggSubSampThresholds_PreFC), length(levels(FairnessDataSetFiltered$Protected_Attribute))))
colnames(GraphThresholds.Baseline) <- c("Type", "ThresholdValue", "ProtectedAttributeLevel", "ThresholdLocation")
GraphThresholds.FC <- data.frame(rep("Post-FairnessCorrected", (length(levels(FairnessDataSetFiltered$Protected_Attribute))*length(ThreshNames))),
                                 as.vector(AggSubSampThresholds_PostFC), 
                                 rep(colnames(AggSubSampThresholds_PostFC),each = length(ThreshNames)), 
                                 rep(rownames(AggSubSampThresholds_PostFC), length(levels(FairnessDataSetFiltered$Protected_Attribute))))
colnames(GraphThresholds.FC) <- c("Type", "ThresholdValue", "ProtectedAttributeLevel", "ThresholdLocation")
GraphThresholds <- as_tibble(rbind(GraphThresholds.Baseline, GraphThresholds.FC))
Figure.4 <- ggplot(data = GraphicFairnessDataSet) +
  aes(x = Protected_Attribute, y = m_pred, color = Protected_Attribute, size = 2) +
  facet_grid(. ~ RiskScore) +
  geom_jitter(width = .4, alpha = .2) +
  geom_segment(data = GraphThresholds %>% filter(Type == "Pre-FairnessCorrected"), 
               aes(x = -Inf, xend = Inf, y = ThresholdValue, yend = ThresholdValue),
               color = "maroon", size = 2.5) +
  labs(x = "Protected Attribute Level by Assigned Risk Score",
       y = "Probability of Success") +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", colour = "black", size = 30),
        axis.text.x = element_text(size = 26)) + 
  theme(axis.title.y = element_text(face="bold", colour = "black", size = 30), 
        axis.text.y = element_text(size = 26)) +
  theme(strip.text.x = element_text(size = 30, colour = "black")) +
  theme(legend.position = "none")
Figure.4
#-------------#
### Step 9. ###
#-------------#
FairnessSubSamp <- CompareOutput$"Statistical_Fairness_Measures__For_All_Subsamples"
Fairness.Summary <- FairnessSubSamp %>% group_by(Threshold, Fairness_Metric, ThresholdType) %>% 
  summarize(Avg_Value = mean(Fairness_Value), SD_Value = sd(Fairness_Value)) %>% arrange(Fairness_Metric, Threshold)
SSaggFairnessERB <- Fairness.Summary %>% filter(Fairness_Metric == "ERB") %>% arrange(ThresholdType) %>% ungroup()
Table.3 <- matrix(NA, NumberThresh, 4)
rownames(Table.3) <- ThreshNames
colnames(Table.3) <- c("Average", "SD", "Average", "SD")
for(k in ThreshNames){
  Table.3[which(rownames(Table.3) == k), c(1, 2)] <- unlist(SSaggFairnessERB %>% filter(Threshold == k, ThresholdType == "PreFC") %>% select(Avg_Value, SD_Value))
  Table.3[which(rownames(Table.3) == k), c(3, 4)] <- unlist(SSaggFairnessERB %>% filter(Threshold == k, ThresholdType == "PostFC") %>% select(Avg_Value, SD_Value))
  
}
Table.3 <- round(Table.3, 3)
Table.3
#--------------#
### Step 10. ###
#--------------#
FairnessSubSamp$Threshold <- fct_relevel(FairnessSubSamp$Threshold, "LR_thresh", "AR_thresh", "HR_thresh")
FairnessSubSamp$Threshold <- recode(FairnessSubSamp$Threshold, LR_thresh = "Low-Risk Threshold", AR_thresh = "Average-Risk Threshold", HR_thresh = "High-Risk Threshold")

Figure.5 <- ggplot(data = FairnessSubSamp %>% filter(Fairness_Metric == "ERB") %>% 
         spread(key = ThresholdType, value = Fairness_Value) %>% mutate(ERB_Increase = PostFC-PreFC),
       mapping = aes(x = ERB_Increase)) +
  geom_histogram(fill = "grey", color = "black") +
  facet_wrap( ~ Threshold, nrow = 3) +
  theme(strip.text.x = element_text(size = 20, colour = "black", angle = 0)) +
  geom_vline(xintercept = 0, col = "blue", size = 2) +
  xlab("Increase in Error Rate Balance") +
  ylab("Frequency") + 
  theme(axis.text.x = element_text(color = "grey20", size = 16, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 16, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 20, angle = 90, face = "plain"))
ggarrange(Figure.5, ncol = 1, nrow = 1, common.legend = TRUE, legend = "top")
#--------------#
### Step 11. ###
#--------------#
PredPerfSubSamp <- CompareOutput$"Predictive_Performance_Measures_Overall_and_by_Protected_Attribute_Level__For_All_Subsamples"
PredPerf.Summary <- PredPerfSubSamp %>% group_by(PerformanceMeasure, Threshold, PA_Level, ThreshType) %>%
  summarize(Avg_Value = mean(PerformanceValue), SD_Value = sd(PerformanceValue)) %>% arrange(Threshold, PerformanceMeasure)
PredPerf.Summary.LR <- PredPerf.Summary %>% filter(Threshold == "LR_thresh") %>% select(PerformanceMeasure, PA_Level, ThreshType, Avg_Value) %>%
  spread(key = PerformanceMeasure, value = Avg_Value) %>% select(PA_Level, ThreshType, ACC, FNR, FPR, NPV, PPV) %>% arrange(ThreshType)
PredPerf.Summary.AR <- PredPerf.Summary %>% filter(Threshold == "AR_thresh") %>% select(PerformanceMeasure, PA_Level, ThreshType, Avg_Value) %>%
  spread(key = PerformanceMeasure, value = Avg_Value) %>% select(PA_Level, ThreshType, ACC, FNR, FPR, NPV, PPV) %>% arrange(ThreshType)
PredPerf.Summary.HR <- PredPerf.Summary %>% filter(Threshold == "HR_thresh") %>% select(PerformanceMeasure, PA_Level, ThreshType, Avg_Value) %>%
  spread(key = PerformanceMeasure, value = Avg_Value) %>% select(PA_Level, ThreshType, ACC, FNR, FPR, NPV, PPV) %>% arrange(ThreshType)
Table.4 <- matrix(NA, 5, (NumberThresh * 2))
rownames(Table.4) <- c("ACC", "FNR", "FPR", "NPV", "PPV")
colnames(Table.4) <- c("LR-PreFC", "LR-PostFC", "AR-PreFC", "AR-PostFC", "HR-PreFC", "HR-PostFC")
Table.4[, 1] <- unlist(PredPerf.Summary.LR %>% filter(PA_Level == "Overall", ThreshType == "PreFC") %>% ungroup() %>% select(ACC, FNR, FPR, NPV, PPV))
Table.4[, 2] <- unlist(PredPerf.Summary.LR %>% filter(PA_Level == "Overall", ThreshType == "PostFC") %>% ungroup() %>% select(ACC, FNR, FPR, NPV, PPV))
Table.4[, 3] <- unlist(PredPerf.Summary.AR %>% filter(PA_Level == "Overall", ThreshType == "PreFC") %>% ungroup() %>% select(ACC, FNR, FPR, NPV, PPV))
Table.4[, 4] <- unlist(PredPerf.Summary.AR %>% filter(PA_Level == "Overall", ThreshType == "PostFC") %>% ungroup() %>% select(ACC, FNR, FPR, NPV, PPV))
Table.4[, 5] <- unlist(PredPerf.Summary.HR %>% filter(PA_Level == "Overall", ThreshType == "PreFC") %>% ungroup() %>% select(ACC, FNR, FPR, NPV, PPV))
Table.4[, 6] <- unlist(PredPerf.Summary.HR %>% filter(PA_Level == "Overall", ThreshType == "PostFC") %>% ungroup() %>% select(ACC, FNR, FPR, NPV, PPV))
Table.4 <- round(Table.4, 3)
Table.4
   ###
PredPerf.Summary.LR %>% select(-ACC, -NPV, -PPV) %>% filter(PA_Level != "Overall")
PredPerf.Summary.AR %>% select(-ACC, -NPV, -PPV) %>% filter(PA_Level != "Overall")
PredPerf.Summary.HR %>% select(-ACC, -NPV, -PPV) %>% filter(PA_Level != "Overall")
   ###
DiffInfo <- ParallelResults.TuningAssess %>% select(ThreshType, Threshold, PerformanceMeasure, AvgPerformanceValue, TuningGridID) %>%
  group_by(TuningGridID, Threshold, PerformanceMeasure) %>% summarize(PerfValue_Increase = diff(AvgPerformanceValue)*(-1))
PostOnlyInfo <- ParallelResults.TuningAssess %>% filter(ThreshType == "Post-Fairness Correction")
ParResultsChange.TuningAssess.Best <- left_join(PostOnlyInfo, DiffInfo, by = c("TuningGridID", "Threshold", "PerformanceMeasure"))
ParResultsChange.TuningAssess.Best <- ParResultsChange.TuningAssess.Best %>% select(Threshold, PerformanceMeasure, PenaltyWeight, AvgPropDeltaScores, PerfValue_Increase, TuningGridID)
FigB <- ggplot(data = ParResultsChange.TuningAssess.Best %>% 
                 filter(PerformanceMeasure == "NPV" | PerformanceMeasure == "FNR" | PerformanceMeasure == "FPR" | PerformanceMeasure == "ACC" | PerformanceMeasure == "PPV"),
               mapping = aes(x = PenaltyWeight, y = PerfValue_Increase)) +
  geom_point(mapping = aes(colour = Threshold),  size = 3.0) +
  facet_grid(~ PerformanceMeasure) +
  theme(strip.text.x = element_text(size = 22, colour = "black", angle = 00)) +
  geom_hline(yintercept = 0, linetype = 1, color = "black", size = 1) + 
  ggtitle("") +
  xlab("Penalty Weight (w)") +
  ylab("Average Change in Performance \n Measure (Post-FC - Pre-FC)")  + 
  theme(axis.text.x = element_text(color = "grey20", size = 20, vjust = 0.5, angle = 90, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 26, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 26, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 24, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 20, angle = 0, face = "plain"))
ggarrange(FigB, ncol = 1, nrow = 1, common.legend = TRUE, legend = "top")
#--------------#
### Step 12. ###
#--------------#
Fairness.Summary$Threshold <- fct_relevel(Fairness.Summary$Threshold, "LR_thresh", "AR_thresh", "HR_thresh")
Fairness.Summary$Threshold <- recode(Fairness.Summary$Threshold, LR_thresh = "Low-Risk Threshold", AR_thresh = "Average-Risk Threshold", HR_thresh = "High-Risk Threshold")
Fairness.Summary$ThresholdType <- fct_relevel(Fairness.Summary$ThresholdType, "PreFC", "PostFC")
Fairness.Summary$ThresholdType <- recode(Fairness.Summary$ThresholdType, PreFC = "Pre-Fairness Correction", PostFC = "Post-Fairness Correction")
Figure.6aV2 <- ggplot(data = Fairness.Summary,
                      mapping = aes(x = Fairness_Metric, y = Avg_Value, group = ThresholdType, fill = ThresholdType)) + 
  geom_col(position = "dodge", show.legend = TRUE) + 
  facet_wrap(~ Threshold, nrow = 3, ncol = 1) + 
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 00)) +
  ggtitle("") +
  xlab("Algorithmic Fairness Measure") +
  ylab("Average Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 20, vjust = 0.5, angle = 90, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 26, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 26, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 16, angle = 0, face = "plain"))
ggarrange(Figure.6aV2, ncol = 1, nrow = 1, common.legend = TRUE, legend = "top")

Figure.6bV2 <- ggplot(data = FairCAL.Summary,
                    mapping = aes(x = Fairness_Metric, y = Avg_Value, group = ThresholdType, fill = ThresholdType)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~ Score, nrow = 1) + 
  theme(strip.text.x = element_text(size = 18, colour = "black", angle = 0)) +
  ggtitle("") +
  xlab("Algorithmic Fairness Measure") +
  ylab("Average Value") + 
  theme(axis.text.x = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", size = 26, angle = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 26, angle = 90, face = "plain"),
        legend.title = element_text(color = "grey20", size = 20, angle = 0, face = "plain"),
        legend.text = element_text(color = "grey20", size = 16, angle = 0, face = "plain"))
ggarrange(Figure.6bV2, ncol = 1, nrow = 1, common.legend = TRUE, legend = "top")


