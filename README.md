# ORRAI_AlgFairnessCorrectionDemo
Objective - Find Algorithmically Fairer Classification Thresholds
------------------------------------------------------------------
This project is the R code repository for the methodology introduced and exemplified in (cite arXiv article here).   Though motivated by and developed for predictive risk tools in Child Welfare, the developed procedure is applicable to any context utilizing risk scores via thresholding of a machine learning binary classification algorithm.

The objective of the procedure is to produce algorithmically fairer risk scores through the identification of protected-attribute-level-specific threshold values.  Such a procedure, though similar to other post-processing group-specific thresholding methods, is novel in its use of a penalized optimizer and contextually requisite subsampling to handle interdependence between and within observational units.   

Through a simple function argument, the procedure can be applied to any of 9 group level definitions of algorithmic fairness, including calibration, conditional use accuracy equality, equal opportunity, error rate balance, overall accuracy equality, predictive equality, predictive parity, statistical parity, and treatment equality, as defined in Verma and Rubin's paper "Fairness Definitions Explained" (https://fairware.cs.umass.edu/papers/Verma.pdf).  Sample size allowing, the procedure can also be applied to a risk scoring system with an arbitrary number of thresholds and an arbitrary number of protected attribute levels.

How to Use
-----------
First open the R script named “Fairness_Functions_Pacakge”.  This script contains all of the necessary functions to perform the procedure.  Apart from the comments provided above each of the functions within this script, it is not needed after running; in other words, as its name would suggest, this script is intended to function like an R package.

Next, open the only other R script, “Driver_AdultDataSet_Demo”.  Because the data set utilized in the paper presenting the methodology cannot be made publicly available, we have used the Adult Data Set from the UCI Machine Learning Repository to demonstrate the application of the R code for the procedure.  Both the training and test sets for these data are provided (AdultData.Test.txt and AdultData.Train.txt); the directory where these data sets are ultimately stored will need to be set in the script on line 102. 

The “Driver_AdultDataSet_Demo” script can then be run to obtain the desired (algorithmic) fairness-corrected threshold values for the data set.  Note that many graphics are incorporated to help visualize the results of and decisions made from running the procedure; for this reason, we recommend running this code block-by-block, rather than all at once.  Furthermore, the penalized optimization procedure that is executed starting on line 490 (under Step 2 of Objective c), can take several hours to run, depending on the number of cores available and the number of re-samples specified in line 475, so it is best not to run the entire script at once. 

Generalizing Beyond Adult Data Set
----------------------------------
To apply the code to a data set other than the Adult Data Set, use the “Driver_AdultDataSet_Demo” script as a template, replacing application-specific details accordingly.  Comments provided throughout this script, along with function preamble comments provided in the “Fairness_Functions_Package” R script, should help guide such replacement.
