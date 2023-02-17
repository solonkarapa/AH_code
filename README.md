# Validating and updating prognostic models for alcoholic hepatitis

This repository contains all the code required to re-run the analyses in the MPhil Dissertation "Validating and updating prognostic models for alcoholic hepatitis". Corresponding data files are not public and therefore not provided.

## Repository structure
The repository contains several folders, with the following files in them:
- Pre-analysis
     - import_data.R. This file imports the STOPAH data and renames some of the clinically relevant variables to have names that are easier to work with. It then creates complete-case dataframes for the different prognostic scores and prints a table of patient characteristics.
     - missing_data.R. This file creates sub-sets of complete and incomplete cases in the data and tabulates and compares patient characteristics across them. This file also analyses the additional discharge data to compare time to hospital discharge between sub-samples.
- Original models
     - prognostic_scores.R. This file calculates the relevant prognostic scores and corresponding survival probabilities.
     - Calibration.R. This file calculates all relevant measures of model calibration.
     - Discrimination.R. This file calculates all relevant measures of discrimination. 
     - clinical_utility.R. This file calculates all relevant measures of clinical utility. 
     - dca.R. Defines dca function needed in clinical_utility.R. This function is taken from decisioncurveanalysis.org.
     - nb_diff.R. Defines functions to formally compare NB across models, based on code introduced by Zhang et al. 
- Updating models
     - recalibrate.R. This file splits the data into training and test sets and runs regressions to update models. It also computes some basic performance measures for updated models.
     - compare_newmodels.R. This file performs all additional model comparisons between the updated models.
     - compare_oldnew.R. This file formally compares certain statistics between original and updated models.
     - val_prob_confidence.R is a function that calculates the calibration measures, including SEs and confidence intervals for the calibration slope and intercepts. This function is an adaptation from two existing functions: val.prob from Frank Harrell's rms package, and val.prob.ci.2 from the CalibrationCurves package (Van Calster et al.)
- Subgroup comparison
     - subgroup_comparison.R. File that is used to analyse model performance in sub-groups of patients. 

In order to perform the analysis, the files import_data.R and prognostic_scores.R should always be run first. Generally, files may only work after another file is run first. As the data is not published, however, I would recommend using this code as a guideline for comparable analyses, rather than as a perfect example of how model validations and comparisons should always be done. 
<<<<<<< HEAD

---- 
=======
>>>>>>> origin
    
