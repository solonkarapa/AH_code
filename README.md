# Validation of prognostic models in alcoholic hepatitis

This repository contains all the code required to re-run the analyses in the MPhil Dissertation "Validation of prognostic models in alcoholic hepatitis". Corresponding data files are not public and therefore not provided.

## Repository structure
The folder **R** contains all R code, separated into several sub-folders, with the following files in them:
- Pre-analysis
     - import_data.R 
     - missing_data.R
- Original models
     - prognostic_scores.R
     - calibration.R
     - discrimination.R
     - clinical_utility.R
     - dca.R
     - nb_diff.R
- Updating models
     - recalibrate.R
     - compare_newmodels.R
     - compare_oldnew.R
- Subgroup comparison
     - subgroup_comparison.R

In order to perform the analysis, the files import_data.R and prognostic_scores.R should always be run first.
     

