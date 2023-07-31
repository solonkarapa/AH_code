######
# This script calculates the prognostic scores (MELD, Lille, CLIF-C ACLF) and corresponding 90-day survival probabilities.
library(tidyverse)
library(dplyr)
library(ggplot2)

#############################################
############### MELD score ##################
#############################################
#####
# First constrain INR, Creatinine, and Bilirubin to be within certain bandwidth
stph.meld$INR <- ifelse(stph.meld$INR < 1, 1, stph.meld$INR)
stph.meld$Creatinine.mg.dl.MELD <- ifelse(stph.meld$Creatinine.mg.dl < 1, 1, stph.meld$Creatinine.mg.dl)
stph.meld$Bilirubin.mg.dl.MELD <- ifelse(stph.meld$Bilirubin.mg.dl < 1, 1, stph.meld$Bilirubin.mg.dl)
stph.meld$Creatinine.mg.dl.MELD <- ifelse(stph.meld$Creatinine.mg.dl > 4, 4, stph.meld$Creatinine.mg.dl)

# Calculate basic MELD score
stph.meld$MELD.calc <- 0.378*log(stph.meld$Bilirubin.mg.dl.MELD) + 
    1.120*log(stph.meld$INR) + 
    0.957*log(stph.meld$Creatinine.mg.dl.MELD) 

# Round score to the nearest tenth
stph.meld$MELD.calc <- round(stph.meld$MELD.calc, 1)

# Multiply score by ten
stph.meld$MELD.calc <- 10*stph.meld$MELD.calc

# Constrain serum sodium to be within certain bandwidth
stph.meld$Sodium <- ifelse(stph.meld$Sodium < 125, 125, stph.meld$Sodium)
stph.meld$Sodium <- ifelse(stph.meld$Sodium > 137, 137, stph.meld$Sodium)

# Recalculate to get the MELD-Na score
for(i in 1:nrow(stph.meld)){
    if(stph.meld$MELD.calc[i] > 11){
        temp <- stph.meld$MELD.calc[i] + 1.32*(137 - stph.meld$Sodium[i]) - 
            0.033*stph.meld$MELD.calc[i]*(137 - stph.meld$Sodium[i])
        stph.meld$MELD.calc[i] <- temp
    } else {
        stph.meld$MELD.calc[i] <- stph.meld$MELD.calc[i]
    }
}

# Round to nearest integer
stph.meld$MELD.calc <- round(stph.meld$MELD.calc, 0)

# Constrain MELD to be between 6 and 40
stph.meld$MELD.calc <- ifelse(stph.meld$MELD.calc < 6, 6, stph.meld$MELD.calc)
stph.meld$MELD.calc <- ifelse(stph.meld$MELD.calc > 40, 40, stph.meld$MELD.calc)

# Calculate 90-day survival probability based on MELD-Na score (2 different functions)
stph.meld$MELD.surv_6M <- 0.621^(exp((stph.meld$MELD.calc/10) - 1.127)) 
#stph.meld$MELD.surv2 <- 0.98465^(exp(0.1635*(stph.meld$MELD.calc - 10)))

#############################################
######### CLIF-C ACLF Score #################
#############################################
#####
# Start by calculating organ-failure sub-scores
stph.clif$liver.score <- ifelse(stph.clif$Bilirubin.mg.dl < 6, 1, 2)
stph.clif$liver.score <- ifelse(stph.clif$Bilirubin.mg.dl < 12, stph.clif$liver.score, 3)

stph.clif$kidney.score <- ifelse(stph.clif$Creatinine.mg.dl < 2, 1, 2)
stph.clif$kidney.score <- ifelse(stph.clif$Creatinine.mg.dl < 3.5, stph.clif$kidney.score, 3) 

stph.clif$brain.score <- ifelse(stph.clif$HE == 0, 1, 2)
stph.clif$brain.score <- ifelse(stph.clif$HE > 2, 3, stph.clif$brain.score)

stph.clif$coag.score <- ifelse(stph.clif$INR < 2, 1, 2)
stph.clif$coag.score <- ifelse(stph.clif$INR < 2.5, stph.clif$coag.score, 3)

stph.clif$circ.score <- ifelse(stph.clif$MAP >= 70, 1, 2)

# Calculate CLIF-OF score, which is the sum of sub-scores
stph.clif$CLIF.OF <- stph.clif$liver.score + stph.clif$kidney.score + 
    stph.clif$brain.score + stph.clif$coag.score + stph.clif$circ.score + 1 # respiratory score equal to 1 for all patients

# Calculate the CLIF-C ACLF score and corresponding survival
stph.clif$CLIF.C <- 10*(0.33*stph.clif$CLIF.OF + 0.04*stph.clif$Age + 0.63*log(stph.clif$WBC) - 2)
stph.clif$CLIF.surv_6M <- exp(-0.0115 * exp(0.0824*stph.clif$CLIF.C))

#############################################
################### Lille score #############
#############################################
#####
# First create renal insufficiency dummy
stph.lille$ren.insuf <- ifelse(stph.lille$Creatinine.mg.dl < 1.3, 0, 1)

# Calculate the change in bilirubin
stph.lille$delta.bili <- stph.lille$Bilirubin.Merged - stph.lille$Bilirubin.day.7

# Calculate the Lille score and corresponding survival probability
stph.lille$LILLE <- 3.19 - 0.101*stph.lille$Age + 0.147*stph.lille$Albumin + 0.0165*stph.lille$delta.bili - 
    0.206*stph.lille$ren.insuf - 0.0065*stph.lille$Bilirubin.Merged - 0.0096*stph.lille$protime
stph.lille$Lille.surv_6M <- 1 - (exp(-stph.lille$LILLE)/(1 + exp(-stph.lille$LILLE)))


#path <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models/"
#setwd(path)
#save(stph.meld, stph.lille, stph.clif, file = "original_models_6M.Rdata")



