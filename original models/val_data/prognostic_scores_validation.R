#############################################
############### MELD score ##################
#############################################
#####
# First constrain INR, Creatinine, and Bilirubin to be within certain bandwidth
Global_AlcHep.meld$INR.MELD <- ifelse(Global_AlcHep.meld$INR < 1, 1, Global_AlcHep.meld$INR)
Global_AlcHep.meld$Creatinine.MELD <- ifelse(Global_AlcHep.meld$Creatinine < 1, 1, 
                                                   ifelse(Global_AlcHep.meld$Creatinine > 4, 4, 
                                                          Global_AlcHep.meld$Creatinine))
#Global_AlcHep.meld$Bilirubin.mg.dl.MELD <- Global_AlcHep.meld$Bilirubin / 10
Global_AlcHep.meld$Bilirubin.MELD <- ifelse(Global_AlcHep.meld$Bilirubin < 1, 1, 
                                                  Global_AlcHep.meld$Bilirubin)

# Calculate basic MELD score
Global_AlcHep.meld$MELD.calc <- 0.378 * log(Global_AlcHep.meld$Bilirubin.MELD) + 
    1.120 * log(Global_AlcHep.meld$INR.MELD) + 
    0.957 * log(Global_AlcHep.meld$Creatinine.MELD)

# Round score to the nearest tenth
Global_AlcHep.meld$MELD.calc <- round(Global_AlcHep.meld$MELD.calc, 1)

# Multiply score by ten
Global_AlcHep.meld$MELD.calc <- 10 * Global_AlcHep.meld$MELD.calc

# Constrain serum sodium to be within certain bandwidth
Global_AlcHep.meld$Sodium <- ifelse(Global_AlcHep.meld$Sodium < 125, 125, Global_AlcHep.meld$Sodium)
Global_AlcHep.meld$Sodium <- ifelse(Global_AlcHep.meld$Sodium > 137, 137, Global_AlcHep.meld$Sodium)

# Recalculate to get the MELD-Na score
for(i in 1:nrow(Global_AlcHep.meld)){
    if(Global_AlcHep.meld$MELD.calc[i] > 11){
        temp <- Global_AlcHep.meld$MELD.calc[i] + 1.32 * (137 - Global_AlcHep.meld$Sodium[i]) - 
            0.033 * Global_AlcHep.meld$MELD.calc[i]*(137 - Global_AlcHep.meld$Sodium[i])
        Global_AlcHep.meld$MELD.calc[i] <- temp
    } else {
        Global_AlcHep.meld$MELD.calc[i] <- Global_AlcHep.meld$MELD.calc[i]
    }
}

# Round to nearest integer
Global_AlcHep.meld$MELD.calc <- round(Global_AlcHep.meld$MELD.calc, 0)

# Constrain MELD to be between 6 and 40
Global_AlcHep.meld$MELD.calc <- ifelse(Global_AlcHep.meld$MELD.calc < 6, 6, Global_AlcHep.meld$MELD.calc)
Global_AlcHep.meld$MELD.calc <- ifelse(Global_AlcHep.meld$MELD.calc > 40, 40, Global_AlcHep.meld$MELD.calc)

# Calculate 90-day survival probability based on MELD-Na score (2 different functions)
Global_AlcHep.meld$MELD.surv <- 0.707^(exp((Global_AlcHep.meld$MELD.calc/10) - 1.127)) 
Global_AlcHep.meld$MELD.surv2 <- 0.98465^(exp(0.1635*(Global_AlcHep.meld$MELD.calc - 10)))  

# Extract 90-day survival probability from VanDerwerke et al, 2021 
library(readxl)
MELD_VanDerwerken <- read_excel("~/IDrive-Sync/Projects/MIMAH/data/MELD_VanDerwerken.xlsx")

for(i in 1:nrow(Global_AlcHep.meld)){
    Global_AlcHep.meld$MELD_Van[i] <- MELD_VanDerwerken[Global_AlcHep.meld$MELD.calc[i] - 5, ]$SURV
}

####
# Calculate the MELD 3.0 score (sex-adjusted version of the MELD)
# Constrain Albumin 
Global_AlcHep.meld$Albumin.MELD <- ifelse(Global_AlcHep.meld$Albumin < 1.5, 1.5, Global_AlcHep.meld$Albumin)
Global_AlcHep.meld$Albumin.MELD <- ifelse(Global_AlcHep.meld$Albumin> 3.5, 3.5, Global_AlcHep.meld$Albumin)

# Calculate MELD 3.0, round to nearest integer, and calculate corresponding survival
#Global_AlcHep.meld$MELD3 <- 1.33 * Global_AlcHep.meld$Gender + 
#    4.56 * log(Global_AlcHep.meld$Bilirubin.mg.dl.MELD) +
#    0.82 * (137 - Global_AlcHep.meld$Sodium) - 
#    (0.24 * (137 - Global_AlcHep.meld$Sodium) * log(Global_AlcHep.meld$Bilirubin.mg.dl.MELD)) +
#    9.09 * log(Global_AlcHep.meld$INR) + 
#    11.14 * log(Global_AlcHep.meld$Creatinine.mg.dl.MELD) + 
#    1.85 * (3.5 - Global_AlcHep.meld$Albumin.MELD) -
#    (1.83 * (3.5 - Global_AlcHep.meld$Albumin.MELD) * log(Global_AlcHep.meld$Creatinine.mg.dl.MELD)) 
#+ 6

#stph.meld$MELD3 <- round(stph.meld$MELD3, 0)
#stph.meld$MELD3.surv <- 0.946^(exp(0.17698*stph.meld$MELD3 - 3.56))

#############################################
################### Lille score #############
#############################################
#####
# First create renal insufficiency dummy
Global_AlcHep.lille$ren.insuf <- ifelse(Global_AlcHep.lille$Creatinine <= 1.3, 0, 1)

# convert from mg/L to micromole per litre (μmol/L)
Global_AlcHep.lille$Bilirubin.Lille <- Global_AlcHep.lille$Bilirubin * 5.5506216696
Global_AlcHep.lille$Bilirubin.day.7.Lille <- Global_AlcHep.lille$Bilirubin.day.7 * 5.5506216696

# convert from g/dL to g/L
Global_AlcHep.lille$Albumin.Lille <- Global_AlcHep.lille$Albumin * 10
    
# Calculate the change in bilirubin
Global_AlcHep.lille$delta.bili <- Global_AlcHep.lille$Bilirubin.Lille - Global_AlcHep.lille$Bilirubin.day.7.Lille

# Calculate the Lille score and corresponding survival probability
Global_AlcHep.lille$LILLE <- 3.19 - 
    0.101 * Global_AlcHep.lille$Age + 
    0.147 * Global_AlcHep.lille$Albumin.Lille + 
    0.0165 * Global_AlcHep.lille$delta.bili - 
    0.206 * Global_AlcHep.lille$ren.insuf - 
    0.0065 * Global_AlcHep.lille$Bilirubin.Lille - 
    0.0096 * Global_AlcHep.lille$protime

Global_AlcHep.lille$Lille.risk <- (exp(-Global_AlcHep.lille$LILLE)/(1 + exp(-Global_AlcHep.lille$LILLE)))
Global_AlcHep.lille$Lille.surv <- 1 - Global_AlcHep.lille$Lille.risk

#path <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models/val_data"
#setwd(path)
#save(stph.meld, stph.lille, stph.clif, file = "original_models.Rdata")

# Tabulate the calculated prognostic scores and survival probabilities
library(table1)
tb_MELD <- table1(~ MELD.calc + MELD.surv + MELD.surv2 | factor(D90_surv), data = Global_AlcHep.meld)
tb_Lille <- table1(~LILLE + Lille.surv | factor(D90_surv), data = Global_AlcHep.lille)

library(xtable)
xtable(as_tibble(tb_MELD))
xtable(as_tibble(tb_Lille))


