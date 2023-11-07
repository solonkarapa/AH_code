
library(readxl)
library(dplyr)

#############################################
################ load data ##################
#############################################
# imputed data path
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/pre-analysis/"

# imputed data - default
load(paste0(path_data, "imputed_data.Rdata"))

# imputed data - sensitivity analysis
load(paste0(path_data, "imputed_data_sens.Rdata"))

#####
# Extract 90-day survival probability from VanDerwerke et al, 2021 
MELD_VanDerwerken <- read_excel("~/IDrive-Sync/Projects/MIMAH/data/MELD_VanDerwerken.xlsx")

#############################################
############### MELD score ##################
#############################################
# data preparation
imp_data2 <- imp_data %>% 
    dplyr::mutate(
        # Survival indicator
        D90_surv = 1 - D90_DTH,
        # MELD
        INR = ifelse(INR < 1, 1, INR),
        Creatinine.mg.dl.MELD = ifelse(Creatinine.mg.dl < 1, 1, Creatinine.mg.dl),
        Bilirubin.mg.dl.MELD = ifelse(Bilirubin.mg.dl < 1, 1, Bilirubin.mg.dl),
        Creatinine.mg.dl.MELD = ifelse(Creatinine.mg.dl > 4, 4, Creatinine.mg.dl),
        MELD.calc = (0.378 * log(Bilirubin.mg.dl.MELD) + 1.120 * log(INR) + 0.957 * log(Creatinine.mg.dl.MELD)),
        MELD.calc = 10 * round(MELD.calc, 1),
        Sodium = ifelse(Sodium < 125, 125, Sodium),
        Sodium = ifelse(Sodium > 137, 137, Sodium),
        MELD.calc = ifelse(MELD.calc > 11, 
                           (MELD.calc + 1.32 * (137 - Sodium) - 0.033 * MELD.calc * (137 - Sodium)),
                           MELD.calc),
        MELD.calc = round(MELD.calc, 0),
        MELD.calc = ifelse(MELD.calc < 6, 6, MELD.calc),
        MELD.calc = ifelse(MELD.calc > 40, 40, MELD.calc),
        MELD.surv1 = 0.707^(exp((MELD.calc/10) - 1.127)),
        MELD.surv2= 0.98465^(exp(0.1635*(MELD.calc - 10))), 
        MELD_Van = MELD_VanDerwerken[MELD.calc - 5, ]$SURV,
        Albumin.MELD = ifelse(Albumin < 1.5, 1.5, Albumin),
        Albumin.MELD = ifelse(Albumin > 3.5, 3.5, Albumin),
        MELD3 = 1.33 * Gender + 
            4.56 * log(Bilirubin.mg.dl.MELD) +
            0.82 * (137 - Sodium) - 
            (0.24 * (137 - Sodium) * log(Bilirubin.mg.dl.MELD)) +
            9.09 * log(INR) + 
            11.14 * log(Creatinine.mg.dl.MELD) + 
            1.85 * (3.5 - Albumin.MELD) -
            (1.83 * (3.5 - Albumin.MELD) * log(Creatinine.mg.dl.MELD)) + 6,
        MELD3 = round(MELD3, 0),
        MELD3.surv = 0.946^(exp(0.17698*MELD3 - 3.56)),
        # CLIF
        liver.score = ifelse(Bilirubin.mg.dl < 6, 1, 2),
        liver.score = ifelse(Bilirubin.mg.dl < 12, liver.score, 3),
        kidney.score = ifelse(Creatinine.mg.dl < 2, 1, 2),
        kidney.score = ifelse(Creatinine.mg.dl < 3.5, kidney.score, 3), 
        brain.score = ifelse(HE == 0, 1, 2),
        brain.score = ifelse(HE > 2, 3, brain.score),
        coag.score = ifelse(INR < 2, 1, 2),
        coag.score = ifelse(INR < 2.5, coag.score, 3),
        circ.score = ifelse(MAP >= 70, 1, 2),
        CLIF.OF = liver.score + kidney.score + brain.score + coag.score + circ.score + 1, 
        CLIF.C = 10 * (0.33 * CLIF.OF + 0.04 * Age + 0.63 * log(WBC) - 2),
        CLIF.surv = exp(-0.0079 * exp(0.0869 * CLIF.C)),
        # Lille
        #ren.insuf = ifelse(Creatinine.mg.dl < 1.3, 0, 1), 
        #Bilirubin.Merged = Bilirubin.mg.dl * 17,
        #delta.bili = Bilirubin.Merged - Bilirubin.day.7,
        #LILLE = 3.19 - 0.101 * Age + 0.147 * Albumin + 0.0165 * delta.bili - 0.206 * ren.insuf - 0.0065 * Bilirubin.Merged - 0.0096 * protime,
        #Lille.surv = 1 - (exp(-LILLE)/(1 + exp(-LILLE)))
        
    )

colnames(imp_data2)

imp_data2_Biliday7 <- imp_data_day7 %>% 
    dplyr::mutate(
        # Survival indicator
        D90_surv = 1 - D90_DTH,
        # Lille
        ren.insuf = ifelse(Creatinine.mg.dl < 1.3, 0, 1), 
        Bilirubin.Merged = Bilirubin.mg.dl * 17,
        delta.bili = Bilirubin.Merged - Bilirubin.day.7,
        LILLE = 3.19 - 0.101 * Age + 0.147 * Albumin + 0.0165 * delta.bili - 0.206 * ren.insuf - 0.0065 * Bilirubin.Merged - 0.0096 * protime,
        Lille.surv = 1 - (exp(-LILLE)/(1 + exp(-LILLE)))
        )

#setwd("~/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models")
#save(imp_data2, imp_data2_Biliday7, file = "imputed_orig_scores.Rdata")
    
# data preparation sensitivity analysis
imp_data3 <- imp_sens_df %>% 
    dplyr::mutate(
        # Survival indicator
        D90_surv = 1 - D90_DTH,
        # MELD
        INR = ifelse(INR < 1, 1, INR),
        Creatinine.mg.dl.MELD = ifelse(Creatinine.mg.dl < 1, 1, Creatinine.mg.dl),
        Bilirubin.mg.dl.MELD = ifelse(Bilirubin.mg.dl < 1, 1, Bilirubin.mg.dl),
        Creatinine.mg.dl.MELD = ifelse(Creatinine.mg.dl > 4, 4, Creatinine.mg.dl),
        MELD.calc = (0.378 * log(Bilirubin.mg.dl.MELD) + 1.120 * log(INR) + 0.957 * log(Creatinine.mg.dl.MELD)),
        MELD.calc = 10 * round(MELD.calc, 1),
        Sodium = ifelse(Sodium < 125, 125, Sodium),
        Sodium = ifelse(Sodium > 137, 137, Sodium),
        MELD.calc = ifelse(MELD.calc > 11, 
                           (MELD.calc + 1.32 * (137 - Sodium) - 0.033 * MELD.calc * (137 - Sodium)),
                           MELD.calc),
        MELD.calc = round(MELD.calc, 0),
        MELD.calc = ifelse(MELD.calc < 6, 6, MELD.calc),
        MELD.calc = ifelse(MELD.calc > 40, 40, MELD.calc),
        MELD.surv1 = 0.707^(exp((MELD.calc/10) - 1.127)),
        MELD.surv2= 0.98465^(exp(0.1635*(MELD.calc - 10))), 
        MELD_Van = MELD_VanDerwerken[MELD.calc - 5, ]$SURV,
        Albumin.MELD = ifelse(Albumin < 1.5, 1.5, Albumin),
        Albumin.MELD = ifelse(Albumin > 3.5, 3.5, Albumin),
        MELD3 = 1.33 * Gender + 
            4.56 * log(Bilirubin.mg.dl.MELD) +
            0.82 * (137 - Sodium) - 
            (0.24 * (137 - Sodium) * log(Bilirubin.mg.dl.MELD)) +
            9.09 * log(INR) + 
            11.14 * log(Creatinine.mg.dl.MELD) + 
            1.85 * (3.5 - Albumin.MELD) -
            (1.83 * (3.5 - Albumin.MELD) * log(Creatinine.mg.dl.MELD)) + 6,
        MELD3 = round(MELD3, 0),
        MELD3.surv = 0.946^(exp(0.17698*MELD3 - 3.56)),
        # CLIF
        liver.score = ifelse(Bilirubin.mg.dl < 6, 1, 2),
        liver.score = ifelse(Bilirubin.mg.dl < 12, liver.score, 3),
        kidney.score = ifelse(Creatinine.mg.dl < 2, 1, 2),
        kidney.score = ifelse(Creatinine.mg.dl < 3.5, kidney.score, 3), 
        brain.score = ifelse(HE == 0, 1, 2),
        brain.score = ifelse(HE > 2, 3, brain.score),
        coag.score = ifelse(INR < 2, 1, 2),
        coag.score = ifelse(INR < 2.5, coag.score, 3),
        circ.score = ifelse(MAP >= 70, 1, 2),
        CLIF.OF = liver.score + kidney.score + brain.score + coag.score + circ.score + 1, 
        CLIF.C = 10 * (0.33 * CLIF.OF + 0.04 * Age + 0.63 * log(WBC) - 2),
        CLIF.surv = exp(-0.0079 * exp(0.0869 * CLIF.C)),
        # Lille
        ren.insuf = ifelse(Creatinine.mg.dl < 1.3, 0, 1), 
        Bilirubin.Merged = Bilirubin.mg.dl * 17,
        delta.bili = Bilirubin.Merged - Bilirubin.day.7,
        LILLE = 3.19 - 0.101 * Age + 0.147 * Albumin + 0.0165 * delta.bili - 0.206 * ren.insuf - 0.0065 * Bilirubin.Merged - 0.0096 * protime,
        Lille.surv = 1 - (exp(-LILLE)/(1 + exp(-LILLE)))
        
    )

colnames(imp_data3)
dim(imp_data3)

#setwd("~/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models")
#save(imp_data3, file = "imputed_sens_orig_scores.Rdata")

