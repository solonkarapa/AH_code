# This script performs all calculations for assessing discrimination

library(pROC)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(purrr)

# data
#path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models/"
#load(paste0(path_data, "original_models.Rdata"))

# imputed data
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models/"
load(paste0(path_data, "imputed_orig_scores.Rdata"))

#############################################   
############### Calculate calibration  ######
#############################################   
imp_index <- group_split(imp_data2, .imp) 

imp_ind <- max(imp_data2$.imp)

imp_index_Bili7 <- group_split(imp_data2_Biliday7, .imp) 

imp_ind_Bili7 <- max(imp_data2_Biliday7$.imp)

# MELD_1
cal_MELD.surv1 <- tibble()
for(i in 1:imp_ind){
    # MELD 1
    temp <- roc(imp_index[[i]]$D90_DTH, imp_index[[i]]$MELD.surv1)
    temp2 <- data.frame(sensitivities = temp$sensitivities, 
                            specificities = temp$specificities, 
                            thresholds = temp$thresholds)
    temp2$Score <- "MELD"
    temp2$.imp <- i
    cal_MELD.surv1 <- rbind(cal_MELD.surv1, temp2)
}
rm(temp2)

# MELD VanDerwerken
cal_MELD.VanDerwerken <- tibble()
for(i in 1:imp_ind){
    temp <- roc(imp_index[[i]]$D90_DTH, imp_index[[i]]$MELD_Van)
    temp2 <- data.frame(sensitivities = temp$sensitivities, 
                        specificities = temp$specificities, 
                        thresholds = temp$thresholds)
    temp2$Score <- "MELD VanDerwerken"
    temp2$.imp <- i
    cal_MELD.VanDerwerken <- rbind(cal_MELD.VanDerwerken, temp2)
}
rm(temp2)

# Lille
cal_Lille <- tibble()
for(i in 1:imp_ind_Bili7){
    temp <- roc(imp_index_Bili7[[i]]$D90_DTH, imp_index_Bili7[[i]]$Lille.surv)
    temp2 <- data.frame(sensitivities = temp$sensitivities, 
                        specificities = temp$specificities, 
                        thresholds = temp$thresholds)
    temp2$Score <- "Lille"
    temp2$.imp <- i
    cal_Lille <- rbind(cal_Lille, temp2)
}
rm(temp2)

# "CLIF-C ACLF" 
cal_CLIF <- tibble()
for(i in 1:imp_ind){
    temp <- roc(imp_index[[i]]$D90_DTH, imp_index[[i]]$CLIF.surv)
    temp2 <- data.frame(sensitivities = temp$sensitivities, 
                        specificities = temp$specificities, 
                        thresholds = temp$thresholds)
    temp2$Score <- "CLIF-C ACLF"
    temp2$.imp <- i
    cal_CLIF <- rbind(cal_CLIF, temp2)
}
rm(temp2)

# combine dfs
df_cal <- rbind(cal_MELD.surv1, cal_MELD.VanDerwerken, cal_Lille, cal_CLIF)

#############################################   
###################### Plots  ###############
############################################# 
# plot without ribbon 
df_cal %>%
    ggplot(., aes(x = 1 - specificities, y = sensitivities, col = Score)) +
    geom_line(aes(group = .imp), lwd = 0.7)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #geom_ribbon(aes(ymin = lower, ymax = upper, linetype = NA), 
    #            alpha = 0.3, show.legend = F) + 
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    facet_grid(. ~ Score) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Sensitivity") + 
    xlab("Specificity") + 
    theme_classic() +
    theme(legend.position = "none")

