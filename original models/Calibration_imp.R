# This script performs all calculations of model calibration
library(ggplot2)
library(dplyr)
library(survminer) # for plotting theme

# funs
path_funs <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/funs"
source(paste0(path_funs, "/calibration_fun.R"))

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
    temp <- calibration(imp_index[[i]]$MELD.surv1, y = imp_index[[i]]$D90_surv)
    temp$Score <- "MELD_1"
    temp$.imp <- i
    cal_MELD.surv1 <- rbind(cal_MELD.surv1, temp)
}
rm(temp)

# MELD 2
cal_MELD.surv2 <- tibble()
for(i in 1:imp_ind){
    temp <- calibration(imp_index[[i]]$MELD.surv2, y = imp_index[[i]]$D90_surv)
    temp$Score <- "MELD_2"
    temp$.imp <- i
    cal_MELD.surv2 <- rbind(cal_MELD.surv2, temp)
}

rm(temp)

# MELD VanDerwerken
cal_MELD.VanDerwerken <- tibble()
for(i in 1:imp_ind){
    temp <- calibration(imp_index[[i]]$MELD_Van, y = imp_index[[i]]$D90_surv)
    temp$Score <- "MELD VanDerwerken"
    temp$.imp <- i
    cal_MELD.VanDerwerken <- rbind(cal_MELD.VanDerwerken, temp)
}

rm(temp)

# Lille
cal_Lille <- tibble()
for(i in 1:imp_ind_Bili7){
    temp <- calibration(imp_index_Bili7[[i]]$Lille.surv, y = imp_index_Bili7[[i]]$D90_surv)
    temp$Score <- "Lille"
    temp$.imp <- i
    cal_Lille <- rbind(cal_Lille, temp)
}

rm(temp)

# "CLIF-C ACLF" 
cal_CLIF <- tibble()
for(i in 1:imp_ind){
    temp <- calibration(imp_index[[i]]$CLIF.surv, y = imp_index[[i]]$D90_surv)
    temp$Score <- "CLIF-C ACLF"
    temp$.imp <- i
    cal_CLIF <- rbind(cal_CLIF, temp)
}

# combine dfs
df_cal <- rbind(cal_MELD.surv1, cal_MELD.surv2, cal_MELD.VanDerwerken, cal_Lille, cal_CLIF)

#############################################   
###################### Plots  ###############
############################################# 
# plot without ribbon 
df_cal %>% 
    ggplot(., aes(x = pred, y = obs, col = Score)) +
    geom_line(aes(group = .imp), lwd = 0.7)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #geom_ribbon(aes(ymin = lower, ymax = upper, linetype = NA), 
    #            alpha = 0.3, show.legend = F) + 
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    facet_grid(. ~ Score) +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed proportion") + 
    xlab("Predicted probability") + 
    theme_classic() +
    theme(legend.position = "none")



