
# This script performs all calculations of model calibration
library(ggplot2)
library(dplyr)
library(survminer) # for plotting theme

# funs
path_funs <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/funs"
source(paste0(path_funs, "/calibration_fun.R"))

# imputed data
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models/"
load(paste0(path_data, "imputed_sens_orig_scores.Rdata"))

#############################################   
############### Calculate calibration  ######
#############################################   
imp_index <- group_split(imp_data3, .imp) 

imp_ind <- max(imp_data3$.imp)
delta <- unique(imp_data3$delta)

# # MELD_1
# cal_MELD.surv1 <- tibble()
# for(i in 1:imp_ind){
#     # MELD 1
#     temp <- calibration(imp_index[[i]]$MELD.surv1, y = imp_index[[i]]$D90_surv)
#     temp$Score <- "MELD_1"
#     temp$delta <- rep(unique(imp_index[[i]]$delta), each = 1068)
#     temp$.imp <- i
#     cal_MELD.surv1 <- rbind(cal_MELD.surv1, temp)
# }
# rm(temp)
# 
# # MELD 2
# cal_MELD.surv2 <- tibble()
# for(i in 1:imp_ind){
#     temp <- calibration(imp_index[[i]]$MELD.surv2, y = imp_index[[i]]$D90_surv)
#     temp$Score <- "MELD_2"
#     temp$delta <- rep(unique(imp_index[[i]]$delta), each = 1068)
#     temp$.imp <- i
#     cal_MELD.surv2 <- rbind(cal_MELD.surv2, temp)
# }
# 
# rm(temp)
# 
# # MELD VanDerwerken
# cal_MELD.VanDerwerken <- tibble()
# for(i in 1:imp_ind){
#     temp <- calibration(imp_index[[i]]$MELD_Van, y = imp_index[[i]]$D90_surv)
#     temp$Score <- "MELD VanDerwerken"
#     temp$delta <- rep(unique(imp_index[[i]]$delta), each = 1068)
#     temp$.imp <- i
#     cal_MELD.VanDerwerken <- rbind(cal_MELD.VanDerwerken, temp)
# }
# 
# rm(temp)

# Lille
cal_Lille <- tibble()
for(i in 1:imp_ind){
    for(g in 1:length(delta)){
        d <- unique(imp_index[[i]]$delta)
        df <- imp_index[[i]] %>% filter(delta == d[g])
        
        temp <- calibration(df$Lille.surv, y = df$D90_surv)
        temp$Score <- "Lille"
        temp$delta <- unique(df$delta)
        temp$.imp <- i
        cal_Lille <- rbind(cal_Lille, temp)
    }
}

# rm(temp)
# 
# # "CLIF-C ACLF" 
# cal_CLIF <- tibble()
# for(i in 1:imp_ind){
#     temp <- calibration(imp_index[[i]]$CLIF.surv, y = imp_index[[i]]$D90_surv)
#     temp$Score <- "CLIF-C ACLF"
#     temp$delta <- rep(unique(imp_index[[i]]$delta), each = 1068)
#     temp$.imp <- i
#     cal_CLIF <- rbind(cal_CLIF, temp)
# }
# 
# # combine dfs
# df_cal <- rbind(cal_MELD.surv1, cal_MELD.surv2, cal_MELD.VanDerwerken, cal_Lille, cal_CLIF)

#############################################   
###################### Plots  ###############
############################################# 
# distribution of predictions
p5 <- cal_Lille %>% ggplot(.) +
    geom_boxplot(aes(x = pred, fill = as.factor(as.numeric(delta)))) +
    scale_fill_brewer(palette = "Dark2") +
    labs(fill = expression(delta)) +
    xlab("Predicted survival distribution - Lille") + 
    theme_classic() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

p6 <- cal_Lille %>% filter(Score == "Lille") %>% 
    mutate(grp = paste0(delta, "_", .imp),
           delta = as.numeric(delta)) %>%
    ggplot(., aes(x = pred, y = obs, group = grp)) +
    geom_line(aes(group = grp))  + 
    geom_line(aes(col = as.factor(delta)), alpha = 0.5)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #geom_ribbon(aes(ymin = lower, ymax = upper, linetype = NA), 
    #            alpha = 0.3, show.legend = F) + 
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    #facet_grid(. ~ delta) +
    xlim(0, 1) + 
    ylim(0, 1) + 
    labs(col = expression(delta)) +
    scale_color_brewer(palette = "Dark2") +
    ylab("Observed proportion") + 
    xlab("Predicted probability") + 
    theme_classic() +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
    #theme(legend.position = "none")
 

