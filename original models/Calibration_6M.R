# This script performs all calculations of model calibration
library(ggplot2)
library(dplyr)
library(survminer) # for plotting theme

# funs
path_funs <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/funs"
source(paste0(path_funs, "/calibration_fun.R"))
source(paste0(path_funs, "/val.prob.ci.2_wrapper.R"))

# data
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models/"
load(paste0(path_data, "original_models_6M.Rdata"))

#############################################   
############### Calculate calibration  ######
#############################################   

# MELD_1 survival function
cal_MELD.surv <- calibration(stph.meld$MELD.surv_6M, y = stph.meld$M6_surv)
cal_MELD.surv$Score <- "MELD_1"

# Lille
cal_Lille <- calibration(stph.lille$Lille.surv_6M, y = stph.lille$M6_surv)
cal_Lille$Score <- "Lille"

# CLIF-C ACLF
cal_CLIF <- calibration(stph.clif$CLIF.surv_6M, y = stph.clif$M6_surv)
cal_CLIF$Score <- "CLIF-C ACLF"

# combine dfs
df_cal <- rbind(cal_MELD.surv, cal_Lille, cal_CLIF)

#############################################   
###################### Plots  ###############
############################################# 

# plot without ribbon and without MELD 3.0
df_cal %>% filter(Score != "MELD 3.0") %>%
    ggplot(., aes(x = pred, y = obs, col = Score)) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #geom_ribbon(aes(ymin = lower, ymax = upper, linetype = NA), 
    #            alpha = 0.3, show.legend = F) + 
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed proportion") + 
    xlab("Predicted probability") + 
    theme_classic() 

# plot with ribbon 
p_calibration <- df_cal %>% filter(Score != "MELD 3.0") %>%
    ggplot(., aes(x = pred, y = obs, col = Score)) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Score, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    facet_grid(. ~ Score) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "#E7298A")) + #palette = "Dark2") +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#E7298A")) + #palette = "Dark2") +
    coord_equal() +
    xlim(0, 1) + 
    ylim(0, 1) + 
    labs(y = "Observed 6-month survival proportion", x = "Predicted 6-month survival probability") +
    theme_classic2() +
    theme(legend.position = "none")


#############################################   
############## Summary Stats  ###############
############################################# 
# compute calibration slopes and intercepts

# MELD_1 survival function
MELD_stats <- val.prob.ci.2_wrapper(stph.meld$MELD.surv_6M, stph.meld$M6_surv, "MELD_1")

# Lille
Lille_stats <- val.prob.ci.2_wrapper(stph.lille$Lille.surv_6M, stph.lille$M6_surv, "Lille")

#CLIF-C ACLF
clif_stats <- val.prob.ci.2_wrapper(stph.clif$CLIF.surv_6M, y = stph.clif$M6_surv, "CLIF-C ACLF")

df_stats <- rbind(MELD_stats, Lille_stats, clif_stats) %>% relocate(Score)

# plot
p_calibration + geom_text(data = df_stats %>% filter(stat == "intercept"), 
                          aes(0.05, 0.95, label = 
                                  paste0("Intercept (95% CI): ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                              hjust = 0), col = "black") +
    geom_text(data = df_stats %>% filter(stat == "slope"), 
              aes(0.05, 0.90, label = 
                      paste0("Slope (95% CI): ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                  hjust = 0), col = "black")


