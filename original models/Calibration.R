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
load(paste0(path_data, "original_models.Rdata"))

#############################################   
############### Calculate calibration  ######
#############################################   

# MELD_1 survival function
cal_MELD.surv <- calibration(stph.meld$MELD.surv, y = stph.meld$D90_surv)
cal_MELD.surv$Score <- "MELD_1"

# MELD_2 survival function
cal_MELD.surv2 <- calibration(stph.meld$MELD.surv2, y = stph.meld$D90_surv)
cal_MELD.surv2$Score <- "MELD_2"

# MELD VanDerwerken 
cal_MELD.VanDerwerken <- calibration(stph.meld$MELD_Van, y = stph.meld$D90_surv)
cal_MELD.VanDerwerken$Score <- "MELD VanDerwerken"

# MELD 3.0
sum(is.na(stph.meld$MELD3.surv)) # 10 missing values due to Albumin.MELD
cal_MELD3.surv <- calibration(stph.meld$MELD3.surv[!is.na(stph.meld$MELD3.surv)], y = stph.meld$D90_surv[!is.na(stph.meld$MELD3.surv)])
cal_MELD3.surv$Score <- "MELD 3.0"
    
# Lille
cal_Lille <- calibration(stph.lille$Lille.surv, y = stph.lille$D90_surv)
cal_Lille$Score <- "Lille"

# CLIF-C ACLF
cal_CLIF <- calibration(stph.clif$CLIF.surv, y = stph.clif$D90_surv)
cal_CLIF$Score <- "CLIF-C ACLF"

# combine dfs
df_cal <- rbind(cal_MELD.surv, cal_MELD.surv2, cal_MELD.VanDerwerken, cal_MELD3.surv, cal_Lille, cal_CLIF)

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
    facet_grid(. ~ Score) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Score, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    coord_equal() +
    xlim(0, 1) + 
    ylim(0, 1) + 
    labs(y = "Observed survival proportion", x = "Predicted survival probability") +
    theme_classic2() +
    theme(legend.position = "none")

#############################################   
############## Summary Stats  ###############
############################################# 

# compute calibration slopes and intercepts

# MELD_1 survival function
MELD1_stats <- val.prob.ci.2_wrapper(stph.meld$MELD.surv, stph.meld$D90_surv, "MELD_1")

# MELD_2 survival function
MELD2_stats <- val.prob.ci.2_wrapper(stph.meld$MELD.surv2, stph.meld$D90_surv, "MELD_2")

# MELD VanDerwerken 
MELD_Van_stats <- val.prob.ci.2_wrapper(stph.meld$MELD_Van, stph.meld$D90_surv, "MELD VanDerwerken")

# Lille
Lille_stats <- val.prob.ci.2_wrapper(stph.lille$Lille.surv, stph.lille$D90_surv, "Lille")

#CLIF-C ACLF
clif_stats <- val.prob.ci.2_wrapper(stph.clif$CLIF.surv, y = stph.clif$D90_surv, "CLIF-C ACLF")


df_stats <- rbind(MELD1_stats, MELD2_stats, MELD_Van_stats, Lille_stats, clif_stats) %>% relocate(Score)

#df_stats$Model <- fct_reorder(df_stats$Model, df_stats$ Point.estimate)
# adding to plot
#p_calibration + geom_text(data = df_stats %>% filter(stat == "intercept"), 
#                  aes(0.1, 0.90, label = 
#                          paste0("Int (95% CI): ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
#                      hjust = 0), col = "black") #+


library(xtable)
xtable((df_stats))




