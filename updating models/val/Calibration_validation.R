
# This script evaluated all calculations of model calibration
library(ggplot2)
library(dplyr)
library(survminer) # for plotting theme

path_funs <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/funs"
source(paste0(path_funs, "/calibration_fun.R"))
source(paste0(path_funs, "/val.prob.ci.2_wrapper.R"))

path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models/val_data"
load(paste0(path_data, "/models_validation.Rdata"))

#############################################   
############### Calculate calibration  ######
############################################# 

severity <- "yes"

if(severity == "yes"){
    data_meld <- Global_AlcHep.meld %>% filter(`DF at admission` >= 32)
    data_lille <- Global_AlcHep.lille %>% filter(`DF at admission` >= 32)
}else{
    data_meld <- Global_AlcHep.meld
    data_lille <- Global_AlcHep.lille
}

############################################# 
# MELD 1 survival function
cal_MELD.surv <- calibration(data_meld$MELD.surv, y = data_meld$D90_surv)
cal_MELD.surv$Score <- "MELD 1"

# MELD_2 survival function
cal_MELD.surv2 <- calibration(data_meld$MELD.surv2, y = data_meld$D90_surv)
cal_MELD.surv2$Score <- "MELD 2"

# MELD updated
cal_MELD.updated <- calibration(data_meld$meld.surv.updated, y = data_meld$D90_surv)
cal_MELD.updated$Score <- "MELD updated"

############################################# 
# Lille
cal_Lille <- calibration(data_lille$Lille.surv, y = data_lille$D90_surv)
cal_Lille$Score <- "Lille original"

# Lille updated
cal_Lille.updated <- calibration(data_lille$lille.surv.updated, y = data_lille$D90_surv)
cal_Lille.updated$Score <- "Lille updated"

# combine dfs
df_cal <- rbind(cal_MELD.surv, cal_MELD.surv2, cal_MELD.updated, cal_Lille, cal_Lille.updated)

############################################# 
# plot without ribbon and without MELD 3.0
df_cal %>%
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
p_calibration <- df_cal %>%
    ggplot(., aes(x = pred, y = obs, col = Score)) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Score, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    facet_grid(. ~ Score) +
    coord_equal() +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed survival proportion") + 
    xlab("Predicted survival probability") + 
    theme_classic2() +
    theme(legend.position = "none")

p_calibration

#############################################   
############## Summary Stats  ###############
############################################# 

# compute calibration slopes and intercepts
# MELD_1 survival function
MELD_stats <- val.prob.ci.2_wrapper(data_meld$meld.surv.updated, data_meld$D90_surv, "MELD")

# Lille
Lille_stats <- val.prob.ci.2_wrapper(data_lille$lille.surv.updated, data_lille$D90_surv, "Lille")

df_stats <- rbind(MELD_stats, Lille_stats) %>% relocate(Score)


p_calibration + geom_text(data = df_stats %>% filter(stat == "intercept"), 
                          aes(0.1, 0.95, label = 
                                  paste0("Intercept (95% CI): ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                              hjust = 0), col = "black") +
    geom_text(data = df_stats %>% filter(stat == "slope"), 
              aes(0.1, 0.90, label = 
                      paste0("Slope (95% CI): ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                  hjust = 0), col = "black")

