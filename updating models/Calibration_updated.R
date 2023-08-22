# This script performs all calculations of model calibration
library(ggplot2)
library(survminer) # for plotting theme

path_funs <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/funs"
source(paste0(path_funs, "/calibration_fun.R"))
source(paste0(path_funs, "/val.prob.ci.2_wrapper.R"))

path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/updating models"
load(paste0(path_data, "/recalibrated_models_default.Rdata"))

#############################################   
############### Calculate calibration  ######
#############################################   
# MELD
cal_MELD.surv <- calibration(test.data$meld.surv.updated, y = test.data$D90_surv)
cal_MELD.surv$Score <- "MELD"

# Lille
cal_Lille <- calibration(test.data$lille.surv.updated, y = test.data$D90_surv)
cal_Lille$Score <- "Lille"

# CLIF-C ACLF
cal_CLIF <- calibration(test.data$clif.surv.updated, y = test.data$D90_surv)
cal_CLIF$Score <- "CLIF-C ACLF"

# combine dfs
df_cal <- rbind(cal_MELD.surv, cal_Lille, cal_CLIF)

#############################################   
###################### Plots  ###############
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
 
#############################################   
############## Summary Stats  ###############
############################################# 

# compute calibration slopes and intercepts

# MELD_1 survival function
MELD_stats <- val.prob.ci.2_wrapper(test.data$meld.surv.updated, test.data$D90_surv, "MELD")

# Lille
Lille_stats <- val.prob.ci.2_wrapper(test.data$lille.surv.updated, test.data$D90_surv, "Lille")

#CLIF-C ACLF
clif_stats <- val.prob.ci.2_wrapper(test.data$clif.surv.updated, y = test.data$D90_surv, "CLIF-C ACLF")


df_stats <- rbind(MELD_stats, Lille_stats, clif_stats) %>% relocate(Score)


p_calibration + geom_text(data = df_stats %>% filter(stat == "intercept"), 
                  aes(0.1, 0.95, label = 
                          paste0("Intercept (95% CI): ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                      hjust = 0), col = "black") +
    geom_text(data = df_stats %>% filter(stat == "slope"), 
              aes(0.1, 0.90, label = 
                      paste0("Slope (95% CI): ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                  hjust = 0), col = "black")

