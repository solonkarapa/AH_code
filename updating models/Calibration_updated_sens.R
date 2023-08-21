
# This script performs all calculations of model calibration
library(ggplot2)

path_funs <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/funs"
source(paste0(path_funs, "/calibration_fun.R"))
source(paste0(path_funs, "/val.prob.ci.2_wrapper.R"))

path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/updating models"
load(paste0(path_data, "/recalibrated_models_sens.Rdata"))

#############################################   
############### Calculate calibration  ######
#############################################   
# MELD
cal_MELD.surv <- calibration(test.meld$meld.surv.updated, y = test.meld$D90_surv)
cal_MELD.surv$Score <- "MELD"

# Lille
cal_Lille <- calibration(test.lille$lille.surv.updated, y = test.lille$D90_surv)
cal_Lille$Score <- "Lille"

# CLIF-C ACLF
cal_CLIF <- calibration(test.clif$clif.surv.updated, y = test.clif$D90_surv)
cal_CLIF$Score <- "CLIF-C ACLF"

#############################################   
############### Calculate stats  ############
############################################# 
# MELD
cal_MELD.stats <- val.prob.ci.2_wrapper(test.meld$meld.surv.updated, y = test.meld$D90_surv, "MELD")

# Lille
cal_Lille.stats <- val.prob.ci.2_wrapper(test.lille$lille.surv.updated, y = test.lille$D90_surv, "Lille")

# CLIF-C ACLF
cal_CLIF.stats <- val.prob.ci.2_wrapper(test.clif$clif.surv.updated, y = test.clif$D90_surv, "CLIF-C ACLF")

#############################################   
###################### Plots  ###############
############################################# 
# plot with ribbon 
p1 <- cal_MELD.surv %>%
    ggplot(., aes(x = pred, y = obs)) +
    geom_line(linewidth = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    facet_grid(. ~ Score) +
    coord_equal() +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed proportion") + 
    xlab("Predicted probability") + 
    theme_classic2() +
    theme_classic() +
    geom_text(data = cal_MELD.stats %>% filter(stat == "intercept"), 
              aes(0.1, 0.90, label = 
                      paste0("Intercept: ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                      hjust = 0), col = "black") + 
    geom_text(data = cal_MELD.stats %>% filter(stat == "slope"), 
              aes(0.1, 0.85, label = 
                      paste0("Slope: ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                  hjust = 0), col = "black")
    

p2 <- cal_Lille %>%
    ggplot(., aes(x = pred, y = obs)) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    facet_grid(. ~ Score) +
    coord_equal() +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed proportion") + 
    xlab("Predicted probability") + 
    theme_classic2() +
    theme_classic() +
    geom_text(data = cal_Lille.stats %>% filter(stat == "intercept"), 
              aes(0.1, 0.90, label = 
                      paste0("Intercept: ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                  hjust = 0), col = "black") + 
    geom_text(data = cal_Lille.stats %>% filter(stat == "slope"), 
              aes(0.1, 0.85, label = 
                      paste0("Slope: ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                  hjust = 0), col = "black")

p3 <- cal_CLIF %>%
    ggplot(., aes(x = pred, y = obs)) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") + 
    #scale_fill_manual("", values = col) + 
    #scale_color_manual(name = "Score", values = col) + 
    facet_grid(. ~ Score) +
    coord_equal() +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed proportion") + 
    xlab("Predicted probability") + 
    theme_classic2() +
    theme_classic() +
    geom_text(data = cal_CLIF.stats %>% filter(stat == "intercept"), 
              aes(0.1, 0.90, label = 
                      paste0("Intercept: ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                  hjust = 0), col = "black") + 
    geom_text(data = cal_CLIF.stats %>% filter(stat == "slope"), 
              aes(0.1, 0.85, label = 
                      paste0("Slope: ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                  hjust = 0), col = "black")

ggarrange(p1, p2, p3, nrow = 1, ncol = 3, common.legend = TRUE)
