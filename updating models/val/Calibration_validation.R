
# This script evaluated all calculations of model calibration
library(ggplot2)
library(dplyr)
library(survminer) # for plotting theme
library(RColorBrewer)

path_funs <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/funs"
source(paste0(path_funs, "/calibration_fun.R"))
source(paste0(path_funs, "/val.prob.ci.2_wrapper.R"))

path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models/val_data"
load(paste0(path_data, "/models_validation.Rdata"))

#############################################   
############### Calculate calibration  ######
############################################# 

data_to_use <- "all" # all, severe, non_severe "no"

if(data_to_use == "all"){
    data_meld <- Global_AlcHep.meld
    data_lille <- Global_AlcHep.lille
    }else if(data_to_use == "severe"){
        data_meld <- Global_AlcHep.meld %>% filter(`DF at admission` >= 32)
        data_lille <- Global_AlcHep.lille %>% filter(`DF at admission` >= 32)
        }else if(data_to_use == "non_severe"){
            data_meld <- Global_AlcHep.meld %>% filter(`DF at admission` < 32)
            data_lille <- Global_AlcHep.lille %>% filter(`DF at admission` < 32)
        }

############################################# 
# MELD 1 survival function
cal_MELD.surv <- calibration(data_meld$MELD.surv, y = data_meld$D90_surv)
cal_MELD.surv$Score <- "MELD 1"

# MELD_2 survival function
cal_MELD.surv2 <- calibration(data_meld$MELD.surv2, y = data_meld$D90_surv)
cal_MELD.surv2$Score <- "MELD 2"

# MELD VanDerwerken 
cal_MELD.VanDerwerken <- calibration(data_meld$MELD_Van, y = data_meld$D90_surv)
cal_MELD.VanDerwerken$Score <- "MELD VanDerwerken"

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
df_cal <- rbind(cal_MELD.surv, cal_MELD.surv2, cal_MELD.VanDerwerken, cal_MELD.updated, cal_Lille, cal_Lille.updated)

df_cal <- df_cal %>% mutate(Status = ifelse(Score == "Lille updated" | Score == "MELD updated", "Updated", "Original"))

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
# display.brewer.pal(n = 8, name = 'Dark2')
col_updated <- brewer.pal(n = 8, name = "Dark2")[c(2, 3)]

p_calibration_updated <- df_cal %>% filter(Status == "Updated") %>%
    ggplot(., aes(x = pred, y = obs, col = Score)) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Score, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    #scale_color_brewer(palette = "Dark2") +
    #scale_fill_brewer(palette = "Dark2") +
    scale_fill_manual("", values = col_updated) + 
    scale_color_manual(name = "Score", values = col_updated) +
    facet_grid(. ~ Score) +
    coord_equal() +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed survival proportion") + 
    xlab("Predicted survival probability") + 
    theme_classic2() +
    theme(legend.position = "none") +
    scale_x_continuous(labels = c(0, 0.25, 0.5, 0.75, 1))

p_calibration_updated

col_original <- brewer.pal(n = 8, name = "Dark2")[c(2, 4, 5, 3)]

p_calibration_original <- df_cal %>% filter(Status == "Original") %>%
    ggplot(., aes(x = pred, y = obs, col = Score)) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Score, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    #scale_color_brewer(palette = "Dark2") +
    #scale_fill_brewer(palette = "Dark2") +
    scale_fill_manual("", values = col_original) + 
    scale_color_manual(name = "Score", values = col_original) + 
    facet_grid(. ~ Score) +
    coord_equal() +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed survival proportion") + 
    xlab(" ") + 
    theme_classic2() +
    theme(legend.position = "none") +
    #scale_y_continuous(labels = scientific)
    scale_x_continuous(labels = c(0, 0.25, 0.5, 0.75, 1.00))

p_calibration_original

ggarrange(p_calibration_original, p_calibration_updated, nrow = 2)

cowplot::plot_grid(p_calibration_original, p_calibration_updated, nrow = 2, scale = 1)

#############################################   
############## Summary Stats  ###############
############################################# 

# compute calibration slopes and intercepts
# MELD updated
MELD_stats <- val.prob.ci.2_wrapper(data_meld$meld.surv.updated, data_meld$D90_surv, "MELD updated")

# Lille updated
Lille_stats <- val.prob.ci.2_wrapper(data_lille$lille.surv.updated, data_lille$D90_surv, "Lille updated")

df_stats <- rbind(MELD_stats, Lille_stats) %>% relocate(Score)

p_calibration_updated + geom_text(data = df_stats %>% filter(stat == "intercept"), 
                          aes(0.1, 0.95, label = 
                                  paste0("Intercept (95% CI): ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                              hjust = 0), col = "black") +
    geom_text(data = df_stats %>% filter(stat == "slope"), 
              aes(0.1, 0.90, label = 
                      paste0("Slope (95% CI): ", Point.estimate, " (", Lower.confidence.limit, "-", Upper.confidence.limit, ")"), 
                  hjust = 0), col = "black")


##### original models 
# MELD 
MELD_stats1 <- val.prob.ci.2_wrapper(data_meld$MELD.surv, data_meld$D90_surv, "MELD 1")
MELD_stats2 <- val.prob.ci.2_wrapper(data_meld$MELD.surv2, data_meld$D90_surv, "MELD 2")
MELD_Van <- val.prob.ci.2_wrapper(data_meld$MELD_Van, data_meld$D90_surv, "MELD Van")

# Lille updated
Lille_stats_orig <- val.prob.ci.2_wrapper(data_lille$Lille.surv, data_lille$D90_surv, "Lille")

df_original_stats <- rbind(MELD_stats1,MELD_stats2, MELD_Van,  Lille_stats_orig)

df_original_stats %>% filter(stat == "slope")
library(xtable)
xtable((df_original_stats))


