
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

# stratify by severity
severity <- "non-severe" # "severe" or non-severe

if(severity == "severe"){
    data_meld <- Global_AlcHep.meld %>% filter(`DF at admission` >= 32)
    data_lille <- Global_AlcHep.lille %>% filter(`DF at admission` >= 32)
    }else{
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

if(severity == "severe"){
    df_cal_severe <- df_cal
    df_cal_severe$severity <- "Severe AH"
    }else{
        df_cal_non_severe <- df_cal
        df_cal_non_severe$severity <- "Non-severe AH"
}

df_cal_final <- rbind(df_cal_severe, df_cal_non_severe)

#############################################

# plot with ribbon 
# display.brewer.pal(n = 8, name = 'Dark2')
col_updated <- brewer.pal(n = 8, name = "Dark2")[c(2, 3)]

p_calibration_updated <- df_cal_final %>% filter(Status == "Updated") %>%
    ggplot(., aes(x = pred, y = obs, col = Score)) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Score, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    #scale_color_brewer(palette = "Dark2") +
    #scale_fill_brewer(palette = "Dark2") +
    scale_fill_manual("", values = col_updated) + 
    scale_color_manual(name = "Score", values = col_updated) +
    facet_grid(severity ~ Score) +
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

p_calibration_original <- df_cal_final %>% filter(Status == "Original") %>%
    ggplot(., aes(x = pred, y = obs, col = Score)) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Score, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    #scale_color_brewer(palette = "Dark2") +
    #scale_fill_brewer(palette = "Dark2") +
    scale_fill_manual("", values = col_original) + 
    scale_color_manual(name = "Score", values = col_original) + 
    facet_grid(severity ~ Score) +
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

ggarrange(p_calibration_original, p_calibration_updated, nrow = 1)

cowplot::plot_grid(p_calibration_original, p_calibration_updated, nrow = 1)


# re-arrange factor levels
df_cal_final2 <- df_cal_final %>% 
    mutate(Score = factor(Score, 
                          levels = c("Lille updated", "MELD updated", "Lille original", "MELD VanDerwerken", "MELD 1", "MELD 2")))

display.brewer.pal(n = 8, name = 'Paired')

col_2 <- brewer.pal(n = 8, name = "Paired")[c(1, 2, 5, 6, 7, 8)]

df_cal_final2 %>% #filter(Status == "Original") %>%
    ggplot(., aes(x = pred, y = obs, col = Score)) +
    geom_line(lwd = 1)  + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Score, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    #scale_color_brewer(palette = "Paired") +
    #scale_fill_brewer(palette = "Paired") +
    scale_fill_manual("", values = col_2) + 
    scale_color_manual(name = "Score", values = col_2) + 
    facet_grid(severity ~ Score) +
    coord_equal() +
    xlim(0, 1) + 
    ylim(0, 1) + 
    ylab("Observed survival proportion") + 
    xlab("Predicted survival probability") + 
    #theme_minimal() +
    theme_classic2() +
    theme(legend.position = "none") +
    #scale_y_continuous(labels = scientific)
    scale_x_continuous(labels = c(0, 0.25, 0.5, 0.75, 1.00))


