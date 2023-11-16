
# load libraries
library(tidyverse)
library(rsample)
library(reshape2)
library(ggplot2)
library(survminer) # for plotting theme
library(ggsci)  # for color palette

# funs
path_funs <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/NB"
source(paste0(path_funs, "/net_benefit_fun.R"))
source(paste0(path_funs, "/net_benefit_wrapper_fun.R"))

# data
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/updating models"
load(paste0(path_data, "/recalibrated_models_default.Rdata"))

test.data.short <- test.data %>% mutate(lille.death.risk = 1 - Lille.surv,
                                        lille.death.risk.updated = 1 -lille.surv.updated,
                                        CLIF.death.risk = 1 - CLIF.surv,
                                        clif.death.risk.updated = 1 - clif.surv.updated,
                                        meld.death.risk = 1 - MELD.surv,
                                        meld.death.risk.updated = 1 - meld.surv.updated,
                                        meld.death.risk2 = 1 - MELD.surv2)

##################################################################    
############### Calculate bootstrapped NB diff  ################## 
##################################################################  
thresholds = seq(0.01, 0.99, 0.04)
n_boot <- 1000


set.seed(353)
bt_resamples <- bootstraps(test.data.short, times = n_boot, strata = "D90_DTH")

# models 
m_lille <- map_df(bt_resamples$splits, net_benefit_treated_wrapper, model = "Lille")
m_lille$boot_id <- rep(1:n_boot, each = length(thresholds) * 2)

m_cliff <- map_df(bt_resamples$splits, net_benefit_treated_wrapper, model = "CLIF-C ACLF")
m_cliff$boot_id <- rep(1:n_boot, each = length(thresholds) * 2)

meld1 <- map_df(bt_resamples$splits, net_benefit_treated_wrapper, model = "MELD 1")
meld1$boot_id <- rep(1:n_boot, each = length(thresholds) * 2)

meld2 <- map_df(bt_resamples$splits, net_benefit_treated_wrapper, model = "MELD 2")
meld2$boot_id <- rep(1:n_boot, each = length(thresholds) * 2)

combined_df <- rbind(m_lille, m_cliff, meld1, meld2)

alpha <- 0.05 # significance level

data_wide <- dcast(combined_df, threshold + model + boot_id ~ status, value.var = "NB") %>%
    group_by(threshold, boot_id, model) %>% 
    mutate(diff_NB = Original - Updated) %>%
    ungroup() %>%
    group_by(threshold, model) %>%
    summarize(meanNB_diff = mean(diff_NB),
              low = quantile(diff_NB, alpha / 2),
              high = quantile(diff_NB, 1 - alpha / 2))

#############################################   
###################### Plots  ###############
############################################# 

# Original vs Updated comparison plots
combined_df %>% group_by(threshold, status, model) %>% 
    summarize(meanNB = mean(NB),
              low = quantile(NB, alpha/2, na.rm = T),
              high = quantile(NB, 1 - alpha/2, na.rm = T),
              mean_NB_all = mean(NB_all)) %>%
    ggplot(aes(x = threshold, y = meanNB)) +
    geom_line(aes(col = status)) +
    #geom_ribbon(aes(ymin = low, ymax = high, fill = model, linetype = NA),  
    #            alpha = 0.3, show.legend = F) + 
    geom_line(aes(y = mean_NB_all), linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted") + 
    coord_cartesian(ylim = c(-0.05, 0.4)) +
    facet_wrap(.~ model) +
    theme_classic2() 

# Updated comparison plots
updated <- combined_df %>% 
    filter(model %in% c("CLIF-C ACLF", "Lille", "MELD 1")) %>%
    group_by(threshold, status, model) %>% 
    summarize(meanNB = mean(NB),
              low = quantile(NB, alpha/2, na.rm = T),
              high = quantile(NB, 1 - alpha/2, na.rm = T),
              mean_NB_all = mean(NB_all)) %>%
    filter(status == "Updated") %>%
    mutate(model = ifelse(model == "MELD 1", "MELD", model)) %>%
    ggplot(aes(x = threshold, y = meanNB)) +
    geom_line(aes(col = model), lwd = 1) +
    #geom_ribbon(aes(ymin = low, ymax = high, fill = model, linetype = NA),  
    #            alpha = 0.3, show.legend = F) + 
    geom_line(aes(y = mean_NB_all), linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted") + 
    coord_cartesian(ylim = c(-0.05, 0.3)) +
    labs(col = "Updated Scores", y = "Net Benefit", x = "Threshold Risk probability") +
    #facet_wrap(.~ model) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme_classic2() 


# Updated original plots
library(RColorBrewer)
customs_cols <- brewer.pal(6, "Dark2")
customs_cols

original <- combined_df %>% 
    #filter(model %in% c("CLIF-C ACLF", "Lille", "MELD 1")) %>%
    group_by(threshold, status, model) %>% 
    summarize(meanNB = mean(NB),
              low = quantile(NB, alpha/2, na.rm = T),
              high = quantile(NB, 1 - alpha/2, na.rm = T),
              mean_NB_all = mean(NB_all)) %>%
    filter(status == "Original") %>%
    #mutate(model = ifelse(model == "MELD 1", "MELD", model)) %>%
    ggplot(aes(x = threshold, y = meanNB)) +
    geom_line(aes(col = model), lwd = 1) +
    #geom_ribbon(aes(ymin = low, ymax = high, fill = model, linetype = NA),  
    #            alpha = 0.3, show.legend = F) + 
    geom_line(aes(y = mean_NB_all), linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted") + 
    coord_cartesian(ylim = c(-0.05, 0.3)) +
    labs(col = "Original Scores", y = "Net Benefit", x = "Threshold Risk probability") +
    #facet_wrap(.~ model) +
    #scale_color_brewer(palette = "Dark2", type = "diverging") +
    scale_color_manual(values = customs_cols[-3]) +
    theme_classic2() 

combined_df %>% 
    #filter(model %in% c("CLIF-C ACLF", "Lille", "MELD 1")) %>%
    group_by(threshold, status, model) %>% 
    summarize(meanNB = mean(NB),
              low = quantile(NB, alpha/2, na.rm = T),
              high = quantile(NB, 1 - alpha/2, na.rm = T),
              mean_NB_all = mean(NB_all), 
              mean_FPR = mean(FPR)) %>% 
    #filter(status == "Original") %>% 
    filter(model == "Lille" & threshold == 0.45)

data_wide %>% filter(model == "Lille" & threshold == 0.45)

tp <- sum(test.data.short$lille.death.risk >= 0.45 & test.data.short$D90_DTH == 1)
tp    
fp <- sum(test.data.short$lille.death.risk >= 0.45 & test.data.short$D90_DTH == 0)
fp
fp/tp

tp <- sum(test.data.short$lille.death.risk.updated >= 0.45 & test.data.short$D90_DTH == 1)
tp    
fp <- sum(test.data.short$lille.death.risk.update >= 0.45 & test.data.short$D90_DTH == 0)
fp

fp/tp



#################
ggplot(data_wide, aes(x = threshold, y = meanNB_diff)) +
    geom_line(lwd = 1) +
    geom_ribbon(aes(ymin = low, ymax = high, linetype = NA),  
                alpha = 0.3, show.legend = F) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(y = "Difference in Net Benefit", x = "Threshold Risk Probability") +
    facet_wrap(.~ model, scales = "free_y") +
    theme_classic2()

# lille threshold 
data_wide %>% filter(model == "Lille" & threshold == 0.45)

data_wide %>% filter(model == "MELD 1" & threshold == 0.45)
#################