

# load libraries
library(tidyverse)
library(rsample)
library(reshape2)
library(survminer) # for plotting theme

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
m_lille <- map_df(bt_resamples$splits, net_benefit_treated_wrapper, model = "lille")
m_lille$boot_id <- rep(1:n_boot, each = length(thresholds) * 2)

m_cliff <- map_df(bt_resamples$splits, net_benefit_treated_wrapper, model = "cliff")
m_cliff$boot_id <- rep(1:n_boot, each = length(thresholds) * 2)

meld1 <- map_df(bt_resamples$splits, net_benefit_treated_wrapper, model = "meld1")
meld1$boot_id <- rep(1:n_boot, each = length(thresholds) * 2)

meld2 <- map_df(bt_resamples$splits, net_benefit_treated_wrapper, model = "meld2")
meld2$boot_id <- rep(1:n_boot, each = length(thresholds) * 2)

combined_df <- rbind(m_lille, m_cliff, meld1, meld2)

alpha <- 0.05 # significance level

data_wide <- dcast(combined_df, threshold + model + boot_id ~ status, value.var = "NB") %>%
    group_by(threshold, boot_id, model) %>% 
    mutate(diff_NB = original - update) %>%
    ungroup() %>%
    group_by(threshold, model) %>%
    summarize(meanNB_diff = mean(diff_NB),
              low = quantile(diff_NB, alpha / 2),
              high = quantile(diff_NB, 1 - alpha / 2))

#############################################   
###################### Plots  ###############
############################################# 
combined_df %>% group_by(threshold, status, model) %>% 
    summarize(meanNB = mean(NB),
              low = quantile(NB, alpha/2),
              high = quantile(NB, 1-alpha/2)) %>%
    ggplot(aes(x = threshold, y = meanNB)) +
    geom_line(aes(col = status)) +
    geom_ribbon(aes(ymin = low, ymax = high, fill = status, linetype = NA),  
                alpha = 0.3, show.legend = F) + 
    #geom_line(aes(col = status), alpha = 0.5) +
    coord_cartesian(ylim = c(-0.1, 0.75)) +
    facet_wrap(.~ model) +
    theme_classic2() 

#################
ggplot(data_wide, aes(x = threshold, y = meanNB_diff)) +
    geom_line() +
    geom_ribbon(aes(ymin = low, ymax = high, linetype = NA),  
                alpha = 0.3, show.legend = F) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(y = "Difference in NB", x = "Threshold Risk Probability") +
    facet_wrap(.~ model, scales = "free_y") +
    theme_classic2()

# lille threshold 
data_wide %>% filter(model == "lille" & threshold == 0.45)
#################




