library(pROC)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(forcats)
#library(ggROC)

# load data
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/AH_code/original models/val_data"
load(paste0(path_data, "/models_validation.Rdata"))

#############################################   
############### Calculate AUCs  #############
#############################################  

data_to_use <- "all" # all, severe, non-severe "no"

if(data_to_use == "all"){
    data_meld <- Global_AlcHep.meld
    data_lille <- Global_AlcHep.lille
}else if(data_to_use == "severe"){
    data_meld <- Global_AlcHep.meld %>% filter(`DF at admission` >= 32)
    data_lille <- Global_AlcHep.lille %>% filter(`DF at admission` >= 32)
}else if(data_to_use == "non-severe"){
    data_meld <- Global_AlcHep.meld %>% filter(`DF at admission` < 32)
    data_lille <- Global_AlcHep.lille %>% filter(`DF at admission` < 32)
}

############################################# 
# MELD original
roc_meld_original <- roc(data_meld$D90_surv, data_meld$MELD.surv)

# MELD updated
roc_meld_updated <- roc(data_meld$D90_surv, data_meld$meld.surv.updated)

# Lille original
roc_lille_original <- roc(data_lille$D90_surv, data_lille$LILLE)

# Lille updated
roc_lille_updated <- roc(data_lille$D90_surv, data_lille$lille.surv.updated)

#############################################   
###################### Plots  ###############
############################################# 
roc.list <- list(
    "Lille original" = roc_lille_original,
    "Lille updated" = roc_lille_updated, 
    "MELD original" = roc_meld_original,
    "MELD updated" = roc_meld_updated)

#setwd(path_data)
#save(roc.list, file = "ROC_updated.Rdata")

g.list <- ggroc(roc.list)

# ROC plot of all models combined
g.list +
    geom_line(lwd = 1) +
    geom_abline(slope = 1, intercept = 1, linetype = "dashed") + 
    theme_classic()

# faceting 
g.list + 
    facet_grid(. ~ name) + 
    geom_line(lwd = 1) + 
    geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
    theme_classic() + 
    theme(legend.position="none") 

# add confidence bands
ci.list <- lapply(roc.list, ci.se, specificities = seq(0, 1, l = 25))

dat.ci.list <- lapply(ci.list, function(ciobj) 
    data.frame(x = as.numeric(rownames(ciobj)),
               lower = ciobj[, 1],
               upper = ciobj[, 3]))

df <- plyr::ldply(dat.ci.list, data.frame, .id = "name")

pl <- ggroc(roc.list, lwd = 1.1) + 
    facet_grid(. ~ name) +
    #theme_minimal() + 
    geom_ribbon(data = df, aes(x = x, ymin = lower, ymax = upper, fill = name), alpha = 0.3, inherit.aes = F) +
    geom_abline(slope = 1, intercept = 1, linetype = "dashed") + 
    labs(x = "Specificity", y = "Sensitivity") + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    coord_equal() +
    theme_classic2() +
    theme(legend.position = "none") 

data_wide$name <- data_wide$condition
data_wide$low_CL <- round(data_wide$low_CL, 2)
data_wide$mean <- round(data_wide$mean, 2)
data_wide$upper_CL <- round(data_wide$upper_CL, 2)

pl + geom_text(data = data_wide, aes(0.01, 0.19, label = paste0("AUC (95% CI): ", mean, " (", low_CL, "-", upper_CL, ")" ), 
                                     hjust = 1), col = "black")

#############################################   
###################### AUC  #################
#############################################
# CI
#auc_meld <- auc(roc_meld)
#auc_meld_ci <- ci.auc(roc_meld) # Confidence intervals
#roc_plot(stph.meld, "D90_surv", "MELD.surv", ci = TRUE, plot_title = "ROC curve for the MELD score")

df_AUC <- as.data.frame(map_dfr(roc.list, ci.auc))
rownames(df_AUC) <- c("low_CL", "mean", "upper_CL")

df_AUC2 <- tibble::rownames_to_column(df_AUC, var = "AUC")

df3 <- gather(df_AUC2, condition, measurement, `Lille original`:`MELD updated`, factor_key = TRUE)
data_wide <- spread(df3, AUC, measurement) %>% arrange(mean)

# reorder factor levels
data_wide$condition <- fct_reorder(data_wide$condition, data_wide$mean)

ggplot(data_wide, aes(x = mean, y = condition, col = condition)) +
    geom_point(lwd = 2)  + 
    coord_cartesian(xlim = c(0.5, 0.86)) +
    geom_errorbar(aes(xmin = low_CL, xmax = upper_CL), 
                  alpha = 1, show.legend = F, lwd = 1, width = 0.5) + 
    labs(y = "Score", col = "Score", x = "AUC with 95% limits") +
    theme_classic() 


pl + geom_text(data = data_wide, aes(0.05, 0.25, label = paste0("AUC (95% CI): ", mean, " (", low_CL, "-", upper_CL, ")" ), 
                                     hjust = 1), col = "black")
