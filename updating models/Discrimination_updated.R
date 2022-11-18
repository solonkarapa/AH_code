
library(pROC)
library(dplyr)
library(ggplot2)
#library(ggROC)

# load data
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/updating models"
load(paste0(path_data, "/recalibrated_models_default.Rdata"))

#############################################   
############### Calculate AUCs  #############
#############################################  

# MELD (survival function 1 and 2 do not matter here)
roc_meld <- roc(test.data$D90_surv, test.data$meld.surv.updated)

# MELD 3.0
#roc_meld3 <- roc(stph.meld$D90_DTH, stph.meld$MELD3.surv)

# MELD from VanDerwerken et al 2021
#roc_meld.VanDerwerken <- roc(stph.meld$D90_DTH, stph.meld$MELD_Van)

# Lille
roc_lille <- roc(test.data$D90_surv, test.data$lille.surv.updated)

# CLIF-C ACLF
roc_clif <- roc(test.data$D90_surv, test.data$clif.surv.updated)

#############################################   
###################### Plots  ###############
############################################# 
roc.list <- list("MELD" = roc_meld, 
                 #"MELD VanDerwerken" = roc_meld.VanDerwerken,
                 "CLIF-C ACLF" = roc_clif, 
                 "Lille" = roc_lille)
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

ggroc(roc.list) + 
    facet_grid(. ~ name) +
    theme_minimal() + 
    geom_ribbon(data = df, aes(x = x, ymin = lower, ymax = upper, fill = name), alpha = 0.3, inherit.aes = F) +
    geom_abline(slope = 1, intercept = 1, linetype = "dashed") + 
    labs(x = "Specificity", y = "Sensitivity") + 
    coord_equal() +
    theme_classic() +
    theme(legend.position = "none") 




