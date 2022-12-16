# This script performs all calculations for assessing discrimination

library(pROC)
library(dplyr)
library(ggplot2)
#library(ggROC)

# data
path_data <- "/Users/work/IDrive-Sync/Projects/MIMAH/code/AH_code/original models/"
load(paste0(path_data, "original_models.Rdata"))

#############################################   
############### Calculate AUCs  #############
#############################################  

# MELD (survival function 1 and 2 do not matter here)
roc_meld <- roc(stph.meld$D90_DTH, stph.meld$MELD.surv)

# MELD 3.0
roc_meld3 <- roc(stph.meld$D90_DTH, stph.meld$MELD3.surv)

# MELD from VanDerwerken et al 2021
roc_meld.VanDerwerken <- roc(stph.meld$D90_DTH, stph.meld$MELD_Van)

# Lille
roc_lille <- roc(stph.lille$D90_DTH, stph.lille$Lille.surv)

# CLIF-C ACLF
roc_clif <- roc(stph.clif$D90_DTH, stph.clif$CLIF.surv)

# CI
#auc_meld <- auc(roc_meld)
#auc_meld_ci <- ci.auc(roc_meld) # Confidence intervals
#roc_plot(stph.meld, "D90_surv", "MELD.surv", ci = TRUE, plot_title = "ROC curve for the MELD score")

#############################################   
###################### Plots  ###############
############################################# 
roc.list <- list("MELD" = roc_meld, 
           "MELD VanDerwerken" = roc_meld.VanDerwerken,
           "CLIF-C ACLF" = roc_clif, 
           "Lille" = roc_lille)

#save(roc.list, file = "ROC_original.Rdata")

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

# To have all the curves of the same color, use aes="group":
#g.group <- ggroc(roc.list, aes="group")
#g.group
#g.group + facet_grid(.~name)

#####
# Formally compare the c-statistics across models using bootstrap method
compareroc.mc <- roc.test(roc_clif, roc_meld) # comparison between MELD and CLIF
compareroc.ml <- roc.test(roc_meld, roc_lille) # comparison between MELD and Lille
compareroc.cl <- roc.test(roc_clif, roc_lille) # comparison between CLIF and Lille

# Tabulate the p-values
roc_pvalues <- c(compareroc.mc$p.value, compareroc.ml$p.value, compareroc.cl$p.value)
names(roc_pvalues) <- c("p-value MELD-CLIF", "p-value MELD-Lille", "p-value CLIF-Lille")
roc_pvalues

#####
# # NRI calculations
# library(nricens)
# 
# # Define events and probability vectors
# event <- stph.c$D90_surv
# p.MELD <- stph.c$MELD.surv
# p.MELD2 <- stph.c$MELD.surv2
# p.LILLE <- stph.c$Lille.surv
# p.CLIF <- stph.c$CLIF.surv
# 
# # Define cut-off points
# cut_lille <- 1 - (exp(-0.45)/(1 + exp(-0.45))) 
# cut_meld <- 0.707^(exp(2.5 - 1.127)) 
# cut_meld2 <- 0.98465^(exp(0.1635*(25 - 10)))  
# cut_clif <- exp(-0.0079 * exp(0.0869*51))
# 
# # Calculate NRI for all models
# # MELD_1 and Lille
# NRI_ML <- nribin(event = event, p.std = p.MELD, p.new = p.LILLE, cut = cut_meld, niter = 0, updown = 'category')
# # MELD_2 and Lille
# NRI_M2L <- nribin(event = event, p.std = p.MELD2, p.new = p.LILLE, cut = cut_meld2, niter = 0, updown = 'category')
# # CLIF-C ACLF and Lille
# NRI_CL <- nribin(event = event, p.std = p.CLIF, p.new = p.LILLE, cut = cut_clif, niter = 0, updown = 'category')
# # MELD_1 and CLIF-C ACLF
# NRI_MC <- nribin(event = event, p.std = p.MELD, p.new = p.CLIF, cut = cut_meld, niter = 0, updown = 'category')
# # MELD_2 and CLIF-C ACLF
# NRI_M2C <- nribin(event = event, p.std = p.MELD2, p.new = p.CLIF, cut = cut_meld2, niter = 0, updown = 'category')
# # Lille and MELD_1
# NRI_LM <- nribin(event = event, p.std = p.LILLE, p.new = p.MELD, cut = cut_lille, niter = 0, updown = 'category')
# # Lille and MELD_2
# NRI_LM2 <- nribin(event = event, p.std = p.LILLE, p.new = p.MELD2, cut = cut_lille, niter = 0, updown = 'category')
# # Lille and CLIF-C ACLf
# NRI_LC <- nribin(event = event, p.std = p.LILLE, p.new = p.CLIF, cut = cut_lille, niter = 0, updown = 'category')
# # CLIF-C ACLF and MELD_1
# NRI_CM <- nribin(event = event, p.std = p.CLIF, p.new = p.MELD, cut = cut_clif, niter = 0, updown = 'category')
# # CLIF-C ACLF and MELD_2
# NRI_CM2 <- nribin(event = event, p.std = p.CLIF, p.new = p.MELD2, cut = cut_clif, niter = 0, updown = 'category')
