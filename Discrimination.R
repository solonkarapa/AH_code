# This script performs all calculations for assessing discrimination
library(tidyverse)
library(pROC)
library(dplyr)
library(ggplot2)
library(runway)

# AUROC calculation and figure for MELD
roc_meld <- roc(stph.meld$D90_DTH, stph.meld$MELD.surv)
auc_meld <- auc(roc_meld)
auc_meld_ci <- ci.auc(roc_meld) # Confidence intervals

roc_plot(stph.meld, "D90_surv", "MELD.surv", ci = TRUE, plot_title = "ROC curve for the MELD score")

# AUROC calculation and figure for MELD 3.0
roc_meld3 <- roc(stph.meld$D90_DTH, stph.meld$MELD3.surv)
auc_meld3 <- auc(roc_meld3)
auc_meld3_ci <- ci.auc(roc_meld3) # Confidence intervals

roc_plot(stph.meld, "D90_surv", "MELD3.surv", ci = TRUE, plot_title = "ROC curve for the MELD score")

# AUROC calculation and figure for Lille
roc_lille <- roc(stph.lille$D90_DTH, stph.lille$Lille.surv)
auc_lille <- auc(roc_lille)
auc_lille_ci <- ci.auc(roc_lille)

roc_plot(stph.lille, "D90_surv", "Lille.surv", ci = TRUE, plot_title = "ROC curve for the Lille score")

# AUROC calculation and figure for CLIF-C ACLF
roc_clif <- roc(stph.clif$D90_DTH, stph.clif$CLIF.surv)
auc_clif <- auc(roc_clif)
auc_clif_ci <- ci.auc(roc_clif)

roc_plot(stph.clif, "D90_surv", "CLIF.surv", ci = TRUE, plot_title = "ROC curve for the CLIF-C ACLF score")

# ROC plot of all models combined
library(ggROC)
ggroc(list(MELD = roc_meld, CLIF = roc_clif, Lille = roc_lille))

#####
# Formally ompare the c-statistics across models using bootstrap method
compareroc.mc <- roc.test(roc_clif, roc_meld) # comparison between MELD and CLIF
compareroc.ml <- roc.test(roc_meld, roc_lille) # comparison between MELD and Lille
compareroc.cl <- roc.test(roc_clif, roc_lille) # comparison between CLIF and Lille

# Tabulate the p-values
roc_pvalues <- c(compareroc.mc$p.value, compareroc.ml$p.value, compareroc.cl$p.value)
names(roc_pvalues) <- c("p-value MELD-CLIF", "p-value MELD-Lille", "p-value CLIF-Lille")
roc_pvalues

#####
# NRI calculations
library(nricens)

# Define events and probability vectors
event <- stph.c$D90_surv
p.MELD <- stph.c$MELD.surv
p.MELD2 <- stph.c$MELD.surv2
p.LILLE <- stph.c$Lille.surv
p.CLIF <- stph.c$CLIF.surv

# Define cut-off points
cut_lille <- 1 - (exp(-0.45)/(1 + exp(-0.45))) 
cut_meld <- 0.707^(exp(2.5 - 1.127)) 
cut_meld2 <- 0.98465^(exp(0.1635*(25 - 10)))  
cut_clif <- exp(-0.0079 * exp(0.0869*51))

# Calculate NRI for all models
# MELD_1 and Lille
NRI_ML <- nribin(event = event, p.std = p.MELD, p.new = p.LILLE, cut = cut_meld, niter = 0, updown = 'category')
# MELD_2 and Lille
NRI_M2L <- nribin(event = event, p.std = p.MELD2, p.new = p.LILLE, cut = cut_meld2, niter = 0, updown = 'category')
# CLIF-C ACLF and Lille
NRI_CL <- nribin(event = event, p.std = p.CLIF, p.new = p.LILLE, cut = cut_clif, niter = 0, updown = 'category')
# MELD_1 and CLIF-C ACLF
NRI_MC <- nribin(event = event, p.std = p.MELD, p.new = p.CLIF, cut = cut_meld, niter = 0, updown = 'category')
# MELD_2 and CLIF-C ACLF
NRI_M2C <- nribin(event = event, p.std = p.MELD2, p.new = p.CLIF, cut = cut_meld2, niter = 0, updown = 'category')
# Lille and MELD_1
NRI_LM <- nribin(event = event, p.std = p.LILLE, p.new = p.MELD, cut = cut_lille, niter = 0, updown = 'category')
# Lille and MELD_2
NRI_LM2 <- nribin(event = event, p.std = p.LILLE, p.new = p.MELD2, cut = cut_lille, niter = 0, updown = 'category')
# Lille and CLIF-C ACLf
NRI_LC <- nribin(event = event, p.std = p.LILLE, p.new = p.CLIF, cut = cut_lille, niter = 0, updown = 'category')
# CLIF-C ACLF and MELD_1
NRI_CM <- nribin(event = event, p.std = p.CLIF, p.new = p.MELD, cut = cut_clif, niter = 0, updown = 'category')
# CLIF-C ACLF and MELD_2
NRI_CM2 <- nribin(event = event, p.std = p.CLIF, p.new = p.MELD2, cut = cut_clif, niter = 0, updown = 'category')
