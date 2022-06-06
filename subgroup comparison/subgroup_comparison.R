# R Script for sub-group analysis
library(rms)
library(pROC)
library(runway)
source("nb_diff.R")
source("dca.R")

######
# Comparison of ROC between MELD3.0 and original MELD
roc_meld_u <- roc(test.meld$D90_surv, test.meld$meld.surv.updated)
roc_meld3 <- roc(stph.meld$D90_DTH, stph.meld$MELD3.surv)

roc.test(roc_meld3, roc_meld)
roc.test(roc_meld3, roc_meld_u)

######
# Subgroups per gender
# Calculations on the MELD3.0 score - first create two separate dfs for simplicity
stph.meld.males <- stph.meld[stph.meld$Gender == 0,]
stph.meld.females <- stph.meld[stph.meld$Gender == 1,]

# Calibration plots for males/females separately
val.prob(stph.meld.males$MELD3.surv, stph.meld.males$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("MELD3.0 - Males - full sample")

val.prob(stph.meld.females$MELD3.surv, stph.meld.females$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("MELD3.0 - Females - full sample")

# Discrimination for males/females separately
# Create ROC plot and calculate AUC for MELD3.0 per gender
roc_meld3_m <- roc(stph.meld.males$D90_DTH, stph.meld.males$MELD3.surv)
auc_meld3_m <- auc(roc_meld3_m)
auc_meld3_ci_m <- ci.auc(roc_meld3_m) # Confidence intervals

roc_plot(stph.meld.males, "D90_surv", "MELD3.surv", ci = TRUE, plot_title = "ROC curve for the MELD score")

roc_meld3_f <- roc(stph.meld.females$D90_DTH, stph.meld.females$MELD3.surv)
auc_meld3_f <- auc(roc_meld3_f)
auc_meld3_ci_f <- ci.auc(roc_meld3_f) # Confidence intervals

roc_plot(stph.meld.females, "D90_surv", "MELD3.surv", ci = TRUE, plot_title = "ROC curve for the MELD score")

######
# Calculations on the updated MELD score
test.meld.males <- test.meld[test.meld$Gender == 0,]
test.meld.females <- test.meld[test.meld$Gender == 1,]

# Calibration 
val.prob(test.meld.males$meld.surv.updated, test.meld.males$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated MELD - Males - test set")

val.prob(test.meld.females$meld.surv.updated, test.meld.females$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated MELD - Females - test set")

# Discrimination
roc_meld_m <- roc(test.meld.males$D90_DTH, test.meld.males$meld.surv.updated)
auc_meld_m <- auc(roc_meld_m)
auc_meld_ci_m <- ci.auc(roc_meld_m) # Confidence intervals

roc_plot(test.meld.males, "D90_surv", "meld.surv.updated", ci = TRUE, plot_title = "ROC curve for the MELD score (males)")

roc_meld_f <- roc(test.meld.females$D90_DTH, test.meld.females$meld.surv.updated)
auc_meld_f <- auc(roc_meld_f)
auc_meld_ci_f <- ci.auc(roc_meld_f) # Confidence intervals

roc_plot(test.meld.females, "D90_surv", "meld.surv.updated", ci = TRUE, plot_title = "ROC curve for the MELD score (females)")

######
# Calculations on the updated CLIF-C ACLF score
test.clif.males <- test.clif[test.clif$Gender == 0,]
test.clif.females <- test.clif[test.clif$Gender == 1,]

# Calibration 
val.prob(test.clif.males$clif.surv.updated, test.clif.males$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated CLIF - Males - test set")

val.prob(test.clif.females$clif.surv.updated, test.clif.females$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated CLIF - Females - test set")

# Discrimination
roc_clif_m <- roc(test.clif.males$D90_DTH, test.clif.males$clif.surv.updated)
auc_clif_m <- auc(roc_clif_m)
auc_clif_ci_m <- ci.auc(roc_clif_m) # Confidence intervals

roc_plot(test.clif.males, "D90_surv", "clif.surv.updated", ci = TRUE, plot_title = "ROC curve for the MELD score (males)")

roc_clif_f <- roc(test.clif.females$D90_DTH, test.clif.females$clif.surv.updated)
auc_clif_f <- auc(roc_clif_f)
auc_clif_ci_f <- ci.auc(roc_clif_f) # Confidence intervals

roc_plot(test.clif.females, "D90_surv", "meld.surv.updated", ci = TRUE, plot_title = "ROC curve for the MELD score (females)")

######
# Calculations on the updated Lille score
test.lille.males <- test.lille[test.lille$Gender == 0,]
test.lille.females <- test.lille[test.lille$Gender == 1,]

# Calibration 
val.prob(test.lille.males$lille.surv.updated, test.lille.males$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated Lille - Males - test set")

val.prob(test.lille.females$lille.surv.updated, test.lille.females$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated Lille - Females - test set")

# Discrimination
roc_lille_m <- roc(test.lille.males$D90_DTH, test.lille.males$lille.surv.updated)
auc_lille_m <- auc(roc_lille_m)
auc_lille_ci_m <- ci.auc(roc_lille_m) # Confidence intervals

roc_lille_f <- roc(test.lille.females$D90_DTH, test.lille.females$lille.surv.updated)
auc_lille_f <- auc(roc_lille_f)
auc_lille_ci_f <- ci.auc(roc_lille_f) # Confidence intervals

######
# Age stratification
hist(stph$Age.at.randomisation..calc.)
summary(stph$Age.at.randomisation..calc.)

summary(stph[stph$D90_DTH == 0,]$Age.at.randomisation..calc.) # 47.4 median age
summary(stph[stph$D90_DTH == 1,]$Age.at.randomisation..calc.) # 53.1 median age

#####
# MELD
# stratify by the median age of 49 years
test.meld.y <- test.meld[test.meld$Age.at.randomisation..calc. < 49,]
test.meld.o <- test.meld[test.meld$Age.at.randomisation..calc. >= 49,]

# Calibration of the age-stratified MELD
val.prob(test.meld.y$meld.surv.updated, test.meld.y$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated MELD - young people - test set")

val.prob(test.meld.o$meld.surv.updated, test.meld.o$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated MELD - old people - test set")

# Discrimination for the MELD
roc_meld_y <- roc(test.meld.y$D90_DTH, test.meld.y$meld.surv.updated)
auc_meld_y <- auc(roc_meld_y)
auc_meld_ci_y <- ci.auc(roc_meld_y) # Confidence intervals

roc_plot(test.meld.y, "D90_surv", "meld.surv.updated", ci = TRUE, plot_title = "ROC curve for the MELD score (males)")

roc_meld_o <- roc(test.meld.o$D90_DTH, test.meld.o$meld.surv.updated)
auc_meld_o <- auc(roc_meld_o)
auc_meld_ci_o <- ci.auc(roc_meld_o) # Confidence intervals

roc_plot(test.meld.o, "D90_surv", "meld.surv.updated", ci = TRUE, plot_title = "ROC curve for the MELD score (females)")

#####
# LILLE
# stratify by the median age of 49 years
test.lille.y <- test.lille[test.lille$Age.at.randomisation..calc. < 49,]
test.lille.o <- test.lille[test.lille$Age.at.randomisation..calc. >= 49,]

# Calibration of the age-stratified Lille
val.prob(test.lille.y$lille.surv.updated, test.lille.y$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated Lille - young people - test set")

val.prob(test.lille.o$lille.surv.updated, test.lille.o$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated Lille - old people - test set")

# Discrimination for the Lille
roc_lille_y <- roc(test.lille.y$D90_DTH, test.lille.y$lille.surv.updated)
auc_lille_y <- auc(roc_lille_y)
auc_lille_ci_y <- ci.auc(roc_lille_y) # Confidence intervals

roc_lille_o <- roc(test.lille.o$D90_DTH, test.lille.o$lille.surv.updated)
auc_lille_o <- auc(roc_lille_o)
auc_lille_ci_o <- ci.auc(roc_lille_o) # Confidence intervals

#####
# CLIF-C ACLF
# stratify by the median age of 49 years
test.clif.y <- test.clif[test.clif$Age.at.randomisation..calc. < 49,]
test.clif.o <- test.clif[test.clif$Age.at.randomisation..calc. >= 49,]

# Calibration of the age-stratified CLIF
val.prob(test.clif.y$clif.surv.updated, test.clif.y$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated CLIF - young people - test set")

val.prob(test.clif.o$clif.surv.updated, test.clif.o$D90_surv, 
         pl = TRUE, smooth = FALSE, logistic.cal = TRUE, legendloc = FALSE, statloc = FALSE)
title("Recalibrated CLIF - old people - test set")

# Discrimination for the CLIF
roc_clif_y <- roc(test.clif.y$D90_DTH, test.clif.y$clif.surv.updated)
auc_clif_y <- auc(roc_clif_y)
auc_clif_ci_y <- ci.auc(roc_clif_y) # Confidence intervals

roc_clif_o <- roc(test.clif.o$D90_DTH, test.clif.o$clif.surv.updated)
auc_clif_o <- auc(roc_clif_o)
auc_clif_ci_o <- ci.auc(roc_clif_o) # Confidence intervals

######
# Clinical utility
# First make dataframes for males and females separately for overlapping observations
stph.males <- merge(test.meld.males, test.clif.males, by = "Subject") 
stph.males <- merge(stph.males, test.lille.males, by = "Subject")
stph.females <- merge(test.meld.females, test.clif.females, by = "Subject") 
stph.females <- merge(stph.females, test.lille.females, by = "Subject")

stph.males$clif.mort <- 1 - stph.males$clif.surv.updated
stph.males$meld.mort <- 1 - stph.males$meld.surv.updated
stph.males$lille.mort <- 1 - stph.males$lille.surv.updated
stph.males$meld3.mort <- 1 - stph.males$MELD3.surv

stph.females$clif.mort <- 1 - stph.females$clif.surv.updated
stph.females$meld.mort <- 1 - stph.females$meld.surv.updated
stph.females$lille.mort <- 1 - stph.females$lille.surv.updated
stph.females$meld3.mort <- 1 - stph.females$MELD3.surv

male_dca <- dca(data = stph.males, outcome = "D90_DTH", predictors = c("clif.mort", "meld.mort", "lille.mort", "meld3.mort"), xstop = 0.75)
nb_data_m <- male_dca$net.benefit

# Create figure
plot(nb_data_m$threshold, nb_data_m$none, type = "l", lwd = 2, xlab = "Threshold probability", ylab = "Net benefit", ylim = c(-0.05, 0.25))
lines(nb_data_m$threshold, nb_data_m$all, type = "l", col = 8, lwd = 2)
lines(nb_data_m$threshold, nb_data_m$meld.mort, type = "l", col = "darkblue", lwd = 2)
lines(nb_data_m$threshold, nb_data_m$clif.mort, type = "l", col = "darkgreen", lwd = 2)
lines(nb_data_m$threshold, nb_data_m$lille.mort, type = "l", col = "darkred", lwd = 2)
lines(nb_data_m$threshold, nb_data_m$meld3.mort, type = "l", col = "orange", lwd = 2)
# Add a legend
legend("topright", cex = 0.8, legend = c("Treat none", "Treat all", "MELD", "CLIF-C ACLF", "Lille", "MELD 3.0"),
       col = c(17, 8, "darkblue", "darkgreen","darkred", "orange"), lwd = c(2, 2, 2, 2, 2, 2))

female_dca <- dca(data = stph.females, outcome = "D90_DTH", predictors = c("clif.mort", "meld.mort", "lille.mort", "meld3.mort"), xstop = 0.75)
nb_data_f <- female_dca$net.benefit

# Create figure
plot(nb_data_f$threshold, nb_data_f$none, type = "l", lwd = 2, xlab = "Threshold probability", ylab = "Net benefit", ylim = c(-0.05, 0.25))
lines(nb_data_f$threshold, nb_data_f$all, type = "l", col = 8, lwd = 2)
lines(nb_data_f$threshold, nb_data_f$meld.mort, type = "l", col = "darkblue", lwd = 2)
lines(nb_data_f$threshold, nb_data_f$clif.mort, type = "l", col = "darkgreen", lwd = 2)
lines(nb_data_f$threshold, nb_data_f$lille.mort, type = "l", col = "darkred", lwd = 2)
lines(nb_data_f$threshold, nb_data_f$meld3.mort, type = "l", col = "orange", lwd = 2)
# Add a legend
legend("topright", cex = 0.8, legend = c("Treat none", "Treat all", "MELD", "CLIF-C ACLF", "Lille", "MELD 3.0"),
       col = c(17, 8, "darkblue", "darkgreen","darkred", "orange"), lwd = c(2, 2, 2, 2, 2, 2))

######
# Same procedure for age-based sub-groups
stph.young <- merge(test.meld.y, test.clif.y, by = "Subject") 
stph.young <- merge(stph.young, test.lille.y, by = "Subject")
stph.old <- merge(test.meld.o, test.clif.o, by = "Subject") 
stph.old <- merge(stph.old, test.lille.o, by = "Subject")

stph.young$clif.mort <- 1 - stph.young$clif.surv.updated
stph.young$meld.mort <- 1 - stph.young$meld.surv.updated
stph.young$lille.mort <- 1 - stph.young$lille.surv.updated

stph.old$clif.mort <- 1 - stph.old$clif.surv.updated
stph.old$meld.mort <- 1 - stph.old$meld.surv.updated
stph.old$lille.mort <- 1 - stph.old$lille.surv.updated

young_dca <- dca(data = stph.young, outcome = "D90_DTH", predictors = c("clif.mort", "meld.mort", "lille.mort"), xstop = 0.75)
nb_data_y <- young_dca$net.benefit

# Create figure
plot(nb_data_y$threshold, nb_data_y$none, type = "l", lwd = 2, xlab = "Threshold probability", ylab = "Net benefit", ylim = c(-0.10, 0.25))
lines(nb_data_y$threshold, nb_data_y$all, type = "l", col = 8, lwd = 2)
lines(nb_data_y$threshold, nb_data_y$meld.mort, type = "l", col = "darkblue", lwd = 2)
lines(nb_data_y$threshold, nb_data_y$clif.mort, type = "l", col = "darkgreen", lwd = 2)
lines(nb_data_y$threshold, nb_data_y$lille.mort, type = "l", col = "darkred", lwd = 2)
# Add a legend
legend("topright", cex = 0.8, legend = c("Treat none", "Treat all", "MELD", "CLIF-C ACLF", "Lille"),
       col = c(17, 8, "darkblue", "darkgreen","darkred"), lwd = c(2, 2, 2, 2, 2, 2))

old_dca <- dca(data = stph.females, outcome = "D90_DTH", predictors = c("clif.mort", "meld.mort", "lille.mort", "meld3.mort"), xstop = 0.75)
nb_data_o <- old_dca$net.benefit

# Create figure
plot(nb_data_o$threshold, nb_data_o$none, type = "l", lwd = 2, xlab = "Threshold probability", ylab = "Net benefit", ylim = c(-0.10, 0.25))
lines(nb_data_o$threshold, nb_data_o$all, type = "l", col = 8, lwd = 2)
lines(nb_data_o$threshold, nb_data_o$meld.mort, type = "l", col = "darkblue", lwd = 2)
lines(nb_data_o$threshold, nb_data_o$clif.mort, type = "l", col = "darkgreen", lwd = 2)
lines(nb_data_o$threshold, nb_data_o$lille.mort, type = "l", col = "darkred", lwd = 2)
# Add a legend
legend("topright", cex = 0.8, legend = c("Treat none", "Treat all", "MELD", "CLIF-C ACLF", "Lille"),
       col = c(17, 8, "darkblue", "darkgreen","darkred"), lwd = c(2, 2, 2, 2, 2, 2))





