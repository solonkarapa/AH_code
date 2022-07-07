# This script performs all calculations of model calibration
library(rms)

# Create calibration plots and compute corresponding slopes and intercepts for all models
# MELD_1 survival function
val.prob(stph.meld$MELD.surv, stph.meld$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability",
         legendloc = FALSE, statloc = FALSE)

# MELD_2 survival function
val.prob(stph.meld$MELD.surv2, stph.meld$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability",
         legendloc = FALSE, statloc = FALSE)

# MELD 3.0
val.prob(stph.meld$MELD3.surv, stph.meld$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability",
         legendloc = FALSE, statloc = FALSE)

# Lille
val.prob(stph.lille$Lille.surv, stph.lille$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability",
         legendloc = FALSE, statloc = FALSE)

# CLIF-C ACLF
val.prob(stph.clif$CLIF.surv, stph.clif$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability",
         legendloc = FALSE, statloc = FALSE)

# Calculate the Hosmer-Lemeshow goodness-of-fit statistic for all models
library(ResourceSelection)

hoslem.test(stph.meld$D90_surv, stph.meld$MELD.surv)
hoslem.test(stph.meld$D90_surv, stph.meld$MELD.surv2)
hoslem.test(stph.lille$D90_surv, stph.lille$Lille.surv)
hoslem.test(stph.clif$D90_surv, stph.clif$CLIF.surv)
