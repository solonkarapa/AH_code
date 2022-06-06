# This script performs all calculations of model calibration
library(rms)

# Create calibration plots and compute correspoding slopes and intercepts for all models
val.prob(stph.meld$MELD.surv, stph.meld$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)
title("Calibration plot for the MELD1 score")

val.prob(stph.meld$MELD.surv2, stph.meld$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)
title("Calibration plot for the MELD2 score")

val.prob(stph.meld$MELD3.surv, stph.meld$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)
title("Calibration plot for the MELD3.0 score")

val.prob(stph.lille$Lille.surv, stph.lille$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)
title("Calibration plot for the Lille score")

val.prob(stph.clif$CLIF.surv, stph.clif$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)
title("Calibration plot for the CLIF-C ACLF score")

# Calculate the Hosmer-Lemeshow goodness-of-fit statistic for all three models
library(ResourceSelection)

hoslem.test(stph.meld$D90_surv, stph.meld$MELD.surv)
hoslem.test(stph.meld$D90_surv, stph.meld$MELD.surv2)
hoslem.test(stph.lille$D90_surv, stph.lille$Lille.surv)
hoslem.test(stph.clif$D90_surv, stph.clif$CLIF.surv)
