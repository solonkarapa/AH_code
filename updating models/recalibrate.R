# This script performs recalibration of models using the method proposed by Steyerberg et al. (2004).
# For each prognostic model (MELD, Lille and CLIF-C ACLF), a logistic regression model is used to 
# find recalibration parameters and the survival probability is re-calculated using these parameters. 
library(rms)
library(pROC)
source("val_prob_confidence.R")

# Set random seed
set.seed(34)

######
# Split complete-case sample in training and test observations
dt <- sort(sample(nrow(stph.c), nrow(stph.c)*.8))
test.data.c <- stph.c[dt,]
train.data.c <- stph.c[-dt,]

# Fit a logistic regression model on the training data
meld_regr <- glm(D90_surv ~ MELD.calc, family = binomial, data = train.data.c)

# Save the regression coefficients (alpha and beta)
ic_meld_c <- meld_regr$coefficients[1]
slope_meld_c <- meld_regr$coefficients[2]

# Calculate adjusted MELD score and survival probability on training set
train.data.c$updated.meld <- ic_meld_c + slope_meld_c*train.data.c$MELD.calc
train.data.c$meld.surv.updated <- 1/(1+exp(-train.data.c$updated.meld))

# Calculate adjusted scores on the test set and assess performance
test.data.c$updated.meld <- ic_meld_c + slope_meld_c*test.data.c$MELD.calc
test.data.c$meld.surv.updated <- 1/(1+exp(-test.data.c$updated.meld))

roc_meld_uc <- roc(test.data.c$D90_surv, test.data.c$meld.surv.updated)
ci.auc(roc_meld_uc)

val.prob(test.data.c$meld.surv.updated, test.data.c$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability",
         legendloc = FALSE, statloc = FALSE)

# Lille score re-calibration
# Run logistic regression and save regression coefficientss
lille_regr <- glm(D90_surv ~ LILLE, family = binomial, data = train.data.c)

ic_lille_c <- lille_regr$coefficients[1]
slope_lille_c <- lille_regr$coefficients[2]

# Update scores in training set
train.data.c$updated.lille <- ic_lille_c + slope_lille_c*train.data.c$LILLE
train.data.c$lille.surv.updated <- 1/(1 + exp(-train.data.c$updated.lille))

# Assess performance on the test set
test.data.c$updated.lille <- ic_lille_c + slope_lille_c*test.data.c$LILLE
test.data.c$lille.surv.updated <- 1/(1 + exp(-test.data.c$updated.lille))

roc_lille_uc <- roc(test.data.c$D90_surv, test.data.c$lille.surv.updated)
ci.auc(roc_lille_uc)

val.prob(test.data.c$lille.surv.updated, test.data.c$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability",
         legendloc = FALSE, statloc = FALSE)

# CLIF-C ACLF score re-calibration
# Run logistic regression and save regression coefficientss
clif_regr <- glm(D90_surv ~ CLIF.C, family = binomial, data = train.data.c)

ic_clif_c <- clif_regr$coefficients[1]
slope_clif_c <- clif_regr$coefficients[2]

# Update scores in training set
train.data.c$updated.clif <- ic_clif_c + slope_clif_c*train.data.c$CLIF.C
train.data.c$clif.surv.updated <- 1/(1 + exp(-train.data.c$updated.clif))

# Assess performance on the test set
test.data.c$updated.clif <- ic_clif_c + slope_clif_c*test.data.c$CLIF.C
test.data.c$clif.surv.updated <- 1/(1 + exp(-test.data.c$updated.clif))

roc_clif_uc <- roc(test.data.c$D90_surv, test.data.c$clif.surv.updated)
ci.auc(roc_clif_uc)

val.prob(test.data.c$clif.surv.updated, test.data.c$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability", 
         legendloc = FALSE, statloc = FALSE)

# Confidence intervals/SEs for the slope and intercept
# val_prob_confidence is used to get SEs for the calibration intercepts and slopes (and CIs in the plots)
val_prob_confidence(test.data.c$meld.surv.updated, test.data.c$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
                    xlab = "Predicted survival probability", ylab = "Actual survival probability",
                    legendloc = T, roundstats = 3)

val_prob_confidence(test.data.c$clif.surv.updated, test.data.c$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
                    xlab = "Predicted survival probability", ylab = "Actual survival probability",
                    legendloc = T, dostats = TRUE, roundstats = 3)

val_prob_confidence(test.data.c$lille.surv.updated, test.data.c$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
                    xlab = "Predicted survival probability", ylab = "Actual survival probability",
                    legendloc = T, dostats = TRUE)

######
# Sensitivity analysis; split data on full stph sample and then select the complete cases per model
# Split full sample in training and test observations
dt <- sort(sample(nrow(stph), nrow(stph)*.8))
test.data <- stph[dt,]
train.data <- stph[-dt,]

# MELD score re-calibration
test.meld <- stph.meld[stph.meld$Subject %in% test.data$Subject,]
train.meld <- stph.meld[stph.meld$Subject %in% train.data$Subject,]

# Fit a logistic regression model on the training data
meld_regr <- glm(D90_surv ~ MELD.calc, family = binomial, data = train.meld)

# Save the regression coefficients (alpha and beta)
ic_meld <- meld_regr$coefficients[1]
slope_meld <- meld_regr$coefficients[2]

# Calculate adjusted MELD score and survival probability on training set
train.meld$updated.meld <- ic_meld + slope_meld*train.meld$MELD.calc
train.meld$meld.surv.updated <- 1/(1+exp(-train.meld$updated.meld))

# Calculate adjusted scores on the test set and assess performance
test.meld$updated.meld <- ic_meld + slope_meld*test.meld$MELD.calc
test.meld$meld.surv.updated <- 1/(1+exp(-test.meld$updated.meld))

roc_meld_u <- roc(test.meld$D90_surv, test.meld$meld.surv.updated)
ci.auc(roc_meld_u)

val.prob(test.meld$meld.surv.updated, test.meld$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability",
         legendloc = FALSE, statloc = FALSE)

# Finally, add the updated scores to the full MELD df
stph.meld$updated.meld <- ic_meld + slope_meld*stph.meld$MELD.calc
stph.meld$meld.surv.updated <- 1/(1+exp(-stph.meld$updated.meld))

# Lille score re-calibration
# Split Lille dataframe into training and test
test.lille <- stph.lille[stph.lille$Subject %in% test.data$Subject,]
train.lille <- stph.lille[stph.lille$Subject %in% train.data$Subject,]

# Run logistic regression and save regression coefficientss
lille_regr <- glm(D90_surv ~ LILLE, family = binomial, data = train.lille)

ic_lille <- lille_regr$coefficients[1]
slope_lille <- lille_regr$coefficients[2]

# Update scores in training set and assess performance
train.lille$updated.lille <- ic_lille + slope_lille*train.lille$LILLE
train.lille$lille.surv.updated <- 1/(1 + exp(-train.lille$updated.lille))

# Assess performance on the test set
test.lille$updated.lille <- ic_lille + slope_lille*test.lille$LILLE
test.lille$lille.surv.updated <- 1/(1 + exp(-test.lille$updated.lille))

roc_lille_u <- roc(test.lille$D90_surv, test.lille$lille.surv.updated)
ci.auc(roc_lille_u)

val.prob(test.lille$lille.surv.updated, test.lille$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability",
         legendloc = FALSE, statloc = FALSE)

# Finally, add the updated scores to the full Lille df
stph.lille$updated.lille <- ic_lille + slope_lille*stph.lille$LILLE
stph.lille$lille.surv.updated <- 1/(1 + exp(-stph.lille$updated.lille))

# CLIF-C ACLF score re-calibration
# Split CLIF-C ACLF dataframe into training and test
test.clif <- stph.clif[stph.clif$Subject %in% test.data$Subject,]
train.clif <- stph.clif[stph.clif$Subject %in% train.data$Subject,]

# Run logistic regression and save regression coefficientss
clif_regr <- glm(D90_surv ~ CLIF.C, family = binomial, data = train.clif)

ic_clif <- clif_regr$coefficients[1]
slope_clif <- clif_regr$coefficients[2]

# Update scores in training set and assess performance
train.clif$updated.clif <- ic_clif + slope_clif*train.clif$CLIF.C
train.clif$clif.surv.updated <- 1/(1 + exp(-train.clif$updated.clif))

# Assess performance on the test set
test.clif$updated.clif <- ic_clif + slope_clif*test.clif$CLIF.C
test.clif$clif.surv.updated <- 1/(1 + exp(-test.clif$updated.clif))

roc_clif_u <- roc(test.clif$D90_surv, test.clif$clif.surv.updated)
ci.auc(roc_clif_u)

val.prob(test.clif$clif.surv.updated, test.clif$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         xlab = "Predicted survival probability", ylab = "Actual survival probability", 
         legendloc = FALSE, statloc = FALSE)

# Finally, add the updated scores to the full CLIF-C ACLF df
stph.clif$updated.clif <- ic_clif + slope_clif*stph.clif$CLIF.C
stph.clif$clif.surv.updated <- 1/(1 + exp(-stph.clif$updated.clif))

# Confidence intervals/SEs for the slope and intercept
# val_prob_confidence is used to get SEs for the calibration intercepts and slopes (and CIs in the plots)
val_prob_confidence(test.meld$meld.surv.updated, test.meld$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
                    xlab = "Predicted survival probability", ylab = "Actual survival probability",
                    legendloc = T, dostats = TRUE, roundstats = 3)

val_prob_confidence(test.clif$clif.surv.updated, test.clif$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
                    xlab = "Predicted survival probability", ylab = "Actual survival probability",
                    legendloc = T, dostats = TRUE, roundstats = 3)

val_prob_confidence(test.lille$lille.surv.updated, test.lille$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
                    xlab = "Predicted survival probability", ylab = "Actual survival probability",
                    legendloc = T, dostats = TRUE, roundstats = 3)


