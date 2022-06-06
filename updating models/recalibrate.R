# This script performs recalibration of models using the method proposed by Janssen et al. (2008).
# For each prognostic model (MELD, Lille and CLIF-C ACLF), a logistic regression model is used to 
# find recalibration parameters and the risk is re-calculated using these parameters. 
library(rms)
library(pROC)

# Set random seed
set.seed(34)

# MELD score re-calibration 
# First split the data into training and test with 80% in training
dt = sort(sample(nrow(stph.meld), nrow(stph.meld)*.8))
test.meld <- stph.meld[dt,]
train.meld <- stph.meld[-dt,]

# Fit a logistic regression model on the training data
meld_regr <- glm(D90_surv ~ MELD.calc, family = binomial, data = train.meld)

# Save the regression coefficients (alpha and beta)
ic_meld <- meld_regr$coefficients[1]
slope_meld <- meld_regr$coefficients[2]

# Calculate adjusted MELD score and survival probability on training set
train.meld$updated.meld <- ic_meld + slope_meld*train.meld$MELD.calc
train.meld$meld.surv.updated <- 1/(1+exp(-train.meld$updated.meld))

# Calculate roc and calibration
roc(train.meld$D90_surv, train.meld$meld.surv.updated)

val.prob(train.meld$meld.surv.updated, train.meld$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)

# Calculate adjusted scores on the test set and assess performance
test.meld$updated.meld <- ic_meld + slope_meld*test.meld$MELD.calc
test.meld$meld.surv.updated <- 1/(1+exp(-test.meld$updated.meld))

roc_meld_u <- roc(test.meld$D90_surv, test.meld$meld.surv.updated)
ci.auc(roc_meld_u)

val.prob(test.meld$meld.surv.updated, test.meld$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)

# Finally, add the updated scores to the full MELD df
stph.meld$updated.meld <- ic_meld + slope_meld*stph.meld$MELD.calc
stph.meld$meld.surv.updated <- 1/(1+exp(-stph.meld$updated.meld))

# Lille score re-calibration
# Split Lille dataframe into training and test
dt = sort(sample(nrow(stph.lille), nrow(stph.lille)*.8))
test.lille <- stph.lille[dt,]
train.lille <- stph.lille[-dt,]

# Run logistic regression and save regression coefficientss
lille_regr <- glm(D90_surv ~ LILLE, family = binomial, data = train.lille)

ic_lille <- lille_regr$coefficients[1]
slope_lille <- lille_regr$coefficients[2]

# Update scores in training set and assess performance
train.lille$updated.lille <- ic_lille + slope_lille*train.lille$LILLE
train.lille$lille.surv.updated <- 1/(1 + exp(-train.lille$updated.lille))

roc(train.lille$D90_surv, train.lille$lille.surv.updated)

val.prob(train.lille$lille.surv.updated, train.lille$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)

# Assess performance on the test set
test.lille$updated.lille <- ic_lille + slope_lille*test.lille$LILLE
test.lille$lille.surv.updated <- 1/(1 + exp(-test.lille$updated.lille))

roc_lille_u <- roc(test.lille$D90_surv, test.lille$lille.surv.updated)
ci.auc(roc_lille_u)

val.prob(test.lille$lille.surv.updated, test.lille$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)

# Finally, add the updated scores to the full Lille df
stph.lille$updated.lille <- ic_lille + slope_lille*stph.lille$LILLE
stph.lille$lille.surv.updated <- 1/(1 + exp(-stph.lille$updated.lille))

# CLIF-C ACLF score re-calibration
# Split CLIF-C ACLF dataframe into training and test
dt = sort(sample(nrow(stph.clif), nrow(stph.clif)*.8))
test.clif <- stph.clif[dt,]
train.clif <- stph.clif[-dt,]

# Run logistic regression and save regression coefficientss
clif_regr <- glm(D90_surv ~ CLIF.C, family = binomial, data = train.clif)

ic_clif <- clif_regr$coefficients[1]
slope_clif <- clif_regr$coefficients[2]

# Update scores in training set and assess performance
train.clif$updated.clif <- ic_clif + slope_clif*train.clif$CLIF.C
train.clif$clif.surv.updated <- 1/(1 + exp(-train.clif$updated.clif))

roc(train.clif$D90_surv, train.clif$clif.surv.updated)

val.prob(train.clif$clif.surv.updated, train.clif$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)

# Assess performance on the test set
test.clif$updated.clif <- ic_clif + slope_clif*test.clif$CLIF.C
test.clif$clif.surv.updated <- 1/(1 + exp(-test.clif$updated.clif))

roc_clif_u <- roc(test.clif$D90_surv, test.clif$clif.surv.updated)
ci.auc(roc_clif_u)

val.prob(test.clif$clif.surv.updated, test.clif$D90_surv, pl = TRUE, smooth = FALSE, logistic.cal = TRUE,
         legendloc = FALSE, statloc = FALSE)

# Finally, add the updated scores to the full CLIF-C ACLF df
stph.clif$updated.clif <- ic_clif + slope_clif*stph.clif$CLIF.C
stph.clif$clif.surv.updated <- 1/(1 + exp(-stph.clif$updated.clif))
