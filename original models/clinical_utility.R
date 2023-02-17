# # This script performs all calculations for decision curve analysis. 
# 
# # add necessary functions to the directory
# source("nb_diff.R")
# source("dca.R") # dca.R is taken from decisioncurveanalysis.org 
# 
# # Since decision curves look better when working with mortality probability rather than survival, first
# # define variables corresponding to 90-day mortality
# stph.c$clif.mort <- 1 - stph.c$CLIF.surv
# stph.c$meld.mort <- 1 - stph.c$MELD.surv
# stph.c$meld.mort2 <- 1 - stph.c$MELD.surv2
# stph.c$lille.mort <- 1 - stph.c$Lille.surv
# stph.c$meld3.mort <- 1 - stph.c$MELD3.surv
# 
# # Perform and plot DCA for the original models 
# full_dca <- dca(data = stph.c, outcome = "D90_DTH", predictors = c("clif.mort", "meld.mort", "meld.mort2", "lille.mort"), xstop = 0.75)
# nb_data <- full_dca$net.benefit
# 
# plot(nb_data$threshold, nb_data$none, type = "l", lwd = 2, xlab = "Threshold mortality probability", ylab = "Net benefit", ylim = c(-0.05, 0.25))
# lines(nb_data$threshold, nb_data$all, type = "l", col = 8, lwd = 2)
# lines(nb_data$threshold, nb_data$meld.mort, type = "l", col = "darkblue", lwd = 2)
# lines(nb_data$threshold, nb_data$meld.mort2, type = "l", col = "darkgreen", lwd = 2)
# lines(nb_data$threshold, nb_data$clif.mort, type = "l", col = "darkred", lwd = 2)
# lines(nb_data$threshold, nb_data$lille.mort, type = "l", col = "orange", lwd = 2)
# # Add a legend
# legend("topright", cex = 0.8, legend = c("Treat none", "Treat all", "MELD_1", "MELD_2", "CLIF-C ACLF", "Lille"),
#        col = c(17, 8, "darkblue", "darkgreen", "darkred", "orange"), lwd = c(2, 2, 2, 2, 2, 2))
# 
# # Create a table of NB values for a set of reasonable threshold probabilities
# table_output <- dca(data = stph.c, outcome = "D90_DTH", 
#                     predictors = c("clif.mort", "meld.mort", "lille.mort"), 
#                     xstart = 0.25, xstop = 0.75, xby = 0.10, graph = F)
# table_output
# 
# ######
# # Formally compare the NB values for different values of the threshold probability using a bootstrap approach
# # The code is based on that proposed by Zhang et al., 2018
# library(boot)
# set.seed(34) 
# R <- 500 # Number of bootstrap samples
# 
# # Compare the CLIF-C ACLF and MELD models
# boot.diff.cm <- boot(data=stph.c, statistic = nb_diff,
#                     R = R, outcome = "D90_DTH", pred1 = "clif.mort",
#                     pred2 = "meld.mort", xstart = 0.25, xstop = 0.75,
#                     step = 0.05)
# pvalue.cm <- NULL
# 
# for(i in 1:length(boot.diff.cm$t0)){
#   pvalue.cm <- c(pvalue.cm, mean(abs(boot.diff.cm$t[,i] - boot.diff.cm$t0[i]) > abs(boot.diff.cm$t0[i])))
# }
# 
# # Compare the MELD and Lille model
# boot.diff.ml <- boot(data=stph.c, statistic = nb_diff,
#                      R = R, outcome = "D90_DTH", pred1 = "lille.mort",
#                      pred2 = "meld.mort", xstart = 0.25, xstop = 0.75,
#                      step = 0.05)
# pvalue.ml <- NULL
# 
# for(i in 1:length(boot.diff.ml$t0)){
#   pvalue.ml <- c(pvalue.ml, mean(abs(boot.diff.ml$t[,i] - boot.diff.ml$t0[i]) > abs(boot.diff.ml$t0[i])))
# }
# 
# # Compare the CLIF-C ACLF and Lille model
# boot.diff.cl <- boot(data=stph.c, statistic = nb_diff,
#                      R = R, outcome = "D90_DTH", pred1 = "clif.mort",
#                      pred2 = "lille.mort", xstart = 0.25, xstop = 0.75,
#                      step = 0.05)
# pvalue.cl <- NULL
# 
# for(i in 1:length(boot.diff.cl$t0)){
#   pvalue.cl <- c(pvalue.cl, mean(abs(boot.diff.cl$t[,i] - boot.diff.cl$t0[i]) > abs(boot.diff.cl$t0[i])))
# }
# 
# # Compare CLIF-C ACLF with the MELD_2
# boot.diff.cm2 <- boot(data=stph.c, statistic = nb_diff,
#                      R = R, outcome = "D90_DTH", pred1 = "clif.mort",
#                      pred2 = "meld.mort2", xstart = 0.25, xstop = 0.75,
#                      step = 0.05)
# pvalue.cm2 <- NULL
# 
# for(i in 1:length(boot.diff.cm2$t0)){
#   pvalue.cm2 <- c(pvalue.cm2, mean(abs(boot.diff.cm2$t[,i] - boot.diff.cm2$t0[i]) > abs(boot.diff.cm2$t0[i])))
# }
# 
# # Compare Lille and the MELD_2
# boot.diff.m2l <- boot(data=stph.c, statistic = nb_diff,
#                      R = R, outcome = "D90_DTH", pred1 = "lille.mort",
#                      pred2 = "meld.mort2", xstart = 0.25, xstop = 0.75,
#                      step = 0.05)
# pvalue.m2l <- NULL
# 
# for(i in 1:length(boot.diff.m2l$t0)){
#   pvalue.m2l <- c(pvalue.m2l, mean(abs(boot.diff.m2l$t[,i] - boot.diff.m2l$t0[i]) > abs(boot.diff.m2l$t0[i])))
# }
# 
# # Compare both MELD options
# boot.diff.mm <- boot(data=stph.c, statistic = nb_diff,
#                      R = R, outcome = "D90_DTH", pred1 = "meld.mort2",
#                      pred2 = "meld.mort", xstart = 0.25, xstop = 0.75,
#                      step = 0.05)
# pvalue.mm <- NULL
# 
# for(i in 1:length(boot.diff.mm$t0)){
#   pvalue.mm <- c(pvalue.mm, mean(abs(boot.diff.mm$t[,i] - boot.diff.mm$t0[i]) > abs(boot.diff.mm$t0[i])))
# }
# 
# # Append p-values together in a dataframe for simplicity
# pvalues_dca <- data.frame("threshold probability" = seq(from = 0.25, to = 0.75, by = 0.05),
#                         "p-values Meld-CLIF" = pvalue.cm,
#                         "p-values MELD-Lille" = pvalue.ml,
#                         "p-values CLIF-Lille" = pvalue.cl,
#                         "p-values MELD2-CLIF" = pvalue.cm2,
#                         "p-values MELD2-Lille" = pvalue.m2l,
#                         "p-values MELD-MELD2" = pvalue.mm)
