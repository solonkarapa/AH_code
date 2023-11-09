

# load libraries
library(tidyverse)
library(broom)
library(rsample)

threshold = seq(0.01, 0.99, 0.1)
    
NB_ci_unstrat = test.data %>%
    broom::bootstrap(100) %>% 
    do(net_benefit_treated(.$lille.surv.updated, .$D90_surv, threshold)) %>%
    group_by(threshold) %>% 
    summarize(meanNB = mean(NB),
              low = quantile(NB, alpha/2),
              high = quantile(NB, 1-alpha/2))


median_diff <- function(splits) {
    x <- analysis(splits)
    median(x$MonthlyIncome[x$Gender == "Female"]) - 
        median(x$MonthlyIncome[x$Gender == "Male"])     
}


data_test <- test.data[1:10, c("lille.surv.updated", "Lille.surv", "D90_surv")]
set.seed(353)
n_boot <- 10
bt_resamples <- bootstraps(data_test, times = n_boot)


net_benefit_treated_wrapper <- function(splits, model){
    
    x <- analysis(splits)
    
    if (model == "lille"){
        nb_original <- net_benefit_treated(x$Lille.surv, x$D90_surv, risk_threshold = threshold)
        nb_update <- net_benefit_treated(x$lille.surv.updated, x$D90_surv, risk_threshold = threshold)
        } else if (model == "cliff"){
            nb_original <- net_benefit_treated(x$CLIF.surv, x$D90_surv, risk_threshold = threshold)
            nb_update <- net_benefit_treated(x$clif.surv.updated, x$D90_surv, risk_threshold = threshold)
            } else if ( model == "meld1"){
                nb_original <- net_benefit_treated(x$meld.surv, x$D90_surv, risk_threshold = threshold)
                nb_update <- net_benefit_treated(x$meld.surv.updated, x$D90_surv, risk_threshold = threshold)
                } else if ( model == "meld2"){
                    nb_original <- net_benefit_treated(x$meld.surv2, x$D90_surv, risk_threshold = threshold)
                    nb_update <- net_benefit_treated(x$meld.surv.updated, x$D90_surv, risk_threshold = threshold)
                }
        nb_original$status <- "original"
        nb_update$status <- "update"
        
        nb <- rbind(nb_original, nb_update)
        nb$model <- model
    
        return(nb)
    }

bla <- map_df(bt_resamples$splits, net_benefit_treated_wrapper, model = "lille")
bla$boot_id <- rep(1:n_boot, each = length(threshold)*2)

ggplot(bla) +
    geom_point(aes(x = threshold, y = NB)) +
    facet_wrap(status ~ model)

bla %>% filter(threshold == 0.51)

ggplot(bt_resamples, aes(x = wage_diff)) + 
    geom_line(stat = "density", adjust = 1.25) + 
    xlab("Difference in Median Monthly Income (Female - Male)")
