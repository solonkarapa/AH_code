

net_benefit_treated_wrapper <- function(splits, model){
    
    x <- analysis(splits)
    
    if (model == "Lille"){
        nb_original <- net_benefit_treated(x$lille.death.risk, x$D90_DTH, risk_threshold = thresholds)
        nb_update <- net_benefit_treated(x$lille.death.risk.updated, x$D90_DTH, risk_threshold = thresholds)
    } else if (model == "CLIF-C ACLF"){
        nb_original <- net_benefit_treated(x$CLIF.death.risk, x$D90_DTH, risk_threshold = thresholds)
        nb_update <- net_benefit_treated(x$clif.death.risk.updated, x$D90_DTH, risk_threshold = thresholds)
    } else if ( model == "MELD 1"){
        nb_original <- net_benefit_treated(x$meld.death.risk, x$D90_DTH, risk_threshold = thresholds)
        nb_update <- net_benefit_treated(x$meld.death.risk.updated, x$D90_DTH, risk_threshold = thresholds)
    } else if ( model == "MELD 2"){
        nb_original <- net_benefit_treated(x$meld.death.risk2, x$D90_DTH, risk_threshold = thresholds)
        nb_update <- net_benefit_treated(x$meld.death.risk.updated, x$D90_DTH, risk_threshold = thresholds)
    }
    
    nb_original$status <- "Original"
    nb_update$status <- "Updated"
    
    nb <- rbind(nb_original, nb_update)
    nb$model <- model
    
    return(nb)
}
