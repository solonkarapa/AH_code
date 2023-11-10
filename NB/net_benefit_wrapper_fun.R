

net_benefit_treated_wrapper <- function(splits, model){
    
    x <- analysis(splits)
    
    if (model == "lille"){
        nb_original <- net_benefit_treated(x$lille.death.risk, x$D90_DTH, risk_threshold = thresholds)
        nb_update <- net_benefit_treated(x$lille.death.risk.updated, x$D90_DTH, risk_threshold = thresholds)
    } else if (model == "cliff"){
        nb_original <- net_benefit_treated(x$CLIF.death.risk, x$D90_DTH, risk_threshold = thresholds)
        nb_update <- net_benefit_treated(x$clif.death.risk.updated, x$D90_DTH, risk_threshold = thresholds)
    } else if ( model == "meld1"){
        nb_original <- net_benefit_treated(x$meld.death.risk, x$D90_DTH, risk_threshold = thresholds)
        nb_update <- net_benefit_treated(x$meld.death.risk.updated, x$D90_DTH, risk_threshold = thresholds)
    } else if ( model == "meld2"){
        nb_original <- net_benefit_treated(x$meld.death.risk2, x$D90_DTH, risk_threshold = thresholds)
        nb_update <- net_benefit_treated(x$meld.death.risk.updated, x$D90_DTH, risk_threshold = thresholds)
    }
    
    nb_original$status <- "original"
    nb_update$status <- "update"
    
    nb <- rbind(nb_original, nb_update)
    nb$model <- model
    
    return(nb)
}
