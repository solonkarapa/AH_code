# function to calculate the NB of the treated i.e those 
# receiving treatment (risk > risk_threshold)
net_benefit_treated <- function(pred_y, obs_y, risk_threshold){
    # pred_y = (vector) of the predicted probabilty of outcome, must be 0 < pred_y < 1
    # obs_y = (vector) of the observed outcome, must be 1 = event and 0 = non-event 
    # risk_threshold = (vector) of the risk thresholds, must be 0 < risk_threshold < 1 
    
    # start checks
    # if (!all(pred_y >= 0) | !all(pred_y <= 1)){
    #    stop("The predicted values must range between 0 and 1")
    # }
    
    if (!all(between(pred_y, 0, 1))){
        stop("The predicted values must range between 0 and 1")
    }
    
    if (!all(obs_y == 0 | obs_y == 1)){
        stop("Outcome must be coded 0 (non event) and 1 (event)")
    }
    
    # if (!all(risk_threshold >= 0) | !all(risk_threshold <= 1)){
    #    stop("The risk threshold must range between 0 and 1")
    # }
    
    if (!all(between(risk_threshold, 0, 1))){
        stop("The risk threshold must range between 0 and 1")
    }
    
    # end checks
    
    # number of observations
    N <- length(obs_y)
    
    # prevalence Pr(obs_y = 1)
    rho = mean(obs_y == 1)
    
    # initialize empty vectors
    tpr <- numeric(length(risk_threshold))
    fpr <- numeric(length(risk_threshold))
    nb <- numeric(length(risk_threshold))
    snb <- numeric(length(risk_threshold))
    U_all <- numeric(length(risk_threshold))
    sU_all <- numeric(length(risk_threshold))
    
    # loop through each risk threshold and calcualte tpr, fpr, nb, snb, U_all, sU_all
    for(i in 1:length(risk_threshold)){
        
        # true positive rate Pr(pred_y > risk_threshold | obs_y = 1)
        tpr[i] <- sum(pred_y >= risk_threshold[i] & obs_y == 1)/sum(obs_y == 1)
        
        # false positive rate Pr(pred_y > risk_threshold | obs_y = 0)
        fpr[i] <- sum(pred_y >= risk_threshold[i] & obs_y == 0)/sum(obs_y == 0) # check again it works ok 
        
        # net benefit
        nb[i] = tpr[i] * rho - (risk_threshold[i]/(1 - risk_threshold[i])) * (1 - rho) * fpr[i]
        
        # standardized net benefit
        snb[i] = nb[i]/rho
        
        # treat all NB = (U_all - U_none)
        U_all[i] = rho - (1 - rho) * (risk_threshold[i]/(1- risk_threshold[i]))
        
        # treat all sNB 
        sU_all[i] = U_all[i]/rho
        
    }
    
    # expected utility treat none
    U_none = 0
    
    output  = data.frame("threshold" = risk_threshold,
                         "TPR" = tpr,
                         "FPR" = fpr,
                         "NB" = nb,
                         "sNB" = snb,
                         "rho" = rho,
                         "NB_none" = U_none, 
                         "NB_all" = U_all,
                         "sNB_all" = sU_all)
    return(output)
    
}
