#' Function that calculates the difference between net benefit and calculates the p-value for
#' formal comparison between the net benefits for different values of the threshold probability
#' The idea for this function is taken from: 
#' Zhang Z, Rousson V, Lee WC, et al. Decision curve analysis: a technical note. Ann Transl Med. 2018;6(15):308. doi:10.21037/atm.2018.07.02
#' link: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6123195/
#' 
#' @param data dataset that includes the predictor and outcome variables
#' @param ii indices parameter for bootstrap
#' @param outcome 0/1 outcome variable
#' @param pred1 first predictor for survival 
#' @param pred2 second predictor for survival 
#' @param xstart lowest value of threshold probability to be looked at
#' @param xstop highest value of threshold probability to consider
#' @param step step size for looking at different threshold probabilities

nb_diff <- function(data, ii, outcome, pred1, pred2, xstart, xstop, step){
  dd <- data[ii,]
  nb1 <- dca(data = dd, outcome = outcome, predictors = pred1, 
             xstart = 0.25, xstop = 0.75, xby = 0.05, graph = F)
  nb2 <- dca(data = dd, outcome = outcome, predictors = pred2, 
             xstart = 0.25, xstop = 0.75, xby = 0.05, graph = F)
  
  nb.diff <- nb2$net.benefit[,4] - nb1$net.benefit[,4]
  return(nb.diff)
}



