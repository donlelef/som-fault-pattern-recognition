#' This function uses Akaike's an information criterion to find the best linear
#' model in a given range of orders. Then returns the best model.
#' The variables in the returned model are named x and y, so you should use
#' these names if yuo want to call a predict on the model.
#' 
#' @title Find the best model according to AIC
#' @export
#' @param interval: a vector containing the lower and the upper limits to the order of the models
#' @param x: the indipendent variable of the model
#' @param y: the dipendent variable of the model, ie the observations
#' @import stats
#' @return the fitted model (as returned by lm()).
#' @seealso lm()

findBestModel = function (x, y, interval) {
  
  # import
  library(stats) # Required for lm
  
  # Find the best model using AIC
  polyfit = function(i) merit = AIC(lm(y~poly(x,i)))
  best = as.integer(optimize(f = polyfit, interval = c(interval[1], interval[2]), maximum = FALSE)$minimum)
  
  # Consider only the best model and find the best bandwidth - 
  # ie. the one which causes the minimun error 
  bestFit = lm(y~poly(x, best))
  
  return(bestFit)
}