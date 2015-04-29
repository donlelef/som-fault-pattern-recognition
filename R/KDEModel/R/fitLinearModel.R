#' This function fits a linear model for every grade from "lowerGrade" to "upperGrade".
#' It returns the predicted values in a given number of points, from the minimum to 
#' the maximum of the x values passed. 
#' 
#' @title Fit a series of linear model
#' @export
#' @param grades: the orders of the fitted models
#' @param newData: a vector containing the x value where the fitted model sholud be evalued.
#' @param x: the indipendent variable of the model
#' @param y: the dipendent variable of the model, ie the observations
#' @import stats
#' @return a vector containing the vectors of the predicted values.

fitLinearModels = function (x, y, grades, newData) {
  
  # import
  library(stats) # Required for lm
  
  predictions = list()
  
  for(i in 1:length(grades)){
    fit = lm(y~poly(x, grades[i]))
    prediction = predict(fit, data.frame(x = newData))
    predictions[[i]] = prediction
  }
  
  return(predictions)
}