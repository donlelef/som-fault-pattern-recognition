#' This function find the minimum of the predicted value of a given model.
#' The model is assumed to be described by the formula y~poly(x) 
#' 
#' @title Minimum from a model
#' @export
#' @param model: model to be optimized
#' @param interval: a vector containing the lower and th upper limit of the interval where the minimum has to be searched
#' @import stats
#' @return A list with components minimum and objective which give the location of the minimum and the value of the function at that point. 

findMinimumFromModel = function (model, interval) {
  
  # import
  library(stats) # Required for optimize
  
  modelValues = function(i) value = predict(object = model, newdata = data.frame(x = i))
  min = optimize(f = modelValues, 
                 interval = interval, 
                 maximum = FALSE)
  
  return(min)
}