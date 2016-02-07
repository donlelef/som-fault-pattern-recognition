#' This function selects a subset of a data.frame's rows and and returns unique values.
#'
#' @title Subset of a data.frame's rows and and return unique values.
#' @export
#' @param columns a vector containing the names of the columns that are to be used as ids.
#' @param dataFrame a data.frame with the supposed format.
#' @return a data.frame with only the selected columns containing unique values.


getUniqueColumns = function(dataFrame, columns){

  ids = subset(x = dataFrame, select = columns)
  ids = unique(ids)
  return(ids)

}
