#' Perform a chi square test on a contingency matrix and saves the result in
#' a data.frame with 6 columns: 
#' VALUE is the chi sqare value
#' BAD_EQUIPMENT is the number of bad wafers processed by the equipment the contingency matrix refers to
#' GOOD_EQUIPMENT is the number of bad wafers processed by the equipment the contingency matrix refers to
#' BAD_OTHER is the number of bad wafers processed by other equipments
#' GOOD_OTHER is the number of good wafers processed by other equipments
#' P_VALUE is the p value of the chi square statistic
#'
#' @title Chi square test on contingency matrix
#' @param contingencyMatrix a contingency matrix as described in createContingencyMatrix()
#'
#' @import stats
#' @return a data.frame as in the description

chiTestEquipment = function(contingencyMatrix){
  testValue = chisq.test(x = contingencyMatrix, correct = FALSE)
  res = data.frame(VALUE = testValue$statistic,
                   BAD_EQUIPMENT = contingencyMatrix[1,1],
                   GOOD_EQUIPMENT = contingencyMatrix[2,1],
                   BAD_OTHER = contingencyMatrix[1,2],
                   GOOD_OTHER = contingencyMatrix[2,2],
                   P_VALUE = testValue$p.value,
                   stringsAsFactors = FALSE)
  return(res)
}