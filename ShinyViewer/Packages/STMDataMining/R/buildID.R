#' This function builds ID from columns of a data frame: the columns of the data frame are
#' casted to character and then pasted.
#'
#' @title Build ID from columns of a data.frame.
#' @export
#' @param idsDataFrame a data.frame.
#' @param sep the separator for the values in different columns.
#' @return a vector containing the IDs.

buildID = function(idsDataFrame, sep = "/"){

  ID = as.vector(idsDataFrame[ , 1], mode = "character")
  if(ncol(idsDataFrame) > 1){
    for(i in 2:ncol(idsDataFrame)){
      ID = paste(ID, idsDataFrame[ , i], sep = "/")
    }
  }

  return(ID)

}
