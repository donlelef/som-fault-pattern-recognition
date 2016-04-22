#' This function splits a string over a specific separator and puts 
#' the tokens in a data.frame
#'
#' @title Split string and put token in a data.frame
#' @export
#' @param ids : the string, or character vector to be splitted
#' @param sep : the separatorto be considered for splitting.
#' @return a data.frame containing the tokens.

splitID = function(ids, sep = "/"){
  
  res = data.frame()
  idsSplitted = strsplit(ids, sep, fixed = TRUE)
  for (i in 1:length(idsSplitted)) {
    for (j in 1:length(idsSplitted[[i]])) {
      res[i,j] = idsSplitted[[i]][j]
    }
  }
  
  return(res)
}
