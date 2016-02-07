#' This function turns fault coordinates in indexes to fill a fault map.
#'
#' @title Turn fault coordinates in indexes.
#' @export
#' @param coords a matrix containing a couple of coordinates in each line
#' @param features a list containing the dimensional features of the wafer. Required fields are:
#' dieWidth: the width of a chip
#' dieHeight: the height of a chip
#' waferRay: the ray of the wafer
#' @param grid a list of two vectors:
#' x: x coordinates of the centers of the chips on the wafer.
#' y: y coordinates of the centers of the chips on the wafer.
#' @return a matrix whose rows are (i,j) index of faulty chips.


indexFromCoord = function(coords, features, grid){

  index = matrix(nrow = nrow(coords), ncol = ncol(coords))
  for(i in 1:nrow(coords)){
    index[i, 1] = which(abs(grid$x - coords[i, 1]) < features$dieWidth/2)
    index[i, 2] = which(abs(grid$y - coords[i, 2]) < features$dieHeight/2)
  }
  return(index)

}
