test.chiSquare = function(){
  trueMatrix = matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
  extimatedMatrix = matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
  checkEquals(target = 0, current = chiTest(trueMatrix = trueMatrix, extimatedMatrix = extimatedMatrix))
  extimatedMatrix = matrix(data = c(0,1,3,2), nrow = 2, ncol = 2)
  checkEquals(target = 0.625, current = chiTest(trueMatrix = trueMatrix, extimatedMatrix = extimatedMatrix))
  extimatedMatrix = matrix(data = c(0,1,3,NA), nrow = 2, ncol = 2)
  checkEquals(target = 0.5, current = chiTest(trueMatrix = trueMatrix, extimatedMatrix = extimatedMatrix))
}



