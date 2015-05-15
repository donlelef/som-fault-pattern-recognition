library(RUnit)
library(KDEBenchmark)

test.bindDefectNumber = function(){
  
  testMatrix = matrix(data = c(1,0,0,1,1,1,0,0,1), nrow = 3, ncol = 3)
  bindedMatrix = bindDefectNumber(startingMatrix = testMatrix, faultValue = 1, notFaultValue = 0, faultNumber = 3)
  checkEquals(target = 3, current = faultNumber(faultMap = bindedMatrix, faultValue = 1))
  checkEquals(target = 2, current = sum(testMatrix - bindedMatrix))
  
}

