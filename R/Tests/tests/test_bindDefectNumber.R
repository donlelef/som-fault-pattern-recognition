library(RUnit)
library(KDEBenchmark)

test.bindDefectNumber = function(){
  
  probabilityMatrix = matrix(data = c(1,0,0,1,1,1,0,0,1), nrow = 3, ncol = 3)
  faults = sum(probabilityMatrix)
  bindedMatrix = bindDefectNumber(probabilityMatrix = probabilityMatrix, faultValue = 1, notFaultValue = 0, faultNumber = faults)
  checkEquals(target = faults, current = faultNumber(faultMap = bindedMatrix, faultValue = 1))
  checkEquals(target = 0, current = sum(probabilityMatrix - bindedMatrix))
  
}

