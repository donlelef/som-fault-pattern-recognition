# Chi test example

badEquipment = 21
goodEquipment = 29
badOther = 39
goodOther = 111

contingencyMatrix = matrix(c(badEquipment, goodEquipment, badOther, goodOther), nrow = 2, ncol = 2, byrow = FALSE)

chisq.test(contingencyMatrix)

# Useless
# determinant = det(contingencyMatrix)
# sum = sum(contingencyMatrix)
# denominator = sum(contingencyMatrix[1, ]) + sum(contingencyMatrix[2, ]) + sum(contingencyMatrix[ ,1]) + sum(contingencyMatrix[ ,2])
# 
# R = determinant * determinant * sum / denominator

