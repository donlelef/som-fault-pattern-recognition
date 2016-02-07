library(RUnit)
source("Scripts/Initializer.R")

test.suite <- defineTestSuite(name = "KDETests",
                              dirs = file.path("tests/tests"),
                              testFileRegexp = '^test_.*\\.R$')

test.result <- runTestSuite(test.suite)
printTextProtocol(test.result)

