library(RUnit)
library(KDEBenchmark)

test.suite <- defineTestSuite(name = "KDETests",
                              dirs = file.path("Tests/tests"),
                              testFileRegexp = '^test_.*\\.R$')

test.result <- runTestSuite(test.suite)
printTextProtocol(test.result)

