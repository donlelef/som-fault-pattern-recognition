# Install all the required packages and their dependences.

requiredPackages = c("kohonen", "plot3D", "ks", "stats", "devtools", "RColorBrewer")
for (package in requiredPackages) {
  if (!is.element(package, installed.packages()[,1]))
    install.packages(package, dep = TRUE, repos = "http://cran.rstudio.com")
  require(package, character.only = TRUE)
}

packageDirectory = "Packages/"
requiredCustomPackage = c("STMFaultPattern", "STMPlotTools", "STMWaferClassification", "STMWrapper", "STMDataMining", "STMAnalysis")
for (package in requiredCustomPackage) {
  if (!is.element(package, installed.packages()[,1])){
    packagePath = paste(packageDirectory, package, sep = "")
    install(packagePath)
  }
  require(package, character.only = TRUE)
}