# Porting of FaultMapPlot from Matlab to R


# Import required libraries
library(mvtnorm) #Needed for dmvnorm()

# Initiale parameters
mu = c(0, 0)
sigma = matrix(data = c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
maximumFaultProbability = 0.2

#Calcuate f(x) for a large number of possible values for x1 and x2
x1 = seq(from = -5, to = 5, length.out = 100)
x2 = seq(from = -5, to = 5, length.out = 100)
grid = expand.grid(x1, x2) #Creates all possible combinations - like meshgrid
densityVector = dmvnorm(x = grid, mean = mu, sigma = sigma, log = FALSE)

#Arrange values in the following form:
#         x2
#         -5   -4.9 -4.8
#x1 -5    f(x) f(x) f(x)
#   -4.9  f(x) f(x) f(x)
#   -4.8  f(x) f(x) f(x)
#   ...
Z = matrix(data = densityVector, nrow = length(x1), ncol = length(x2), byrow = FALSE)

#3D plot with surf3D()
grid = mesh(x1, x2)
X = grid$x
Y = grid$y
surf3D(x = X, y = Y, z = Z,  
       xlim = c(min(x1),max(x1)), ylim = c(min(x2), max(x2)),
       lighting = TRUE, phi = 30, theta = 45, bty = "b2",
       main = "Bivariate Normal Distribution", sub = bquote(bold(mu[1])==.(mu[1])~
       ", "~sigma[1]==.(sigma[1,1])~", "~mu[2]==.(mu[2])~", "~sigma[2]==.(sigma[2,2])~
       ", "~sigma[xy]==.(sigma[2,1])))

# Fill a square grid with 0s or 1s according to the probabilityFuncion 
# The higher is the value probabilityFunction takes in a cetrain point,the
# higher is the probability that pixel is set to 1.
faultMap = matrix(data = 0, nrow = length(x1), ncol = length(x2))
for (i in 1:length(x1)){
  for (j in 1:length(x2)){
   if (runif(n = 1, min = 0, max = 1) < (Z[i,j]/max(Z))*maximumFaultProbability){
    faultMap[i,j] = 1 
   }
    else{
      faultMap[i,j] = 0
    } 
  }
}

# Compute the fault number
faultNumber = length(faultMap[faultMap==1])

# Plot the fault map
par(pty = "s") # Force a square plot
image2D(x = 1:nrow(faultMap), y = 1:ncol(faultMap), z = faultMap, border = "black") 
