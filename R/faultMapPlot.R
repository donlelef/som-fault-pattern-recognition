# This script cumputes a bidimensional gaussian distribution and plots it.
# This probability function is assumed to represent the probability of a fault
# to happen on the chip in the coordinates (x1, x2).
# After that, a map is created where random faults are simulated. The value
# 0 in the map means 'no fault' and 1 means 'fault', whereas -1 indicates 
# the points out of the circular wafer.

# Import required libraries
library(mvtnorm)  # Needed for dmvnorm()
library(plot3D)   # Needed for mesh()

# Initial parameters
ray = 50
mu = c(0, 0)
sigma = matrix(data = c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
maximumFaultProbability = 0.2

#Calcuate f(x) for a large number of possible values for x1 and x2
x1 = seq(from = -5, to = 5, length.out = 2*ray)
x2 = seq(from = -5, to = 5, length.out = 2*ray)
grid = expand.grid(x1, x2) #Creates all possible combinations
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
grid = mesh(x1, x2) # Like meshgrid
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
    faultMap[i,j] = TRUE 
   }
    else{
      faultMap[i,j] = FALSE
    } 
  }
}

# Creates a circular grid from a square one by inserting the value -1 in
# every pixel which is not whitin the circle of ray "ray".

for (i in 1:nrow(faultMap)){
  for (j in 1:ncol(faultMap)){
    if ((i-ray)^2 + (j-ray)^2 >= ray^2)
      faultMap[i,j]=NA;
  }
}

# Compute the fault number
faultNumber = length(faultMap[faultMap==1])

# Plot the fault map
par(pty = "s") # Force a square plot
image2D(
        x = 1:nrow(faultMap), y = 1:ncol(faultMap), z = faultMap, border = "black", 
        grid(nx=nrow(faultMap)), ny = ncol(faultMap),
        colkey = FALSE, NAcol = "white",  col = heat.colors(2)
        ) # Colkey = FALSE: no color key legend will be added
