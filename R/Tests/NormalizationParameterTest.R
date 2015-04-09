# This script cumputes a bidimensional gaussian distribution and plots it.
# This probability function is assumed to represent the probability of a fault
# to happen on the chip in the coordinates (x1, x2).
# After that, a map is created where random faults are simulated. The value
# FALSE in the map means 'no fault' and TRUE means 'fault', whereas NA indicates 
# a point out of the circular wafer.

# Import required libraries
library(plot3D)   # Needed for mesh()
library(KernSmooth) # Needed for KDE
library(KDEBenchmark) 
library(cubature) # Needed for integral

# Initial parameters
ray = 30
mu = c(ray, ray)
maximumFaultProbability = 0.1

#Calcuate f(x) for a large number of possible values for x1 and x2
x = seq(from = 0, to = 2*ray, length.out = 2*ray + 1)
y = seq(from = 0, to = 2*ray, length.out = 2*ray + 1)
grid = mesh(x,y)
paraboloid = function(x,y){
    2/(pi*ray^4)*((x-ray)^2+(y-ray)^2)
}
vectorParaboloid = function(x){
  if((x[1]-ray)^2 + (x[2]-ray)^2 <= ray^2){
    2/(pi*ray^4)*((x[1]-ray)^2+(x[2]-ray)^2)
  }
  else{
    0
  }
}
vol = adaptIntegrate(f = vectorParaboloid, lowerLimit = c(0, 0), upperLimit = c(2*ray, 2*ray))
Z = outer(x,y,"paraboloid")

#3D plot with surf3D()
surf3D(x = grid$x, y = grid$y,  z = Z,  
       xlim = c(min(x),max(x)), ylim = c(min(y), max(y)),
       lighting = TRUE, phi = 30, theta = 45, bty = "b2",
       main = "Normalized parabolic distribution")