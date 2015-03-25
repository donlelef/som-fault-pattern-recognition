# Gaussiana monovariata

# x è un vettore equispaziato (linspace in Matlab)
x <- seq(-4, 4, length=100);

# Normal distribution
distribution <- dnorm(x);

# Linear plot of the distribution ("type" option binds continuous line)
plot(distribution, type = 'l');



# Bivariate gaussian distribution plot

library(rgl)  #Needed for 3D plot
library(mvtnorm) #Needed for dmvnorm()
library(plot3D) #Needed for contour2D()

# Initializing parameters
mu = c(0, 0)
sigma = matrix(data = c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)

#Calcuate f(x) for a large number of possible values for x1 and x2
x1 = seq(from = -5, to = 5, length.out = 100)
x2 = seq(from = -5, to = 5, length.out = 100)
grid = expand.grid(x1, x2) #Creates all possible combinations - like meshgrid
densityVector = dmvnorm(x = grid, mean = mu, sigma = sigma, log = FALSE)

#Arrange values in the following form:
#         x2
#         17.0 17.1 17.2  ...
#x1 11.0  f(x) f(x) f(x)
#   11.1  f(x) f(x) f(x)
#   11.2  f(x) f(x) f(x)
#   ...
fx = matrix(data = densityVector, nrow = length(x1), ncol = length(x2), byrow = FALSE)

#3D plot
persp3d(x = x1, y = x2, z = fx, col = "green", xlab = "x1",
        ylab = "x2", zlab = "f(x)", xlim = c(min(x1),max(x1)), ylim = c(min(x2), max(x2))) 
#Plane at f(x) = 0.14 - helps to see what the contour plot gives
#persp3d(x = c(10, 20), y = c(15,25), z = matrix(data = c(0.14, 0.14, 0.14, 0.14), nrow = 2, ncol = 2),
#   col = "blue", add = TRUE)
#Plane at f(x) = 0.12 - helps to see what the contour plot gives
#persp3d(x = c(10, 20), y = c(15,25), z = matrix(data = c(0.12, 0.12, 0.12, 0.12), nrow = 2, ncol = 2),
#   col = "red", add = TRUE)

#3D plot with surf3D()
grid = mesh(x1, x2)
X = grid$x
Y = grid$y
surf3D(x = X, y = Y, z = fx, phi = 30, theta = 45, bty = "b2", 
       lighting = TRUE, xlim = c(min(x1),max(x1)), ylim = c(min(x2), max(x2)),
       main = "Bivariate Normal Distribution", sub = bquote(bold(mu[1])==.(mu[1])~
       ", "~sigma[1]==.(sigma[1,1])~", "~mu[2]==.(mu[2])~", "~sigma[2]==.(sigma[2,2])~
       ", "~sigma[xy]==.(sigma[2,1]))) 

# Standard 3D plot - not using additional packages
persp(x = x1, y = x2, z = fx, main = "Bivariate Normal Distribution",
      sub = bquote(bold(mu[1])==.(mu[1])~", "~sigma[1]==.(sigma[1,1])~", "~mu[2]==.(mu[2])~
                     ", "~sigma[2]==.(sigma[2,2])~", "~sigma[xy]==.(sigma[2,1])),
      col="orchid2", theta = 55, phi = 30, r = 40, d = 0.1, expand = 0.5,
      ltheta = 90, lphi = 180, shade = 0.4, ticktype = "detailed", nticks=5)



# Contour plot - purposely made x and y-axes the same length so that one can judge variability
par(pty = "s")
contour2D(x = x1, y = x2, z = fx, xlab = expression(x[1]), ylab = expression(x[2]), 
        xlim = c(min(x1),max(x1)), ylim = c(min(x2), max(x2)),
        main = "Bivariate Normal Distribution", sub = bquote(bold(mu[1])==.(mu[1])~
        ", "~sigma[1]==.(sigma[1,1])~", "~mu[2]==.(mu[2])~", "~sigma[2]==.(sigma[2,2])~
        ", "~sigma[xy]==.(sigma[2,1])))
abline(h = seq(from = min(x1), to = max(x1), by = 1), lty = "dotted", col = "lightgray")
abline(v = seq(from = min(x2), to = max(x2), by = 1), lty = "dotted", col = "lightgray")
