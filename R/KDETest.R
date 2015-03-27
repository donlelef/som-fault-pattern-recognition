# Gaussiana monovariata
min = -5
max = 5
samples = 10000

# x è un vettore equispaziato (linspace in Matlab)
x <- seq(-5, 5, length.out = samples)

# Normal distribution
randomValues <- rnorm(n = samples, mean = 0, sd = 1)

# Linear plot of the distribution ("type" option binds continuous line)
hx <- hist(randomValues, breaks=sqrt(samples), plot=FALSE)
par(mar = c(5,5,5,5))
plot(hx, col=ifelse(abs(hx$breaks) < 1.669, 4, 2), xlim = c(min,max))

#hist(x = randomValues, main = "Real distribution vs KDE", xlim = c(min,max))

# KDE
library(KernSmooth)
bandwidth = dpik(x = randomValues)
KDE = bkde(x = randomValues, bandwidth = bandwidth, gridsize = samples)
par(new = TRUE)
plot(x = KDE$x, y = KDE$y, type = "l", xlim = c(min,max), axes = FALSE, xlab = NA, ylab = NA, col = "green")
axis(side = 4)
mtext("probabiltity", side = 4, line = 3)

