# x è un vettore equispaziato (linspace in Matlab)
x <- seq(-4, 4, length=100);

# Normal distribution
distribution <- dnorm(x);

# Linear plot of the distribution
plot(distribution, type = 'l');