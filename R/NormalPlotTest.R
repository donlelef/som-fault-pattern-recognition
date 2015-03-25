# x è un vettore equispaziato (linspace in Matlab)
x <- seq(-4, 4, length=100);

# Normal distribution
distribution <- dnorm(x);

# Linear plot (the type specifies the continuity of the line) of the distribution
plot(distribution, type = 'l');