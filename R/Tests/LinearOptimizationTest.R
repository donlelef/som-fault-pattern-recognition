x <- 1:10
y <- x + c(-0.5,0.5)

polyfit <- function(i) x <- AIC(lm(y~poly(x,i)))
as.integer(optimize(polyfit,interval = c(1,length(x)-1))$minimum)