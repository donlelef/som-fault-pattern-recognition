x <- 1:10
y <- x + c(-0.5,0.5)

plot(x,y, xlim=c(0,11), ylim=c(-1,12))

fit1 <- lm( y~offset(x) -1 )
fit2 <- lm( y~x )
fit3 <- lm( y~poly(x,3) )
fit4 <- lm( y~poly(x,9) )
library(splines)
fit5 <- lm( y~ns(x, 3) )
fit6 <- lm( y~ns(x, 9) )

fit7 <- lm( y ~ x + cos(x*pi) )

xx <- seq(0, 11, length.out=250)
lines(xx, predict(fit1, data.frame(x=xx)), col='blue')
lines(xx, predict(fit2, data.frame(x=xx)), col='green')
lines(xx, predict(fit3, data.frame(x=xx)), col='red')
lines(xx, predict(fit4, data.frame(x=xx)), col='purple')
lines(xx, predict(fit5, data.frame(x=xx)), col='orange')
lines(xx, predict(fit6, data.frame(x=xx)), col='grey')
lines(xx, predict(fit7, data.frame(x=xx)), col='black')