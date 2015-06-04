# Animated KDE test

library(animation)
set.seed(11)
fittedCurve = vector(mode = "numeric", length = 100)
interval = seq(from = -4, to = 4, length.out = 100)
# data = rnorm(n = 100, mean = 0, sd = 1)
data = runif(100, min = min(interval), max = max(interval))
index = 1

animatedPlot = function(){
  for(randomGaussian in data){
    fittedCurve = vector(mode = "numeric", length = 100)
    for(i in 1:index){
      fittedCurve = fittedCurve + dnorm(x = interval, mean = data[i], sd = 1)/index
    }
    plot(main = paste("KDE after", index, "data", sep = " "), 
         x = interval, y = fittedCurve, ylim = c(-0.1, 0.4), xlim = c(min(interval), max(interval)),
         type = "l")
    par(new = TRUE)
    abline(v = randomGaussian, col = "red")
    stripchart(x = data[c(rep(TRUE, times = index), rep(FALSE, times = length(data) - index))], 
               at = 0, pch = "x", col = "blue", xlim = c(min(interval), max(interval)), add = TRUE)
    index = index + 1 
  }
}

ani.options(interval = 0.4, nmax = length(data))
saveGIF(expr = animatedPlot(), clean = TRUE)


