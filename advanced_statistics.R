#Chapter 1 Getting and Getting Acquainted with R
library(ggplot2)

x <- seq(-10, 10, by = 0.01)
qplot(x = x, y = x^2, geom = "line")
qplot(x = x, y = sin(x), geom = "line")
i <- rnorm(1000)
qplot(x = i, geom = "histogram")
hist(i, breaks = 100)
