f <- function(x) x^2
curve(f, from = -4, to = 4)
install.packages("DescTools")
library(DescTools)
#加红色影线
Shade(f, breaks =  c(-4,4), col = "red")
#加网格
grid()

query_points <- runif(n = 2000, min =  -4, max = 4)
8*mean(f(query_points))

area_estimates <- vector(length = 10000)
for(i in 1:1000){
  query_points <- runif(n = i, min = -4, max = 4)
  area_estimates[i] <- 8*mean(f(query_points))
}
plot(area_estimates)
abline(h = 18, col = "red", lwd = 2)


s.f <- function(N){
  x <- runif(N, 0, 1)
  y <- runif(N, 0, 1)
  n <- length(x[which(y < x ^ 2)])#计算落入区域的点数
  return(n / N)
}
s.f(1000000)

f <- function(x){
  f(x) = x ^ 2
}

s.f <- function(N){
  a = 2
  b = 5
  x <- runif(N, a, b)
  c <- min(f(x))
  d <- max(f(x))
  y <- runif(N, 0, d)
  n <- length(x[which(y < f(x))])
  s.j <- (b - a) * d
  return(s.j * n / N)
}
s.f(100000)

#
g <- function(x,y) x^2 + y^2
x <- y <- seq(-3, 3, length = 100)
z <- outer(x, y, g)
persp(x, y, z)

library(dplyr)
library(plotly)
plot_ly(x=x,y=y,z=z) %>% add_surface()

query_points_x <- runif(n = 1000, min = -3, max = 3)
query_points_y <- runif(n = 1000, min = -3, max = 3)
36*mean(g(query_points_x,query_points_y))

volume_estimates <- vector(length = 10000)
for (i in 1:10000) {
  query_points_x <- runif(n = i, min = -3, max = 3)
  query_points_y <- runif(n = i, min = -3, max = 3)
  volume_estimates[i] <- 36*mean(g(query_points_x,query_points_y))
}
plot(volume_estimates)
abline(h = 216, col = "red", lwd = 2)

h <- Vectorize(function(x,y){
  if(x^2 + y^2 <=1){
    return(1)
  }
  0
},vectorize.args = c("x","y"))
h(0,0)
h(c(0,1),c(0,1))

x <- y <- seq(-1, 1, length = 400)
z <- outer(x, y, h)
plot_ly(x = x, y = y ,z = z) %>% add_surface()

volume_estimates <- vector(length = 4000)
for (i in 1:4000) {
  query_points_x <- runif(n = i, min = -1, max = 1)
  query_points_y <- runif(n = i, min = -1, max = 1)
  volume_estimates[i] <- 4*mean(h(query_points_x,query_points_y))
}
plot(volume_estimates)
abline(h = pi, col = "red", lwd = 2)


