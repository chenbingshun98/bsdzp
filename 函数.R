f <- function(){
  x <- seq(0, 2*pi, length = 50)
  y1 <- sin(x)
  y2 <- cos(x)
  plot(x, y1,
       type = 'l',
       lwd = 2,
       col = 'red',
       xlab = 'x',
       ylab = "")
  lines(x, y2,
        lwd = 2,
        col = 'blue',
        abline(h = 0, col = 'gray'))
}
f()

f <- function(x) 1/sqrt(1 + x^2)
f(c(-1, 0, 1, 2))

skewness <- function(x) {
  n <- length(x)
  xbar <- mean(x)
  S <- sd(x)
  n/(n-1)/(n-2)*sum( (x - xbar)^3 ) / S^3
}

#
fib1 <- function(n){
  if(n == 0) return(0)
  else if(n == 1) return(1)
  else if(n >= 2){
    Recall(n - 1) + Recall(n - 2)
  }
}

for (i in 0:10) {
  cat("i =", i, "x[i] =", fib1(i), "\n")
}

#向量化
f <- function(x){
  x^2
}

g <- function(x){
  if(abs(x) <= 1){
    y <- x ^ 2
  }else{
    y <- 1
  }
  y
}
