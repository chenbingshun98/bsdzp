onenormrecursive <- function(v) {
  if (length(v) > 1) {
    sum1 <- onenormrecursive(v[1:floor(length(v) / 2)])
    sum2 <- onenormrecursive(v[(floor(length(v) / 2) + 1):length(v)])
    sumvalue <- sum1 + sum2
  }
  else {
    sumvalue <- abs(v[1])
  }
  return(sumvalue)
}
install.packages("writexl")

# 差的算法
onenormrecursive <- function(v) {
  if (length(v) > 1) {
    sum1 <- onenormrecursive(v[1:1])
    sum2 <- onenormrecursive(v[2:length(v)])
    sumvalue <- sum1 + sum2
  }
  else {
    sumvalue <- abs(v[1])
  }
  return(sumvalue)
}

fiborecursive <- function(i) {
  if (i <= 2) {
    value <- 1
  }
  else {
    value1 <- fiborecursive(i - 1)
    value2 <- fiborecursive(i - 2)
    value <- value1 + value2
  }
  return(value)
}
fiborecursive(20)

fiborecursive <- function(i){
  if(i <= 2){
    return(1)
  }
  else{
    return(fiborecursive(i-1)+fiborecursive(i-2))
  }
}