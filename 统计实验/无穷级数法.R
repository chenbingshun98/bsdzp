# 计算pi值
pi # pi是R的常变量，可以直接调用
options(digits=22) # 为了显示更多位数
# 算法2
# 1-第一个公式
tayor01 <- function(n){
  tayor01a <- function(i){ (-1)^((i-1)/2) * 1/i }
  sum( sapply( 2 * 1:n - 1 ,tayor01a) ) * 4
}
tayor01(10) # 分别取不同的n进行试验
tayor01(100)
tayor01(1000)
tayor01(10000)
# 2-第二个公式：Machin公式
16*atan(0.2) - 4*atan(1/239)