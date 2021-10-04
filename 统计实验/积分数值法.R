# 计算 pi 值
pi # pi 是 R 的常变量，可以直接调用
options(digits=22) # 为了显示更多位数
options(scipen=100) # 防止科学计数法
# 算法 1
f <- function(x){ sqrt(1-x^2) }
# 1-直接用 R 的积分函数
y = integrate(f,0,1)
( z1.1 = y$value * 4 )
# 2-采用梯形公式逼近
xi <- function(i,a,b,n){ a + i*(b-a)/n }
n=100000; a=0; b=1
y = ( f(a)+f(b) )/2 + sum( f( sapply(1:(n-1),xi,a,b,n) ) )
y = y * ( (b-a)/n )
( z1.2 = y * 4 )
# 3-采用辛普森公式逼近
xi <- function(i,a,b,n){ a + i*(b-a)/n }
n=10000000; a=0; b=1
ya = f(a)+f(b)
yb = 4 * sum( f( sapply(1:(n-1)+0.5,xi,a,b,n) ) )
yc = 2 * sum( f( sapply(1:(n-1),xi,a,b,n) ) )
yd = (b-a)/(6*n)
y = (ya+yb+yc)*yd
z1.3 = y * 4 
# 4-误差比较
z1.1
z1.2
z1.3
c( pi-z1.1, pi-z1.2, pi-z1.3 )

