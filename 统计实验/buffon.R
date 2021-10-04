#蒲丰（Buffon）随机投针方法
buffon<-function(n, l=0.8, a=1){
  k<-0
  theta<-runif(n,0, pi); x<-runif(n,0, a/2)
  for (i in 1:n){
    if (x[i]<= l/2*sin(theta[i]))
      k<-k+1
  }
  2*l*n/(k*a)
}
buffon(100000000, l=0.8, a=1)

MC1<-function(n){
  k<-0;x<- runif(n);y<-runif(n)
  for (i in 1:n){
    if (x[i]^2+y[i]^2<1)
      k<- k+1
  }
  4*k/n
}

MC1(100000)

x <- 6
sum<-0
for(i in 1:x){
if(i == 5)next #如果 x 等于 5 则跳出循环
sum=sum+i
}
sum

