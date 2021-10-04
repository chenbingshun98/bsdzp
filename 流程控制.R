# switch(expr,list)
## [1] 1 2 3 4 5
# 列表：mean(1:10),1:5,1:10
#其中2的含义为选择第二种，即为1:5
switch(2,
       mean(1:10),
       1:5,
       1:10)

#[1] "apple
y <- "fruit"
switch (y,
  fruit = "apple",
  vegetable = "brocoli",
  meat = "beef"
)

# for(i in seq){expr}
#print 函数无法打印多个对象，所以在这里使用cat函数
n <- c(2,5,10)
for(i in n){
  x <- sqrt(i)
  cat("sqrt(",i,"):",x,"\n")#\n的正则表达式为回车
}

for(i in n){
  print(x)
}

for(i in n){
  print(i)
}

print("1\2")

for(i in 1:10){print(i)}

#while循环
# while(condition){expr}
x <- c(1,1)#生成10个斐波那契数列，这个c(1,1)为数列的两个初始值
i <- 3#从第三项开始的意思
while(i <= 10){
  x[i] <- x[i-1] + x[i-2]
  i <- i + 1
}
x

#repeat-break 循环
#repeat 是无限循环语句
#直到达到循环条件时，使用break 语句直接跳出循环
# repeat expr 或 repeat{if(condition){break}}
pv <- c(1,1,2,3,1,1,15,7,18)
i <- 1
results <- ""#空的字符串
repeat{
  if(i>length(pv))break
  if(pv[i]<=5) results[i] <- "初级用户" else
  if(pv[i]<=15) results[i] <- "中级用户" else
  results[i] <- "高级用户"
  i <- i+1
}
results

# myfun <- function(arglist){
#   statements
#   return(object)
# }

x <- c(1,2,3)
std2 <- function(x){
  if(!is.numeric(x)){
    stop("wrong input \n")
  }
  if(length(x)==1){
    stop("cannot complete calculation \n")
  }
  result <- sqrt((sum(x-mean(x))^2)/(length(x)-1))
  return(result)               
}
std2(x)

#编写函数
mystats <- function(x, parametric = TRUE, print = FALSE){
  if(parametric){#若果parametric为TRUE
    center <- mean(x)
    spread <- sd(x)
  }else{#如果parametric为FALSE
    center <- median(x)
    spread <- mad(x)
  }
  if(print & parametric){#如果两个皆为TRUE
    cat("Mean = ", center, "\n", "SD = ", spread, "\n")
  }else if(print & !parametric){#如果print为TRUE但是它不是parametric的选项的话
    cat("Median = ", center, "\n", "MAD = ", spread, "\n")
  }
  result <- list(center = center, spread = spread)
  return(result)
}

#首先要生成一些服从于正态分布的数据
set.seed(1234)
x <- rnorm(500)
y <- mystats(x)
y <- mystats(x, parametric = FALSE, print = TRUE)

###
x <- c(1:5)
for (n in x) {
  print(n + 1)
}

#while
x <- 2
while(x <= 15) x <- x + 2
x

#repeat
x <- 2
repeat{
  x <- x + 2
  if(x > 15) break
}
x

#next & break
x <- 6
sum <- 0
for(i in 1:x){
  if(i == 5)next
  sum = sum + i
}
sum

#流程控制
x <- c(T, F, F)
x

y <- c(T, T, F)
y

x&&y#只判断第一个元素

x&y

for(i in c(-1:3, 9)) print(switch(i, "壹", "贰" , "叁", "肆"))

stock<-c("600292","600116")

for(i in stock){
  print(switch (i,
                "600292" = "九州龙电",
                "600116" = "三峡水利",
                "600452" = "涪陵电力",
                "600098" = "广州控股",
                "601991" = "大唐发电",
                "009508" = "东方发电"
  ))
}
