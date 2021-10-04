# 对象？对象就是类的实例

## base R:
### S3
### S4
### reference classes(RC)

## CRAN
### R6
### R.oo
### proto

install.packages("sloop")
library(sloop)

a <- 1
attr(a, "class") <- "bar"

a

class(a)

attr(a, "class")

# sloop::otype判断何种对象
sloop::otype(a)
sloop::otype(1)

# structure 构建一个S3对象

b <- structure(2, class = "foo")
b

# 还可以使用为class(var)赋值的方式构建
x <- list(a = 1)
class(x)

sloop::otype(x)

class(x) <- 'foo'
class(x)

sloop::otype(x)

#还可以将类属性设置为向量
#为S3对象指定多个类型
c <- structure(3, class = c('bar', 'foo'))
class(c)

sloop::otype(c)

#创建泛型函数
#通常使用UseMethod()来创建一个泛型函数
person <- function(x, ...){
  UseMethod('person')
}

#定义完泛型函数之后
#`person.xxx` 定义名为xxx的方法
#person.default 定义默认方法

person.default <- function(x, ...){
  print("I am human.")
}

person.sing <- function(x, ...){
  print("I can sing.")
}

person.name <- function(x, ...){
  print(paste0("My name is ", x))
}

#如何调用？
#定义一个class属性为 “sing”的变量
a <- structure("tom", class = 'sing')

#然后，将该对象a传入person中
person.sing(a)


b <- structure("tom", class = 'name')

person(b)

person("joy")

#S3对象的方法
#methods()
methods(person)

library(magrittr)
methods(generic.function = print) %>% head()
methods(class = lm) %>% head()
