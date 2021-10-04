library(tidyverse)
library(rlang)

# 写代码的过程中，我们会遇到对不同的数据框，执行相同的操作。比如
df1 %>% group_by(x1) %>% summarise(mean = mean(y1))
df2 %>% group_by(x2) %>% summarise(mean = mean(y2))
df3 %>% group_by(x3) %>% summarise(mean = mean(y3))
df4 %>% group_by(x4) %>% summarise(mean = mean(y4))

# 为了减少代码的重复，我们考虑将共同的部分保留，变化的部分用参数名提取出来
data %>% group_by(group_var) %>% 
  summarise(mean = mean(summary_var))

#很自然地，我们想到写一个子函数的形式，比如
grouped_mean <- function(data, group_var, summary_var){
  data %>% 
    group_by(group_var) %>% 
    summarise(mean = mean(summary_var))
}

#当我们试图运行这段代码的时候，却发现报错了
grouped_df(mtcars, cyl, mpg)

#Hadley Wickham告诉我们，正确的写法应该是，
grouped_mean <- function(data, group_var, summary_var){
  group_var <- enquo(group_var)
  summary_var <- enquo(summary_var)
  
  data %>% 
    group_by(!!group_var) %>% 
    summarise(mean = mean(!!summary_var))
}

#再运行
grouped_mean(mtcars, cyl, mpg)

#或者更简便的
grouped_mean <- function(data, group_var, summary_var){
  data %>% 
    group_by({{group_var}}) %>% 
    summarise(mean = mean({{summary_var}}))
}
grouped_mean(mtcars, cyl, mpg)

#dplyr1.0之后，可以这样写
sum_group_vars <- function(df, group_vars, sum_vars){
  df %>% 
    group_by(across({{group_vars}})) %>% 
    summarise(n = n(),
              across({{sum_vars}},
                     list(mean = mean, sd = sd)))
}

sum_group_vars(mpg, c(model, year), c(hwy, cty))

#29.2 看看发生了什么
# 弄清楚之前，这里需要明白两个概念：
# 
# 环境变量(env-variables) ，一般你在Rstuido右上角的Environment中发现它。比如n <- 10这里的n
# 
# 数据变量(data-variables)，一般指数据框的某个变量。比如data <- data.frame(x = 1, n = 2)中的data$n

#那么，对于我们这里编写的函数中
grouped_mean(mtcars, cyl, mpg)
# cyl和mpg是打算传递的参数，是环境变量，
#但我们期望他们在函数中当作mtcars中的数据变量，
#即当做mtcars的一个列的名字来使用， 
#那么要完成这个角色转换，
#就需要引用(quote)和解引用(unquote)两个工序：

# 第一步，
#用 enquo()把用户传递过来的参数引用起来（引用可以理解为冷冻起来）
# 
# 第二步，
#用 !! 解开这个引用（解引用可以理解为解冷），然后使用参数的内容

#这个quote-unquote的过程让环境变量名变成了数据变量，
#也可以理解为在函数评估过程中，
#数据变量（data-variable）遮盖了环境变量（env-variable），
#即数据遮盖（data masking），
#看到cyl，正常情况下，本来应该是到环境变量里去找这个cyl对应的值，
#然而，数据遮盖机制，插队了，
#让代码去数据变量中去找cyl以及对应的值。

var <- quote(height)
qq_show(!!var)

#再看看grouped_mean()的代码
group_var <-  quote(cyl)
summary_var <-  quote(mpg)

rlang::qq_show( 
  data %>%
    group_by(!!group_var) %>%
    summarise(mean = mean(!!summary_var))
)

#29.3 处理多个参数
#前面讲了如何传递分组参数和统计参数到子函数。
#如果传递更多的参数，
#可以用...代替group_var ，
#然后传递到group_by()，
#比如
grouped_mean <- function(data, summary_var, ...){
  summary_var <- enquo(summary_var)
  group_var <- enquos(...)
  
  data %>% 
    group_by(!!!group_var) %>% 
    summarise(mean = mean(!!summary_var))
}

#指定统计参数disp，分组参数(cyl am)，然后运行代码,
grouped_mean(mtcars, disp, cyl, am)

#或者指定统计参数disp，更多的分组参数(cyl, am, vs)
grouped_mean(mtcars, disp, cyl, am, vs)

#注意到...代表的是多个参数，
#因此在引用的时候用的是enquos()，
#在解引用的时候 用的是group_by(!!!group_var). 
#事实上, ...是一个特殊的符号，
#我们可以省略引用后再解引用的过程，
#直接传给给group_by()， 
#比如
grouped_mean <- function(data, summary_var, ...){
  summary_var <- enquo(summary_var)
  
  data %>% 
    group_by(...) %>% 
    summarise(mean = mean(!!summary_var))
}

grouped_mean(mtcars, disp, cyl, am, vs)

# 29.4 调整输入的表达式
# 29.4.1 修改引用参数的默认名

#我们希望输出的统计结果中，统计参数名加一个前缀 “avg_”， 可以分三步完成

# 获取引用参数的默认名
# 修改参数的默认名，比如加前缀或者后缀
# !! 解引用并放在 := 左边

grouped_mean2 <- function(.data,
                          .summary_var, ...){
  summary_var <- enquo(.summary_var)
  group_vars <- enquos(...)
  
  #get and modify the default name
  summary_nm <- as_label(summary_var)
  summary_nm <- paste0("avg_", summary_nm)
  
  .data %>% 
    group_by(!!!group_vars) %>% 
    summarise(!!summary_nm := mean(!!summary_var))#unquote the name
}

grouped_mean2(mtcars, disp, cyl, am)

#或者更简洁的办法
my_summarise <- function(data, group_var, summary_var){
  data %>% 
    group_by(across({{group_var}})) %>% 
    summarise(across({{summary_var}},
                     mean,
                     .names = "mean_{col}"))
}

my_summarise(starwars, species, height)
