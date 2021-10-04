a_list <- list(
  num = c(8, 9),
  log = TRUE,
  cha = c("a", "b", "c")
)
a_list

a_list["num"]
a_list[["num"]]
a_list$num
a_list %>% pluck(1)

v <- c(-2, -1, 0, 1, 2)
v
abs(v)
#如果是列表形式，
#abs函数应用到列表中就会报错
lst <- list(-2, -1, 0, 1, 2)
abs(lst)

exams <- list(
  student1 = round(runif(10, 50, 100)),
  student2 = round(runif(10, 50, 100)),
  student3 = round(runif(10, 50, 100)),
  student4 = round(runif(10, 50, 100)),
  student5 = round(runif(10, 50, 100))
)

exams
mean(exams)
#帮助文档告诉我们，
#mean()要求第一个参数是数值型或者逻辑型的向量。 
#而我们这里的exams是列表，
#因此无法运行
list(
  student1 = mean(exams$student1),
  student2 = mean(exams$student2),
  student3 = mean(exams$student3),
  student4 = mean(exams$student4),
  student5 = mean(exams$student5)
)
exams %>% map(mean)
#哇，短短几句话，
#得出了相同的结果。
#如果希望返回的是数值型的向量，
#可以这样写
exams %>% map_dbl(mean) 
#如果希望返回的结果是数据框
exams %>% map_df(mean)

#function
map()#list
map_chr()#character vector
map_dbl()#double vector(numeric)
map_int()#integer vector
map_lgl()#logical vector
map_df()#data frame

my_fun <- function(x){
  x - mean(x)
}

exams %>% map_df(my_fun)

#当然可以偷懒将函数直接写在map()里，
#用~代替my_fun，
#但代价是参数必须是规定的写法，
#比如.x
exams %>% map_df(~ .x - mean(.x))
exams %>% map_df(~ . - mean(.))
map(.x, mean, na.rm = TRUE)

map(.x, 
    funciton(.x) {
      mean(.x, na.rm = TRUE)
    }
)

# 在dplyr函数中的运用map
# 如果想显示列表中每个元素的长度，
# 可以这样写
tibble(
  x = list(1, 2:3, 4:6)
) %>% 
  mutate(l = purrr::map_int(x, length))

#用于各种函数，比如产生随机数
tibble(
  x = c(3, 5, 6)
) %>% 
  mutate(r = purrr::map(x, ~rnorm(.x, mean = 0, sd = 1)))

#用于建模
mtcars %>%
  group_by(cyl) %>%
  nest() %>%#Nesting creates a list-column of data frames
  mutate(model = purrr::map(data, ~ lm(mpg ~ wt, data = .))) %>%
  mutate(result = purrr::map(model, ~ broom::tidy(.))) %>%
  unnest(result)#unnesting flattens it back out into regular columns


exams %>% map(~ . - mean(.))

exams %>% modify(~ . - mean(.))

#这个还是列表
exams %>% as_tibble() %>% map(~ . - mean(.))

#这个会转换成tibble
exams %>% as_tibble() %>% modify(~ . - mean(.))

mtcars %>% map_chr(typeof)
mtcars %>% map_lgl(is.double)
mtcars %>% map_int(n_unique)
mtcars %>% map_dbl(mean)

# 11.7 在dplyr函数中的运用map
# 如果想显示列表中每个元素的长度，可以这样写
tibble(
  x = list(1, 2:3, 4:6)
) %>% 
  mutate(l = purrr::map_int(x, length))

tibble(
  x = c(3, 5, 6)
) %>% 
  mutate(r = purrr::map(x, ~rnorm(.x, mean = 0, sd = 1)))

#用于建模
mtcars %>%
  group_by(cyl) %>%
  nest() %>%
  mutate(model = purrr::map(data, ~ lm(mpg ~ wt, data = .))) %>%
  mutate(result = purrr::map(model, ~ broom::tidy(.))) %>%
  unnest(result)

