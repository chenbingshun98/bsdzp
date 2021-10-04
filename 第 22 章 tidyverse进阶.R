library(tidyverse)

iris <- iris %>% as_tibble()

df_iris <- iris %>% head()

#可以一次性增加多列
df_iris %>% mutate_if(is.numeric,
                      list(scale, log)) %>% view()

#也可以把函数放在list()中，用 Purrr-style lambda 形式写出
df_iris %>% mutate_if(is.numeric,
                      list(~scale(.), ~log(.)))

#select_if()
df <- tibble::tibble(
  x = letters[1:3],
  y = c(1:3),
  z = c(0, 0, 0)
)
df

df %>% select_if(is.numeric)

df %>% select_if(~ n_distinct(.) > 2)


df %>% select_if(
  list(~(is.numeric(.) | is.character(.)))
)

df %>% select_if(
  ~(is.numeric(.) | is.character(.))
)

to_keep <- function(x)is.numeric(x) | is.character(x)
df %>% select_if(to_keep)

df %>% select_if(
  list(~(is.numeric(.) && sum(.) > 2))#&&只匹配一次
)

df %>% select_if(
  list(~(is.numeric(.) && mean(.) > 1))
)

to_want <- function(x)is.numeric(x) && sum(x) > 2
df %>% select_if(to_want)

#summarise_if
data(msleep)
msleep %>% group_by(vore) %>% 
  summarise_all(~mean(., na.rm = T)) %>% 
  select(-c(name, genus, order, conservation))

msleep %>% 
  group_by(vore) %>% 
  summarise_if(is.numeric, 
               mean,
               na.rm = T)

#filter_if
msleep %>% 
  select(name, sleep_total) %>% 
  filter(sleep_total > 18)

msleep %>% 
  select(name, sleep_total) %>% 
  filter(between(sleep_total, 16, 18))

msleep %>% 
  select(name, sleep_total) %>% 
  filter(near(sleep_total,
              mean(sleep_total),
              tol = 0.5*sd(sleep_total)))

#mtcars
data("mtcars") 
mtcars <- as_tibble(mtcars)
mode(mtcars)

#filter_if()配合all_vars(), any_vars()函数，
#可以完成很酷的工作. 比如，要求一行中所有变量的值都大于150
mtcars %>% filter_all(all_vars(. > 150))

#比如，要求一行中至少有一个变量的值都大于150
mtcars %>% filter_all(any_vars(. > 150))

# You can vary the selection of columns on which to apply the predicate.
# filter_at() takes a vars() specification:
mtcars %>% filter_at(vars(starts_with("d")),#列
                     any_vars((. %% 2) == 0))#行
mtcars %>% filter_at(vars(starts_with("d")))#报错

#报错
mtcars %>% filter_if(vars(starts_with("d")),#列
                     any_vars((. %% 2) == 0))#行
# And filter_if() selects variables with a predicate function:
# filter_if(.tbl, .predicate, .vars_predicate)
# mtcars %>% map_df(~ all(floor(.) == .) )
# mtcars %>% select_if( ~ all(floor(.) == .) )

mtcars %>% filter_if(~ all(floor(.) == .), all_vars(. != 0))
#所以这里是，先通过.predicate = ~ all(floor(.) == .)
# 选取变量值为整数的列，
#然后再看选取的这些列的行方向，
#如果每一行的值.vars_predicate = all_vars(. != 0) ，
#都不为0，就保留下来，否则过滤掉。

# 简单点说，这段代码的意思，
#数值全部为整数的列，不能同时为0

#group_by()
mtcars %>% dplyr::group_by(cyl)
mtcars %>% group_by_at(vars(cyl))

# Group a data frame by all variables:
mtcars %>% group_by_all()

# Group by variables selected with a predicate:
iris %>% group_by_if(is.factor)

#22.4.1 group_split(), group_map(), group_modify()
iris %>% 
  group_by(Species) %>% 
  group_split()

iris %>%
  dplyr::group_split(Species)

#既然是列表，当然想到用前面讲到的purrr::map()家族
iris %>% 
  group_split(Species) %>% 
  map(~broom::tidy(lm(Petal.Length ~ Sepal.Length,
                      data = .x))) 

iris %>% 
  group_split(Species) %>% 
  map(
    ~ broom::tidy(
    lm(Petal.Length ~ Sepal.Length,
                       data = .x)
                    )
    )

iris %>%
  dplyr::group_split(Species) %>%
  purrr::map_df(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))

iris %>% 
  group_split(Species) %>% 
  map_df(
    ~broom::tidy(
      lm(Petal.Length ~ Sepal.Length,
         data = .x)
    )
  )
## The result of .f should be a data frame(.f 必须返回数据框)
## `group_map()` return a list of tibble
#(返回元素均为df的一个列表list(df1,df2,df3))
iris %>%
  dplyr::group_by(Species) %>%
  dplyr::group_map(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))

iris %>% 
  group_by(Species) %>% 
  group_map(
    ~ broom::tidy(
      lm(Petal.Length ~ Sepal.Length,
         data = .x)
    )
  )
#数据框进来，然后分组，依次处理成一个个数据框，
#最后以列表形式（a list of tibble）输出。

# 事实上，group_map()是返回list形式，
#也就是说，可以是返回任何形式，（a list of tibble）是其中特殊形式。
#可以看看下面这个
iris %>% 
  group_by(Species) %>% 
  group_map(
    ~lm(Petal.Length~Sepal.Length,
        data = .x)
  )

#group_modify() 才是真正意义上的“数据框进、数据框出”。
iris %>% 
  group_by(Species) %>% 
  group_modify(~broom::tidy(lm(Petal.Length~Sepal.Length,
                               data = .x)))

#我常用的批量出图的语句
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
nobel_winners <- read.csv("nobel_winners.csv")
nobel_winners %>%
  dplyr::group_split(category) %>%
  purrr::map(
    ~ ggplot(data = .x, aes(x = prize_age)) +
      geom_density() +
      ggtitle(.x$category)
  )

nobel_winners %>%
  dplyr::group_by(category) %>%
  dplyr::group_map(
    ~ ggplot(data = .x, aes(x = prize_share)) +
      geom_density() +
      ggtitle(.y)
  )

nobel_winners %>%
  dplyr::group_by(category) %>%
  dplyr::group_walk(
    ~ ggsave(
      paste0(.y, ".png"),
      ggplot(data = .x, aes(x = prize_age)) +
        geom_density() +
        ggtitle(.y),
      device = "png",
      path = temp
    )
  ) %>%
  invisible()

#其他group函数
library(readxl)
library(janitor) # install.packages("janitor")

roster_raw <- read_excel("dirty_data.xlsx")

glimpse(roster_raw)

roster <- roster_raw %>%
  janitor::clean_names()

glimpse(roster)

#缺失值检查与处理
#22.6.1 purrr & dplyr 技巧
library(purrr)
airquality <- as_tibble(airquality)

airquality %>% purrr::map(~ sum(is.na(.)))

airquality %>% 
  map_df(~sum(is.na(.)))

airquality %>%
  summarise_at(2:3,
               ~sum(is.na(.)))

#22.6.2 缺失值替换
airquality %>% 
  mutate_all(funs(replace(., 
                         is.na(.),
                         0)))

airquality %>% 
  mutate_all(replace_na, replace = 0)

airquality %>% 
  mutate_if(is.numeric,
            replace_na,
            replace = 0)

airquality %>% 
  mutate_all(as.numeric) %>% 
  mutate_all(~coalesce(., 0))

tibble(
  y = c(1, 2, NA, NA, 5),
  z = c(NA, NA, 3, 4, 5)
) %>%
  mutate_all(~ coalesce(., 0))

#标准化
df_mtcars
mtcars %>% select_if(is.numeric)
mtcars %>% select_if(funs(is.numeric))

mtcars %>% 
  mutate_at(vars(mpg, disp),
            ~scale(., center = T, scale = T))

#c更方便的colwise操作
# multiple
df <- tibble(x = 1:3, y = 3:5, z = 5:7)
mult <- list(x = 1, y = 10, z = 100)

df %>% mutate(across(all_of(names(mult)), ~ .x * mult[[cur_column()]]))



# weights
df <- tibble(x = 1:3, y = 3:5, z = 5:7)
df
weights <- list(x = 0.2, y = 0.3, z = 0.5)

df %>% dplyr::mutate(
  across(
    all_of(names(weights)),
         list(wt = ~ .x * weights[[cur_column()]]),
         .names = "{col}.{fn}"
  )
)

df %>% 
  mutate(
    across(
      all_of(names(weights)),
        list(wt = ~.x * weights[[cur_column()]]),
        .names = "{col}.{fn}"
    )
  )

# cutoffs
df <- tibble(x = 1:3, y = 3:5, z = 5:7)
df

cutoffs <- list(x = 2, y = 3, z = 7)
class(cutoffs)
df %>% dplyr::mutate(
  across(all_of(names(cutoffs)), ~ if_else(.x > cutoffs[[cur_column()]], 1, 0))
)

df %>% 
  mutate(
    across(
      all_of(names(cutoffs)),
      ~if_else(.x > cutoffs[[cur_column()]], 1, 0)
      )
  )
