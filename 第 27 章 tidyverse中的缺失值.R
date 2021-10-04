library(tidyverse)
library(palmerpenguins)
penguins <- read_csv(here::here("demo_data", "penguins.csv"))
penguins

#NA的计算
c(NA, 1) + 2

isTRUE(NA)

isFALSE(NA)

# 既不是真，也不是假，真假难辨。因此，在逻辑运算时，可以按下表指引

# Some logical operations do not return NA
c(TRUE, FALSE) & NA

c(TRUE, FALSE) | NA

#判断
c(1, 2, NA, 4) %>% is.na()

#强制转换
c(TRUE, FALSE, NA) %>% class()

#但如果 NA 放在数值型的向量中，
c(1, 2, NA, 4) %>% class()

# 如果 NA 放在字符串的向量中，
c("1", "2", NA, "4") %>% class()

c(1, 2, TRUE, 4) 
c(1, 2, TRUE, 4) %>% class()

c(1, 2, NA_real_, 4)
c(1, 2, NA_real_, 4) %>% class()

#当逻辑型变量和字符串型变量组合在一起时，逻辑型会强制转换成字符串型。
c("1", "2", TRUE, "4")

c("1", "2", NA, "4")

c("1", "2", NA_character_, "4") 

#除了逻辑型NA, 数值型NA_real_, 字符串型NA_character_外, 还有整数型NA_integer_, 和复数型NA_complex. 我们再看下面的例子
c(TRUE, NA) %>%
  purrr::map(., ~is.logical(.))

c("a", NA, NA_character_) %>% 
  purrr::map(., ~is.character(.))

c(123, NA, NA_real_) %>% 
  purrr::map(., ~is.numeric(.))

c(NA_real_, NA_complex_, NA_character_, NA_integer_, NA) %>% #被强制转换成character类型
  purrr::map(., ~is.character(.))

#统计NA
c(1, 2, NA, 4) %>% is.na() %>% as.integer() %>% sum()
c(1, 2, NA, 4) %>% is.na() %>% sum()

sum_of_na <- function(x){
  sum(is.na(x))
}

c(1, 2, NA, 4) %>% sum_of_na()

penguins$bill_length_mm %>% sum_of_na()

penguins %>% summarise(
  N1 = sum_of_na(bill_length_mm),
  N2 = sum_of_na(bill_depth_mm)
)

penguins %>% summarise(
  across(everything(), sum_of_na)
)

penguins %>% 
  summarise(
    across(everything(), ~sum(is.na()))
  )

#数据框的一列中每个元素的数据类型是要求相同的，这是构建数据框的基本要求。
#因此，在dplyr中mutate()函数创建数据框的新列时， 
#这一列的元素必须是同一种类型，如果遇到新列中包含NA，
#也要确保NA的类型与其它元素的类型要一致，
#比如其它元素是字符串，
#那么就应该使用字符串类型的缺失值，即NA_character_.
d <- tibble(x = c(1, 3, 6, NA, 8, NA))
d

#报错
d %>% mutate(
  is_even = case_when(
    x %% 2 == 0 ~ "even",
    x %% 2 == 1 ~ "not even",
    TRUE ~ NA
  )
)

# 上面这个代码中，本意是希望构建一个新列存储(“even,” “not even”)字符串；而NA是逻辑型的，类型不一致，因此会报错。 正确的写法是使用NA_character_
d %>% mutate(
  is_even = case_when(
    x %% 2 == 0 ~ "even",
    x %% 2 == 1 ~ "not even",
    TRUE ~ NA_character_
  )
)
