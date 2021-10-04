library(tidyverse)
library(palmerpenguins)

# 比如我们想筛选数据框有缺失值的行
penguins %>% 
  filter(
    across(everything(), any_vars(is.na(.)))
  )

#代码能运行，但结果明显不正确。我搜索了很久，发现只能用dplyr 1.0.0之前的filter_all()函数实现
penguins %>% 
  filter_all( any_vars(is.na(.)) )

#如今，dplyr 1.0.4推出了 if_any() and if_all() 两个函数，正是弥补这个缺陷
penguins %>% 
  filter(if_any(everything(),is.na)) 

#从函数形式上看，if_any 对应着 across的地位，
#这就意味着列方向我们有across()，行方向我们有if_any()/if_all()了，可谓 纵横武林，倚天屠龙、谁与争锋?
penguins %>% 
  filter(if_any(everything(), is.na))

#筛选全部是缺失值的行
penguins %>% 
  filter(if_all(everything(), is.na))
## # A tibble: 0 x 8
## # ... with 8 variables: species <fct>, island <fct>,
## #   bill_length_mm <dbl>, bill_depth_mm <dbl>,
## #   flipper_length_mm <int>, body_mass_g <int>,
## #   sex <fct>, year <int>

# 筛选企鹅嘴峰(长度和厚度)全部大于21mm的行
penguins %>% 
  filter(if_all(contains("bill"),
                ~. > 21))

# 当然可以弄成更骚一点喔
penguins %>% 
  filter(if_all(contains("bill"), `>`,21))

#筛选企鹅嘴峰（长度或者厚度）大于21mm的行
penguins %>% 
  filter(if_any(contains("bill"),
                ~. > 21))

#在指定的列(嘴峰长度和厚度)中检查每行的元素，
#如果这些元素都大于各自所在列的均值，就保留下来
bigger_than_mean <- function(x){
  x > mean(x, na.rm = T)
}

penguins %>% 
  filter(if_all(contains("bill"),
                bigger_than_mean))

#在指定的列(嘴峰长度和嘴峰厚度)中检查每行的元素，
#如果这些元素都大于各自所在列的均值，
#就“both big”；
#如果这些元素有一个大于自己所在列的均值，
#就“one big”，
#(注意case_when中if_all要在if_any之前)

penguins %>% 
  filter(!is.na(bill_length_mm)) %>%
  mutate(
    category = case_when(
      if_all(contains("bill"),
             bigger_than_mean)~"both big",
      if_any(contains("bill"),
             bigger_than_mean)~"one big",
      TRUE ~"small"
    )
  ) %>% 
  filter(category == "small") %>% view()

penguins %>% 
  filter(!is.na(bill_length_mm)) %>% 
  mutate(
    category = case_when(
      if_all(contains("bill"), bigger_than_mean) ~ "both big",
      if_any(contains("bill"), bigger_than_mean) ~ "one big",
      TRUE ~ "small"
    ) 
  ) %>% 
  filter(category == "small")
