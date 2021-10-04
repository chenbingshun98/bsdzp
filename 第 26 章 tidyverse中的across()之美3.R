library(tidyverse)

d <- tibble::tribble(
  ~water, ~food,
  10.0,   10.0,
  12.1,   10.3,
  13.5,   19.1,
  17.4,   16.0,
  25.8,   15.6,
  27.4,   19.8
)
d

#26.2 传统的方法
# 传统的方法是，把数据框旋转成长表格，计算所占比例后，再旋转回来

d %>% 
  rownames_to_column() %>% 
  pivot_longer(
    cols = !rowname
  ) %>% 
  group_by(rowname) %>%
  mutate(
    percent = 100 * value / sum(value)
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = name,
    values_from = c(value, percent),
    names_glue = "{name}_{.value}"
  )

#across
#传统的方法，用到基本的dplyr函数，
#思路很清晰，但有点周折。
#下面，我列出几个比较新颖的方法，
#当然这些方法都来源于强大across()函数
d %>% 
  mutate(100 * across(.names = "%{.col}") / rowSums(across()))

d %>% 
  mutate(100 * across(.names = "%{.col}") /rowSums(across())) #rowsums是矩阵用的

d %>% 
  mutate(100 * across(.names = "%{.col}") / rowSums(across())) %>% 
  ungroup() %>% janitor::clean_names()

#
rowPercent <- function(df){
  df / rowSums(df) * 100
}

d %>% 
  mutate(rowPercent(across(.names = "%{.col}")))

d %>% 
  janitor::adorn_percentages()#???

d %>% 
  janitor::adorn_totals("row") %>% 
  janitor::adorn_percentages()

mtcars %>%
  tabyl(am, cyl) %>%
  adorn_percentages("col")

d %>% 
  rowwise() %>% 
  mutate(
    100 * across(.names = "%{.col}") / sum(c_across())
  ) %>% 
  ungroup()

scale <- function(x){
  100 * x / sum(x, na.rm = T)
}

d %>% 
  rowwise() %>% 
  mutate(
    scale(across(.names = "%{.col}"))
  )

d %>% rowwise() %>% 
  mutate(
    100 * proportions(across(.names = "%{.col}"))
  ) 