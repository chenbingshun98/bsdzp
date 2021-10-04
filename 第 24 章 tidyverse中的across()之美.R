library(tidyverse)
library(palmerpenguins)
penguins

#看到数据框里有很多缺失值，
#需要统计每一列缺失值的数量，
#按照常规的写法

penguins %>% 
  summarise(
    na_in_species = sum(is.na(species)),
    na_in_island = sum(is.na(island)),
    na_in_length = sum(is.na(bill_length_mm)),
    na_in_depth = sum(is.na(bill_depth_mm)),
    na_in_flipper = sum(is.na(flipper_length_mm)),
    na_in_body = sum(is.na(body_mass_g)),
    na_in_sex = sum(is.na(sex)),
    na_in_year = sum(is.na(year))
  )

#幸亏数据框的列数不够多，只有8列，如果数据框有几百列，那就成体力活了，同时代码复制粘贴也容易出错。想偷懒，我们自然想到用summarise_all()
penguins %>% 
  summarise_all(
    ~sum(is.na(.))
  )

#挺好。接着探索，
#我们想先按企鹅类型分组，
#然后统计出各体征数据的均值，
#这个好说，直接写代码
penguins %>% 
  group_by(species) %>% 
  summarise(
    mean_length = mean(bill_length_mm,
                       na.rm = T),
    mean_depth = mean(bill_depth_mm,
                      na.rm = T),
    mean_flipper = mean(flipper_length_mm,
                        na.rm = T),
    mean_body = mean(body_mass_g,
                     na.rm = T)
  )
#或者用summarise_if()偷懒
d1 <- penguins %>% 
  group_by(species) %>% 
  summarise_if(is.numeric,
               mean,
               na.rm = T)
d1

#方法还不错，
# 从语义上还算很好理解。
# 但多了一列year，
#我想在summarise_if()中用
#is.numeric%!year去掉year,
#却没成功。

d2 <- penguins %>% 
  group_by(species) %>% 
  summarise(
    n = n()
  )
d2

d1 %>% left_join(d2,
                 by = "species")
#结果应该没问题，然鹅，总让人感觉怪怪的，
#过程有点折腾，希望不这么麻烦。


#across
penguins %>% 
  group_by(species) %>% 
  summarise(
    across(where(is.numeric) & !year,
           mean,
           na.rm = T),
    n = n()
  )

#24.3.1 求每一列的缺失值数量
penguins %>%
  summarise(
    na_in_species = sum(is.na(species)),
    na_in_island  = sum(is.na(island)),
    na_in_length  = sum(is.na(bill_length_mm)),
    na_in_depth   = sum(is.na(bill_depth_mm)),
    na_in_flipper = sum(is.na(flipper_length_mm)),
    na_in_body    = sum(is.na(body_mass_g)),
    na_in_sex     = sum(is.na(sex)),
    na_in_year    = sum(is.na(year))
  )

# using across()
penguins %>%
  summarise(
    across(everything(), function(x) sum(is.na(x)))
  )
#or
# or
penguins %>%
  summarise(
    across(everything(), ~ sum(is.na(.)))
  )

#24.3.2 每个类型变量下有多少组？
penguins %>%
  summarise(
    distinct_species = n_distinct(species),
    distinct_island  = n_distinct(island),
    distinct_sex     = n_distinct(sex)
  )

# using across()
penguins %>%
  summarise(
    across(c(species, island, sex), n_distinct)
  )

#24.3.3 多列多个统计函数
penguins %>%
  group_by(species) %>%
  summarise(
    length_mean  = mean(bill_length_mm, na.rm = TRUE),
    length_sd    = sd(bill_length_mm, na.rm = TRUE),
    depth_mean   = mean(bill_depth_mm, na.rm = TRUE),
    depth_sd     = sd(bill_depth_mm, na.rm = TRUE),
    flipper_mean = mean(flipper_length_mm, na.rm = TRUE),
    flipper_sd   = sd(flipper_length_mm, na.rm = TRUE),
    n            = n()
  )

#using across()
penguins %>% 
  group_by(species) %>% 
  summarise(
    across(ends_with("_mm"),
           list(mean = mean, sd = sd),
           n = n())
  )

penguins %>% 
  group_by(species) %>% 
  summarise(
    across(endsWith(suffix = "_mm"),#报错
           list(mean = mean, sd = sd),
           n = n())
  )

#24.3.4 不同分组下数据变量的多个分位数
penguins %>%
  group_by(species, island) %>%
  summarise(
    prob    = c(.25, .75),
    length  = quantile(bill_length_mm, prob, na.rm = TRUE),
    depth   = quantile(bill_depth_mm, prob, na.rm = TRUE),
    flipper = quantile(flipper_length_mm, prob, na.rm = TRUE)
  )

#using across
penguins %>% 
  group_by(species, island) %>% 
  summarise(
    prob = c(.25, .75),
    across(
      c(bill_length_mm, bill_depth_mm,
        flipper_length_mm),
      ~quantile(., prob, na.rm = T))
  )

#or
penguins %>%
  group_by(species, island) %>% 
  summarise(
    prob = c(.25, .75),
    length = quantile(bill_length_mm, prob, na.rm = T),
    depth = quantile(bill_depth_mm, prob, na.rm = T),
    flipper = quantile(flipper_length_mm, prob, na.rm = T)
  )
  
#using across()
penguins %>% 
  group_by(species, island) %>% 
  summarise(
    prob = c(.25, .75),
    across(
      c(bill_length_mm, bill_depth_mm, flipper_length_mm),
      ~quantile(., prob, na.rm = T)
    )
  )

#or 
penguins %>% 
  group_by(species, island) %>% 
  summarise(
    prob = c(.25, .75),
    across(where(is.numeric) & !year,
           ~quantile(.,prob, na.rm = T))
  )

#24.3.6 数据标准化处理
std <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

#using across()
penguins %>% 
  summarise(
    across(where(is.numeric), std),
    across(where(is.character), as.factor)
  )

#using across() and purr style
penguins %>% 
  drop_na() %>% 
  summarise(
    across(starts_with("bill_"),
           ~(.x - mean(.x)) / sd(.x))
  )

#数据对数化处理
#using across()
penguins %>% 
  drop_na() %>% 
  mutate(
    across(where(is.numeric),
           log),
    across(where(is.character),
           as.factor)
  ) %>% view()

#using across()
penguins %>% 
  drop_na() %>% 
  mutate(
    across(where(is.numeric),
           .fns = list(log = log),
           .names = "{.fn}_{.col}"),
    across(where(is.character),
           as.factor)
  ) %>% view()

#在分组建模中与cur_data()
#配合使用
penguins %>% 
  group_by(species) %>% 
  summarise(
    broom::tidy(lm(bill_length_mm~bill_depth_mm,
                   data = cur_data()))
  )

penguins %>% 
  group_by(species) %>% 
  summarise(
    broom::tidy(lm(bill_length_mm~.,
                   data = cur_data() %>% select(where(is.numeric))))
  )

penguins %>% 
  group_by(species) %>% 
  summarise(
    broom::tidy(lm(bill_length_mm~.,
                   data = cur_data() %>% 
                     transmute(across(is.numeric))))#transmute() adds new variables and drops existing ones
  )

penguins %>% 
  group_by(species) %>% 
  summarise(
    broom::tidy(lm(bill_length_mm~.,
                   data = across(is.numeric)))
  )

#y与cur_column()配合使用
# 每一列乘以各自的系数
df   <- tibble(x = 1:3, y = 3:5, z = 5:7)
mult <- list(x = 1, y = 10, z = 100)

df %>% 
  mutate(across(all_of(names(mult)),
                ~.x*mult[[cur_column()]]))
df %>% 
  mutate(
    across(
      mult %>% names() %>% all_of(),
      ~.x*mult[[cur_column()]]
    )
  )
# 每一列乘以各自的权重
df      <- tibble(x = 1:3, y = 3:5, z = 5:7)
weights <- list(x = 0.2, y = 0.3, z = 0.5)

df %>% 
  mutate(
    across(all_of(names(weights)),
           list(wt = ~.x*weights[[cur_column()]]),
           .names = "{col}.{fn}"
           )
  )
df %>% 
  mutate(
    across(
      weights %>% names() %>% all_of(),
      list(wt = ~.x*weights[[cur_column()]]),
      .names = "{col}.{fn}"
           )
  )
# 每一列有各自的阈值，如果在阈值之上为1，否则为 0
df      <- tibble(x = 1:3, y = 3:5, z = 5:7)
cutoffs <- list(x = 2, y = 3, z = 7)

df %>% 
  mutate(
    across(
      all_of(names(cutoffs)),
    ~if_else(.x > cutoffs[[cur_column()]],1,0)
    )
  )
df %>% 
  mutate(
    across(
      cutoffs %>% names() %>% all_of(),
      ~if_else(.x>cutoffs[[cur_column()]], 1, 0)
    )
  )
# 与c_across()配合也挺默契
# 在一行中的占比
df <- tibble(x = 1:3, y = 3:5, z = 5:7)
df %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(x:z))) %>% 
  ungroup() %>%
  mutate(across(x:z,
                ~. / total))
df %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(x:z))) %>% 
  ungroup() %>% 
  mutate(
    across(
      x:z,
      ~./total
    )
    )

#错误写法
# df %>% 
#   rowwise() %>% 
#   mutate(total = sum(...))

df %>%
  dplyr::mutate(., sum = purrr::pmap_dbl(., ~ sum(...)))


df %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(x:z))) %>% 
  # ungroup() %>% 没区别
  mutate(across(x:z,
                ~. / total))

#看一行中哪个最大，最大的变为1，其余的变为0
replace_col_max <- function(vec){
  if(!is.vector(vec)){
    stop("input of replace_col_max must be vector.")
  }
  if_else(vec == max(vec), 1L, 0L)
}

#长数据
df %>% 
  rowwise() %>% 
  mutate(
    new = list(replace_col_max(c_across(everything())))
  ) %>% unnest_auto(new)#或者unnest_auto()

df %>% 
  rowwise() %>% 
  mutate(
    new = list(replace_col_max(c_across(everything())))
  ) %>% 
  unnest_wider(new,
               names_sep = "_")
