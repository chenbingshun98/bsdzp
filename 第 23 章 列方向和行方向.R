library(tidyverse)
# 23.3 summarise()更强大了
# 在dplyr 1.0之前，summarise()会把统计结果整理成一行一列的数据框，现在可以根据函数返回的结果，可以有多种形式：
# 
# 长度为 1 的向量，比如，min(x), n(), or sum(is.na(y))
# 长度为 n 的向量，比如，quantile()
# 数据框

df <- tibble(
  grp = rep(c("a", "b"), each = 5),
  x = c(rnorm(5, -0.25, 1), rnorm(5, 0, 1.5)),
  y = c(rnorm(5, 0.25, 1), rnorm(5, 0, 0.5))
)
df

df %>% 
  group_by(grp) %>% 
  summarise(rng = mean(x))
#当统计函数返回多个值的时候，
#比如range()返回是最小值和最大值，
#summarise()很贴心地将结果整理成多行，
#这样符合tidy的格式。

df %>% 
  group_by(grp) %>% 
  summarise(rng = range(x))

#类似的还有quantile()函数，也是返回多个值
df %>% 
  group_by(grp) %>% 
  summarise(
    rng = quantile(x, probs = c(0.05, 0.5, 0.95))
  )

df %>% 
  group_by(grp) %>% 
  summarise(
    x = quantile(x, c(0.25, 0.5, 0.75)),
    q = c(0.25, 0.5, 0.75)
  )

# summarise()可以输出数据框，比如
my_quantile <- function(x, probs){
  tibble(x = quantile(x, probs), probs = probs)
}

mtcars %>% 
  group_by(cyl) %>% 
  summarise(my_quantile(disp, c(0.25, 0.75)))

#再比如：

# dplyr 1.0 之前是需要group_modify()来实现数据框进，数据框出
mtcars %>% 
  group_by(cyl) %>% 
  group_modify(
    ~broom::tidy(lm(mpg~wt, data = .))
  )

#dplyr 1.0 之后，有了新的方案
mtcars %>% 
  group_by(cyl) %>% 
  summarise(
    broom::tidy(lm(mpg~wt))
  )
#23.4 summarise()后的分组信息是去是留？
#当 group_by()与summarise()配合使用的时候，
#summarise()默认会抵消掉最近一次的分组信息，
#比如下面按照cyl和vs分组，
#但summarise()后，
#就只剩下cyl的分组信息了。
mtcars %>% 
  group_by(cyl, vs) %>% 
  summarise(cyl_n = n())

mtcars %>% 
  group_by(cyl, vs) %>% 
  summarise(cyl_n = n()) %>% 
  group_vars()

#如果想保留vs的分组信息，就需要设置.groups = keep参数
mtcars %>% 
  group_by(cyl, vs) %>% 
  summarise(
    cyl_n = n(),
    .groups = "keep"
  ) %>% 
  group_vars()

#当然summarise()可以控制输出的更多形式

# 丢弃所有的分组信息
mtcars %>% 
  group_by(cyl, vs) %>% 
  summarise(cyl_n = n(),
            .groups = "drop") %>% 
  group_vars()

#变成行方向分组，即，每行是一个分组
mtcars %>% 
  group_by(cyl, vs) %>% 
  summarise(
    cyl_n = n(),
    .groups = "rowwise"
  ) %>% 
  group_vars()

#选择某列
df %>% 
  select(1, 3)

df %>% 
  select(2:3)

df %>% 
  select(grp, x, y)

df %>% select(x:y)

df %>% select(starts_with("x"))
df %>% select(ends_with("p"))
df %>% select(contains("x"))

df %>% select(matches("x"))

df %>% select(where(is.character))
df %>% select(where(vars(is.character)))#报错
class(vars(is.character()))
df %>% select(contains(is.character))

df %>% select(where(is.numeric))
df %>% select(!where(is.character))

df %>% select(where(is.numeric) & starts_with("x"))
df %>% select(starts_with("g") | ends_with("y"))

# 注意any_of和all_of的区别

vars <- c("x", "y", "z")
df %>% select(all_of(vars))
df %>% select(any_of(vars))

#重命名某列
df %>% rename(group = grp)
df %>% rename_with(toupper)

df %>% rename_with(toupper, where(is.numeric))
df %>% rename_with(toupper, starts_with("x"))

#调整列的位置
df %>% arrange(desc(abs(x)))

df %>% select(x, grp, y)

#如果列变量很多的时候，上面的方法就不太好用，因此推荐大家使用relocate()
df %>% relocate(grp, .after = y)

df %>% relocate(x, .before = grp)

df %>% relocate(grp, .after = last_col())

#强大的across函数
iris <- iris %>% as_tibble()
iris

iris %>%
  group_by(Species) %>%
  summarise(
    mean_Sepal_Length = mean(Sepal.Length),
    mean_Sepal_Width = mean(Sepal.Width),
    mean_Petal_Length = mean(Petal.Length),
    mean_Petal_Width = mean(Petal.Width)
  )

iris %>% 
  group_by(Species) %>%
  summarise(
    across(everything(), mean)
  )

iris %>% group_by(Species) %>% 
  summarise(
    across(is.numeric, mean)
  )

std <- function(x) {
  (x - mean(x)) / sd(x)
}

iris %>%
  group_by(Species) %>%
  summarise(
    across(starts_with("Sepal"), std)
  )

# purrr style
iris %>%
  group_by(Species) %>%
  summarise(
    across(starts_with("Sepal"), 
           ~ (.x - mean(.x)) / sd(.x))
  )

iris %>%
  group_by(Species) %>%
  summarise(
    across(
      starts_with("Petal"), 
      list(min = min, max = max)
      )
    # across(starts_with("Petal"), list(min = min, max = max), .names = "{fn}_{col}")
  )

iris %>%
  group_by(Species) %>%
  summarise(
    across(starts_with("Sepal"), mean),
    Area = mean(Petal.Length * Petal.Width),
    across(c(Petal.Width), min),
    n = n()
  )

iris %>% mutate(across(is.numeric, mean))
iris %>% mutate(across(starts_with("Sepal"), mean))

iris %>% mutate(across(is.numeric, std)) # std function has defined before

iris %>% mutate(
  across(is.numeric, ~ .x / 2),
  across(is.factor, stringr::str_to_upper)
)

iris %>% mutate(
  across(where(is.numeric), ~ .x / 2),
  across(where(is.factor), stringr::str_to_upper)
)

#"current" group or
#"current" variable
df <- tibble(
  g = sample(rep(letters[1:3], 1:3)),
  x = runif(6),
  y = runif(6)
)
df

df %>% 
  group_by(g) %>% 
  summarise(
    n = n()
  )
df %>% 
  group_by(g) %>% 
  count()

df %>% 
  group_by(g) %>% 
  summarise(
    data = list(cur_group())
  ) 

df %>% 
  group_by(g) %>% 
  summarise(
    data = list(cur_data())
  )

mtcars %>% 
  group_by(cyl) %>% 
  summarise(
    broom::tidy(lm(mpg~wt,
                   data = cur_data()))
  )

df %>% 
  group_by(g) %>% 
  mutate(across(everything(),
                ~paste(cur_column(),
                       round(.x, 2))))

wt <- c(x = 0.2, y = 0.8)

df %>% mutate(
  across(c(x,y),
         ~.x*wt[cur_column()])
)

#行方向
#tidyverse遵循的tidy原则，
#一列表示一个变量，
#一行表示一次观察。
#这种数据的存储格式，
#对ggplot2很方便，
#但对行方向的操作或者运算不同友好。比如
df <- tibble(id = letters[1:6],
             w = 10:15,
             x = 20:25,
             y = 30:35,
             z = 40:45)
df

#计算每行的均值
df %>% mutate(avg = mean(c(w, x, y, z)))
#不对，为什么？
#tidy的方法
df %>% 
  pivot_longer(
    cols = -id,
    names_to = "variable",
    values_to = "value"
  ) %>% 
  group_by(id) %>% 
  summarise(
    r_mean = mean(value)
  )
#如果保留原始数据，就还需要再left_join()一次，虽然思路清晰，但还是挺周转的。

# 按照Jenny Bryan的方案，使用purrr宏包的pmap_dbl函数
df %>% 
  mutate(r_mean = pmap_dbl(select_if(.,
                                     is.numeric),
                           lift_vd(mean)))

#rowwise()
df %>% 
  rowwise() %>% 
  mutate(avg = mean(c(w, x, y, z)))

#变量名要是很多的话，又变了体力活了，怎么才能变的轻巧一点呢？

# rowwise() + c_across()，现在dplyr 1.0终于给出了一个很好的解决方案
df %>% 
  rowwise() %>% 
  mutate(
    avg = mean(c_across(w:z))
  )

df %>%
  rowwise(id) %>% 
  mutate(mean = mean(c_across(is.numeric)))

df %>% 
  rowwise(id) %>% 
  summarise(
    m = mean(c_across(is.numeric))
  )

