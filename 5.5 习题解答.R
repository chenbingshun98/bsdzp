ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()


ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot(mapping = aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy
  ))

ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN  = median),
      y = hwy
    )
  ) +
  coord_flip()

ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = fct_reorder(class, hwy, .fun  = median),
      y = hwy
    )
  ) +
  coord_flip()

nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(
    mapping = aes(color = cancelled),
    binwidth = 1/4
  )
#前面对比了已取消航班和未取消航班的出发时间，
#使用学习到的知识对这个对比的可视化结构进行改善
library(tidyverse)
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,#取模（余数）
    sched_min = sched_dep_time %%100,#取整除
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot()+
  geom_boxplot(mapping = aes(y = sched_dep_time, x = cancelled))

data("diamonds")
ggplot(data = diamonds,
       mapping = aes(x = carat,
                     y = price))+
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1), 
                             orientation = "x"))
diamonds %>% 
  mutate(color = fct_rev(color)) %>% 
  ggplot(aes(x = color,
             y = price))+
  geom_boxplot()+
  coord_flip()

ggplot(data = mpg)+
  geom_boxplot(mapping = aes(y = reorder(class, 
                                         hwy,
                                         FUN = median), 
                             x = hwy), 
               orientation = "y")
#相线图存在的问题是，
#在小数据集时代开发而成，
#对于现在的大数据集会显示数量极其 庞大的异常值。
#解决这个问题的一种方法是字母价值图
#geom_lv()显示价格基于切割质量的分布。

p_load(lvplot)

ggplot(diamonds,
       aes(x = cut,
           y = price))+
  geom_lv()

#
ggplot(data = diamonds,
       mapping = aes(x = price,
                     y = ..density..))+
  geom_freqpoly(mapping = aes(color = cut),
                binwidth = 500)

diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()

install.packages("hexbin")
ggplot(diamonds, aes(carat, price)) + 
  geom_hex()
