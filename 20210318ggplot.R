library(tidyverse)
library(palmerpenguins)

ggplot(data = penguins, mapping = aes(x = body_mass_g)) +
  geom_histogram()

d <- tibble::tribble(
  ~variable, ~subject1, ~subject2, ~subject3,
  "mass",         75,     70,    55,
  "height",       154,    172,   144
)
d

# 用geom_point(aes(x = mass, y = height)) 画图，
#却报错了。
#初学者可能苦苦搜索答案，
#然后被告知，
#ggplot画图需要先弄成tidy格式
d %>% pivot_longer(
  cols = subject1:subject3,
  names_to = "subject",
  names_pattern = "subject(\\d)",
  values_to = "value"
) %>% 
  pivot_wider(names_from = variable,
              values_from = value)

#现在数据tidy了，
#你可以使用ggplot()，
#问题得以解决。
#于是我们得出了一个结论：
#想要ggplot工作就需要tidy data。 
#如果这样想，
#那么今天的内容ggplot2统计图层就更加有必要了。


#18.2 为何及何时使用统计图层

simple_data <- tibble(group = factor(rep(c("A","B"),
                                         each = 15)),
                      subject = 1:30,
                      score = c(rnorm(15,40,20),
                                rnorm(15,60,10)))
simple_data

#假定我们现在想画一个柱状图，
#一个柱子代表每一组group，
#柱子的高度代表的score的均值。

# 好比，按照我们的想法，
#我们首先规整(tidy)数据，
#并且确保数据包含每个geom所需的美学映射，
#最后传递给ggplot()
simple_data %>% 
  group_by(group) %>% 
  summarise(
    mean_score = mean(score),
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = group,
             y = mean_score))+
  geom_col()

#报错
simple_data %>% 
  group_by(group) %>% 
  summarise(
    mean_score = mean(score),
    #.groups = 'drop'
  ) %>% 
  ggplot(aes(x = group,
             y = mean_score))+
  geom_col()

# 需求很简单，
#很容易搞定。
#但如果我们想加误差棒(stand error)呢?
#那我们需要再对数据整理统计，
#然后再传给ggplot().
# 
# 于是，
#我们再计算误差棒，
#这里变型的数据是这个样子的

simple_data %>% 
  group_by(group) %>% 
  summarise(
    mean_score = mean(score),
    se = sqrt(var(score)/length(score)),
    .groups = "drop"
  ) %>% 
  mutate(
    lower = mean_score - se,
    upper = mean_score + se
  )

simple_data %>% 
  group_by(group) %>% 
  summarise(
    mean_score = mean(score),
    se = sqrt(var(score)/length(score)),
  ) %>% ungroup() %>% 
  mutate(
    lower = mean_score - se,
    upper = mean_score + se
  )
simple_data %>% 
  group_by(group) %>% 
  summarise(
    mean_score = mean(score),
    se = sqrt(var(score)/length(score)),
    .groups = "keep"
  ) %>% 
  mutate(
    lower = mean_score - se,
    upper = mean_score + se
  )

simple_data %>% 
  group_by(group) %>% 
  summarize(
    mean_score = mean(score),
    se = sqrt(var(score)/length(score)),
    .groups = 'drop'
  ) %>% 
  mutate(
    lower = mean_score - se,
    upper = mean_score + se
  ) %>% 
  ggplot(aes(x = group, y = mean_score, ymin = lower, ymax = upper)) +
  geom_errorbar()


simple_data_bar <- simple_data %>% 
  group_by(group) %>% 
  summarize(
    mean_score = mean(score),
    .groups = "drop"
  )

simple_data_errorbar <- simple_data %>% 
  group_by(group) %>% 
  summarise(
    mean_score = mean(score),
    se = sqrt(var(score)/length(score)),
    .groups = "drop"
  ) %>% 
  mutate(
    lower = mean_score - se,
    upper = mean_score + se
  )

ggplot()+
  geom_col(
    aes(x = group,
        y = mean_score),
    data = simple_data_bar
  )+
  geom_errorbar(
    aes(x = group,
        y = mean_score,
        ymin = lower,
        ymax = upper),
    data = simple_data_errorbar
  )

# OMG, 为了画一个简单的图，
#我们需要写这么长的一段代码。
#究其原因就是，
#我们认为，
#一定要准备好一个tidy的数据，
#并且把想画的几何对象所需要的美学映射，
#都整理到这个tidy的数据框中
# 
# 事实上，理论上讲，
#simple_data_bar 和 simple_data_errorbar 并不是真正的tidy格式。
#因为按照Hadley Wickham的对tidy的定义是，
#一行代表一次观察。 
#而这里的柱子的高度以及误差棒的两端不是观察出来的，
#而是统计计算出来的。

# 既然 simple_data_bar 和 simple_data_errorbar都来源于simple_data，
#那为何不直接传递simple_data给ggplot()，
#让数据在内部转换，
#得到每个几何对象所需的美学映射呢？
# 
# 或许，你想要的是这样？
simple_data %>% 
  ggplot(aes(group, score)) +
  stat_summary(geom = "bar") +
  stat_summary(geom = "errorbar")

# 18.3 用 stat_summary() 理解统计图层
height_df <- tibble(group = "A",
                    height = rnorm(30,170,10))
height_df %>% 
  ggplot(aes(x = group,
             y = height))+
  geom_point()

height_df %>% 
  ggplot(aes(x = group,
             y = height))+
  stat_summary()

mean_se(height_df$height)

pointranege_plot <- height_df %>% 
  ggplot(aes(x = group,
             y = height))+
  stat_summary()

layer_data(pointranege_plot,1)

# 函数stat_summary()里若没有指定数据，
#那就会从ggplot(data = .)里继承
# 参数fun.data 会调用函数将数据变形，
#这个函数默认是mean_se()
# fun.data 返回的是数据框，
#这个数据框将用于geom参数画图，
#这里缺省的geom是pointrange
# 如果fun.data 返回的数据框包含了所需要的美学映射，
#图形就会显示出来。

height_df %>% 
  ggplot(aes(x = group,
             y = height))+
  stat_summary(
    geom = "pointrange",
    fun.data = mean_se
  )

# 18.4.1 包含95%置信区间的误差棒
my_penguins <- na.omit(penguins)

my_penguins %>% 
  ggplot(aes(sex,body_mass_g))+
  stat_summary(
    fun.data = ~mean_se(.,mult = 1.96),
    # Increase `mult` value for bigger interval
    geom = "errorbar"
  )

# 那么这里在stat_summary()函数内部发生了什么呢？
# 
# 分组分别各自的mean_se()，
female_mean_se <- my_penguins %>% 
  filter(sex == "female") %>% 
  pull(body_mass_g) %>% 
  mean_se(.,mult = 1.96)

male_mean_se <- my_penguins %>% 
  filter(sex == "male") %>% 
  pull(body_mass_g) %>% 
  mean_se(.,mult = 1.96)

bind_rows(female_mean_se,male_mean_se)

# 当ggplot()中提供了分组变量（比如这里的sex），
#stat_summary()会分组计算，
#再次感受到ggplot2的强大气息！
calc_median_and_color <- function(x,threshold = 40){
  tibble(y = median(x)) %>% 
    mutate(fill = ifelse(y < threshold, "pink", "grey35"))
}

my_penguins %>% 
  ggplot(aes(species, bill_length_mm))+
  stat_summary(
    fun.data = calc_median_and_color,
    geom = "bar"
  )

my_penguins %>% 
  group_split(species) %>% 
  map(~pull(.,bill_length_mm)) %>% 
  map_dfr(calc_median_and_color)
