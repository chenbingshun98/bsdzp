library(tidyverse)
library(gapminder)

gapminder %>% 
  ggplot(aes(x = lifeExp))+
  stat_bin()

gapminder %>% 
  ggplot(aes(reorder(continent, continent, length)))+
  geom_bar()

gapminder %>% 
  ggplot(aes(fct_reorder(continent, continent, length)))+
  geom_bar()+
  coord_flip()

gapminder %>% 
  ggplot(aes(fct_reorder(continent, continent, length)))+
  geom_bar()

#geom_bar vs geom_count
gapminder %>% 
  ggplot(aes(x = continent, fill = continent))+
  stat_count()

gapminder %>% count(continent)

gapminder %>% 
  distinct(continent, country) %>% 
  group_by(continent) %>% 
  summarise(num = n()) %>% 
  ggplot(aes(x = continent, y = num))+
  geom_col()

#直方图
gapminder %>% 
  ggplot(aes(x = lifeExp))+
  geom_histogram()
##' histograms, 默认使用 `position = "stack"`
gapminder %>% 
  ggplot(aes(x = lifeExp))+
  geom_histogram(binwidth = 1)

gapminder %>% 
  ggplot(aes(x = lifeExp, fill = continent))+
  geom_histogram()

#' 使用`position = "identity"`
gapminder %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_histogram(position = "identity")

gapminder %>% 
  ggplot(aes(x = lifeExp, color = continent))+
  geom_freqpoly()

#' `smooth histogram = densityplot``
gapminder %>%
  ggplot(aes(x = lifeExp)) +
  geom_density()

gapminder %>%
  ggplot(aes(x = lifeExp)) +
  geom_line(stat = "density")

# adjust 调节bandwidth,
# adjust = 1/2 means use half of the default bandwidth.
gapminder %>%
  ggplot(aes(x = lifeExp)) +
  geom_density(adjust = 1)

gapminder %>%
  ggplot(aes(x = lifeExp)) +
  geom_density(adjust = 0.2)

gapminder %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.2)

gapminder %>%
  filter(continent != "Oceania") %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.2)

gapminder %>%
  ggplot(aes(x = lifeExp)) +
  geom_density() +
  # facet_wrap(vars(continent))
  facet_grid(. ~ continent)

gapminder %>%
  filter(continent != "Oceania") %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_histogram() +
  facet_grid(continent ~ .)

gapminder %>%
  filter(continent != "Oceania") %>%
  ggplot(aes(x = lifeExp, y = stat(density))) +
  geom_histogram(aes(fill = continent)) +
  geom_density() +
  facet_grid(continent ~ .)

#直方图和密度图画在一起。
#注意y = stat(density)表示y是由x新生成的变量，
#这是一种固定写法，
#类似的还有stat(count), stat(level)
gapminder %>%
  filter(continent != "Oceania") %>%
  ggplot(aes(x = lifeExp, y = density)) +#报错
  geom_histogram(aes(fill = continent)) +
  geom_density() +
  facet_grid(continent ~ .)

#箱线图
#' 思考下结果为什么是这样？
#一个离散变量 + 一个连续变量
gapminder %>%
  ggplot(aes(x = year, y = lifeExp)) +
  geom_boxplot()

# 数据框中的year变量是数值型，需要先转换成因子型，弄成离散型变量
gapminder %>%
  ggplot(aes(x = as.factor(year), y = lifeExp)) +
  geom_boxplot()

gapminder %>% 
  ggplot(aes(x = year, y = lifeExp))+
  geom_boxplot(aes(group = year))

gapminder %>% 
  ggplot(aes(x = year, y = lifeExp))+
  geom_violin(aes(group = year))+
  geom_jitter(alpha = 1 / 4)+
  geom_smooth(se = F)

#抖散图
gapminder %>% ggplot(aes(x = continent,
                         y = lifeExp))+
  geom_point()

#It adds a small amount of random variation to the location of each point, 
#and is a useful way of handling overplotting caused by discreteness in smaller datasets.
gapminder %>% ggplot(aes(x = continent,
                         y = lifeExp))+
  geom_jitter()

gapminder %>% ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot()

gapminder %>% ggplot(aes(x = continent,
                         y = lifeExp))+
  geom_boxplot()+
  geom_jitter()

gapminder %>% 
  ggplot(aes(x = continent,
             y = lifeExp))+
  geom_jitter()+
  stat_summary(
    fun = median,
    colour = "red",#用红色标记中位数的点
    geom = "point",
    size = 5
  )

gapminder %>% 
  ggplot(aes(reorder(x = continent, lifeExp),
             y = lifeExp))+
  geom_jitter()+
  stat_summary(
    fun = median,
    colour = "red",
    geom = "point",
    size = 5
  )

gapminder %>% 
  ggplot(aes(x = continent,
             y = lifeExp))+
  geom_violin(
    trim = F,
    alpha = 0.5
  )+
  stat_summary(
    fun = mean,
    fun.max = function(x){
      mean(x) + sd(x)
    },
    fun.min = function(x){
      mean(x) - sd(x)
    },
    geom = "pointrange"
  )

#山峦图
#常用于一个离散变量 + 一个连续变量
gapminder %>% 
  ggplot(aes(
    x = lifeExp,
    y = continent,
    fill = continent
  ))+
  ggridges::geom_density_ridges()

# https://learnui.design/tools/data-color-picker.html#palette
gapminder %>%
  ggplot(aes(
    x = lifeExp,
    y = continent,
    fill = continent
  )) +
  ggridges::geom_density_ridges() +
  scale_fill_manual(
    values = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")
  )

gapminder %>%
  ggplot(aes(
    x = lifeExp,
    y = continent,
    fill = continent
  )) +
  ggridges::geom_density_ridges() +
  scale_fill_manual(
    values = colorspace::sequential_hcl(5, palette = "Peach")
  )

#散点图
#常用于两个连续变量
gapminder %>% 
  ggplot(aes(
    x = gdpPercap,
    y = lifeExp
  ))+
  geom_point()

gapminder %>% 
  ggplot(aes(
    x = log(gdpPercap),
    y = lifeExp
  ))+
  geom_point()

gapminder %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp))+
  geom_point()+
  scale_x_log10() # A better way to log transform

gapminder %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp))+
  geom_point(aes(color = continent))

gapminder %>% 
  ggplot(aes(x = gdpPercap,
             y = lifeExp))+
  geom_point(alpha = (1/3), size = 2)

gapminder %>% 
  ggplot(aes(
    x = gdpPercap,
    y = lifeExp
  ))+
  geom_point()+
  geom_smooth()

gapminder %>% 
  ggplot(aes(
    x = gdpPercap,
    y = lifeExp
  ))+
  geom_point()+
  geom_smooth(lwd = 3,
              se = F)

gapminder %>% 
  ggplot(aes(
    x = gdpPercap,
    y = lifeExp
  ))+
  geom_point()+
  geom_smooth(
    lwd = 3,
    se = F,
    method = "lm"
  )

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  geom_smooth(lwd = 3, se = FALSE, method = "lm")

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~continent)

jCountries <- c("Canada","Rwanda","Cambodia","Mexico")

gapminder %>% 
  filter(country %in% jCountries) %>% 
  ggplot(aes(
    x = year,
    y = lifeExp,
    color = country
  ))+
  geom_line()+
  geom_point()

gapminder %>%
  filter(country %in% jCountries) %>%
  ggplot(aes(
    x = year, y = lifeExp,
    color = reorder(country, lifeExp, max)
  )) +
  geom_line() +
  geom_point()

gapminder %>%
  filter(country %in% jCountries) %>%
  ggplot(aes(
    x = year, y = lifeExp,
    color = reorder(country, -1 * lifeExp, max)
  )) +
  geom_line() +
  geom_point()

gapminder %>%
  filter(country %in% jCountries) %>%
  ggplot(aes(
    x = year, y = lifeExp,
    color = reorder(country, lifeExp, min)
  )) +
  geom_line() +
  geom_point()
