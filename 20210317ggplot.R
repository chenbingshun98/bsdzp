library(tidyverse)
mpg %>% ggplot(aes(x = displ, y = hwy))+
  geom_point()

#事实上，根据映射关系和变量名，我们将标度写完整，
#应该是这样的
ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  scale_x_continuous()+
  scale_y_continuous()+
  scale_color_discrete()

ggplot(mpg, aes(x = displ, y = hwy))+
  geom_point(aes(color = class))+
  scale_x_continuous(name = "这是我的x坐标")+
  scale_y_continuous(name = "这是我的y坐标")+
  scale_color_brewer()

# 15.3 坐标轴和图例是同样的东西
# 注意到，标度函数是由"_"分割的三个部分构成的 
#- scale - 视觉属性名 (e.g., colour, shape or x) 
#- 标度名 (e.g., continuous, discrete, brewer).

# 每个标度函数内部都有丰富的参数系统
scale_color_manual(
  palette = function(),
  limits = NULL,#参数limits, 坐标或图例的范围区间。连续性c(n, m)，离散型c("a", "b", "c")
  name = waiver(),
  labels = waiver(),
  breaks = waiver(),#参数breaks, 控制显示在坐标轴或者图例上的值（元素）
  minor_breaks = waiver(),
  values = waiver(),
  ...
)


library(gapminder)

newgapdata <- gapminder %>% 
  group_by(continent, country) %>% 
  summarise(
    across(c(lifeExp,gdpPercap,pop), mean)
  )
newgapdata

newgapdata %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point(aes(color = continent, size = pop))+
  scale_x_continuous()

newgapdata %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point(aes(color = continent, size = pop))+
  scale_x_log10()

newgapdata %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point(aes(color = continent, size = pop))+
  scale_x_log10(breaks = c(500, 1000, 3000, 10000, 30000), labels = scales::dollar)

newgapdata %>% 
  ggplot(aes(x = gdpPercap, y =lifeExp))+
  geom_point(aes(color = continent, size = pop))+
  scale_x_log10(
    name = "人均GDP",
    breaks = c(500, 1000, 3000, 10000, 30000),
    labels = scales::unit_format(unit = "美元")
  )

newgapdata %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point(aes(color = continent, size = pop))+
  scale_x_log10()+
  scale_color_viridis_d()

newgapdata %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point(aes(color = continent, size = pop))+
  scale_x_log10()+
  scale_color_brewer(type = "qual",
                     palette = "Set1")

newgapdata %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point(aes(color = continent, size = pop))+
  scale_x_log10()+
  scale_color_manual(
    name = "五大洲",
    values = c("Africa" = "red",
               "Americas" = "blue",
               "Asia" = "orange",
               "Europe" = "black",
               "Oceania" = "gray"),
    breaks = c("Africa",
               "Americas",
               "Asia",
               "Europe",
               "Oceania"),
    labels = c("非洲",
               "美洲",
               "亚洲",
               "欧洲",
               "大洋洲")
  )+
  scale_size(
    name = "人口数量",
    breaks = c(2e8, 5e8, 7e8),
    labels = c("2亿","5亿","7亿")
  )

# 16.1 图例系统
# 为了方便演示，
#我们还是用熟悉的配方ggplot2::mpg

library(tidyverse)

mpg %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point()  

# 如果想调整图例的样式，
#可以使用guides()函数，
#用法类似上节课中的theme函数, 
#具体参数为：
# 
# 要么是字符串 (i.e. "color = colorbar" or "color = legend"),
# 要么是特定的函数 (i.e. color = guide_colourbar() or color = guide_legend())

mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = factor(cyl)))+
  geom_point()+
  ggtitle("这是我的标题")+
  labs(x = "x_displ", y = "y_hwy")+
  guides(color = "legend")

mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = factor(cyl)))+
  geom_point()+
  ggtitle("这是我的标题")+
  labs(x = "x_displ", y = "y_hwy")+
  guides(color = guide_bins(
    title = "my title",
    label.hjust = 1
  ))

mpg %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  ggtitle("这是我的标题") +
  labs(x = "x_displ", y = "y_hwy") +
  guides(color = guide_legend(ncol = 4))

mpg %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  ggtitle("这是我的标题") +
  labs(x = "x_displ", y = "y_hwy") +
  guides(color = guide_legend(
    title = "这标题好像有点高",
    title.position = "top",
    title.vjust = 5,
    label.position = "left",
    label.hjust = 1,
    label.theme = element_text(size = 15,
                               face = "italic",
                               color = "red",
                               angle = 0),
    keywidth = 5,
    reverse = T
  ))

mpg %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  ggtitle("这是我的标题") +
  labs(x = "x_displ", y = "y_hwy") +
  guides(color = guide_legend(
    title = "这标题好像有点高",
    title.position = "top",
    title.vjust = -5,
    label.position = "left",
    label.hjust = 1,
    label.theme = element_text(size = 15,
                               face = "italic",
                               color = "red",
                               angle = 0),
    keywidth = 5,
    reverse = T
  ))

mpg %>%
  ggplot(aes(x = displ, y = hwy, color = class, size = cyl)) +
  geom_point()

mpg %>% 
  ggplot(aes(x = displ, y = hwy,
             color = class, size = cyl))+
  geom_point()+
  guides(color = guide_legend("汽车类型"),# keep
         size = F)# remove

mtcars %>%
  as_tibble() %>%
  ggplot(aes(x = wt, y = mpg, shape = factor(vs), color = hp)) +
  geom_point(size = 3) +
  colorspace::scale_color_continuous_sequential(palette = "Dark Mint") +
  scale_shape_discrete(labels = c("V-shaped", "Straight")) +
  labs(
    x = "Weight (1000 lbs)", y = "Miles per gallon",
    title = "Motor Trend Car Road Tests",
    shape = "Engine", color = "Horsepower"
  ) +
  theme(
    text = element_text(size = 18, color = "white"),
    rect = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black"),
    axis.text = element_text(color = "white"),
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(
    shape =
      guide_legend(override.aes = list(color = "white"))
  )
?mtcars

install.packages(c("sf", "cowplot", "patchwork", "gghighlight", "ggforce", "ggfx"))
library(tidyverse)
library(gghighlight)
library(cowplot)
library(patchwork)
library(ggforce)
library(ggridges)

p1 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  geom_smooth() +
  labs(title = "1: geom_point() + geom_smooth()") +
  theme(plot.title = element_text(face = "bold"))
p1

p2 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_hex() +
  labs(title = "2: geom_hex()") +
  guides(fill = FALSE) +
  theme(plot.title = element_text(face = "bold"))
p2

p3 <- ggplot(mpg, aes(x = drv, fill = drv)) +
  geom_bar() +
  labs(title = "3: geom_bar()") +
  guides(fill = F) +
  theme(plot.title = element_text(face = "bold"))
p3

p4 <- ggplot(mpg, aes(x = cty)) +
  geom_histogram(binwidth = 2, color = "white") +
  labs(title = "4: geom_histogram()") +
  theme(plot.title = element_text(face = "bold"))
p4

p5 <- ggplot(mpg, aes(x = cty, y = drv, fill = drv)) +
  geom_violin() +
  guides(fill = FALSE) +
  labs(title = "5: geom_violin()") +
  theme(plot.title = element_text(face = "bold"))
p5

p6 <- ggplot(mpg, aes(x = cty, y = drv, fill = drv)) +
  geom_boxplot() +
  guides(fill = FALSE) +
  labs(title = "6: geom_boxplot()") +
  theme(plot.title = element_text(face = "bold"))
p6

p7 <- ggplot(mpg, aes(x = cty, fill = drv)) +
  geom_density(alpha = 0.7) +
  guides(fill = FALSE) +
  labs(title = "7: geom_density()") +
  theme(plot.title = element_text(face = "bold"))
p7

p8 <- ggplot(mpg, aes(x = cty, y = drv, fill = drv)) +
  geom_density_ridges() +
  guides(fill = FALSE) +
  labs(title = "8: ggridges::geom_density_ridges()") +
  theme(plot.title = element_text(face = "bold"))
p8

p9 <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_density_2d() +
  labs(title = "9: geom_density_2d()") +
  theme(plot.title = element_text(face = "bold"))
p9

p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 +
  plot_layout(nrow = 3)

library(gapminder)
gapminder %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp,
             color = continent))+
  geom_point()+
  scale_x_log10()+
  ggtitle("My Plot Title")+
  xlab("The X Variable")+
  ylab("The Y Variable")

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10() +
  labs(
    title = "My Plot Title",
    subtitle = "My Plot subtitle",
    x = "The X Variable",
    y = "The Y Variable"
  )

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10() +
  scale_color_manual(
    values = c("#195744", "#008148", "#C6C013", "#EF8A17", "#EF2917")
  )

# 可以使用 cowplot 宏包的plot_grid()函数完成多张图片的组合，
#使用方法很简单。

p1 <- gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = lifeExp > mean(lifeExp))) +
  scale_x_log10() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("orange", "pink")) +
  labs(
    title = "My Plot Title",
    x = "The X Variable",
    y = "The Y Variable"
  )
p1

p2 <- gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10() +
  scale_color_manual(
    values = c("#195744", "#008148", "#C6C013", "#EF8A17", "#EF2917")
  ) +
  theme(legend.position = "none") +
  labs(
    title = "My Plot Title",
    x = "The X Variable",
    y = "The Y Variable"
  )
p2

cowplot::plot_grid(
  p1,
  p2,
  labels = c("A", "B")
)
p1 + p2
p1 / p2

p1 + p2 +
  plot_annotation(
    tag_levels = "A",
    title = "The surprising truth about mtcars",
    subtitle = "These 3 plots will reveal yet-untold secrets about our beloved data-set",
    caption = "Disclaimer: None of these plots are insightful"
  )

library(palmerpenguins)

g1 <- penguins %>% 
  ggplot(aes(bill_length_mm, body_mass_g, color = species)) +
  geom_point() + 
  theme_bw(base_size = 14) +
  labs(tag = "(A)", x = "Bill length (mm)", y = "Body mass (g)", color = "Species")
g1

g2 <- penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, color = species)) +
  geom_point() + 
  theme_bw(base_size = 14) +
  labs(tag = "(B)", x = "Bill length (mm)", y = "Bill depth (mm)",  color = "Species")

g1 + g2 + patchwork::plot_layout(guides = "collect")
?plot_layout

install.packages("showtext")
library(showtext)
showtext_auto()

gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10() +
  scale_color_manual(
    values = c("#195744", "#008148", "#C6C013", "#EF8A17", "#EF2917")
  ) +
  theme(legend.position = "none") +
  labs(
    title = "这是我的标题美美哒",
    x = "这是我的x坐标",
    y = "这是我的y坐标"
  )

#高亮
# 17.5.1 ggplot2方法
# 这种方法是将背景部分和高亮部分分两步来画
drop_facet<- function(x)select(x,-continent)

gapminder %>%
  ggplot()+
  geom_line(
    data = drop_facet,
    aes(x = year,
        y = lifeExp,
        group = country),
    color = "grey"
  )+
  geom_line(aes(x = year,
                y = lifeExp,
                color = country,
                group = country))+
  facet_wrap(vars(continent))+
  theme(legend.position = "none")

gapminder %>% 
  mutate(group = country) %>% 
  filter(continent == "Asia") %>% 
  ggplot()+
  geom_line(
    data = function(d)select(d,-country),
    aes(x = year,
        y = lifeExp,
        group = group),
    color = "grey"
  )+
  geom_line(aes(x = year,
                y = lifeExp,
                group = country),
            color = "red")+
  facet_wrap(vars(country))+
  theme(legend.position = "none")

#17.5.2 gghighlight方法
gapminder %>% filter(country == "China")

install.packages("gghighlight")
library(gghighlight)

gapminder %>% 
  ggplot(
    aes(x = year,
        y = lifeExp,
        color = continent,
        group = country)
  )+
  geom_line()+
  gghighlight(
    country == "China",
    label_key = country
  )

gapminder %>% filter(continent == "Asia")

gapminder %>%
  filter(continent == "Asia") %>%
  ggplot(aes(year, lifeExp, color = country, group = country)) +
  geom_line(size = 1.2, alpha = .9, color = "#E58C23") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  gghighlight(
    country %in% c("China", "India", "Japan", "Korea, Rep."),
    use_group_by = FALSE,
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "grey90")
  ) +
  facet_wrap(vars(country))

#函数图
tibble(x = seq(from = -3, to = 3, by = .01)) %>%
  mutate(y = dnorm(x, mean = 0, sd = 1)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(color = "grey33")
