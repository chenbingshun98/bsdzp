library(tidyverse)
library(cowplot)
p1 <- ggplot(mtcars) +
  geom_bar(aes(mpg)) +
  scale_x_binned(name = expression(1+exp(2)))

p2 <- ggplot(mtcars) +
  geom_bar(aes(mpg)) +
  scale_x_binned() +
  labs(title = "This is example", tag = "A", x = expression(1+exp(2)))

p3 <- ggplot(mtcars) +
  geom_bar(aes(mpg)) +
  scale_x_binned(name = expression(1+exp(2)), limits = c(10, 20), 
  )

p4 <- ggplot(mtcars) +
  geom_bar(aes(mpg)) +
  scale_x_binned(name = expression(1+exp(2)), limits = c(10, 20), 
                 breaks = seq(10, 20, 2), labels = c("A", 'B', 'C', 'D', 'E', 'F'),
  )

cowplot::plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2)

p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()

p2 <- p + labs(colour = "Cylinders")

p3 <- p + labs(x = "New x label")

p4 <- p + labs(title = "New plot title")

p5 <- p + labs(title = "New plot title", subtitle = "A subtitle")

p6 <- p + labs(caption = "(based on data from ...)")

p7 <- p + labs(title = "title", tag = "A")

p8 <- p + labs(title = "title") + labs(title = NULL)

plot_grid(p, p2, p3, p4, p5, p6, p7, p8, nrow = 4, ncol = 2,
          labels = c("A", 'B', 'C', 'D', 'E', 'F', 'G', 'H'))

#在默认的情况下，超出范围的数据都会被设置为 NA 值，
#这意味着超出范围的数据将会被删除
p1 <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point()

p2 <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  xlim(15, 20)

##如果更大的值在前面，轴的数据将会反转，
p3 <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  xlim(20, 15)

#你也可以将一个值设置为 NA，让 ggplot2 来推断数值范围。
p4 <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  xlim(NA, 20)

plot_grid(p1, p2, p3, p4, 
          labels = c('A', 'B', 'C', 'D')
)

##我们可以使用 lims 在不同的图片中设置同样的配色方案，如
small <- subset(mtcars, cyl == 4)
big <- subset(mtcars, cyl > 4)

p1 <- ggplot(small, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() +
  lims(colour = c("4", "6", "8"))

p2 <- ggplot(big, aes(mpg, wt, colour = factor(cyl))) +
  geom_point() +
  lims(colour = c("4", "6", "8"))

plot_grid(p1, p2)

#对于下面这个例子
last_month <- Sys.Date() - c(0:59)
last_month <- Sys.Date() - 0:59

df <- data.frame(
  date = last_month,
  price = c(rnorm(30, mean = 15), runif(30) + 0.2 * (1:30))
)

p <- ggplot(df, aes(date, price)) +
  geom_line() +
  stat_smooth()
p

p + lims(x= c(Sys.Date() - 30, NA), y = c(10, 20))

p + coord_cartesian(xlim =c(Sys.Date() - 30, NA), ylim = c(10, 20))

#除了设置坐标轴的范围，我们也可以设置图形中必须包含某个或某些值。

#可以使用 expand_limits() 函数，该函数接受一个命名列表，名称必须为图形属性
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()

mtcars %>% distinct(mpg) %>% summarise(min=min(.), max=max(.))

mtcars %>% distinct(wt) %>% summarise(min=min(.), max=max(.))

#下面，让我们用 expand_limits 来调整数据范围
p1 <- p + expand_limits(x = 0)

p2 <- p + expand_limits(y = c(1, 9))

p3 <- p + expand_limits(x = 0, y = 0)

#在图 A 中，我们设置 X 轴数据要包含 0，尽管并没有对应的值，但是 X 坐标轴还是会从 0 开始。

# 类似的，图 B 设置了 Y 轴需要包含 1 和 9，超过了数据的范围，相当于扩大了 Y 轴的范围

plot_grid(p1, p2, p3, labels = c('A', 'B', 'C'), nrow = 1)

p1 <- ggplot(mtcars,
             aes(mpg, wt))+
  geom_point(aes(colour = cyl))+
  expand_limits(colour = seq(2, 10, by = 2))

p2 <- ggplot(mtcars, 
             aes(mpg, wt))+
  geom_point(aes(colour = factor(cyl)))+
  expand_limits(colour = factor(seq(2, 10, by = 2)))

plot_grid(p1, p2, labels = c('A', 'B'))

mtcars %>% distinct(cyl)

#设置间隔
#可以使用 expansion() 函数设置数据与轴之间的间隔，该函数主要搭配 scale_(x|y)_continuous 与 scale_(x|y)_discrete 使用。

# 推荐使用
expansion(mult = 0, add = 0)

# 已被弃用
expand_scale(mult = 0, add = 0)

# mult: 百分比间距，接受一个向量，
#如果向量长度为 1，则上下间距的间隔是一样的，
#如果长度为 2，则 mult[1] 为下间距，mult[2] 为上间距
# add: 常数值单位间距，类似 mult

p1 <- ggplot(mtcars)+
  geom_bar(aes(x = factor(cyl)))

p2 <- ggplot(mtcars)+
  geom_bar(aes(x = factor(cyl)))+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))

p3 <- ggplot(subset(diamonds, carat > 2),
             aes(cut, clarity))+
  geom_jitter()+
  scale_x_discrete(expand = expansion(add = 2))

p4 <- ggplot(subset(diamonds, carat > 2),
             aes(cut, price))+
  geom_jitter()+
  scale_x_discrete(expand = expansion(add = .6))+
  scale_y_continuous(expand = expansion(mult = .05))

plot_grid(p1, p2, p3, p4, labels = LETTERS[1:4],
          nrow = 2)

#设置透明度
scale_alpha(..., range = c(0.1, 1))

scale_alpha_continuous(..., range = c(0.1, 1))

scale_alpha_binned(..., range = c(0.1, 1))

scale_alpha_discrete(...)

scale_alpha_ordinal(..., range = c(0.1, 1))

#首先在aes中设置透明度，如
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(alpha = year))
p

#然后使用标度函数来修改透明度范围
#在图 A 中，我们传递了一个字符串，相当于设置了 name 参数的值，更改了图例的名称
p1 <- p + scale_alpha("cylinders")

p2 <- p + scale_alpha(range = c(0.4, 0.8))

plot_grid(p1, p2, labels = LETTERS[1:2],
          nrow = 1)

#自定义透明度
p <- ggplot(mpg,
            aes(displ, hwy))+
  geom_point(aes(alpha = factor(year)))
p          

p+scale_alpha_manual(values = c(0.1, 0.2))

#设置线条类型
# 0 = blank         1 = solid
# 2 = dashed        3 = dotted
# 4 = dotdash       5 = longdash
# 6 = twodash

df_lines <- tibble(
  linetype = factor(0:6)
)

ggplot(df_lines)+
  geom_hline(aes(linetype = linetype,
                 yintercept = 0),
             size = 2)+
  scale_linetype_discrete()+
  facet_grid(linetype ~ .)+
  theme_void(20)

economics_long
base <- ggplot(economics_long,
               aes(date, value01))
base+geom_line(aes(group = variable))


#为图片添加线型和颜色
base+geom_line(aes(linetype = variable,
                   colour = variable))

#自定义线条映射
df <- economics_long %>% filter(variable %in% c("pop", "pce", "uempmed"))
lt <- c('pop' = 1, 'pce' = 3, 'uempmed' = 5)

ggplot(df,
       aes(date, value01))+
  geom_line(aes(linetype = variable, colour = variable))+
  scale_linetype_manual(values = lt)

ggplot(df,
       aes(date, value01))+
  geom_line(aes(colour = variable))+
  scale_linetype_manual(values = lt)#不一样

#设置形状
#scale_shape() 函数可以将离散变量映射为 6 个不同的形状。

# 如果你有超过 6 个类别，你将会得到一个警告信息，并且第 7 个或更后面的类别将不会在图片中显示
#其中 solid 参数控制形状是不是实心的
scale_shape(..., solid = TRUE)

scale_shape_binned(..., solid = TRUE)

df_shapes <- data.frame(shape = 0:25)

ggplot(df_shapes,
       aes(0, 0,
           shape = shape))+
  geom_point(aes(shape = shape),
             size = 5,
             fill = 'red')+
  scale_shape_identity()+
  facet_wrap(~shape)+
  theme_void()

dsmall <- diamonds[sample(nrow(diamonds), 100), ]

d <- ggplot(dsmall,
            aes(carat, price))+
  geom_point(aes(shape = cut))

p2 <- d+scale_shape(solid = F)

p3 <- d+scale_shape(name = 'Cut of diamond')

levels(dsmall$cut) <- c('Fair', 'Good', "Very Good", 'Premium', 'Ideal')

#注意：在绘制图 D 时，会输出警告信息，提示不建议对排序的变量设置形状
p4 <- ggplot(dsmall, 
             aes(price, carat))+
  geom_point(aes(shape = cut))

plot_grid(d, p2, p3, p4, labels = LETTERS[1:4])

#自定义形状映射
shape_map <- c('Fair' = 13, 'Good' = 20, 'Very Good' = 15, 'Premium' = 2, 'Ideal' = 11)

d+scale_shape(solid = F)+
  scale_shape_manual(values = shape_map)

#设置大小
scale_size(..., range = c(1, 6))

scale_radius(..., range = c(1, 6))

scale_size_binned(..., range = c(1, 6))

scale_size_area(..., max_size = 6)

scale_size_binned_area(..., max_size = 6)

#散点图
p <- ggplot(mpg, aes(displ, hwy, size = hwy)) +
  geom_point()

# 设置图例标题
p1 <- p + scale_size("Highway mpg")

# 设置点的大小范围
p2 <- p + scale_size(range = c(0, 10))

# 允许将 0 值映射为面积为 0 的点（不绘制）
p3 <- p + scale_size_area()

# 方便从看出图例中看出分箱
p4 <- p + scale_size_binned()

# This is most useful when size is a count
p5 <- ggplot(mpg, aes(class, cyl)) +
  geom_count() +
  scale_size_area()

# 将数据映射为半径，不推荐
p6 <- p + scale_radius()

plot_grid(p1, p2, p3, p4, p5, p6,
          labels = LETTERS[1:6], nrow = 3)

#自定义数据大小，我们根据 cyl 的值倒序设置点的大小
ggplot(mpg, aes(displ, hwy, size = factor(cyl)))+
  geom_point()+
  scale_size_manual(
    name = 'cyl',
    values = c(
      '4' = 5,
      '5' = 4,
      '6' = 3,
      '8' = 2
    )
  )

#10.1 ColorBrewer 配色
#有三个不同的标度函数来设置

# 针对离散型数据
scale_*_brewer()
# 针对连续型数据
scale_*_distiller()
# 针对分箱数据
scale_*_fermenter()
#其中 * 代表 colour（轮廓颜色） 和 fill（填充色）。

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]

(d <- ggplot(dsamp,
             aes(carat, price))+
    geom_point(aes(colour = clarity)))

#但是我觉得这种配色不太好看，想换一种。我们进行如下尝试
p1 <- d + scale_colour_brewer()

p2 <- d + scale_colour_brewer(palette = "Greens")

p3 <- d + scale_colour_brewer(palette = "Set1")

p4 <- d + scale_colour_brewer(palette = "Spectral")

plot_grid(p1, p2, p3, p4, labels = LETTERS[1:4], nrow = 2)

#连续性的
d1 <- ggplot(dsamp, aes(carat, price)) +
  geom_point(aes(colour = depth))

d2 <- d + scale_colour_distiller(palette = "Greens")
d2
d3 <- d + scale_colour_distiller(palette = "Set1")
d3
d4 <- d + scale_colour_distiller(palette = "Spectral")
d4
plot_grid(d1, d2, d3, d4, labels = LETTERS[1:4], nrow = 2)

#分箱
ggplot(dsamp, aes(carat, price)) +
  geom_point(aes(colour=depth)) +
  scale_colour_fermenter(palette = "Spectral")

# 为每个分箱设置颜色相同的配色
ggplot(dsamp,
       aes(carat, price, colour = price))+
  geom_point()+
  scale_x_binned()+
  scale_color_fermenter(palette = 'Spectral')

#渐变色
# scale_*_gradient：双色渐变，使用 low 和 high 两个参数控制两端的颜色
# 
# scale_*_gradient2：三色渐变，有 low、mid 和 high 三个参数，low 和 high 作用同上，mid 默认值为 0 表示中点的颜色，可以使用 midpoint 参数设置中点位置
# 
# scale_*_gradientn：多色渐变，为 colours 参数设置一个颜色向量，不加其他参数会选择范围内的均匀分布值，离散型颜色可以指定 values 参数。

df <- data.frame(
  x = runif(100),
  y = runif(100),
  z1 = rnorm(100),
  z2 = abs(rnorm(100))
)

df_na <- data.frame(
  value = seq(1, 20),
  x = runif(20),
  y = runif(20),
  z1 = c(rep(NA, 10), rnorm(10))
)

#默认双色渐变
p1 <- ggplot(df, aes(x, y))+
  geom_point(aes(colour = z2))

#调整双色渐变区间
p2 <- ggplot(df, aes(x, y))+
  geom_point(aes(colour = z2))+
  scale_color_gradient(low = 'white',
                       high = 'black')

#三色渐变
p3 <- ggplot(df, aes(x, y))+
  geom_point(aes(colour = z1))+
  scale_color_gradient2()

#调整三色渐变区间
p4 <- ggplot(df, aes(x, y))+
  geom_point(aes(colour = z1))+
  scale_color_gradient2(low = 'green',
                        high = 'blue',
                        midpoint = 1)

plot_grid(p1, p2, p3, p4, labels = LETTERS[1:4], nrow = 2)

#自定义颜色范围，以及 NA 值的处理
p5 <- ggplot(df, aes(x, y))+
  geom_point(aes(color = z1))+
  scale_color_gradientn(colours = topo.colors(10))

p6 <- ggplot(df, aes(x, y))+
  geom_point(aes(color = z1))+
  scale_color_gradientn(colors = rainbow(10))

#去除NA值
p7 <- ggplot(df_na, aes(x = value, y))+
  geom_bar(aes(fill = z1),
           stat = 'identity')+
  scale_fill_gradient(low = 'yellow',
                      high = 'red',
                      na.value = NA)

p8 <- ggplot(df_na, aes(x, y))+
  geom_point(aes(color = z1))+
  scale_color_gradient(low = "yellow",
                       high = 'red',
                       na.value = NA)

plot_grid(p5, p6, p7, p8, labels = LETTERS[1:4], nrow = 2)

#灰色渐变
#使用 start 和 end 控制灰度范围
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(colour = factor(cyl)))

p + scale_colour_grey()

#更改设置
p1 <- p + scale_color_grey(end = 0.5)

#更改主题
p2 <- p + scale_color_grey()+
  theme_bw()

#缺失值
miss <- factor(sample(c(NA, 1:5),
                      nrow(mtcars),
                      replace = T))

p3 <- ggplot(mtcars,
             aes(mpg, wt))+
  geom_point(aes(color = miss))+
  scale_color_grey()

p4 <- ggplot(mtcars,
             aes(mpg, wt))+
  geom_point(aes(color = miss))+
  scale_color_grey(na.value = 'green')

plot_grid(p1, p2, p3, p4, labels = LETTERS[1:4], nrow = 2)

lego <- tibble(x = LETTERS[1:7],
               y = c(6:3,2,2,1))
lego

ggplot(lego, aes(x, y, fill = x))+
  geom_col()+
  scale_fill_manual(
    name = '',
    values = c('A' = '#70D500',
               'B' = '#E0EAEC',
               'C' = '#EB6A74',
               'D' = '#F7F252',
               'E' = '#4A72FA',
               'F' = '#70FFDC',
               'G' = '#FA7D26')
  )+
  theme_classic()+
  scale_x_discrete("",
                   labels = NULL)+
  scale_y_discrete("",
                   labels = NULL)+
  theme(legend.position = "none")

?legend

ggplot(lego, aes(x, y, color = x))+
  geom_col()+
  scale_color_manual(
    name = '',
    values = c('A' = '#70D500',
               'B' = '#E0EAEC',
               'C' = '#EB6A74',
               'D' = '#F7F252',
               'E' = '#62FAE6',
               'F' = '#70FFDC',
               'G' = '#FA7D26')
  )+
  theme_classic()
