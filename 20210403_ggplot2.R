library(tidyverse)
library(cowplot)
#分箱渐变色
scale_*_steps()
scale_*_steps2()
scale_*_stepsn()

df <- data.frame(
  x = runif(100),
  y = runif(100),
  z1 = rnorm(100)
)

p1 <- ggplot(df, aes(x, y))+
  geom_point(aes(color = z1))

p2 <- ggplot(df, aes(x, y))+
  geom_point(aes(color = z1))+
  scale_color_steps(low = 'skyblue',
                    high = 'blue')

p3 <- ggplot(df, aes(x, y))+
  geom_point(aes(color = z1))+
  scale_color_steps2()

p4 <- ggplot(df, aes(x, y))+
  geom_point(aes(color = z1))+
  scale_color_stepsn(colors = terrain.colors(10))

plot_grid(p1, p2, p3, p4, labels = LETTERS[1:4], nrow = 2)

#色轮颜色
# 色相（hue）：0-360 之间的值（代表角度），代表颜色。如红、黄、蓝等
# 
# 亮度（luminance）：表示颜色的明亮程度，0 表示黑色，100 表示白色
# 
# 色度（chroma）：颜色的纯度，色度为 0 表示灰色，最大值根据色相和亮度的组合而变化
scale_*_hue(
  ...
  h = c(0, 360) + 15,
  c = 100,
  l = 65,
)

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsamp,
            aes(carat, price))+
  geom_point(aes(color = clarity))

p1 <- d + scale_color_hue()

p2 <- d + scale_color_hue('Clarity')

p3 <- d + scale_color_hue(expression(clarity[beta]))

d + scale_color_hue(l = 40, c = 30)

plot_grid(d, p1, p2, p3, labels = LETTERS[1:4], nrow = 2)
d + scale_color_hue(expression(clarity[i]))

#调整亮度和色相
p1 <- d + scale_colour_hue(l = 40, c = 30)

p2 <- d + scale_colour_hue(l = 70, c = 30)

p3 <- d + scale_colour_hue(l = 70, c = 150)

p4 <- d + scale_colour_hue(l = 80, c = 150)

plot_grid(p1, p2, p3, p4, labels = LETTERS[1:4], nrow = 2)

#设置色相的范围
p1 <- d + scale_colour_hue(h = c(0, 90))

p2 <- d + scale_colour_hue(h = c(90, 180))

p3 <- d + scale_colour_hue(h = c(180, 270))

p4 <- d + scale_colour_hue(h = c(270, 360))

plot_grid(p1, p2, p3, p4, labels = LETTERS[1:4], nrow = 2)

#设置透明度

d <- ggplot(dsamp, aes(carat, price, colour = clarity))

p2 <- d + geom_point(alpha = 0.9)

p3 <- d + geom_point(alpha = 0.5)

p4 <- d + geom_point(alpha = 0.2)

plot_grid(p2, p3, p4, labels = LETTERS[1:4], nrow = 3)

#给 NA 值设置特殊的颜色
miss <- factor(sample(c(NA, 1:5),
                      nrow(mtcars),
                      replace = T))

p1 <- ggplot(mtcars,
             aes(mpg, wt))+
  geom_point(aes(color = miss))

p2 <- ggplot(mtcars,
             aes(mpg, wt))+
  geom_point(aes(color = miss))+
  scale_color_hue(na.value = 'black')


plot_grid(p1, p2, labels = LETTERS[1:4], nrow = 2)

#10.6 Viridis 配色
# viridis 提供的颜色映射，让无论是在彩色还是黑白图片均能够很容易看出区别。
# 
# 它还被设计能够让色盲观众也能看清
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
dsamp

#默认的颜色设置方式会根据因子的顺序上色

ggplot(dsamp,
       aes(carat, price))+
  geom_point(aes(color = clarity))

# viridis_d 用于离散数据
txsamp <- subset(txhousing, city %in%
                   c("Houston", "Fort Worth", "San Antonio", "Dallas", "Austin"))
d <- ggplot(data = txsamp, aes(x = sales, y = median)) +
  geom_point(aes(colour = city))

# 设置配色和标签
p1 <- d + scale_colour_viridis_d("City\nCenter")

# 选择调色板，使用 ?scales::viridis_pal 获取更多细节
p2 <- d + scale_colour_viridis_d(option = "plasma")

p3 <- d + scale_colour_viridis_d(option = "inferno")

plot_grid(d, p1, p2, p3, labels = LETTERS[1:4], nrow = 2)

# 设置填充色
p <- ggplot(txsamp, aes(x = median, fill = city)) +
  geom_histogram(position = "dodge", binwidth = 15000)
p1 <- p + scale_fill_viridis_d()

# 反转颜色
p2 <- p + scale_fill_viridis_d(direction = -1)

plot_grid(p1, p2, labels = LETTERS[1:4], nrow = 2)

# 连续型数据
#geom_tile()热力分布图
v <- ggplot(faithfuld) +
  geom_tile(aes(waiting, eruptions, fill = density))

p2 <- v + scale_fill_viridis_c()

p3 <- v + scale_fill_viridis_c(option = "plasma")
# 分箱数据
p4 <- v + scale_fill_viridis_b()

plot_grid(v, p2, p3, p4, labels = LETTERS[1:4], nrow = 2)

#10.6 连续型和离散型
scale_colour_continuous(
  ...,
  type = getOption("ggplot2.continuous.colour", default = "gradient")
)

scale_fill_continuous(
  ...,
  type = getOption("ggplot2.continuous.fill", default = "gradient")
)
faithfuld
v <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile()

p1 <- v + scale_fill_continuous(type = "gradient")

p2 <- v + scale_fill_continuous(type = "viridis")

p3 <- v + scale_fill_gradient()

p4 <- v + scale_fill_viridis_c()

plot_grid(p1, p2, p3, p4, labels = LETTERS[1:4], nrow = 2)