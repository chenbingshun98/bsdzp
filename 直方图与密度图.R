library(tidyverse)

ggplot(diamonds, aes(carat))+
  geom_histogram()

ggplot(diamonds, aes(carat))+
  geom_histogram(bins = 100)

ggplot(diamonds, aes(carat))+
  geom_histogram(binwidth = 0.01)

ggplot(diamonds, aes(y = carat))+
  geom_histogram(binwidth = 0.01)

ggplot(diamonds, aes(price, fill = cut))+
  geom_histogram(bins = 40)

ggplot(diamonds, aes(price, color = cut))+
  geom_freqpoly(bins = 40)

ggplot(diamonds, aes(price, after_stat(density),
                     color = cut))+
  geom_freqpoly(bins = 40)

data <- tibble(
  var1 = rnorm(1000),
  var2 = rnorm(1000, mean = 2)
)

ggplot(data, aes(x = x))+
  geom_histogram(aes(x = var1,
                     y = after_stat(density)),
                 fill = "#69b3a2")+
  geom_label(aes(x = 4.5,
                 y = 0.25,
                 label = "variable1"),
             color = "#69b3a2")+
  geom_histogram(
    aes(x = var2,
        y = -after_stat(density)),
    fill = "#404080"
  )+
  geom_label(
    aes(x = 4.5,
        y = -0.25,
        label = "variable2"),
    color = "#404080"
  )

#多变量直方图
tibble(
  type = c(rep("variable 1", 1000), rep("variable 2", 1000)),
  value = c(rnorm(1000), rnorm(1000, mean = 4))
) %>% 
  ggplot(aes(x = value, fill = type))+
  geom_histogram(color = "#e9ecef",
                 alpha = 0.6,
                 position = "identity")+
  scale_fill_manual(values = c("#377eb8", "#4daf4a"))

#密度图
#adjust,用于调整带宽
ggplot(diamonds, aes(carat))+
  geom_density()

ggplot(diamonds, aes(carat))+
  geom_density(adjust = 1/5)

ggplot(diamonds, aes(carat))+
  geom_density(adjust = 5)

#分组密度图
ggplot(diamonds, aes(depth, color = cut))+
  geom_density()+
  xlim(55, 70)

ggplot(diamonds, aes(depth, fill = cut, color = cut))+
  geom_density(alpha = 0.1)+
  xlim(55, 70)

#堆积密度图
ggplot(diamonds, aes(carat, fill = cut))+
  geom_density(position = "stack")

ggplot(diamonds, aes(carat, after_stat(count), fill = cut))+
  geom_density(position = "stack")

#百分比密度图
ggplot(diamonds, aes(carat, after_stat(count), fill = cut))+
  geom_density(position = "fill")

#二维直方图
d <- ggplot(diamonds,
            aes(x, y))+
  xlim(4, 10)+
  ylim(4, 10)
d+geom_bin2d()

d + geom_bin2d(bins = 10)

d+geom_bin2d(binwidth = c(0.1, 0.1))

#更改颜色
d + geom_bin2d(aes(fill = after_stat(count)))+
  scale_fill_gradientn(colors = rainbow(10))

d + geom_bin2d(size = 1, color = 'green')

#geom_hex
d <- ggplot(diamonds, aes(x, y))+
  xlim(4, 10)+
  ylim(4, 10)

d + geom_hex()

d + geom_hex(size = 1, color = "#FF9900FF")

#二维密度图
m <- ggplot(faithful, aes(x = eruptions, y = waiting))+
  geom_point()+
  xlim(0.5, 6)+
  ylim(40, 110)

m + geom_density2d()

m + geom_density_2d_filled(alpha = 0.5)

m+geom_density_2d_filled(alpha = 0.5)+
  geom_density_2d(size = .25,
                  color = 'black')

d <- sample_n(diamonds, 1000) %>% 
  ggplot(aes(x, y))

d + geom_density_2d(aes(color = cut))

#进行分面
d + geom_density_2d_filled()+
  facet_wrap(vars(cut))

#将观测值的数量映射到颜色强度
d + geom_density_2d_filled(contour_var = "count")+
  facet_wrap(vars(cut))

#色块图函数，raster
d + stat_density_2d(
  geom = "raster",
  aes(fill = after_stat(density)),
  contour = FALSE
)+
  scale_fill_viridis_c()

d + stat_density_2d(
  geom = "point",
  aes(size = after_stat(density)),
  n = 20,
  contour = FALSE
)

d + stat_density_2d(
  geom = "polygon",
  aes(fill = after_stat(level)),
  bins = 30
)+
  scale_fill_viridis_c()

#组合分布图
library(cowplot)

N <- 500
df <- tibble(
  x1 = rnorm(n = N, mean = 2),
  x2 = rnorm(n = N, mean = 2),
  
  y1 = rnorm(n = N, mean = 2),
  y2 = rnorm(n = N, mean = 2)
)

?system
?dir
?dir.create

top_hist <- ggplot(df, aes(x1))+
  geom_histogram(
    bins = 35,
    fill = "#1f78b4",
    color = "black"
  )+
  theme_void()

right_hist <- ggplot(df, aes(x2))+
  geom_histogram(
    bins = 35,
    fill = "#1f78b4",
    color = "black"
    )+
  coord_flip()+
  theme_void()

center <- ggplot(df, aes(x1, x2))+
  geom_hex(color = "black")+
  scale_fill_gradientn(colors = rainbow(10))+
  theme(
    panel.background = element_rect(
      fill = "white",
      color = "black",
      size = 0.25
      ),
    axis.line = element_line(color = "black", size = 0.25),
    axis.title = element_text(size = 13, face = "plain", color = "black"),
    axis.text = element_text(size = 12, face = "plain", color = "black"),
    legend.position = c(0.10, 0.80),
    legend.background = element_blank()
  )

p1 <- plot_grid(top_hist, center,
                align = "v",
                nrow = 2,
                rel_heights = c(1, 4))

p2 <- plot_grid(NULL, right_hist,
                align = "v",
                nrow = 2,
                rel_heights = c(1, 4))

plot_grid(p1, p2, ncol = 2, rel_widths = c(4, 1))

glist <- list(top_hist, center, right_hist)
