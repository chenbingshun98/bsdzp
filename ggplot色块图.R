library(tidyverse)

#色块图
#3种
#geom_rect()和geom_tile()仅参数不同
#geom_raster 
ggplot(faithfuld, aes(waiting, eruptions))+
  geom_raster(aes(fill = density))

ggplot(faithfuld, aes(waiting, eruptions))+
  geom_raster(aes(fill = density), interpolate = TRUE)

#矩形块，geom_tile()
df <- tibble(
  x = rep(c(2, 5, 7, 9, 12), 2),
  y = rep(c(1, 2), each = 5),
  z = factor(rep(1:5, each = 2)),
  w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
)
ggplot(df, aes(x, y))+
  geom_tile(aes(fill = z), color = "grey50")

ggplot(df, aes(x, y, width = w))+
  geom_tile(aes(fill = z), color = "grey50")

#geom_rect()
ggplot(df, aes(
  xmin = x - w / 2,
  xmax = x + w / 2,
  ymin = y,
  ymax = y + 1
  )
  )+
  geom_rect(aes(fill = z),
            color = "grey50")

p1 <- ggplot(df, aes(x, y, width = w))+
  geom_tile(aes(fill = z), color = "grey50")+
  coord_polar(theta = "x")

p2 <- ggplot(df, aes(x, y, width = w))+
  geom_tile(aes(fill = z), color = "grey50")+
  coord_polar(theta = "y")

cowplot::plot_grid(p1, p2)


#华夫饼图
#侧重于展示类别数值
df <- tibble(
  x = rep(1:10, 10),
  y = rep(1:10, each = 10),
  class = sort(sample(mpg$class, 100))
)

sample_n(df, 67) %>% 
  arrange(x, y) %>% 
  group_by(x) %>% 
  mutate(y = 1:n()) %>% 
  ggplot(aes(x, y, fill = class))+
  geom_tile(color = "white")+
  coord_fixed()+
  theme(
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

#侧重于展示类别占比的百分比堆积型
ggplot(df, aes(x, y, fill = class))+
  geom_tile(color = "white")+
  scale_y_continuous(trans = "reverse")+
  theme(
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

#点状华夫饼图
#点阵，离散
ggplot(df, aes(x, y, fill = class))+
  geom_point(size = 12, shape = 21)+
  coord_fixed()+
  scale_y_continuous(trans = "reverse")+
  theme(panel.background = element_blank())

#马赛克图
df <- tibble(
  gene = rep(LETTERS[1:5], 4),
  status = rep(c("alpha", "beta", "gamma", "delta"), each = 5),
  value = sample(1:100, 20),
  percent = rep(c(10, 30, 30, 20, 10), 4)
)

df %>% 
  group_by(status) %>% 
  mutate(
    xmax = cumsum(percent),
    xmin = xmax - percent
    ) %>%
  group_by(gene) %>% 
  mutate(
    ytmp = value * 100 / sum(value),
    ymax = cumsum(ytmp),
    ymin = ymax - ytmp
  ) %>% 
  mutate(
    x = (xmin + xmax) / 2,
    y = (ymin + ymax) / 2
  ) %>% 
  ggplot() +
  geom_rect(
    aes(
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax,
      fill = status
    ),
    color = "black"
  )+
  geom_text(aes(x, y, label = paste0(round(ytmp, 2), "%")))+
  geom_text(aes(x = x, y = 103, label = gene))+
  theme(panel.background = element_blank())

data <- df %>% 
  select(sample, gene, MutFunc) %>% 
  distinct() %>% 
  filter(MutFunc != ".")

#
p <- group_by(mpg, class) %>% 
  summarise(percent = n() / nrow(mpg)) %>% 
  ggplot(aes(x = factor(1), y = percent, fill = class))+
  geom_col(color = "white")
p
p + coord_polar(theta = "y")

p + coord_polar(theta = "y")+
  geom_text(aes(label = paste0(round(percent * 100, 2), "%")),
            position = position_fill(vjust = 0.5))+
  theme(
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

#甜甜圈图
p <- group_by(mpg, class) %>% 
  summarise(percent = n() / nrow(mpg)) %>% 
  ggplot(aes(x = 1, y = sort(percent, decreasing = T),
             fill = class))+
  geom_col(color = "white")

p + coord_polar(theta = "y", start = 1.65)+
  geom_text(aes(label = paste0(round(percent * 100, 2), "%")),
            position = position_fill(vjust = 0.75))+
  xlim(c(-1, 2))+
  theme(
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

P <- group_by(mpg, class) %>% 
  summarise(percent = n() / nrow(mpg)) %>% 
  ggplot(aes(x = 2, y = sort(percent,decreasing = T), 
             fill = class))+
  geom_col(color = "white")

P + coord_polar(theta = "y", start = 1.65)+
  geom_text(aes(x = 1, 
                label = paste0(class, "(", round(percent * 100, 2), "%)")),
            position = position_fill(vjust = 0.5),
            size = 3,
            hjust = 0.5
            )+
  xlim(c(-1, 3))+
  theme(
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
