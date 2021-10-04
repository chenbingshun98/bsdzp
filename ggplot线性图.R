esamp <- sample_n(economics, 10)
m <- ggplot(
  esamp,
  aes(unemploy / pop, psavert)
)

p1 <- m + geom_path()

p2 <- m + geom_path(aes(color = as.numeric(date)))

plot_grid(p1, p2)

c <- ggplot(
  economics,
  aes(date, pop)
)
c1 <- c + geom_line(arrow = arrow())
c2 <- c + geom_line(
  arrow = arrow(angle = 15, ends = "both", type = "closed")
)

plot_grid(c1, c2)

base <- tibble(
  x = 1:3,
  y = c(4, 1, 9)
) %>% ggplot(aes(x, y))

b1 <- base + geom_path(size = 8)
b2 <- base + geom_path(
  size = 8,
  lineend = "round"
)
b3 <- base + geom_path(
  size = 8,
  lineend = "round",
  color = "red"
)
b4 <- base + geom_path(
  size = 8,
  linejoin = "mitre",
  lineend = "round"
)
plot_grid(b1, b2, b3, b4)

# 当线条的中间有NA值时，则会有一个断点
df <- tibble(
  x = 1:5,
  y = c(1, 2, NA, 4, 5)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line()

# 设置线条类型
economics_long %>%
  filter(variable %in% c("uempmed", "unemploy")) %>%
  ggplot(aes(date, value01, colour = variable)) +
  geom_line(aes(linetype = factor(variable))) +
  scale_linetype_manual("variable",
    values = c(5, 3)
  )

# 报错
# 无法同时设置渐变色与线条类型
economics_long %>%
  filter(variable %in% c("uempmed", "unemploy")) %>%
  ggplot(aes(date, value01, group = variable)) +
  geom_line(aes(color = value01),
    linetype = 2
  )

# 参考线
# geom_hline:yintercept
# geom_vline:xintercept
# geom_abline:slope, intercept

p <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()

# 固定值
p1 <- p + geom_vline(xintercept = 5)

# 使用向量
p2 <- p + geom_vline(xintercept = 1:5)

# 水平线
p3 <- p + geom_hline(yintercept = 20)

p4 <- p + geom_abline(
  intercept = 31,
  slope = -5
)

plot_grid(p1, p2, p3, p4)

# 计算拟合曲线的拮据和斜率，然后绘制直线
coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline(intercept = 37, slope = -5)

p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  facet_wrap(~cyl)

mean_wt <- data.frame(
  cyl = c(4, 6, 8),
  wt = c(2.28, 3.11, 4.00)
)

p + geom_hline(aes(yintercept = wt),
  data = mean_wt
)

# 添加其他属性
ggplot(
  mtcars,
  aes(mpg, wt, color = wt)
) +
  geom_point() +
  geom_hline(
    aes(
      yintercept = wt,
      color = wt
    ),
    mean_wt
  ) +
  facet_wrap(~cyl)

# 线段和曲线
# geom_segment 用于绘制两个点之间的直线
# geom_curve 用于绘制两点之间的曲线
b <- ggplot(
  mtcars,
  aes(wt, mpg)
) +
  geom_point()

df <- data.frame(
  x1 = 2.320,
  x2 = 3.520,
  y1 = 22.8,
  y2 = 15.5
)

b + geom_curve(aes(
  x = x1,
  y = y1,
  xend = x2,
  yend = y2,
  color = "curve"
),
data = df
) +
  geom_segment(
    aes(
    x = x1,
    y = y1,
    xend = x2,
    yend = y2,
    color = "segment"
  ),
  data = df
  )

# 设置不同的曲率
b1 <- b + geom_curve(aes(
  x = x1,
  y = y1,
  xend = x2,
  yend = y2
),
data = df,
curvature = -0.2
)

b2 <- b + geom_curve(
  aes(
    x = x1,
    y = y1,
    xend = x2,
    yend = y2
  ),
  data = df,
  curvature = 0.9
)

plot_grid(b1, b2)

# 添加箭头
b + geom_curve(
  aes(
    x = x1,
    y = y1,
    xend = x2,
    yend = y2
  ),
  data = df,
  arrow = arrow(length = unit(0.05, "npc"))
)

# 使用geom_segment通过设置线段大小来绘制直方图
counts <- as.data.frame(table(x = rpois(100, 5)))
counts$x <- as.numeric(as.character(counts$x)) # 把因子转成数值型

ggplot(
  counts,
  aes(x, Freq)
) +
  geom_segment(aes(
    xend = x,
    yend = 0
  ),
  size = 10,
  lineend = "butt"
  )

df <- expand_grid(
  x = 1:10,
  y = 1:10
)

df$angle <- runif(100, 0, 2 * pi)
df$speed <- runif(100, 0, sqrt(0.1 * df$x))

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_spoke(aes(angle = angle),
    radius = 0.5
  )

# 函数曲线
set.seed(2021)
ggplot(
  tibble(x = rnorm(100)),
  aes(x)
) +
  geom_density() +
  geom_function(
    fun = dnorm,
    color = "red"
  )

# 指定范围
base <- ggplot() +
  xlim(-5, 5)
base + geom_function(fun = dnorm)

# 设置函数的参数值
base + geom_function(
  fun = dnorm,
  args = list(mean = 2, sd = .5)
)

b1 <- base + stat_function(
  fun = dnorm,
  geom = "point"
)

b2 <- base + stat_function(
  fun = dnorm,
  geom = "point",
  n = 20
)

cowplot::plot_grid(b1, b2)

# 自定义函数
p1 <- base +
  geom_function(
    aes(color = "normal"),
    fun = dnorm
  ) +
  geom_function(
    aes(color = "t, df = 1"),
    fun = dt,
    args = list(df = 1)
  )

# 使用匿名函数
p2 <- base + geom_function(fun = function(x) 0.5 * exp(-abs(x)))

p3 <- base + geom_function(fun = ~ 0.5 * exp(-abs(.x)))

cowplot::plot_grid(p1, p2, p3)

# 路线图
sample_n(mtcars, 10) %>%
  ggplot(aes(mpg, disp)) +
  geom_point(
    color = "#69b3a2",
    na.rm = T
  ) +
  geom_segment(
    aes(
      xend = c(tail(mpg, n = -1), NA),
      yend = c(tail(disp, n = -1), NA)
    ),
    arrow = arrow(length = unit(0.3, 'cm')),
    color = '#69b3a2'
  )+
  geom_text(aes(label = disp), hjust = 1.2)+
  theme_bw()

#坡度图
library(ggrepel)
mpg %>% 
  group_by(year, manufacturer) %>% 
  summarise(value = sum(displ)) %>% 
  pivot_wider(
    names_from = year,
    values_from = value
  ) %>% 
  mutate(class = if_else((`1999` - `2008`) > 0, '#8dd3c7', '#bebada')) %>% 
  ggplot()+
  geom_segment(
    aes(
      x = 1,
      xend = 2,
      y = `1999`,
      yend = `2008`,
      color = class
    ),
    size = .75,
    show.legend = F
  )+
  geom_vline(xintercept = 1,
             linetype = 'solid',
             size = 1,
             color = '#ff7f00')+
  geom_vline(xintercept = 2,
             linetype = 'solid',
             size = 1,
             color = '#1f78b4')+
  geom_point(aes(x = 1, y = `1999`),
             size = 3,
             shape = 21,
             fill = 'green')+
  geom_point(aes(x = 2, y = `2008`),
             size = 3,
             shape = 21,
             fill = 'red')+
  scale_color_manual(
    labels = c('Up', 'Down'),
    values = c('#8dd3c7', '#bebada')
  )+
  xlim(.5, 2.5)+
  geom_text_repel(
    aes(
      x = 1,
      y = `1999`,
      label = `1999`
      ),
    hjust = 'left',
    size = 3.5
    )+
  geom_text_repel(
    aes(
      x = 2,
      y = `2008`,
      label = `2008`
    ),
    hjust = 'right',
    size = 3.5
  )+
  geom_text(
    aes(
      y = 1.03*max(max(`1999`), max(`2008`))
    ),
    label = '1999',
    x = 1,
    size = 5,
    hjust = 1.2
  )+
  geom_text(
    aes(
      y = 1.03*max(max(`1999`), max(`2008`))
    ),
    label = '2008',
    x = 2,
    size = 5,
    hjust = -.2
  )+
  theme_void()
