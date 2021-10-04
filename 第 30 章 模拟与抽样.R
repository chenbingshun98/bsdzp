library(tidyverse)

rnorm(n = 5, mean = 0, sd = 1)

dnorm(seq(0.1, 0.5, length.out = 10), mean = 0, sd = 1)

tibble(
  x = rnorm(n = 100, mean = 0, sd = 1)
) %>%
  ggplot(aes(x = x)) +
  geom_density()

# 我们将模拟的正态分布和理论上正态分布画在一起
tibble(
  x = rnorm(
    n = 100,
    mean = 0,
    sd = 1
  )
) %>%
  ggplot(aes(x = x)) +
  geom_density() +
  stat_function( # Draw a function as a continuous curve
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    color = "red" # 理论上的曲线为红色
  )
# 例子2，在数据框(data.frame)下，
# 建立模拟
# x和 y的线性关系y

# 现实中，观察值往往会带入误差，假定误差服从正态分布，那么
# x和 y的线性关系重新表述为

beta_0 <- 4
beta_1 <- 3.2
epsilon <- rnorm(n = 1000, mean = 0, sd = 1)

sim_normal <- tibble(
  # x_vals = runif(1000, 0, 10)
  x_vals = seq(from = 0, to = 5, length.out = 1000),
  y_vals = beta_0 + beta_1 * x_vals + epsilon,
)

sim_normal %>% head()

sim_normal %>%
  ggplot(aes(x = x_vals, y = y_vals)) +
  geom_point()

# 或者
tibble(
  a = runif(1000, 0, 5),
  b = 4 + rnorm(1000, mean = 3.2 * a, sd = 1)
) %>%
  ggplot(aes(x = a, y = b)) +
  geom_point()

# MASS::mvrnorm
# 产生多元高斯分布的随机数，
# 每组随机变量高度相关。
a <- 3.5
b <- -1
sigma_a <- 1
sigma_b <- 0.5
rho <- -0.7

mu <- c(a, b)
cov_ab <- sigma_a * sigma_b * rho # 协方差
cov_ab

# 构建协方差矩阵
sigma <- matrix(c(
  sigma_a^2, cov_ab,
  cov_ab, sigma_b^2
), ncol = 2)
sigma

d <- MASS::mvrnorm(
  1000,
  mu = mu, Sigma = sigma
) %>%
  data.frame() %>%
  set_names("group_a", "group_b")
head(d)

d %>%
  ggplot(aes(x = group_a)) +
  geom_density(
    color = "transparent",
    fill = "dodgerblue3",
    alpha = 1 / 5
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = 3.5, sd = 1),
    color = "red",
    linetype = 2
  )

d %>%
  ggplot(aes(x = group_b)) +
  geom_density(
    color = "transparent",
    fill = "dodgerblue3",
    alpha = 1 / 2
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = -1, sd = 0.5),
    linetype = 2
  )

d %>%
  ggplot(aes(
    x = group_a,
    y = group_b
  )) +
  geom_point() +
  stat_ellipse(
    type = "norm",
    level = 0.95
  )

d %>% summarise(
  a_mean = mean(group_a),
  b_mean = mean(group_b),
  a_sd = sd(group_a),
  b_sd = sd(group_b),
  cor = cor(group_a, group_b),
  cov = cov(group_a, group_b)
)

# 蒙特卡洛
set.seed(2019)
n <- 50000
po <- tibble(
  "x" = runif(n),
  "y" = runif(n)
)

po <- po %>%
  mutate(
    inside = map2_dbl(
      .x = x,
      .y = y,
      ~ if_else(.x**2 + .y**2 < 1, 1, 0)
    )
  ) %>%
  rowid_to_column("N") # Also included is rowid_to_column(), which adds a column at the start of the dataframe of ascending sequential row ids starting at 1.


# 正方形的面积是1，圆的面积是 πr^2 = 1/4 π
# 如果知道两者的比例，就可以估算

po <- po %>%
  mutate(
    estimate = 4 * cumsum(inside) / N
  )

po %>% tail()

po %>%
  ggplot() +
  geom_line(aes(
    y = estimate,
    x = N
  ),
  colour = "#82518c"
  ) +
  geom_hline(yintercept = pi)

# 抽样与样本
# 总体分布
true.mean <- 175.7
true.sd <- 15.19

pop.distn <- tibble(
  height = seq(100, 250, 0.5),
  density = dnorm(height,
    mean = true.mean,
    sd = true.sd
  )
)


ggplot(pop.distn) +
  geom_line(aes(height, density)) +
  geom_vline(
    xintercept = true.mean,
    color = "red",
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = true.mean + true.sd,
    color = "blue",
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = true.mean - true.sd,
    color = "blue",
    linetype = "dashed"
  ) +
  labs(
    x = "Height(cm)",
    y = "Density",
    title = "川师男生身高分布"
  )

# 样本
sample.a <- tibble(height = rnorm(
  n = 30,
  mean = true.mean,
  sd = true.sd
))
sample.a %>%
  ggplot(aes(x = height)) +
  geom_histogram(aes(y = after_stat(density)),
    fill = "steelblue",
    alpha = 0.75,
    bins = 10
  ) +
  geom_line(
    data = pop.distn,
    aes(
      x = height,
      y = density
    ),
    alpha = 0.25,
    size = 1.5
  ) +
  geom_vline(
    xintercept = true.mean,
    linetype = "dashed",
    color = "red"
  ) +
  geom_vline(
    xintercept = mean(sample.a$height),
    linetype = "solid"
  )

# 红色的虚线代表分布的总体的均值，黑色实线代表30个样本的均值，
sample.a %>%
  summarize(
    sample.mean = mean(height),
    sample.sd = sd(height)
  )

# 重新找了30个男生，和上次类似，用rnorm函数模拟，我们记为样本b
sample.b <- tibble(height = rnorm(30, mean = true.mean, sd = true.sd))

sample.b %>%
  ggplot(aes(x = height)) +
  geom_histogram(aes(y = stat(density)),
    fill = "steelblue",
    alpha = 0.75,
    bins = 10
  ) +
  geom_line(
    data = pop.distn,
    aes(
      x = height,
      y = density
    ),
    alpha = 0.25,
    size = 1.5
  ) +
  geom_vline(
    xintercept = true.mean,
    linetype = "dashed",
    color = "red"
  ) +
  geom_vline(
    xintercept = mean(sample.a$height),
    linetype = "solid"
  )

sample.b %>%
  summarize(
    sample.mean = mean(height),
    sample.sd = sd(height)
  )

# 为了避免重复写代码 ，我把上面的过程整合到一起，写一个子函数，专门模拟抽样过程
rnorm.stats <- function(n, mu, sigma) {
  the.sample <- rnorm(n, mu, sigma)
  tibble(
    sample.size = n,
    sample.mean = mean(the.sample),
    sample.sd = sd(the.sample)
  )
}

true.mean <- 175.7
true.sd <- 15.19

# 哇，一下子抽了2500个样本,全部装进了df.sample.of.30这个数据框， 偷偷看一眼呢
rnorm.stats(30, true.mean, true.sd)
df.samples.of.30 <-
  purrr::rerun(2500, rnorm.stats(30, true.mean, true.sd)) %>%
  dplyr::bind_rows()

df.samples.of.30 %>% head()

df.samples.of.30 %>%
  ggplot(aes(
    x = sample.mean,
    y = stat(density)
  )) +
  geom_histogram(
    bins = 25,
    fill = "firebrick",
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = true.mean,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    title = "抽样2500次（每次30个人男生）身高均值的分布",
    subtitle = "Distribution of mean heights for 2500 samples of size 30"
  )

df.samples.of.30 %>%
  ggplot(aes(
    x = sample.mean,
    y = stat(density)
  )) +
  geom_histogram(
    bins = 50,
    fill = "firebrick",
    alpha = 0.5
  ) +
  geom_histogram(
    data = sample.a,
    aes(
      x = height,
      y = stat(density)
    ),
    bins = 11,
    fill = "steelblue",
    alpha = 0.25
  ) +
  geom_vline(
    xintercept = true.mean,
    linetype = "dashed",
    color = "red"
  ) +
  geom_line(
    data = pop.distn,
    aes(
      x = height,
      y = density
    ),
    alpha = 0.25,
    size = 1.5
  ) +
  xlim(125, 225)

df.samples.of.30 %>%
  summarize(
    mean.of.means = mean(sample.mean),
    sd.of.means = sd(sample.mean)
  )

df.samples.of.50 <- rerun(2500, rnorm.stats(50, true.mean, true.sd)) %>%
  bind_rows()

df.samples.of.100 <- rerun(2500, rnorm.stats(100, true.mean, true.sd)) %>%
  bind_rows()

df.samples.of.250 <- rerun(2500, rnorm.stats(250, true.mean, true.sd)) %>%
  bind_rows()

df.samples.of.500 <- rerun(2500, rnorm.stats(500, true.mean, true.sd)) %>%
  bind_rows()

# 忍不住想画图看看，每次抽取的男生数量不同，均值的分布会有不同？
df.combined <- bind_rows(
  df.samples.of.30,
  df.samples.of.50,
  df.samples.of.100,
  df.samples.of.250,
  df.samples.of.500
) %>%
  mutate(
    sample.sz = as.factor(sample.size)
  )

df.combined %>%
  ggplot(aes(
    x = sample.mean,
    y = stat(density),
    fill = sample.sz
  )) +
  geom_histogram(
    bins = 25,
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = true.mean,
    linetype = "dashed"
  ) +
  facet_wrap(vars(sample.sz),
    nrow = 1
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Sample means",
    y = "Density",
    title = "Distribution of mean heights for samples of varying size"
  )

sampling.distn.mean.table <- df.combined %>%
  group_by(sample.size) %>%
  summarize(
    mean.of.means = mean(sample.mean),
    sd.of.means = sd(sample.mean)
  )
sampling.distn.mean.table

# 标准误
df.se.mean.theory <- tibble(
  sample.size = seq(10, 500, 10)
) %>%
  mutate(
    std.error = true.sd / sqrt(sample.size)
  )

df.se.mean.theory

sampling.distn.mean.table %>%
  ggplot(aes(x = sample.size, y = sd.of.means))+
  geom_point()+
  geom_line(aes(x = sample.size, y = std.error),
            data = df.se.mean.theory,
            color = 'red')+
  labs(
    x = "Sample size", 
    y = "Std Error of Mean",
    title = "平均值标准误差随样本大小变化（理论值和模拟值对比）"
  )

sampling.distn.sd.table <- df.combined %>% 
  group_by(sample.size) %>% 
  summarise(
    mean.of.sds = mean(sample.sd),
    sd.of.sds = sd(sample.sd)
  )

sampling.distn.sd.table
