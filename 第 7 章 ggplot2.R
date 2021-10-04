library(tidyverse)
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
penguins <- read_csv("penguins.csv") %>%
  janitor::clean_names() %>%
  drop_na()
penguins <- read.csv("penguins.csv") %>%
  janitor::clean_names() %>%
  drop_na()

library(ggplot2)

ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm, color = species)
) +
  geom_point()

ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm, size = species)
) +
  geom_point()

ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm, shape = species)
) +
  geom_point()

ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm, alpha = species)
) +
  geom_point()

ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(color = "blue")

ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(size = 5)

ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(shape = 2)

ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm)
) +
  geom_point(alpha = 0.5)

p1 <-
  ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()
p1

p2 <-
  ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_smooth()
p2

p3 <-
  ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth()
p3

install.packages("patchwork")
library(patchwork)
(p1 / p2) | p3

# 全局变量与局部变量
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point()

ggplot(penguins) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species))

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point()

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species)) +
  geom_smooth()

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(aes(color = sex))

# 分别拟合
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_smooth(method = lm) +
  geom_point()

# 保存照片
ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_smooth(method = lm) +
  geom_point(aes(color = species)) +
  ggtitle("This is my first plot")


ggsave(
  filename = "myfirst_plot.pdf",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

# 作业
# 企鹅嘴巴长度和嘴巴厚度的散点图（bill_length_mm，bill_depth_mm）
# 不同企鹅种类用不同的颜色
# 整体的线性拟合
# 不同种类分别线性拟合
#
# ggplot(penguins, aes(x = ___, y = ___)) +
#   geom_point() +
#   geom_smooth() +
#   geom_smooth()


ggplot(penguins, aes(
  x = bill_length_mm,
  y = bill_depth_mm
)) +
  geom_point(aes(color = species)) +
  geom_smooth(aes(
    x = bill_length_mm,
    y = bill_depth_mm,
    color = species
  ),
  method = lm
  ) +
  geom_smooth(method = lm)

# 棒棒糖图
penguins$island <- as.factor(penguins$island)
penguins$island
p1 <- ggplot(penguins, aes(x = species, y = island))
p1
p1 <- p1 + geom_point(aes(species),
  size = 10
)
p1

penguins %>%
  count(c(species, island), sort = T)
table(subset(penguins, select = c(island, species)))
?count
# 转换为因子
data("mtcars")
df <- mtcars
df$cyl <- as.factor(df$cyl)
df$name <- row.names(df)
head(df)

ggplot(df, aes(name, mpg)) +
  geom_point(size = 5) +
  geom_segment(aes(x = name, xend = name, y = 0, yend = mpg))

ggplot(df, aes(name, mpg)) +
  geom_point(
    size = 5, color = "red", fill = alpha("orange", 0.3),
    alpha = 0.7, shape = 21, stroke = 3
  ) +
  geom_segment(aes(x = name, xend = name, y = 0, yend = mpg)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggplot(df, aes(name, mpg)) +
  geom_point(aes(size = cyl, color = cyl)) +
  geom_segment(aes(x = name, xend = name, y = 0, yend = mpg),
    size = 1, color = "blue", linetype = "dotdash"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0))

ggplot(df, aes(name, mpg)) +
  geom_point(aes(color = cyl), size = 8) +
  geom_segment(aes(x = name, xend = name, y = 0, yend = mpg),
    size = 1, color = "grey"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_flip()

styler:::style_active_file()
