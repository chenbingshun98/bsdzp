if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(ggplot2, ggthemes, dplyr, readr, showtext)


# 准备数据集
chilean_exports <- "year,product,export,percentage
2006,copper,4335009500,81
2006,others,1016726518,19
2007,copper,9005361914,86
2007,others,1523085299,14
2008,copper,6907056354,80
2008,others,1762684216,20
2009,copper,10529811075,81
2009,others,2464094241,19
2010,copper,14828284450,85
2010,others,2543015596,15
2011,copper,15291679086,82
2011,others,3447972354,18
2012,copper,14630686732,80
2012,others,3583968218,20
2013,copper,15244038840,79
2013,others,4051281128,21
2014,copper,14703374241,78
2014,others,4251484600,22
2015,copper,13155922363,78
2015,others,3667286912,22
"

#把原本用逗号分隔的文本变成数据框
exports_data <- read_csv(chilean_exports)

# 数据结构理解
str(exports_data)

p1 <- ggplot(data = exports_data,
             aes(x = year, y = export, colour = product)) +
  geom_line(size = 1.5)
p1

exports_data <- exports_data %>% 
  mutate(product = factor(product, levels = c("copper","others"),
                          labels = c("Copper","Pulp wood, Fruit, Salmon & Others")))
p3 <- ggplot(data = exports_data,
             aes(x = year,
                 y = export,
                 colour = product))+
  geom_line(size = 1.5)+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())# 用于控制图例 底部 水平 不显示标题
p3


p4 <- p3 + scale_x_continuous(breaks = seq(2006, 2015, 1))
p4

p5 <- p4+
  labs(title = "Composition of Exports to China($)",
       subtitle = "Source: The Observatory of Economic Complexity")+
  labs(x = "Year",y="USD million")
p5

colour <- c("#5F9EA0","#E1B378")
p6 <- p5 + scale_colour_manual(values = colour)
p6

#字体设置
font_add("Tahoma","Tahoma.ttf")
font_add("Roboto Condensed", "RobotoCondensed-Regular.ttf")
showtext_auto()

#白色主题

p7_1 <- ggplot(data = exports_data,
               aes(x = year, y = export, colour = product)) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2006, 2015, 1)) +
  labs(title = "Composition of Exports to China ($)",
       subtitle = "Source: The Observatory of Economic Complexity") +
  labs(x = "Year", y = "USD million") +
  scale_colour_manual(values = colour) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank())
p7_1


p7_2 <- ggplot(data = exports_data,
               aes(x = year, y = export, colour = product)) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2006,2015,1)) +
  labs(title = "Composition of Exports to China ($)",
       subtitle = "Source: The Observatory of Economic Complexity") +
  labs(x = "Year", y = "USD million") +
  theme_economist() + scale_colour_economist() +
  theme(axis.line.x = element_line(size = .5, colour = "black"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(family = "Roboto Condensed"),
        text = element_text(family = "Roboto Condensed"))
p7_2


p7_3 <- ggplot(data = exports_data,
               aes(x = year, y = export, colour = product)) +
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(2006,2015,1)) +
  labs(title = "Composition of Exports to China ($)",
       subtitle = "Source: The Observatory of Economic Complexity") +
  labs(x = "Year", y = "USD million") +
  theme_fivethirtyeight() + scale_colour_fivethirtyeight() +
  theme(axis.title = element_text(family = "Roboto Condensed"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(family = "Roboto Condensed"),
        legend.text = element_text(family = "Roboto Condensed"),
        text = element_text(family = "Roboto Condensed"))
p7_3
