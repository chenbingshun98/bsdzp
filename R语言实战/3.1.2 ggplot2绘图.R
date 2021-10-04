rm(list = ls())
# install.packages(ggplot2)
library(ggplot2)
# install.packages(plyr)
library(plyr)
# 加载钻石数据
data("diamonds")
# 抽取前500条数据,且保留6个变量
set.seed(30)
diamond = diamonds[sample(nrow(diamonds), 500), c(1:4, 7, 10)]
head(diamond)
summary(diamond)

## 柱状图 ##
# 基础柱状图
p = ggplot(data = diamond, mapping = aes(x = clarity))
p + geom_bar()

# 增加其他映射元素,成为累计柱状图
p = ggplot(data = diamond, mapping = aes(x = clarity, fill = cut))
p + geom_bar()

# 分组柱状图
p = ggplot(data = diamond, mapping = aes(x = clarity, fill = cut))
p + geom_bar(position = "dodge")

## 饼图 ##
df1 = ddply(diamond, .(cut), nrow)
(df1 = df1[order(df1$V1, decreasing = T), ])
(pos = (cumsum(df1$V1) - df1$V1/2))

ggplot(df1, aes(x="", y = V1, fill = factor(cut))) +
  geom_bar(width = 1,stat = "identity") +
  scale_fill_manual(values = rainbow(5,alpha = 0.4))

ggplot(df1, aes(x="", y = V1, fill = factor(cut))) +
  geom_bar(width = 1,stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(y = pos, label = paste(round(V1 / sum(V1) * 100, 2), "%", ""))) +
  scale_fill_manual(values = rainbow(5,alpha = 0.4)) +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
## 直方图 ##
# 基础作图
p = ggplot(data = diamond, mapping = aes(x = price))
p + geom_histogram()

# 调整组数
p = ggplot(data = diamond, mapping = aes(x = price))
p + geom_histogram(bins = 100)

# 按照切工分颜色
p = ggplot(data = diamond, mapping = aes(x = price, fill = cut))
p + geom_histogram(binwidth = 500)

# 按照切工画密度曲线
ggplot(data = diamond, mapping = aes(price, colour = cut)) +
  geom_freqpoly(binwidth = 500)

# 分面来看不同切工的对比
p = ggplot(data = diamond, mapping = aes(x = price, fill = cut))
p + geom_histogram() + facet_grid( ~ cut)

## 箱线图 ##
# 分组箱线图
ggplot(diamond) + geom_boxplot(aes(x = cut, y = price, fill = cut))

# 增加自定义配色
ggplot(diamond) + geom_boxplot(aes(x = cut, y = price, fill = cut)) + scale_fill_manual(values = c("lightpink", "lightyellow", "lightgreen", "lightblue", "mediumpurple1"))

ggplot(diamond) + geom_boxplot(aes(x = cut, y = price, fill = cut)) + scale_fill_manual(values = rainbow(5, alpha = 0.4))                                                                                        

## 折线图 ##
# 将搜索指数index变成时间序列格式
index = c(127910, 395976, 740802, 966845, 1223419, 1465722, 1931489, 2514324, 3024847, 3174056, 3208696, 3644736, 4198117, 3868350, 3576440, 3524784, 3621275, 3695967, 3728965, 3845193, 3525579, 3452680, 3535350, 3655541, 3884779, 3780629) / 10000
dat = seq(as.Date("2017/3/28"), length = 26, by = "day")
people_index = data.frame(date = dat, index = index)
p = ggplot(people_index, mapping = aes(x = date, y = index))
p + geom_line()

p + geom_line(colour = "green") + geom_area(colour = "green", alpha = 0.2)

## 散点图 ##
# 基础作图
p = ggplot(data = diamond, mapping = aes(x = carat, y = price))
p + geom_point()

# 添加映射元素
# 根据定性变量标识不同颜色
p = ggplot(data = diamond, mapping = aes(x = carat, y = price, colour = cut))
p + geom_point()

# 根据定量变量标识不同颜色
p = ggplot(data = diamond, mapping = aes(x = carat, y = price, colour = z))
p + geom_point()

# 增加统计变换
p = ggplot(diamond, aes(x = carat, y = price)) + geom_point()
p + scale_y_log10()

# 增加拟合曲线
p = ggplot(diamond, aes(x = carat, y = price)) + geom_point()
p + scale_y_log10() + stat_smooth()

# 基于cut分块
p = ggplot(diamond, aes(x = carat, y = price)) + geom_point() + scale_y_log10() + stat_smooth()
p + facet_grid( ~ cut)
