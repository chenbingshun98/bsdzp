### 数据准备 ###
# 清空工作空间
rm(list = ls())
# 载入相关包及设定路径
# install.packages(plyr)
library(plyr)
# install.packages("reshape2")
library(reshape2)
# 读入数据
novel = read.csv("C:/Users/terry/Documents/我爱学习/统计软件/R/R_code/R与千寻（代码+数据）/第三章 R语言与统计分析/3.1 描述分析及可视化/3.1.1 基础描述分析/novel.csv", fileEncoding = "UTF-8")
# 数据查看与异常处理
head(novel)

### 单变量 ###

## 定性变量--柱状图 ##
a = table(novel$小说类型)
a = a[order(a, decreasing = T)]
barplot(a[1:5], names.arg = names(a)[1:5], col = rainbow(5, alpha = 0.4), xlab = "小说类型", ylab="频数")

## 定性变量--饼图 ##
pie(c(4000, 3000, 2000, 1000), labels = c("北京", "天津", "上海", "广州"), main = "熊粉成员分布", col = 2:5)

# 将小说类型进行简要合并
novel$'小说类别' = "其他"
novel$'小说类别'[novel$小说类型 == "都市小说" | novel$小说类型 == "职场小说"] = "都市类小说"
novel$'小说类别'[novel$小说类型 == "科幻小说" | novel$小说类型 == "玄幻小说" | novel$小说类型 == "奇幻小说"] = "幻想类小说"
novel$'小说类别'[novel$小说类型 == "武侠小说" | novel$小说类型 == "仙侠小说"] = "武侠类小说"
# 求出每一类所占百分比
ratio = table(novel$'小说类别') / sum(table(novel$'小说类别')) * 100
# 定义标签
label1 = names(ratio)
label2 = paste0(round(ratio, 2), "%")
# 画饼图
pie(ratio, col = heat.colors(5, alpha = 0.4), labels = paste(label1, label2, sep = "\n"), font = 1)

## 定量变量--直方图 ##
novel$总字数 = novel$总字数 / 10000
par(mfrow = c(1, 2))
chara = sort(novel$总字数)[1:1500]  # 去掉异常值
hist(chara, breaks = 10, xlab = "总字数(万字)", ylab = "频数", main = "", col = "lightblue")
hist(chara, breaks = 100, xlab = "总字数(万字)", ylab = "频数", main = "", col = "lightblue")

## 定量变量--折线图 ##
par(mfrow = c(1, 1))
# 画时间序列图
data(AirPassengers)
head(AirPassengers)
##  [1] 112 118 132 129 121 135
class(AirPassengers)
##  [1] "ts"
plot(AirPassengers)
# 人民的名义百度搜索指数图
# install.packages(zoo)
library(zoo)

# 将搜索指数index变成时间序列格式
index = c(127910, 395976, 740802, 966845, 1223419, 1465722, 1931489, 2514324, 3024847, 3174056, 3208696, 3644736, 4198117, 3868350, 3576440, 3524784, 3621275, 3695967, 3728965, 3845193, 3525579, 3452680, 3535350, 3655541, 3884779, 3780629) / 10000
date = seq(as.Date("2017-3-28"), length = 26, by = "day")
#使用zoo( )函数将时间标签及对应的数据“组合”在一起
people_index = zoo(index, date)
class(people_index)
##  [1] "zoo"
plot(people_index, xlab = "时间", ylab = "百度搜索指数（万）", main = "《人民的名义》搜索指数折线图")

#如果对横轴显示时间的格式不满意，
#还可以通过axis( )函数中的tick 和label_name参数来自定义标签，
#其中tick用来确定横轴标记（即小竖线）的位置，
#label_name用来设定显示的标签
# 更改坐标轴显示内容
plot(people_index, xaxt = "n", xlab = "时间", ylab = "百度搜索指数（万）", main = "《人民的名义》搜索指数折线图")
times = date #or directly times = x.Date
ticks = seq(times[1], times[length(times)], by = "weeks")  # month, weeks, year etc.
label_name = c("3月28日", "4月4日", "4月11日", "4月18日")
axis(1, at = ticks, labels = label_name, tcl = -0.3)

### 两个变量 ###

## 定性与定量变量--分组箱线图 ##
# 将画板分成1行2列
par(mfrow = c(1, 2))    
# 不同性质的小说总点击数和评论数有差别吗
boxplot(log(总点击数) ~ 小说性质, data = novel, col = rainbow(2, alpha = 0.3), ylab = "总点击数对数")
boxplot(log(评论数) ~ 小说性质, data = novel, col = rainbow(2, alpha = 0.3), ylab = "总评论数对数")

# 将画板恢复
par(mfrow = c(1, 1))    

## 两个定量变量--散点图 ##
# 去除较大的异常值后画图
test = novel[novel$评论数 < 8000 & novel$总点击数 < 200000, ]
x = test$总点击数
y = test$评论数
plot(x, y, pch = 1, cex = 0.6, xlab = "总点击数", ylab = "评论数")

# 分组做分组箱线图
aa = cut(x, breaks = c(0, 50000, 100000, 150000, 200000), labels = c("(0-5w]", "(5w-10w]", "(10w-15w]", "(15w-20w]"))
boxplot(y ~ aa, col = rainbow(4, alpha = 0.4), xlab = "总点击数", ylab = "评论数")

# 散点图矩阵
plot(iris[, 1:4])
## 两个定性变量--柱状图 ##
a = ddply(novel, .(小说类别,小说性质), nrow)
d = dcast(a, stri_enc_toutf8(小说性质 ~ 小说类别)[, -1])
##  Using V1 as value column: use value.var to override.
rownames(d) = c("VIP作品", "大众作品")
(d = as.matrix(d))
##           都市类小说 幻想类小说 其他 武侠类小说
##  VIP作品          34         45  188         18
##  大众作品        339        404  370        149
# beside = T，按列累计
barplot(d, beside = F, col = rainbow(2, alpha = 0.3))
legend("topright", legend = c("VIP作品", "大众作品"),
       fill = rainbow(2, alpha = 0.3), cex = 0.8)

