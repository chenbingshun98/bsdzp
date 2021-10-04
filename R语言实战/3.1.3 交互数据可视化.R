## 读取数据 ##
rm(list = ls())
# install.packages("plotly")
library(plotly)
# install.packages(plyr)
library(plyr)
# install.packages("reshape")
library(reshape)
load("AAPL.rda")
load("pm2.5.rda")
novel = read.csv("novel.csv", fileEncoding = "UTF-8", stringsAsFactors = F)
names(beijing) = iconv(names(beijing), "utf-8", "gbk")  # 解决Windows用户中文乱码问题，Mac用户可跳过
head(AAPL)
head(beijing)

## 柱状图 ##
(region = colnames(beijing))

(ave = colMeans(beijing, na.rm = TRUE))

(p = plot_ly(x = region, y = ave, type = "bar"))

(p = plot_ly(x = region, y = ave, type = "bar",
             marker = list(color = c('rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                     'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                     'rgba(204,204,204,1)', 'rgba(222,45,38,0.8)'))
))

## 饼图 ##
(pieData = data.frame(value = c(10, 30, 40), group = c("A", "B", "C")))
##    value group
##  1    10     A
##  2    30     B
##  3    40     C
(p = plot_ly(pieData, values = ~ value, labels = ~ group, type = "pie"))

plot_ly(AAPL, x = ~ Volume, type = "histogram")

# 增加坐标轴及标题信息
p = plot_ly(AAPL, x = ~ Volume, type = "histogram")
layout(p,
       title = "苹果股票成交量分布直方图", 
       xaxis = list( title = "股票成交量", showgrid = F),     
       yaxis = list( title = "频数"),    
       margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
)

## 折线图 ##
# 苹果公司股价变化图，Date为时间，Adj.Close为股票每日的调整收盘价
mat = data.frame(Date = AAPL$Date, 
                 AAPL = round(AAPL$Adj.Close, 2))
p = plot_ly(mat, x = ~ Date, y = ~ AAPL, type = 'scatter', mode = 'lines')
layout(p, xaxis = list(title = " ", showticklabels = TRUE, tickfont = list(size = 8)))

# 文中小作业：添加发布会日期
# 添加苹果发布会日期，其中%>%为管道符号，符号前面的结果可以作为符号后面函数的第一个参数输入
apple = data.frame(Date = AAPL$Date,
                   price = round(AAPL$Adj.Close, 2))
rownames(apple) = AAPL$Date

annouced = c("2010-06-07", "2011-10-04", "2012-09-12", "2013-09-10", "2014-09-09", "2015-09-09", "2016-09-08")
product = c("iPhone 4", "iPhone 4s", "iPhone 5", "iPhone 5s", "iPhone 6", "iPhone 6s", "iPhone 7")
(p = plot_ly(apple,x = ~ Date) %>% 
    add_trace(y = ~ price, type = 'scatter', mode = 'lines', name = "股票价格") %>%
    add_trace(x = annouced, y = apple[annouced, "price"], type = 'scatter', mode = 'text', text = product, textposition = 'down', showlegend = FALSE ) %>% 
    add_trace(x = annouced, y = apple[annouced, "price"], type = 'scatter', mode = 'markers', name = "发布会" ) %>% 
    layout(title = "苹果股票价格与新苹果手机的发布", 
           xaxis = list(title = " ", showticklabels = TRUE, tickfont = list(size = 8)),
           yaxis = list(title = "调整收盘价（美元）"),
           margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)))

## 散点图 ##
(p = plot_ly(novel, x = ~ 总点击数, y = ~ 评论数, type = "scatter", mode = "markers"))

## 箱线图 ##
# 选择已经完成小说
novel_finish = novel[novel$写作进程 %in% c("已经完本", "接近尾声", "出版中"), ]
medi = ddply(novel_finish, .(小说类型), function(x) {median(x$总字数)})
medi = medi[order(medi$V1, decreasing = T), ]
novel_finish$小说类型 = factor(novel_finish$小说类型, levels = medi$小说类型)
(p = plot_ly(novel_finish, y = ~ 总字数, color = ~ 小说类型, type = "box"))

## 分组箱线图 ##
novel = novel[!novel$小说性质 == "", ]
p = plot_ly(novel, x = ~ 小说类型, color = ~ 小说性质, type = "histogram")
layout(p,
       title = "不同小说的作品类型分布", 
       xaxis = list( title = "小说类型", showgrid = F, showticklabels = TRUE, tickfont = list(size = 10)),     
       yaxis = list( title = "频数"),    
       margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
)
