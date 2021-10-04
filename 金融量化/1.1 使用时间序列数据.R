install.packages("zoo")
library("zoo")

aapl<-read.zoo("C:/finalproject/archive/AAPL.csv", sep=",", header = TRUE, format = "%Y-%m-%d")

#我们画出股票价格图形
plot(aapl, main = "APPLE Closing Prices on NASDAQ", ylab = "Price (USD)", xlab = "Date")

head(aapl)

tail(aapl)
View(aapl)
#使用下面的命令，可以找出苹果股价在所有时间中的高点，
#和这个高点发生的日期。
aapl[which.max(aapl)]
View(aapl$High)

#当处理时间序列时，通常收益率更受关注，价格却不会。
#其原因是收益率通常平稳。
#因此我们会计算简单收益率或连续复合收益率（按百分比的形式）
ret_simple <- diff(aapl) / lag(aapl, k = -1) * 100
ret_cont <- diff(log(aapl)) * 100

#同时，我们也可以得到简单收益率的概括统计。
#在这里，我们使用coredata方法来表明我们仅仅关注股票价格，
#而非索引（日期）。
summary(coredata(ret_simple))
ret_simple[which.min(ret_simple)]

hist(ret_simple, breaks=100, main = "Histogram of Simple Returns", xlab="%")

aapl_2013 <- window(aapl, start = '2013-01-01', end = '2013-12-31')
aapl_2013[which.max(aapl_2013)]

#比如，我们使用简单的历史模拟法，
#可以很容易确定一天中置信水平为99%的在险价值（Value-at-Risk）
quantile(ret_simple,probs = 0.01)