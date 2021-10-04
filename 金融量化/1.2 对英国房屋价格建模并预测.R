## p 11
install.packages("forecast")
library("forecast")
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Quantitative_Finance-master/R_for_Quantitative_Finance-master/Introduction-to-R-for-Quantitative-Finance/ch1_time_series_analysis")
library(zoo)
hp <- read.zoo("UKHP.csv", sep = ",", header = TRUE, format = "%Y-%m", FUN = as.yearmon)

frequency(hp)
#结果表示，一个周期（称为年）中有12个子周

#为了深入分析，我们再次计算数据的简单收益率。
hp_ret <- diff(hp) / lag(hp, k = -1) * 100
hp_ret

mod <- auto.arima(hp_ret, stationary = TRUE, seasonal = FALSE, ic="aic")
mod
summary(mod)

confint(mod)

tsdiag(mod)

plot(mod$x, lty = 1, main = "UK house prices: raw data vs. fitted values", ylab = "Return in percent", xlab = "Date")
lines(fitted(mod), lty = 2,lwd = 2, col = "red")
accuracy(mod)

predict(mod, n.ahead=3)

plot(forecast(mod))
