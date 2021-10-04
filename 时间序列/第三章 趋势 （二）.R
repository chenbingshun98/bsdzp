#(二)
library(TSA)
data(rwalk)
lm_rwalk <- lm(rwalk ~ time(rwalk))
summary(lm_rwalk)

win.graph(width = 4.8, height = 2.5, pointsize = 8)
plot(rwalk, col = "blue")
abline(lm_rwalk, col = "red")
data(wages)
lm2_wages = lm(wages ~ time(wages) +I(time(wages)^2))
summary(lm2_wages)

xfit <- time(wages)
yfit_2 <- fitted(lm2_wages)
win.graph(width = 4.8,  height = 2.5, pointsize = 8)
plot(wages, ylab = "Monthly Wages", xlab = "Time", type = "l").
lines(as.vector(xfit), as.vector(yfit_2), col = "red")

data("tempdub")
temper_month <- season(tempdub)
lm_temper_1 <- lm(tempdub ~ temper_month - 1)#减一意味着不包含截距项  
summary(lm_temper_1)

lm_temper_2 <- lm(tempdub ~ temper_month)
summary(lm_temper_2)#虽然这是两种不同的回归结果，但是表达的情况一样

har_temper <- harmonic(tempdub,1)#将这里的气温转换为cos(2*pi*t)和 sin(2*pi*t) 的n阶矩阵
lm_temper_3 <- lm(tempdub ~har_temper)
summary(lm_temper_3)
