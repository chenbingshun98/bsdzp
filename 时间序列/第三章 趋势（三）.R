library(TSA)
data("tempdub")
temper_month <- season(tempdub)
lm_temper_1 <- lm(tempdub~temper_month-1)
summary(lm_temper_1)


xfit <- time(tempdub)
yfit <- fitted(lm_temper_1)
res_temper <- rstudent(lm_temper_1)#Regression Deletion Diagnostics
win.graph(width = 4.8,height = 2.5,pointsize = 8)
plot(y = res_temper,
     x = as.vector(xfit),
     xlab = 'Time',
     ylab = "standardized Residuals",
     type = 'l')
points(res_temper,
       x = as.vector(xfit),
       col = 'blue')

win.graph(width = 4.8,height = 2.5,pointsize = 8)
plot(y = res_temper,
     x = as.vector(xfit),
     xlab = 'Time',
     ylab = "standardized Residuals",
     type = 'l')
points(res_temper,
       x = as.vector(xfit),
       pch = as.vector(temper_month),
       col = 'blue')

win.graph(width = 4.8,
          height = 2.5,
          pointsize = 8)
qqnorm(res_temper,main = "",col = "blue")
hist(res_temper,
     xlab = "Standized Residuals",
     main = "",
     col = 'blue')

win.graph(width = 4.8,
          height = 2.5,
          pointsize = 8)
acf(res_temper,
    main = '',
    col = 'blue')

t.test(as.vector(res_temper))
t.test(res_temper)

shapiro.test(as.vector(res_temper))

runs(as.vector(res_temper))
