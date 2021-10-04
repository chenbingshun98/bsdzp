install.packages("TSA")
library("TSA")
data(larain)
win.graph(width = 4.8, height = 2.5, pointsize = 8)
plot(larain, ylab = "Inches", xlab = "Year", type = "l", col = "black")
points(larain,col = "blue")
savePlot(filename = "LA.eps", type = c("eps"))
win.graph(width = 8, height = 8 ,pointsize = 8)
plot(y = larain, x = zlag(larain), ylab = "Inches", xlab = "Previous Year Inches", col = "blue")

a <- larain
b <- zlag(larain)
n <-length(larain)
cor(a[2:n],b[2:n])
?legend

