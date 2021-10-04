library(TSA)
Rwalk <- vector()
Rwalk[1] <- rnorm(1)
for(i in 2:60){
  Rwalk[i] <- Rwalk[i-1] + rnorm(1)
}
ts_Rwalk <- ts(Rwalk)
win.graph(width = 4.8, height = 2.5, pointsize = 8)
plot(ts_Rwalk, ylab = "Random Walk", xlab = "Time", type = "l")
points(ts_Rwalk, col = "blue")



