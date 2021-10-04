dat <- read.table('C:/Users/terry/Documents/我爱学习/统计软件/R/datforfor.txt', header = TRUE, sep = '\t')
dat <- data.table::fread('https://github.com/linhesun/bilibiliRlearning/blob/master/20201026/datforfor.txt',
                         header = TRUE, sep = "\t") #不行

dat_net <- read_delim(clipboard(), delim = "\t")
dat_net <- data.table::fread("clipboard")#不行
dat_net <- data.table::fread(clipboard(), sep = "\t")#不行
dat_net <- read_delim("clipboard", "\t")#不行

plotdat <- dat[,1:4]
names(plotdat)[4] <- 'rmse'
plotdat$model <- names(dat)[4]

plotdat2 <- dat[,c(1:3, 5)]
names(plotdat2)[4] <- 'rmse'
plotdat2$model <- names(dat)[5]
plotdat3 <- rbind(plotdat, plotdat2)


plotdat <- dat[,1:4]
names(plotdat)[4] <- 'rmse'
plotdat$model <- names(dat)[4]
for(i in 5:9){print(i)
  onedat <- dat[,c(1:3, i)]
  names(onedat)[4] <- 'rmse'
  onedat$model <- names(dat)[i]
  plotdat <- rbind(plotdat, onedat)
}

library(ggplot2)
ggplot(plotdat, aes(x = MLR.R2, y = rmse, colour = model, shape = model)) + geom_point(size = 3)

aa <- 1:10
bb <- NULL
for (i in 1:10){
  bb[i] <- sum(aa[1:i])
}
bb
cumsum(aa)

i = 0
while(i < 10){
  print(i)
  i <- i + 1
}
