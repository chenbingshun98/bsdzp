install.packages("plotROC")

library(plotROC)
library(ggplot2)

set.seed(1234)

outcome <- rbinom(200,size = 1,prob = 0.5)
marker1 <- rnorm(200,mean = outcome,sd=0.65)
marker2 <- rnorm(200,mean = outcome,sd=1.5)

mydata <- data.frame(outcome,marker1,marker2)
summary(mydata)

roc1 <- ggplot(mydata,aes(d=outcome,m=marker1))+
  geom_roc(n.cuts = 0)

roc1

roc2 <- roc1+geom_rocci()
roc2

roc1+style_roc(theme = theme_gray(),
                xlab = ("1-Specificity"))+
  annotate("text",x=0.25,y=0.75,label="Marker1")

head(mydata)

longdata <- melt_roc(mydata,"outcome",c("marker1","marker2"))
head(longdata)

roc3 <- ggplot(longdata,aes(d=D,m=M,linetype=name))+
  geom_roc()+
  style_roc(theme = theme_gray())+
  theme(legend.position = "none")+
  annotate("text",x=0.25,y=0.79,label="Marker1")+
  annotate("text",x=0.5,y=0.6,label="Marker2")

roc3
