# 清空工作空间 
rm(list = ls())

# 环境设置
# install.packages(stringr)
library(stringr)
# install.packages(dplyr)
library(dplyr)
# install.packages("showtext")
library(showtext)
# install.packages("pander")
library(pander)

setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_code/R与千寻（代码+数据）/第四章 R语言与非结构化数据分析/4.1 文本分析")
# 一键读入
dat = read.csv("novel.csv", fileEncoding = "UTF-8")

# 探索分析--观察类别变量的分布
table(dat$小说类型)

mean.order = dat %>% group_by(小说类型) %>% summarise(me = median(评论数)) %>% as.data.frame()
levels(dat$小说类型) = factor(dat$小说类型, levels = levels(dat$小说类型)[order(mean.order$me, decreasing = T)]) 
dat$小说类型 = factor(dat$小说类型, levels = dat$小说类型[mean.order$me]) 
 = factor(dat$小说类型, levels = order(mean.order$me, decreasing = T)) 

labels = levels(dat$小说类型)

# 描述分析--观察某个定量变量在不同类别上的分布
bp = boxplot(formula = log(dat$评论数) ~ dat$小说类型, col = rainbow(7, alpha = 0.4), xlab = "小说类型", ylab = "log(评论数)", xaxt = "n")
# axis(1, at = dat$小说类型, labels = FALSE)
text(1:13, par("usr")[3] - 0.6, labels = labels, srt = 45, pos = 1, xpd = TRUE)

# 建立基准组
dat = within(dat, 小说类型 <- relevel(dat$小说类型, ref = "二次元小说")) 

# 将基准组定位“二次元小说”

# 建模分析--作为回归变量
lm1 = lm(评论数 ~ 小说类型 + 总字数 + 评分, data = dat)
summary(lm1)
