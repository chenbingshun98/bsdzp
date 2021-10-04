##### 3.3 回归分析 #####
rm(list = ls())  # 清空工作空间

#### 3.3.2 逻辑回归 ####
# 加载所需R包
# install.packages("ggplot2")
library(ggplot2)
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_code/R与千寻（代码+数据）/第三章 R语言与统计分析/3.3 回归分析/3.3.2 逻辑回归")
# 读入数据
dat1 = read.csv("JuneTrain.csv")
dat2 = read.csv("JulyTest.csv")
head(dat1)  # 查看数据的前几行
# 重新命名数据以及排列
dat1 = dat1[, c("tenure", "expense", "COUNT", "perperson", "entropy", "chgexpense", "chgcount", "churn")]
colnames(dat1) = c("tenure", "expense", "count", "perperson", "entropy", "chgexpense", "chgcount", "churn")
dat2 = dat2[, c("tenure", "expense", "COUNT", "perperson", "entropy", "chgexpense", "chgcount", "churn")]
colnames(dat2) = c("tenure", "expense", "count", "perperson", "entropy", "chgexpense", "chgcount", "churn")

# churn:是否流失;tenure:在网时长;expense:当月话费;count:通话人数;
# perperson:人均通话时长;entropy:通话时长分布;chgexpense:花费变化率;chgcount:通话人数变化率

# 描述分析，用ggplot2画对比箱线图。
# 描述分析内容 "tenure"     "expense"    "count"      "perperson"  "entropy"    "chgexpense" "chgcount"
# tenure和是否流失
p1 = ggplot(dat1, aes(x = as.factor(churn), y = tenure, fill = as.factor(churn)))  + geom_boxplot() +
  guides(fill = FALSE) + theme_minimal() + xlab("是否流失") + ylab("在网时长") +
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold")) +
  scale_fill_hue(c = 45, l = 80)
# expense和是否流失
p2 = ggplot(dat1, aes(x = as.factor(churn), y = expense, fill = as.factor(churn)))  + geom_boxplot() +
  guides(fill = FALSE) + theme_minimal() + xlab("是否流失") + ylab("当月话费") +
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size =12, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold")) +
  scale_fill_hue(c = 45, l = 80)
# 加载所需R包
# install.packages("gridExtra")
library(gridExtra)
# 将2张图并列输出
grid.arrange(p1,p2,ncol = 2)

# count和是否流失
p3 = ggplot(dat1, aes(x = as.factor(churn), y = count, fill = as.factor(churn)))  + geom_boxplot() +
  guides(fill = FALSE) + theme_minimal() + xlab("是否流失") + ylab("通话人数") +
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold")) +
  scale_fill_hue(c = 45, l = 80)
# perperson和是否流失
p4 = ggplot(dat1, aes(x = as.factor(churn), y = perperson, fill = as.factor(churn))) + geom_boxplot() +
  guides(fill = FALSE) + theme_minimal() + xlab("是否流失") + ylab("人均通话时长") +
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold")) +
  scale_fill_hue(c = 45, l = 80)
# 将2张图并列输出
grid.arrange(p3, p4, ncol = 2)

# entropy和是否流失
p5 = ggplot(dat1, aes(x = as.factor(churn), y = entropy, fill = as.factor(churn)))  + geom_boxplot() +
  guides(fill = FALSE) + theme_minimal() + xlab("是否流失") + ylab("通话时长分布") +
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold")) +
  scale_fill_hue(c = 45, l = 80)
# chgcount和是否流失
p6 = ggplot(dat1, aes(x = as.factor(churn), y = chgcount, fill = as.factor(churn))) + geom_boxplot() +
  guides(fill = FALSE) + theme_minimal() + xlab("是否流失") + ylab("通话人数变化率") +
  theme(axis.title.x = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold")) +
  scale_fill_hue(c = 45, l = 80)
# 将2张图并列输出
grid.arrange(p5, p6, ncol = 2)

# chgexpense和是否流失
p7 = ggplot(dat1, aes(x = as.factor(churn), y = chgexpense, fill = as.factor(churn))) + geom_boxplot() +
  guides(fill = FALSE) + theme_minimal() + xlab("是否流失") + ylab("花费变化率") +
  theme(axis.title.x = element_text(size = 16, face ="bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold")) +
  scale_fill_hue(c = 45, l = 80)

# 计算异常值
flag = apply(dat1[, 1:(ncol(dat1) - 1)], 2, function(s) {
  # s = log(s - min(s) + 1)
  m1 = mean(s)
  s1 = sd(s)
  res = (s > m1 - 3 * s1 & s < m1 + 3 * s1) * 1
  return(res)
})
flagsub = apply(flag, 1, function(s) {mean(s) == 1})
# 取出测试集中无异常值的部分
dat1 = dat1[flagsub, ]

# 标准化，用训练集的标准标准化测试集
for(i in 1:(ncol(dat1) - 1)){
  mm1 = mean(dat1[, i])
  ss1 = sd(dat1[, i])
  dat1[, i] = (dat1[, i] - mm1) / ss1
  dat2[, i] = (dat2[, i] - mm1) / ss1
}

# 建立模型
lm1 = glm(churn ~., data = dat1, family = binomial())
summary(lm1)

# 模型预测
Yhat = predict(lm1, newdata = dat2, type = "response")
ypre1 = 1 * (Yhat > 0.5)
table(ypre1, dat2$churn)

ypre2 = 1 * (Yhat > mean(dat2$churn))
table(ypre2, dat2$churn)

# 覆盖率捕获率曲线
sub = seq(0, 1, 1 / 7)
tol = sum(dat2$churn)
catch = sapply(sub, function(s) {
  ss = quantile(Yhat, 1 - s)
  res = sum(dat2$churn[Yhat > ss]) / tol
  return(res)
})
plot(sub, catch, type = "l", xlab = "覆盖率", ylab = "捕获率")

# 系数图示
coef = lm1$coefficients[-1]
coef = sort(coef)
barplot(coef, col = rainbow(10), width = 1)

# 加载所需R包
# install.packages("pROC")
library(pROC)
# 生成ROC曲线
plot.roc(dat2$churn, Yhat, col = "red", lwd = 2, xaxs = "i", yaxs = "i")

# 比较ROC曲线优劣
lm2 = glm(churn ~ chgcount, data = dat1, family = binomial())
Yhat2 = predict(lm2, newdata = dat2, type = "response")
plot.roc(dat2$churn, Yhat2, col = "blue", lwd = 2, xaxs = "i", yaxs = "i")
lines.roc(dat2$churn, Yhat, col = "red", lwd = 2)

# auc曲线
auc(dat2$churn, Yhat)
