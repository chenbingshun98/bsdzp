##### 3.3 回归分析 #####
rm(list = ls())  # 清空工作空间

#### 3.3.1 线性回归 ####
### 1.数据预处理：从jobinfo.xlsx 到 数据分析岗位招聘.csv ###

# install.packages(readxl)
library(readxl)
# install.packages(ggplot2)
library(ggplot2)
# install.packages(jiebaR)
library(jiebaR)
library(tidyverse)

setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_code/R与千寻（代码+数据）/第三章 R语言与统计分析/3.3 回归分析/3.3.1 线性回归")
# options(scipen = 200)  # 去除科学计数法

jobinfo = read_excel("jobinfo.xlsx")  # 读取原始数据
str(jobinfo)  # 查看数据结构

## (1) 构造因变量：平均薪资的变量 ##
jobinfo$最低薪资 = as.numeric(jobinfo$最低薪资)  # 将最低薪资的字符型变量改为数值型变量
jobinfo$最高薪资 = as.numeric(jobinfo$最高薪资)  # 将最高薪资的字符型变量改为数值型变量
# 在jobinfo中创建平均薪资的变量
jobinfo$平均薪资 = (jobinfo$最高薪资 + jobinfo$最低薪资) / 2

## (2) 按照disctrict向量将地区重新划分为北上深和非北上深两个水平 ##
loc = which(jobinfo$地区 %in% c("北京", "上海", "深圳"))
loc_other = which(!jobinfo$地区 %in% c("北京", "上海", "深圳"))
jobinfo$地区[loc] = 1
jobinfo$地区[loc_other] = 0
jobinfo$地区 = as.numeric(jobinfo$地区)

## (3) 将公司规模转化为因子型变量，便于画图 ##
jobinfo$公司规模 = factor(jobinfo$公司规模, levels = c("少于50人", "50-150人", "150-500人", "500-1000人", "1000-5000人", "5000-10000人", "10000人以上"))
levels(jobinfo$公司规模)[c(2, 3)] = c("50-500人", "50-500人")
# 将50-150人和150-500人合并为一个水平：50-500人

## (4) 将学历转化为因子型变量，便于画图 ##
jobinfo$学历 = factor(jobinfo$学历, levels = c("中专", "高中", "大专", "无", "本科", "硕士", "博士"))

## (5) 匹配各个公司要求的统计软件 ##
# 首先建立software数据框，用于存放各个公司的软件匹配结果
software = as.data.frame(matrix(0, nrow = length(jobinfo$描述), ncol = 12))  # 先建立一个0矩阵，行数为观测数，列数为统计软件的个数，并转化为data frame格式
colnames(software) = c("R", "SPSS", "Excel", "Python", "MATLAB", "Java", "SQL", "SAS", "Stata", "EViews", "Spark", "Hadoop")  # 将software的data frame的列名改为软件名称

mixseg = worker()  # 按照缺省值，设置分词引擎

# 对每个描述观测进行分词，并存储在software里面，循环次数为总观测数，总观测数可通过length(jobinfo$描述)获取
for (j in 1:length(jobinfo$描述)){
  
  subdata = as.character(jobinfo$描述[j])  # 取出每个观测，保存在subdata变量
  fenci = mixseg[subdata]  # 对取出的观测进行分词，保存在分词变量
  
  # 设置各个软件的判别条件，以R为例，R.indentify表示r或R是否在fenci这个变量里
  R.identify = ("R" %in% fenci) | ("r" %in% fenci)
  SPSS.identify = ("spss" %in% fenci) | ("Spss" %in% fenci) | ("SPSS" %in% fenci)
  Excel.identify = ("excel" %in% fenci) | ("EXCEL" %in% fenci) | ("Excel" %in% fenci)
  Python.identify = ("Python" %in% fenci) | ("python" %in% fenci) | ("PYTHON" %in% fenci)
  MATLAB.identify = ("matlab" %in% fenci) | ("Matlab" %in% fenci) | ("MATLAB" %in% fenci)
  Java.identify = ("java" %in% fenci) | ("JAVA" %in% fenci) | ("Java" %in% fenci)
  SQL.identify = ("SQL" %in% fenci) | ("Sql" %in% fenci) | ("sql" %in% fenci)
  SAS.identify = ("SAS" %in% fenci) | ("Sas" %in% fenci) | ("sas" %in% fenci)
  Stata.identify = ("STATA" %in% fenci) | ("Stata" %in% fenci) | ("stata" %in% fenci)
  EViews.identify = ("EViews" %in% fenci) | ("EVIEWS" %in% fenci) | ("Eviews" %in% fenci) | ("eviews" %in% fenci) 
  Spark.identify = ("Spark" %in% fenci) | ("SPARK" %in% fenci) | ("spark" %in% fenci)
  Hadoop.identify = ("HADOOP" %in% fenci) | ("Hadoop" %in% fenci) | ("hadoop" %in% fenci)
  
  # 判断各个描述变量里面是否有某软件要求，以R为例，第j个描述变量，若R.identify为TRUE时，software的第j行的R变量为1，反之为0；
  # 1表示有要求，0表示无要求
  if (R.identify) software$R[j] = 1
  if (SPSS.identify) software$SPSS[j] = 1
  if (Excel.identify) software$Excel[j] = 1
  if (Python.identify) software$Python[j] = 1
  if (MATLAB.identify) software$MATLAB[j] = 1
  if (Java.identify) software$Java[j] = 1
  if (SQL.identify) software$SQL[j] = 1
  if (SAS.identify) software$SAS[j] = 1
  if (Stata.identify) software$Stata[j] = 1
  if (EViews.identify) software$EViews[j] = 1
  if (Spark.identify) software$Spark[j] = 1
  if (Hadoop.identify) software$Hadoop[j] = 1
}
# 将平均薪资和software这两个数据框合并
jobinfo.new = cbind(jobinfo$平均薪资, software)
colnames(jobinfo.new) = c("平均薪资", colnames(software))

## (6) 加入需要的变量 ##
# 地区
jobinfo.new$地区 = jobinfo$地区
# 公司类别
jobinfo.new$公司类别 = jobinfo$公司类别
# 公司规模
jobinfo.new$公司规模 = jobinfo$公司规模
# 学历
jobinfo.new$学历 = jobinfo$学历
# 要求经验
jobinfo.new$经验要求 = jobinfo$经验
# 行业类别
jobinfo.new$行业类别 = jobinfo$行业类别

## (7) 处理观测：公司类别中，非营利机构与事业单位两子类观测过少，没有对比价值，予以删除 ##
table(jobinfo.new$公司类别)
##  
##    创业公司 非营利机构       国企       合资   民营公司   上市公司 
##          90         20        291        762       4917        406 
##    事业单位       外资 
##          15        869
jobinfo.new = jobinfo.new[-which(jobinfo.new$公司类别 %in% c("非营利机构", "事业单位")), ]

## (8) 重赋列名 ##
colnames(jobinfo.new) = c("aveSalary", colnames(jobinfo.new[2:13]), "area", "compVar", "compScale", "academic", "exp", "induCate")

## (9) 保存做过预处理的数据集 ##
write.csv(jobinfo.new, file = "数据分析岗位招聘.csv", row.names = FALSE)

### 2.数据集读入与包的加载 ###

# install.packages("showtext")
library(showtext)
# install.packages(plyr)
library(plyr)

dat0 = read.csv("数据分析岗位招聘.csv", header = T)  # 读入清洗过后的数据
dat0 = na.omit(dat0)
n = dim(dat0)[1]  # n是样本量
summary(dat0)  # 查看数据
dat0 = dat0[, -19]  # 去除行业类别一类变量

### 3.数据描述性分析 ###
## (1) 因变量直方图 ##
hist(dat0$aveSalary, xlab = "平均薪资（元/月）", ylab = "频数", main = "", col = "dodgerblue", xlim = c(1500, 11000),
     breaks = seq(0, 500000, by = 1500))

summary(dat0$aveSalary)
##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1500    5250    7000    8980   12000   50000

## (2) 平均薪资 ~ 经验要求 ##
dat0$exp_level = cut(dat0$exp, breaks = c(-0.01, 3.99, 6, max(dat0$exp)))
dat0$exp_level = factor(dat0$exp_level,levels = levels(dat0$exp_level), labels = c("经验：0-3年", "经验：4-6年", "经验：>6年"))

# 为画图观察趋势，临时生成新变量，将经验年限要求划分为(-0.01, 3.99], (3.99, 6], >6三个档。即经验要求：0~3, 4~6, 6~10 年三个档
boxplot(aveSalary ~ exp_level, data = dat0, col = "dodgerblue", ylab = "平均薪资（元/月）", ylim = c(0, 45000))

summary(lm(aveSalary ~ exp_level, data = dat0))

table(dat0$exp_level)  # 样本量分布

dat0 = dat0[, -which(colnames(dat0) == "exp_level")]  # 删去临时的exp_level变量

## (3) 平均薪资 ~ 学历 ##
summary(lm(aveSalary ~ academic, data = dat0))  # 默认基准组为“本科”

dat0$academic = factor(dat0$academic, levels = c("无", "中专", "高中", "大专", "本科", "硕士", "博士"))  
dat0$compVar = factor(dat0$compVar, levels = c("民营公司", "创业公司", "国企", "合资", "上市公司", "外资"))
# 改变水平顺序，基准组设为“无”，“民营公司”
boxplot(aveSalary ~ academic, data = dat0, col = "dodgerblue", ylab = "平均薪资（元/月）", ylim = c(0, 45000))

summary(lm(aveSalary ~ academic, data = dat0))

table(dat0$academic)  # 样本量分布

### 4.数据直接建立回归模型 ###

lm1 = lm(aveSalary ~., data = dat0)
summary(lm1)  # 回归结果展示

### 5.回归诊断 ###

## (1)线性回归模型 ##
lm1 = lm(aveSalary ~., data = dat0)

## (2)回归诊断及处理 ##
par(mfrow = c(2, 2))  # 画2*2的图
plot(lm1, which = c(1:4))  # 模型诊断图，存在非正态、异常点现象，先解决非正态性：对因变量取对数

# install.packages("rms")
library(rms)
vif(lm1)  # 计算VIF，>5代表共线性较大（对其他自变量回归的R^2>80%）

# 去除共线性因素，把VIF较大的几项与基准组合并为一项
# dat0$compVar = as.character(dat0$compVar)  # 先转换成字符型，否则替换时会出现错误
# dat0[which(dat0$compVar %in% c("合资", "外资", "民营公司", "创业公司")), "compVar"] = "其他"
# dat0$compVar = factor(dat0$compVar, levels = c("其他", "国企", "上市公司", "事业单位", "非营利机构" ))  
# lm1 = lm(aveSalary ~., data = dat0)
# summary(lm1)
# vif(lm1) 

## (3) 对数线性模型（去除非正态影响） ##
lm2 = lm(log(aveSalary) ~., data = dat0)
summary(lm2)  # 回归结果展示
par(mfrow = c(2, 2))  # 画2*2的图
plot(lm2, which = c(1:4))  # 模型诊断图

### 6.最终模型解释及预测 ###

## (1) 最终模型：有交互项的对数线性模型 ##
lm4=lm(log(aveSalary)~. + compScale*area,data=dat0)  # 地区与公司规模之间的交互作用
summary(step(lm4))  # 变量选择：step AIC

## (2) 预测 ##
# 预测1：会用r和python，本科毕业，无工作经验，公司位于上海，规模87人，上市公司

# 创建一个名为new.data1的data frame
new.data1 = matrix(c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, "上市公司", "50-500人", "本科", 0), 1, 17) 
new.data1 = as.data.frame(new.data1)
colnames(new.data1) = names(dat0)[-1]  # 对data frame命名
for(i in 1:13){
  new.data1[, i] = as.numeric(as.character(new.data1[, i]))
}
new.data1$exp = as.numeric(as.character(new.data1$exp))  # 将factor类型改为数值型
exp(predict(lm4, new.data1))  # 预测值
##         1 
##  9625.873

# 预测2：会用r，java，sas和python，博士毕业，7年工作经验，公司位于北京，中小型公司（规模150-500人），创业公司

# 创建一个名为new.data2的data frame
new.data2 = matrix(c(1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, "上市公司", "50-500人", "博士", 7), 1, 17)
new.data2 = as.data.frame(new.data2)
colnames(new.data2) = names(dat0)[-1]  # 对data frame命名
for(i in 1:13){
  new.data2[, i] = as.numeric(as.character(new.data2[, i]))
}
new.data2$exp = as.numeric(as.character(new.data2$exp))  # 将factor类型改为数值型
exp(predict(lm4, new.data2))  # 预测值

# 预测3：没有学历、微弱的国企工作经验、不会任何统计软件

# 创建一个名为new.data3的data frame
new.data3 = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "国企", "少于50人", "无", 0), 1, 17)
new.data3 = as.data.frame(new.data3)
colnames(new.data3) = names(dat0)[-1]  #对data frame命名
for(i in 1:13){
  new.data3[, i] = as.numeric(as.character(new.data3[, i]))
}
new.data3$exp = as.numeric(as.character(new.data3$exp))  # 将factor类型改为数值型
exp(predict(lm4, new.data3))  # 预测值
##         1 
##  4206.697