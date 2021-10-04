d5.1=read.table("clipboard"
                ,header=T) #读取例5.1数据
logit.glm<-glm(y~x1+x2+x3,family=binomial,data=d5.1) #Logistic回归模型
summary(logit.glm) #Logistic回归模型结果

logit.step<-step(logit.glm,direction="both")  ####逐步筛选法变量选择

summary(logit.step)
#逐步筛选法变量选择结果

pre1<-predict(logit.step,data.frame(x1=1)) #预测视力正常司机Logistic回归结果
###x 1 : 表 示 视 力 状 况 ， 它 是 一 个 分 类 变 量 ， 1 表 示 好 ， 0 表 示 有 问 题 ；
p1<-exp(pre1)/(1+exp(pre1)) #预测视力正常司机发生事故概率
pre2<-predict(logit.step,data.frame(x1=0)) #预测视力有问题的司机Logistic回归结果
p2<-exp(pre2)/(1+exp(pre2)) #预测视力有问题的司机发生事故概率
c(p1,p2) #结果显示

glm(formula = y~x1+x2,family = possion(link=log),data = d5.2)#建立Poisson对数线性模型

d5.3=read.table("clipboard"
                ,header=T) #读取例5.3数据
anova(lm(Y~factor(A),data=d5.3)) #完全随机设计模型方差分析

d5.4=read.table("clipboard"
                ,header=T) #读取例5.4数据
anova(lm(Y~factor(A)+factor(B),data=d5.4)) #随机单位组设计模型方差分析

#关于40个不同年龄（age，定量变量）和性别（sex，定性变量，用0和1代表
#女和男）的人对某项服务产品的观点（y，二水平定性变量，用1和0代表认可
#与不认可）的数据。
Case4=read.table("clipboard"
                 ,header=T);Case4
fm=glm(y~sex+age,family=binomial,data=Case4)
fm
summary(fm)
attach(Case4)
Pr=predict(fm,data.frame(list(sex,age))) #模型预测
p=exp(Pr)/(1+exp(Pr))
cbind(sex,age,y,p)
plot(age,Pr)
detach(Case4)