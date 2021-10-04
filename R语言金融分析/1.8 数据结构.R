####chapter 1 R 简介（下）
#1.8 数据结构（1）
#1.8.1 向量
 #向量赋值
xx1080 <- 20 #向量赋值，右边赋值给左边
xx1080 = 20 #向量赋值，右边赋值给左边
20 -> xx1080 #左边赋值给右边
20 = xx1080 #Error in 20 = xx1080 : invalid (do_set) lefthand side to assignment

xx1081 <- c("金融 1701","金融 1702")#将右边内容赋值给左边变量，也可以->赋值号，
#意味着左边值赋给右边变量，如果是字符型变量必须加上引号


xx1081
#不同数据类型强行一致化
 xx2 <- c("abd",123) #原则上单个向量里面的数据应该同类型，如果不同类型将会被强制变成同一类型,[1] "abd" "123"
 xx22 <- c(123,FALSE)
 xx23 <- c("abc",123,FALSE)

#向量命名
xx20 <- c(x=3,y=4,z=5) ###给向量赋值，并对其中的元素进行命名
 xx20
names(xx20) <- c("A","B","c") ###修改向量 xx20 的元素名
 xx20
names(xx20)[2] <- "U" ###修改向量 xx20 的第 2 个元素名
 xx20

 #自动补齐
xx21 <- c(1,3,5,7,8) ###给向量 xx21 赋值
xx21+2
xx21+c(2,3)

#生成有规律的向量
 ###1）冒号的神奇用法
xx3 <- c(1:10) ###将 1 到 10 的值赋值给 xx3,[1] 1 2 3 4 5 6 7 8 9 10
xx30 <- c(0.5:10.5)
xx4 <- c(10:1) ###将 10 到 1 的值赋值给 XX4，[1] 10 9 8 7 6 5 4 3 2 1

 ###2）利用 seq()函数

xx1085<- seq(1,10) ###将 1 到 10 的值赋给 xx1085，步长默认为 1
xx1085 ###读取 xx1085

xx1086<- seq(1,10, by=2) ###将 1 到 10 的值赋给 xx1086，步长为 2
xx1086 ###读取 xx1086

xx1087<- seq(1,10, length=4) ###将 1 到 10 的值赋给 xx1087，总长度为 4
xx1087 ###读取 xx1087

 #3)rep()
 rep(c(1,2),3) ###序列 12 重复 3 次
 rep(c(1,2),c(2,3)) ###第一个元素重复 2 遍，第二个元素重复 3 遍
 rep(c(1,2),each=2) ###第一个元素重复 2 遍，第二个元素重复 2 遍

 #4)利用 paste 函数将字符串相连
 <- paste("金融",1701,sep="") #利用 paste 函数可以将字符和其他字符连接成字符串
xx50 <- paste("金融",c(1701,1702),sep="") #长度不相等时，较短的向量自动补齐
xx51 <- paste("金融",c(1701,1702),c("甲","乙"),sep="") #也可以将三个字符串，甚至更多的字符串黏在一起

 #如果想生成金融 1701 甲、金融 1701 乙、金融 1702 甲、金融 1702 乙。。。怎么办
####chapter 1 R 简介（下）
 #1.8 数据结构(2)
 #如果想生成金融 1701 甲、金融 1701 乙、金融 1702 甲、金融 1702 乙。。。怎么办
#方法一
 xx70 <- paste("金融",rep(1701:1704,each=2),sep="")
xx71 <- paste(xx70,c("甲","乙"))
 xx71
 #方法二
 xx7 <- paste(rep(paste("金融",1701:1704,sep="") ,each=2),c("甲","乙"),sep="")
 xx7

#生成随机数
#生成随机整数
 x <- sample(1:100,3) #产生随机整数
runif(10,min=0,max=1) #产生 10 个最小值为 0，最大值为 1 的随机数
rnorm(10,mean=0,sd=1) ###产生 10 个平均值为 0，标准差为 1 的正态分布随机数
rpois(20,5) ###产生 20 个参数为 5 的泊松分布随机数

 #向量访问
xx1089 <- c(1:10) ###将 1 至 10 赋值给 xx1089
xx1089[3] ###取向量 xx1089 的第 3 个元素


 xx1089[c(3,6)] <- c(NA,-2) ###将 NA 和-2 分别赋值给向量 xx1089 的第 3 个和第个元素。
xx1089

xx1089[xx1089 < 3] ###将向量 xx1089 中小于 3 的值返回

xx1090 <- xx1089[!is.na(xx1089)] ###将 xx1089 向量中非缺失值赋值给
xx1090

#向量常用函数
xx1092 <- c(3:10) ###生成向量
length(xx1092) ###向量 xx1092 的长度
 mode(xx1092) ###向量 xx1092 的数据类型
range (xx1092) ###向量 xx1092 的范围
which.max(xx1092) ###向量 xx1092 的第几个元素最大
append(xx1092,20:23,after=4) ###在向量 xx1092 的第 4 个元素后添加 20、21、22、


 #今日问题
 #1）随机产生一个包括负数和正数的向量(整数），
#负数采用 1-负数替代，即-3 变成 4，正数
#用 1+正数替代，即 2 变成 3，怎么编写代码？
#2）随机产生一个包括负数和正数的向量(整数），
#负数用 NA 替代，再将 NA 用 0 替代

#数据结构（3）矩阵
 #1.8.2 矩阵
 #创建矩阵
 x1820 <- matrix(1:12,nrow=3,ncol=4) #生成 3 行 4 列，以列排序元素的矩阵
x1820
 x1821 <- matrix(1:12,nrow=3,ncol=4,byrow=T) ###生成 3 行 4 列，以行排序元素的矩阵
 x1821

 #创建矩阵，将向量转换为矩阵
xx1820 <- 1:12 #赋值
dim(xx1820) <- c(3,4) #将向量转换为矩阵，定义维度
xx1820

 #给矩阵的行和列命名
x1823 <- matrix(1:12,nrow=3,ncol=4,dimnames=list(letters[1:3],LETTERS[24:27
      ])) ###生成 3 行 4 列，以列排序元素的矩，并给出行名和列名阵
 x1823

rownames(xx1820) <- letters[1:3]
 colnames(xx1820) <- LETTERS[24:27]
 xx1820
 ###生成单位矩阵：
diag(4) ###生成 4×4 的单位矩阵
diag(1:3)

 ###2）访问矩阵中的元素
x1820[2, ] ###提取矩阵 x1820 的第 2 行
x1820[, 2] ###提取矩阵 x1820 的第 2 列
x1820[2, 2] ###提取矩阵 x1820 的第 2 行第 2 列的元素
x1820[2,c(1, 2)] ###提取矩阵 x1820 的第 2 行第 1 和第 2 列的元素
###取对角线元素
diag(x1820) ###取矩阵 x1820 的对角线元素
diag(diag(x1820)) ###以矩阵 x1820 的对角线元素生成新矩阵
x1821[x1821[,2]>5,]
x1821[,x1821[,2]>5]
x1821[x1821[,2]>5,2]

###3）矩阵运算
t(x1820) ###计算 x1820 矩阵的转置矩阵
###3）矩阵相乘
 t(x1820) %*% x1820 ###矩阵 x1820 的转置与矩阵 x1820 相乘

crossprod(x1820,x1820) ###矩阵 x1820 的转置与矩阵 x1820 相乘
###点乘
x18201 <- matrix(1:12,nrow=3,ncol=3) ###生成 3 行 3 列，以列排序元素的矩阵
x18201
t(x18201) * x18201

###4)求逆矩阵
x1824 <- matrix(rnorm(4),2,2) ###生成 4 个标准正态分布随机数形成 2*2 矩阵
x1824
solve(x1824) ###求矩阵 x1824 的逆矩阵

###矩阵中常用函数
###求特征值和特征向量
x1825 <- matrix(1:16,4,4) ###生成 4*4 的矩阵 x1825
eigen(x1825) ###取矩阵 x1825 的特征值和特征向量

###Choleskey 分解
x1825 <- diag(4)+3 ###生成矩阵 x1825
chol(x1825) ###对矩阵 x1825 进行 Choleskey 分解


 #1.问题：产生随机数，形成矩阵，负的 1-该数，正的 1 加该数
#2.问题：如何取矩阵上三角和下三角矩阵
#两种方法：1）用 lower.tri( )和 upper.tri( )函数;
 #2)利用矩阵取元素的方法
#3.问题：产生随机数（有正有负），形成矩阵，矩阵中小于 0 的用 NA 替代
#4.问题：把矩阵中 NA 用 0 替代

#1.8 数据结构（4）列表和数组
#前一次作业
#问题：产生随机数，形成矩阵，负的 1-该数，正的 1 加该数
x18211 <- matrix(sample(-50:50,16),nrow=4,ncol=4,byrow=T) ###生成 4 行 4列，以行排序元素的矩阵
x18211
x182110 <- x18211
x182110[x182110 <= 0] <- 1 - x182110[x182110 <= 0]
x182110[x182110 > 0] <- 1 + x182110[x182110 > 0]
 x182110

 #方法一
 x18212 <- x18211
 x18212[x18211 <=0 ] <- 1- x18211[x18211 <=0 ]
x18212[x18211 >0 ] <- 1+ x18211[x18211 >0 ]
x18212
#方法二，先做加，再做减
x18211[x18211 > 0 ] <- 1+x18211[x18211 > 0 ]
x18211[x18211 <= 0 ] <- 1-x18211[x18211 <= 0 ]
x18211



###问题：怎么取上三角和下三角矩阵
###取下三角
###1）用 upper.tri 函数
x1820[upper.tri(x1820)] <- 0

###2）取矩阵 x1820 的下三角矩阵
 x1820[row(x1820) < col(x1820)] <- 0 ###取矩阵 x1820 的下三角矩阵
x1820

###取上三角
 ###1）用 lower.tri 函数
x1820[lower.tri(x1820)] <- 0

###2）取矩阵 x1820 的下三角矩阵
x1820[row(x1820) < col(x1820)] <- 0 ###取矩阵 x1820 的下三角矩阵
x1820
 #问题：矩阵中小于 0 的用 NA 替代
 x18201 <- matrix(runif(12,-1,1),nrow=3,ncol=4,byrow=T) 
 ###生成 3 行 4 列，以行排序元素的矩阵
x18201
x18201[x18201 < 0] <- NA
#问题：把矩阵中 NA 用 0 替代
x18201[is.na(x18201)] <- 0

 ###列表
 ###创建列表
stuID <- c(102,118,213,323,231) ###输入学生学号
 stuage <- c(20,21,22,21,22) ###输入学生年龄
stuclass <- c("cx","cx","cx","cx","sy") ###输入学生所在班级
 stuperf <- c("excellent","average","good","poor","average") ###输入学生成绩
studata0 <- list("学号"=stuID,"年龄"=stuage,"班级"=stuclass,"成绩
"=stuperf) ###生成列表
studata0
 #列表和矩阵放在一个新列表中
x18200 <- matrix(1:16,nrow=4,ncol=4)
studata1 <- list(studata0,x18200)
studata1

#列表命名
names(studata1) <- c("列表 1","矩阵")
studata1
 #访问列表中元素
studata0[[2]] ###提取列表中元素
 studata0$学号 ###
 studata0$学号[1]

 studata1[[1]][[1]][1]

 #向量转换为列表
stuID1 <- as.list(stuID)
 stuID1
 class(stuID1)
 stuID2 <- unlist(stuID1)
 stuID2

#问题：班级前加金融
 studata0[[1]] <- paste("金融",studata0[[1]],sep="")
 #问题：把年龄都加上 5
 studata0[[2]] <- studata0[[2]]+5

 #1.8 数据结构（5）数据框
 ###创建数据框

 stuID <- c(102,118,213,323,231) ###输入学生学号
 stuage <- c(20,21,22,21,22) ###输入学生年龄
stuclass <- c("cx","cx","cx","cx","sy") ###输入学生所在班级
 stuperf <- c("excellent","average","good","poor","average") ###输入学生成绩
studata1 <- data.frame(stuID, stuage,stuclass, stuperf) ###生成数据框
 studata1
str(studata1)
 studata2 <- data.frame(stuID, stuage,stuclass, stuperf,stringsAsFactors=F)
 str(studata2)
 . ###修改行名

row.names(studata1) <- c("王一","张二","赵三","李四","刘五") ###修改行名
studata1
 ###修改列名
 studata1 <- data.frame("学号"=stuID,"年龄"=stuage,"班级"=stuclass,"成绩
"=stuperf) ###修改列名
 studata1
studata11 <- studata1
colnames(studata11) <- c("A","B","C","D")
 studata11

 ###访问元素
studata1[3,] ###提取数据框 studata1 中第 3 行数据

 studata1[,4] ###提取数据框 studata1 中第 4 列数据
studata1$"成绩" ###提取数据框 studata1 中列名为“成绩“的列
studata11$C

###行和列重新赋值
studata1$"成绩" <- c("优","中","良","及格","中") ###改变数据框 studata1 中列名为”成绩“的列内容
studata1

studata1$"成绩 2" <- c("优 2","中 2","良 2","及格 2","中 2") ###数据框 studata1中增加一列名为”成绩 2“的列内容
studata1
###条件提取
studata1[studata1$"年龄" < 22,] ######提取数据框 studata1 中年龄小于 22 的学生

###绑定数据框
 attach(studata1) ###绑定数据框 studata1
年龄 ###读取”年龄“列
 detach(studata1) #接触绑定数据框

 #问题：班级前加金融

#问题：把年龄小于 22 的设为 0，大于等于 22 的设为 1

 #1.8 数据结构（6）数组和因子
 stuID <- c(102,118,213,323,231) ###输入学生学号
stuage <- c(20,21,22,21,22) ###输入学生年龄
 stuclass <- c("cx","cx","cx","cx","sy") ###输入学生所在班级
stuperf <- c("excellent","average","good","poor","average") ###输入学生成绩
studata1 <- data.frame(stuID, stuage,stuclass, stuperf,stringsAsFactors=F)
###生成数据框
studata1
#问题：班级前加金融
 studata1$stuID <- paste("金融",studata1$stuID,sep="")
studata1
 #问题：把年龄小于 22 的设为 0，大于等于 22 的设为 1
studata1$stuage[studata1$stuage < 22] <- 0
studata1$stuage[studata1$stuage >= 22] <- 1
studata1

 ###因子
 stuperf<- c("excellent","average","good","poor","average")
 stuperf<- factor(stuperf)

 
 mode(stuperf) ###读取 stuperf2 的数据类型
class(stuperf) ###显示 stuperf2 的类

###将数值改为罗马数字
 xx1860 <- c(1,2,2,3,3,2,2,1,1) ###创建向量
fxx1860 <- factor(xx1860,labels=c("I","II","III")) ###定义有序因子
 fxx1860

 
 
  ### 自定义排序 ###
stuperf2 <- factor(stuperf, order = TRUE, levels = c("excellent","good","av
erage","poor"))
stuperf2 <- factor(stuperf, order = TRUE, levels = c("excellent","good","av
erage","poor"))
stuperf2

### 自定义排序 中文 ###
stuperfc <- factor(c("优","中","良","差","中"), order = TRUE, levels = c("优
","良","中","差"))

###删除部分分类
score11 <- c('A', 'B', 'A', 'C', 'B')
score11 <- factor(score11,levels = c('C', 'B', 'A'),exclude="B",ordered = is.ordered(score))
score11

score12 <- c('A', 'B', 'A', 'C', 'B') ###赋值
score12 <- factor(score12,levels = c('E','D','C', 'B', 'A'),ordered = is.ordered(score))
###定义有序型因子
levels(score12) ###提取分类值

 ###书中针对案例
stuID <- c(102,118,213,323,231) ###输入学生学号
 stunames <- c("cql","jjl","yyr","cy","lq") ###输入学生姓名
stuage <- c(20,21,22,21,22) ###输入学生年龄
 stuclass <- c("cx","cx","cx","cx","sy") ###输入学生所在班级
stuperf <- c("excellent","average","good","poor","average") ###输入学生成绩
stuclass <- factor(stuclass) ###将班级变量定义为因子
stuperf2 <- factor(stuperf, order = TRUE, levels = c("excellent","good","av
erage","poor"))
studata2 <- data.frame(stuID, stuage, stunames, stuclass, stuperf2)
str(studata2)
summary(studata2)