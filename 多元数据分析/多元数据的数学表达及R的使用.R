#创建按照行排列的矩阵

matrix(x1,nrow=3,ncol=4，byrow=T)

#矩阵相乘
A=matrix(1:12,nrow=3,ncol=4)
B=matrix(1:12,nrow=4,ncol=3)
A%in%B

#获取对角线元素
A=matrix(1:16,nrow=4,ncol=4)
diag(A)

#利用对角线元素创建对角矩阵
diag(diag(A))

#创建3阶单位矩阵
diag(3)

#求逆矩阵
A=matrix(rnorm(16),4,4)
solve(A)

#求矩阵特征根与特征向量
A=diag(4)+1
A.e=eigen(A,symmetric=T)

#矩阵的Choleskey分解
A.c=chol(A)

#矩阵奇异值分解
A=matrix(1:18,3,6)
A.s=svd(A)

#矩阵的维数
A=matrix(1:12,3,4)
dim(A)

#矩阵的行数
nrow(A)

#矩阵的行数
ncol(A)

#矩阵按行求和
rowSums(A)

#矩阵按行求均值
rowMeans(A)

#矩阵按列求和
colSums(A)

#矩阵按列求均值
colMeans(A)

#矩阵按行求和
apply(A,1,sum)

#矩阵按行求均值
apply(A,1,mean)

#矩阵按列求和
apply(A,2,sum)

#矩阵按列求均值
apply(A,2,mean)

#矩阵按列求方差
A=matrix(rnorm(100),20,5)
apply(A,2,var)

#矩阵按列求函数结果
B=matrix(1:12,3,4)
apply(B,2,function(x,a) x*a, a=2)

#由x1和x2构建数据框
X=data.frame(x1,x2)

#赋予数据框新的列标签
X=data.frame('身高'=x1,'体重'=x2)

#读取名为textdata的txt格式文档
X=read.table("textdata.txt")

#将剪切板数据读入数据框d2.1中
d2.1=read.table("clipboard",header=T)

#显示数据前6行 
head(d2.1)

#绑定数据
attach(d2.1)
#一维列联表
table(年龄)

#以年龄、性别排列的结果频数三维列联表
ftable(年龄,性别,结果)

#以性别、年龄排列的结果频数三维列联表 
ftable(性别,年龄,结果) 

ft=ftable(性别,结果,年龄)


