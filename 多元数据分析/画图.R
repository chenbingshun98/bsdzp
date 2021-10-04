#按行做均值条形图 
barplot(apply(X,1,mean))

#修改横坐标位置
barplot(apply(X,1,mean),las=3)

#按列做均值图条形
barplot(apply(X,2,mean))

#按列做彩色均值图条形图
barplot(apply(X,2,mean),col=1:8)

boxplot(X) #按列做垂直箱线图

boxplot(X,horizontal=T)#水平箱线图

#简单星相图
stars(X)

#带图例的星相图mic
stars(X,key.loc=c(17,7))

#带图例度彩色星相图
stars(X,key.loc=c(17,7),
      draw.segments=T)
