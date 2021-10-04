c0 <- c(2.3,3,1.5,4.8,9,3,4,7,8,9.5)   #被划分的对象
cuttime <- c(0,3,6,9,10) #分割点
cut(c0,cuttime)  #判断所在区间
cut(c0, cuttime, labels = F)#仅显示区间的序号
cut(c0, cuttime,labels = F, right = F)#设置左闭右开区间
#	logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.


note1#
#right＝F：每个区间左闭右开，否则默认左开右闭
cut(c0,cuttime,right=F)

#note2#
#include.lowest:第一个区间包含左端点／最后一个区间包含右端点
cut(c0,cuttime,include.lowest=T)
cut(c0,cut time,right=F,include.lowest=T)

#note3#
#label=F:只返回在第几个区间,否则会显示具体的区间
cut(c0,cuttime)
