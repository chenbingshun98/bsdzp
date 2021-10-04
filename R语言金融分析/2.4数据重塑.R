###2.5. 3 长宽格式的转换
 ###1）stack()和 unstack()
 xx01 <- data.frame(age=c(20,21,20,22,19),per=c(60,50,70,80,90))###产生数据框
xx01
xx02 <- stack(xx01) ###转换成长数据框
xx02
xx03 <- data.frame(stunames=c("王一","张二","陈三","李四","孙六"),age=c(20,21,20,22,19),per=c(60,50,70,80,90)) ###产生数据框
 xx03
 xx04 <- stack(xx03,select=-stunames) ###转换成长数据框，忽略 stunames 变量
 xx04
xx05 <- unstack(xx04) ###将长数据转换成宽数据
xx05
##2)melt()函数
xx10 <- data.frame(stunames=c("王一","张二","陈三","李四","孙六"),age=c(20,21,20,22,19),per=c(60,50,70,80,90)) ###产生数据框
xx10 ###读取数据
 install.packages("reshape") ###安装 reshape 包 library(reshape) ###载入 reshape 包
library("reshape")
 xx101 <- melt(xx10,id.vars="stunames") ###将宽数据转换成长数据
xx101
###将数组转换成数据框
xx102 <- array(1:3, dim=c(2,2,2)) ###产生一个数组
xx102 ###读取数据
xx103 <- melt(xx102,varnames=LETTERS[24:26],value.name="Val") ###溶解数组,并定义列名
xx103 ###读取数据
 xx104 <- melt(xx10,id.vars="stunames") ###将宽数据转换成长数据
xx104 ###读取数据

###还原
xx104 <- melt(xx10,id.vars="stunames") ###将宽数据转换成长数据
 xx104 ###读取数据
xx105 <- cast(xx104) ###还原数据框
xx105 ###读取数据

 ##3)reshape()函数
install.packages("RODBC")
library("RODBC") ###载入 RODBC 包
xx253 <- read.csv("D:/R data class/xx253.csv",stringsAsFactors=F) ###读取.csv 文件
xx2533 <- reshape(xx253,
                  timevar = "year",
                  idvar = "country", 
                  direction = "wide")
View(xx2533)


 xx2534 <- reshape(xx2533,direction = "long")

 state.x77 <- as.data.frame(state.x77)
View(state.x77)
xx2544 <- reshape(state.x77, idvar = "state", 
                  ids = row.names(state.x77),
              times = names(state.x77), timevar = "Characteristic",
                      varying = list(names(state.x77)), 
              direction = "long")
 View(xx2544)
xx2545 <- reshape(xx2544, direction = "wide")
View(xx2545)

 #4)reshape2 包的 dcast()函数和 melt 函数

 #问题：处理我 1992.xlsx 的数据问题
library(reshape2)
AG6.1992 <- read.csv("C:/Users/terry/Documents/我爱学习/统计软件/R/1992.csv",stringsAsFactors = F)
#处理成长数据
AG6.1992.1 <- melt(AG6.1992,id="Commodity.Code")
AG6.1992.2 <- dcast(AG6.1992.1,
                    Commodity.Code~variable,
                    function(x)sum(x,na.rm=T))                                                  
library(dplyr)
filter(AG6.1992,"Commodity.Code" == 10110)
filter(AG6.1992,"Commodity.Code" == 10111|row.names(AG6.1992)=="Australia")
summary(AG6.1992$Australia)
AG6.1992 %>% dplyr::filter(Commodity.Code == 10111) %>% select(Australia)
