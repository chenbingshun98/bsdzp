 #3.1 流程控制

 #3.1.1 循环执行结构
 #1.for
x<-c(1,2,3,4,5)
 for(n in x) print(n+1)

  #2.while
 x<-2
while(x<=15) x<-x+2 #如果 x 小于等于 15，则将 x+2 赋值给 x
 x

#3.repeat
x<-2
repeat{
x<-x+2
if(x>15) break #如果 x 大于 15 则跳出循环
}
x

#4.next & break
x <- 6
sum<-0
for(i in 1:x){
 if(i == 5)next #如果 x 等于 5 则跳出循环
 sum=sum+i
 print(sum)
  }
 sum

 
#3.1 流程控制

#3.1.2 条件执行结构
x<-c(TRUE,FALSE,FALSE)
x
y<-c(TRUE,TRUE,FALSE)
y

 x&&y #只判断第一个元素

x&y #判断向量里所有元素

#if_else 语句
x<-1
 y<-if(x>1) x else -x
y

#等同于下面的代码
if(x>1){
 y <- x
   } else{
    y <- -x
     }
y
#ifelse 语句
x<-1:3
y<-ifelse(x>1,x,-x)
y

 #3.switch
 for(i in c(-1:3, 9)) print(switch(i, 1, 2 , 3, 4))

 stock<-c("600292","600116")
for(i in stock){
   print(switch(i,"600292"="九州龙电" ,
                "600116"="三峡水利" ,
                "600452"="涪陵电力" ,
                "600098"="广州控股" ,
                "601991"="大唐发电" ,
                "000958"="东方热电"
   ))
  }
 