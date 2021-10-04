#EasyCharts团队出品，
#如有问题修正与深入学习，可联系微信：EasyCharts
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

#---------------------------单数剧系列柱形图----------------------------------------------------
mydata<-data.frame(Cut=c("Fair","Good","Very Good","Premium","Ideal"),
                   Price=c(4300,3800,3950,4700,3500))

#mydata$Cut <- factor(mydata$Cut, levels = mydata$Cut[order(mydata$Order)])
ggplot(data=mydata,aes(x=Cut,y=Price))+
  geom_bar(stat = "identity", 
           width = 0.8,colour="black",size=0.25,
           fill="#FC4E07",alpha=1)+
  ylim(0, 6000)+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black")
  )


#---------------------------双数剧系列柱形图----------------------------------------------------
library(reshape2)
mydata<-read.csv("MultiColumn_Data.csv",check.names=FALSE,
                 sep=",",na.strings="NA",
                 stringsAsFactors=FALSE)

mydata<-read.csv("MultiColumn_Data.csv",check.names=FALSE,
                 sep=",",na.strings="NA",
                 stringsAsFactors=FALSE)

mydata <- tribble(
  ~"Catergory",	~"1996",	~"1997",
  "Temporary Stream"	,7.67,	5.84,
  "Permanent Stream"	,4.02	,6.45,
  "Lake"	,3.95	,6.76,
)
mydata
mydata<-melt(mydata,id.vars='Catergory')
mydata <- mydata %>% 
  pivot_longer(
    cols = !Catergory,
    names_to = "variable"
  )

ggplot(data=mydata,aes(Catergory,value,fill=variable))+
  geom_bar(stat="identity",position=position_dodge(),
           color="black",width=0.7,size=0.25)+
  scale_fill_manual(values=c("#00AFBB", "#FC4E07"))+
  ylim(0, 10)+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.background  =element_blank(),
    legend.position = c(0.88,0.88)
  )


#-------------------------------堆积柱形图-------------------------------------------------------
mydata<-read.csv("StackedColumn_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)
mydata <- read_csv()
mydata <- tribble(
  ~"Clarity"	,~"I1"	,~"SI2"	,~"SI1"	,~"VS2"	,~"VS1"	,~"S2"	,~"S1"	,~"IF",
  "Fair"	,150,	400	,390	,300,	130	,100,	100	,150,
  "Good",	1200,	1100,	1700	,900,	790	,1300	,1200	,1100,
  "Very_Good",	1300,	2300	,3300,	1900	,1800	,1900	,1700	,1300,
  "Premium"	,2800	,2900,	3500,	2800	,3000	,1800	,1600	,1280,
  "Ideal",	2000,	2700,	4200,	3300,	4200,	2700	,2100,	1300,
)
mydata
mydata<-melt(mydata,id.vars='Clarity')
mydata <- mydata %>% pivot_longer(
  cols = !Clarity,
  names_to = "variable"
)
mydata
ggplot(data=mydata,aes(variable,value,fill=Clarity))+
  geom_bar(stat="identity",position="stack", color="black", width=0.7,size=0.25)+
  scale_fill_manual(values=brewer.pal(9,"YlOrRd")[c(6:2)])+
  ylim(0, 15000)+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.background  =element_blank(),
    legend.position = c(0.85,0.82)
  )

#------------------------------百分比堆积柱形图-------------------------------------------------------

mydata<-read.csv("StackedColumn_Data.csv",sep=",",na.strings="NA",stringsAsFactors=FALSE)
mydata <- read_delim(clipboard(),delim = " ")
library(rvest)
web <- "https://github.com/EasyChart/Beautiful-Visualization-with-R/blob/master/%E7%AC%AC3%E7%AB%A0_%E7%B1%BB%E5%88%AB%E6%AF%94%E8%BE%83%E5%9E%8B%E5%9B%BE%E8%A1%A8/DotPlots_Data.csv"
mydata <- read_html(web) %>% 
  html_elements("table") %>% 
  html_table(header = TRUE)
?html_table
mydata <- mydata[[1]]
mydata <- mydata %>% 
  select(!1)
mydata<-melt(mydata,id.vars='Clarity')

ggplot(data=mydata,aes(variable,value,fill=Clarity))+
  geom_bar(stat="identity", position="fill",color="black", width=0.8,size=0.25)+
  scale_fill_manual(values=brewer.pal(9,"GnBu")[c(7:2)])+
  theme(
    axis.title=element_text(size=15,face="plain",color="black"),
    axis.text = element_text(size=12,face="plain",color="black"),
    legend.title=element_text(size=14,face="plain",color="black"),
    legend.position = "right"
  )
