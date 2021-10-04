library(rvest)
library(stringr)
# 豆瓣排名前25电影及评价爬取
url <-'http://movie.douban.com/top250?format=text'
webpage <- read_html(url)

# 1， 获取排名
# 用CSS选择器获取排名部分【关键是这句话】
rank_data_html <- html_nodes(webpage,'.pic em') # class 为 pic类的子标签em
rank_data_html
# 把排名转换为文本
rank_data <- html_text(rank_data_html)
rank_data
# 检查一下数据
head(rank_data)
## [1] "1" "2" "3" "4" "5" "6"
# 2， 获取名称
# 用CSS选择器获取标题部分
title_data_html <- html_nodes(webpage,'.info .hd a')
title_data_html
title_data_html <- html_node(title_data_html,'.title')
title_data_html
# 爬虫综合实践
# 三、设计R语言程序
# 把排名转换为文本，并提取第一个“/”左侧部分
title_data <- html_text(title_data_html) %>% # 去掉HTML标记
  str_trim # 去掉字符串头和尾部的空格
# 检查一下数据
head(title_data)
## [1] "肖申克的救赎" "霸王别姬" "这个杀手不太冷" "阿甘正传"
## [5] "美丽人生" "泰坦尼克号"
# 3， 获取上映年份
year_data_html <- html_nodes(webpage,'.info .bd p') # 获取子代码块
year_data <- html_text(year_data_html) %>% # 去掉HTML标记
  str_extract("[0-9]{4}.{1}\\/") %>% # 提取 数值 加 空格 加 斜杠的部分
str_extract("[0-9]{4}") %>% # 提取数值部分
  na.omit() %>% # 去掉缺失的元素
  as.numeric() # 转为数值类型
year_data
## [1] 1994 1993 1994 1994 1997 1997 2001 1993 2010 2008 2009 2009 1998 2004
## [15] 1995 1998 1972 1988 2014 2011 2002 2011 1939 2006 2010
# 爬虫综合实践
# 三、设计R语言程序
# 4, 获取评论人数
Rating <- str_extract_all(webpage,
                          pattern = "<span>[0-9]{1:10}+人评价</span>")
Rating.num_line <- unlist(Rating)

Rating.num <- str_extract(Rating.num_line,
                          pattern = "[:digit:]+") %>%as.numeric()
Rating.num 


Rating_num <- html_nodes(webpage,'.star') %>% 
  html_text() %>% strsplit( "\n")
Rating_num
length(Rating_num)

Rating_Num <- c()
for(i in 1:length(Rating_num)){
  show(i)
  Rating_Num[i] <- Rating_num[[i]][5]
}
Rating_Num
class(Rating_Num)

Rating_Num <- str_extract(string = Rating_Num,
                              pattern = "[:digit:]*") %>% unlist()

Rating_Num
Rating_Num <- as.numeric(Rating_Num)
  

Rating.num_line <- unlist(Rating)
test <- html_nodes(webpage,'.inq')
test2 <- html_text(test) 
test2
  webpage %>% html_nodes('.bd star') %>% 
  
  html_node() %>% html_text() %>% str_extract_all('[0-9]') 
# 检查结果
## [1] 1116962 815015 1035401 882652 515091 821022 821626 466127
## [9] 909701 599642 577582 817720 690299 554332 608895 584045
## [17] 411845 506965 606484 337492 495287 442243 316873 652575
## [25] 695467
# 5， 获取评分
# 用CSS选择器获取评分部分【关键是这句话】
Score_html <- html_nodes(webpage,'.rating_num')
Score <- html_text(Score_html) %>% as.numeric()

#获取评价分数（2）
Score_line<-str_extract_all(string = webpage, pattern = '<span class="rating_num" property="v:average">[\\d\\.]+</span>')

Score_line<- unlist(Score_line)

Score<- str_extract(string = Score_line, pattern = '\\d\\.\\d') %>%
  as.numeric(.)



# 数据合并
MovieData <- data.frame(MovieName = title_data, RatingNum = Rating_Num,
                        Score = Score, Rank = rank_data,
                        year = year_data,
                        stringsAsFactors = FALSE)
MovieData$RatingNum <- gsub("人评价","",MovieData$RatingNum)

class(MovieData$RatingNum)
MovieData$RatingNum <- as.numeric(MovieData$RatingNum)

summary(MovieData)
(lm1<- summary(lm(Score~RatingNum ,data = MovieData)))
corrention <- cor(x = MovieData$Score, y = MovieData$RatingNum)
MovieData$Rank <- as.numeric(MovieData$Rank)
cor(subset(MovieData,select = -MovieName)) %>% heatmap()
?heatmap
subset(MovieData,select = -MovieName)
class(MovieData$Score)
class(cor(x = MovieData$Score, y = MovieData$RatingNum))
class(MovieData)
heatmap(cor(x = MovieData$Score, y = MovieData$RatingNum))

?cor
plot(MovieData$RatingNum,MovieData$Score)
abline(reg = lm(Score~RatingNum ,data = MovieData),col = "red")

?abline
## (2)回归诊断及处理 ##
par(mfrow = c(2, 2))  # 画2*2的图
plot(lm1, which = c(1:4))  # 模型诊断图，存在非正态、异常点现象，先解决非正态性：对因变量取对数


library(ggplot2)
ggplot(data = MovieData, aes(x = year,y = Score)) +
  geom_point(aes(size = RatingNum), color = "black")
#在网址没规律（例子：不是从一到N）
link <- news %>% html_attrs()
link[[3]][1]
link2 <- paste("https://www.thepaper.cn/",link[[3]][1],sep ="")
news_content <- read_html(link2) %>% html_nodes('div.news_txt') %>% html_text()

# 设定网址链接
urli = paste0("https://movie.douban.com/top250?start=", (0:9)*25 )
# 分别读取信息
tempi = list()
for(i in 1:length(urli)){
  tempi[[i]] = getData(urli[i])
}
# 合并为一个数据框
for(i in 1:length(urli)){
  if(i==1){
    MovieData250 = tempi[[i]]
  }else{
    MovieData250 = rbind(MovieData250, tempi[[i]])
  }
}
# 检查
head(MovieData250)


