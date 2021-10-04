## 抓取某网页二手房数据 ##
# 加载所需的包
library("xml2")
library("rvest")
library("dplyr")
library("stringr")

# 对爬取页数进行设定并创建数据框
i = 1:100
house_inf = data.frame()

# 利用for循环封装爬虫代码，进行批量抓取
for (i in 1:100) {
  # 发现url规律，利用字符串函数进行url拼接并规定编码
  web = read_html(str_c("http://hz.lianjia.com/ershoufang/pg", i), encoding = "UTF-8")
  # 提取房名信息
  house_name = web %>% html_nodes(".houseInfo a") %>% html_text()
  # 提取房名基本信息并消除空格
  house_basic_inf = web %>% html_nodes(".houseInfo") %>% html_text()
  house_basic_inf = str_replace_all(house_basic_inf, " ", "")
  # 提取二手房地址信息
  house_address = web %>% html_nodes(".positionInfo a") %>% html_text()
  # 提取二手房总价信息
  house_totalprice = web %>% html_nodes(".totalPrice") %>% html_text()
  # 提取二手房单价信息
  house_unitprice = web %>% html_nodes(".unitPrice span") %>% html_text()
  # 创建数据框存储以上信息
  house_name <- house_address[seq(1,59,2)]
  house_address <- house_address[seq(2,60,2)]
  house = data.frame(house_name, house_basic_inf, house_address, house_totalprice, house_unitprice)
  house_inf = rbind(house_inf, house)
}

# 将数据写入csv文档
write.csv(house_inf, file = "C:/Users/terry/Documents/我爱学习/统计软件/R/R脚本/R语言实战/house_inf.csv")
  

setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R脚本/R语言实战")
house_inf <- read.csv("house_inf.csv",stringsAsFactors = FALSE, encoding = "UTF-8")

house_inf2 <- house_inf %>%
  separate(house_basic_inf,c('室和厅的数量',
                             '平米',
                             '方位',
                             '装修程度',
                             '所在楼层',
                             '房屋种类'),
           sep = "_")
#为了不能用|进行分割，难道是与1混淆了。
#有可能
house_inf2 <- gsub("万$","",house_inf2$house_totalprice)
# str_replace_all(house_totalprice, "万", "")为什么不行？
  
house_inf2 <- gsub("元/平米$","",house_inf2$house_unitprice)
house_inf2 <- gsub("^单价","",house_inf2$house_unitprice)

str_replace(house_inf2$house_unitprice, "单价","")
# str_replace_all(house_inf2$house_unitprice，"单价.", "")
# 只替换匹配的第一项
str_replace(house_inf2$house_totalprice, "万","")
?str_replace_all
str_extract("[0-9]{4}") %>%  unlist(Rating)


?str_extract
