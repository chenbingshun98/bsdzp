#6.2.2 如何在R语言中解析XML
library(XML)
nbadata = xmlParse(file = "./nbaplayer.xml")
nbadata

##6.2.3 XPath表达式
# install.packages(rvest)
library(rvest)
# install.packages(xml2)
library(xml2)
# install.packages(dplyr)
library(dplyr)
read_html('http://movie.douban.com/subject/26862829/') %>% html_nodes(xpath = "//h1//span")

#6.3.2 URL语法
# URL字符串的编码及解码
char = 'Golden states worries is the NBA champion in 2017'
URLencode(char, reserve = TRUE)
##  [1] "Golden%20states%20worries%20is%20the%20NBA%20champion%20in%202017"
URLdecode(char)

