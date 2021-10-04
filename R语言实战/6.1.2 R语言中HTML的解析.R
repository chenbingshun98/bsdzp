# 清空工作目录
rm(list = ls())

# install.packages("XML")
library(XML)
# install.packages("bitops")
library(bitops)
# install.packages("RCurl")
library(RCurl)
temp = getURL('http://movie.douban.com/subject/26862829/?from=subject-page')
fanghua = htmlParse(temp)
fanghua
##  <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd">
##  <html>
##  <head><title>301 Moved Permanently</title></head>
##  <body bgcolor="white">
##  <center><h1>301 Moved Permanently</h1></center>
##  <hr>
##  <center>nginx</center>
##  </body>
##  </html>
##  