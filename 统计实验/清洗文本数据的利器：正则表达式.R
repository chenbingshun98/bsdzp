library(stringr)
x <- c("<title>HTML 教程</title>", "Nanjing 210000",
    "Nanjing University of Finance & Economics, 210023",
    "<div id=\"navfirst\">
<ul id=\"menu\">
<li id=\"h\"><a href=\"/h.asp\" title=\"HTML 系列教程\">HTML 系列教程</a></li>
<li id=\"b\"><a href=\"/b.asp\" title=\"浏览器脚本教程\">浏览器脚本</a></li>
<li id=\"s\"><a href=\"/s.asp\" title=\"服务器脚本教程\">服务器脚本</a></li>
<li id=\"d\"><a href=\"/d.asp\" title=\"ASP.NET 教程\">ASP.NET 教程</a></li>
<li id=\"x\"><a href=\"/x.asp\" title=\"XML 系列教程\">XML 系列教程</a></li>
<li id=\"ws\"><a href=\"/ws.asp\" title=\"Web Services 系列教程\">Web Services
系
列教程</a></li>
<li id=\"w\"><a href=\"/w.asp\" title=\"建站手册\">建站手册</a></li>
</ul>
</div>")

# 匹配<title>里的内容
(t1 = str_extract_all(x, "<title>.*</title>"))
## [[1]]
## [1] "<title>HTML 教程</title>"
##
## [[2]]
## character(0)
##
## [[3]]
## character(0)
##
## [[4]]
## character(0)
gsub("<.+?>", "", t1[[1]])
## [1] "HTML 教程"
# 匹配标签<li>中的内容
(t1 = str_extract_all(x, "<li.*>.*</li>"))
## [[1]]
## character(0)
##
## [[2]]
## character(0)
##
## [[3]]
## character(0)
##
## [[4]]
## [1] "<li id=\"h\"><a href=\"/h.asp\" title=\"HTML 系列教程\">HTML
# 系列教程</a></li>"
# ## [2] "<li id=\"b\"><a href=\"/b.asp\" title=\"浏览器脚本教程\">浏览器脚本</a></li>"
## [3] "<li id=\"s\"><a
# 清洗文本数据的利器：正则表达式
# href=\"/s.asp\" title=\"服务器脚本教程\">服务器脚本
# </a></li>"
# ## [4] "<li id=\"d\"><a href=\"/d.asp\" title=\"ASP.NET 教程\">ASP.NET
# 教程</a></li>"
# ## [5] "<li id=\"x\"><a href=\"/x.asp\" title=\"XML 系列教程\">XML 系列教
# 程</a></li>"
## [6] "<li id=\"ws\"><a href=\"/ws.asp\" title=\"Web Services 系列教程
# # \">Web Services 系列教程</a></li>"
# ## [7] "<li id=\"w\"><a href=\"/w.asp\" title=\"建站手册\">建站手册
# </a></li>"
# (t1 = t1[[
# 4]])
# ## [1] "<li id=\"h\"><a href=\"/h.asp\" title=\"HTML 系列教程\">HTML 系列
# 教程</a></li>"
# ## [2] "<li id=\"b\"><a href=\"/b.asp\" title=\"浏览器脚本教程\">浏览器脚本
# </a></li>"
# ## [3] "<li id=\"s\"><a href=\"/s.asp\" title=\"服务器脚本教程\">服务器脚本
# </a></li>"
# ## [4] "<li id=\"d\"><a href=\"/d.asp\" title=\"ASP.NET 教程\">ASP.NET
# 教程</a></li>"
# ## [5] "<li id=\"x\"><a href=\"/x.asp\" title=\"XML 系列教程\">XML 系列教
# 程</a></li>"
# ## [6] "<li id=\"ws\"><a href=\"/ws.asp\" title=\"Web Services 系列教程
# \">Web Services 系列教程</a></li>"
# ## [7] "<li id=\"w\"><a href=\"/w.asp\" title=\"建站手册\">建站手册
# </a></li>"
# # 匹配超级链接标签<a>中的内容
(t1 = str_extract_all(x, "<a.*>.*</a>"))

(t1 = t1[[4]])

# 匹配所有数字
str_extract_all(x, "[0-9]+")

str_extract_all(x, "[:digit:]+")

#
#+表示的是一个或以上
#ignore.case为忽略大小写
grep("a.+b", c("ab","a b","A b","a#b","a##b"))

#?表示的是零或一个
grep("a.?b", c("ab","a b","A b","a##b"))

#制定一个和三个
grep("a.{1,3}b",c("ab","a b","A b","a##b"))

#指定两个和三个
grep("a.{2,3}b", c("ab","a b","A b","a##b"))

#2个
grep("a.{2}b", c("ab","a b","A b","a##b"))

#3个
grep("a.{3}b", c("ab","a b","A b","a##b"))

ss <- c("1314","abc","a b c","ABC","aB12c","13ab14c")

#^表示的为以什么为开头
#这个是以1为开头
grep("^1",ss)

#$表示的是以什么结尾
grep("4$",ss)

#错误的写法
grep("$4",ss)

#[]的用法
#符合[]里的任意字符
grep("a[2c]b", c("a2b","a1cb","ab","a111b","a1b","acb"))
#*表示多个的
grep("a[2c]*b", c("a2b","a1cb","ab","a111b","a1b","acb"))

grep("a[1-9]b", c("a2b","a1cb","ab","a111b","a1b","acb"))

grep("a[a-z]b", c("a2b","a1cb","ab","a111b","a1b","acb"))

#这里的^为逻辑符号的否
grep("a[^c]b", c("a2b","a1cb","ab","a111b","a1b","acb"))

ss
#圆括号的运用
#表示的是组合
grep("(13).1",ss)

grep("(13).4",ss)

#|表示的是逻辑符号的或者
grep("(13|13ab).4",ss)

#gsub
gsub("(13|13ab).4",ss)
##Error in gsub("(13|13ab).4", ss) : 缺少参数"x",也没有缺省值

#把13或13ab替换成"XXXX"
gsub("(13|13ab).4","XXXX",ss)
ss

#反向引用
#两个反斜杠一就表示第一次匹配的内容就可以纪录下来
gsub("(13|13ab).4","\\1",ss)

gsub("(13|13ab).4","\\2",ss)

#
grep("a\\?b",c("acb","a?b","a??b"))
grep("a\\?+",c("acb","a?b","a??b"))

gsub("(13|13ab).4","\\1",ss)

grep("^\\w+$",ss)#因为空不属构成文字的字符
grep("^\\W+$",ss)

grep("^\\W",ss)#因为有有非字符组成的只有第三个

#查找有数字的
grep("\\d",ss)
# 查找一数字开头也以数字结尾
grep("^\\d+$",ss)
#从头到尾的没有数字的
grep("^\\D+$",ss)
