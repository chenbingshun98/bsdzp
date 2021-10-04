# 基础字符处理函数的正则表达式应用
example_text1 = c("23333#RRR#PP", "35555#CCCC", "bearclub#2017")
# 以#进行字符串切分
unlist(strsplit(example_text1, "#"))
##  [1] "23333"    "RRR"      "PP"       "35555"    "CCCC"     "bearclub"
##  [7] "2017"
# 以空字符集进行字符串切分
unlist(strsplit(example_text1, "\\s"))
##  [1] "23333#RRR#PP"  "35555#CCCC"    "bearclub#2017"
# 以空字符替换字符串第一个#匹配
sub("#", "", example_text1)
##  [1] "23333RRR#PP"  "35555CCCC"    "bearclub2017"
# 以空字符集替换字符串全部#匹配
gsub("#", "", example_text1)
##  [1] "23333RRRPP"   "35555CCCC"    "bearclub2017"
# 查询字符串中是否存在3333或5555的特征并返回所在位置
grep("[35]{4}", example_text1)
##  [1] 1 2
# 查询字符串中是否存在3333或5555的特征并返回逻辑值
grepl("[35]{4}", example_text1)
##  [1]  TRUE  TRUE FALSE

# stringr包函数的正则表达式应用
example_text2 = "1. A small sentence. -2. Another tiny sentence."
# install.packages(stringr)
library(stringr)
# 提取small特征字符
str_extract(example_text2, "small")
##  [1] "small"
# 提取包含sentence特征的全部字符串
unlist(str_extract_all(example_text2, "sentence"))
##  [1] "sentence" "sentence"
# 提取以1开始的字符串
str_extract(example_text2, "^1")
##  [1] "1"
# 提取以句号结尾的字符
unlist(str_extract_all(example_text2, ".$"))
##  [1] "."
# 提取包含tiny或者sentence特征的字符串
unlist(str_extract_all(example_text2, "tiny|sentence"))
##  [1] "sentence" "tiny"     "sentence"
# 点号进行模糊匹配
str_extract(example_text2, "sm.ll")
##  [1] "small"
# 中括号表示可选字符串
str_extract(example_text2, "sm[abc]ll")
##  [1] "small"

str_extract(example_text2, "sm[a-p]ll")
##  [1] "small"
# 提取全部单词字符
unlist(str_extract_all(example_text2, "\\w+"))
##  [1] "1"        "A"        "small"    "sentence" "2"        "Another" 
##  [7] "tiny"     "sentence"