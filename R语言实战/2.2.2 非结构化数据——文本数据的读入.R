rm(list = ls())
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_code/R与千寻（代码+数据）/第二章 R语言数据操作/2.2 数据读入/2.2.2 非结构化数据——文本数据的读入")
## 1.读入简单文本数据 ##
novel = read.csv("novel.csv", fileEncoding = "UTF-8")
head(novel)

## 2.用readtable读入文本 ##
# 文本数据普通读法
# test = read.table("weibo.txt", sep = "\t")
# 空字段需要fill参数来填满
# test = read.table("weibo.txt", sep = "\t", fill = T)
# 看看最后一行发生了什么
# test[92, ]
# 设置quote = ""意思是禁止所有引用符号
weibo = read.table("weibo.txt", sep = "\t", fill = T, quote = "", fileEncoding = "UTF-8")
# stringsAsFactors将文本转化为字符，strip.white将字符中的前后空格去掉
weibo = read.table("weibo.txt", sep = "\t", fill = T, quote = "", strip.white = T, stringsAsFactors = F, fileEncoding = "UTF-8")
head(weibo)

# 查看第75至80行数据
weibo[75:80, ]

# 处理缺失数据
weibo = weibo[substr(weibo$V1, 1, 2) == "熊粉", ]

## 3.用readLines读入文本 ##
# weibo1是个字符向量
weibo1 = readLines("weibo.txt", encoding = "UTF-8")
head(weibo1)

# 使用字符分割函数将weibo1分开
tmp = strsplit(weibo1, " \t") 
class(tmp)

# 查出每个list的元素的长度，来查看异常值
ll = sapply(tmp, length)     
# 长度为0和1是异常行，长度为7和8的可以采用
table(ll)

# 检验是否均在最后一行缺失,hi即储存所有长度为7的行的最后一个元素
hi = c()
for(i in 1:26){
  show(i)
  hi[i] = tmp[ll == 7][[i]][7]
}

# 对含有7个字符补充一个空字符,使得最后选择数据框较完整
tmp[ll == 7] = lapply(tmp[ll == 7], function(x) c(x, ""))                               
tmp[ll == 7][1]

# 将原来含有7个字符和8个字符的合在一起
infoDf = as.data.frame(do.call(rbind, tmp[ll == 7 | ll == 8]), stringsAsFactors = F)     
colnames(infoDf) = c("name", "location", "gender", "Nfollowers",
                     "Nfollow", "Nweibo", "createTime", "description")               
head(infoDf)

## 4.readLines的其他用法 ##
yitian = readLines("倚天屠龙记.Txt", encoding = "UTF-8")
yitian[1:10]


# 分段
# 在每个字符中找至少有一个空格的标号
para_head = grep("\\s+", yitian,perl = T)
para_head[1:10]
##   [1]  1  2  6 14 20 22 28 55 61 81
# 首先构造一个矩阵，第一列是一段之首的序号，第二列是一段之尾的序号
cut_para1 = cbind(para_head[1:(length(para_head) - 1)], para_head[-1] - 1)
head(cut_para1)

# 编写一个函数，将属于一段的文字粘贴起来
yitian_para = sapply(1:nrow(cut_para1), function(i) paste(yitian[cut_para1[i, 1]:cut_para1[i, 2]], collapse = ""))
yitian_para[1:4]
