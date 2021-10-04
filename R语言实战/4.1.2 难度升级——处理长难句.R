setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_code/R与千寻（代码+数据）/第四章 R语言与非结构化数据分析/4.1 文本分析")

df = read.csv("BearTrip_RawData.csv", fileEncoding = "UTF-8", stringsAsFactors = F)  # 读入数据
names(df) = c("threemonth", "info", "satisfactory", "name", "title", "price", "storerating", 
              "promise", "combo", "creditLevel", "unique", "destination", "type", "tripschedule", "num_spot")  # 重命名为英文

## 1.提取固定长度文本--提取旅游类型 ##
head(df$title)

df$title.type = str_sub(df$title, 2, 4) 
head(df$title.type)

## 2.提取固定关键词--生成“交通方式”新变量 ##
head(df$combo)

#可以用grepl keywords text来判断text中（这里也就是combo变量）
#是否出现keywords，也就是“飞机”（“高铁”）。
#如果combo包括“飞机”（“高铁”），grepl( )则返回True；否则，返回False。
df$plane = grepl('飞机', df$combo) 
df$CRH = grepl('高铁', df$combo) 
head(df$plane)

head(df$CRH)

## 3.提取词类并打标签--生成项目档次 ##
df$title[100:110]

high_end = "五星|豪华|奢华|VIP|商务|奢享|铂金|至尊|尊享|顶级|高端|高档|定制"  # 豪华关键词
middle_end = "品质|四星|精品|精致|精华|经典|精选|升级|舒适"  # 轻奢关键词
df$title.level = ifelse(grepl(middle_end, df$title), "轻奢", "普通")
df$title.level = ifelse(grepl(high_end, df$title), "豪华", df$title.level)
cbind(head(df$title), head(df$title.level))
