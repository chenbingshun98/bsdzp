## 1.统计各个角色出场次数 ##
library(ggplot2)
library(data.table)
library(reshape2)

setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_code/R与千寻（代码+数据）/第四章 R语言与非结构化数据分析/4.1 文本分析")

# 载入数据
roles <- readLines("主角名单.txt", encoding = "UTF-8")
yitian <- readLines("倚天屠龙记.Txt", encoding = "UTF-8")

# 将原小说进行分段
para_head <- grep("\\s+", yitian, perl = T) # “\ s＋”为匹配一个或多个空格的正则表达式
cut_para1 <- cbind(para_head[1:(length(para_head) - 1)], para_head[-1] - 1)
yitian_para <- sapply(1:nrow(cut_para1), function(i) paste(yitian[cut_para1[i, 1]:cut_para1[i, 2]], collapse = ""))
yitian_para[1:2]

# 对角色进行统计
roles1 <- paste0("(", gsub(" ", ")|(", roles), ")")
roles_l <- strsplit(roles, " ") # 总结每个人的不同称呼

# 计算每个人物名称的出现次数count
role_para <- sapply(roles1, grepl, yitian_para)
colnames(role_para) <- sapply(roles_l, function(x) x[1])
# 将角色出现次数赋值到一个数据框中以便作图
role_count <- data.frame(role = factor(colnames(role_para), levels = colnames(role_para)[order(colSums(role_para), decreasing = T)]), count = colSums(role_para))
head(role_count)

# 根据出现次数绘图
ggplot(role_count[1:5, ], aes(x = role, y = count, fill = role)) +
  geom_bar(stat = "identity", width = 0.75) +
  xlab("Role") +
  ylab("Count") +
  theme_grey(base_family = "STKaiti") +
  theme(
    axis.text = element_text(size = 24, angle = 90),
    axis.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(vjust = -2),
    legend.position = "none"
  )
# 计算各人物的出场顺序
# 将出场顺序放入数据框中以便作图
main_roles <- c("殷离", "周芷若", "赵敏", "小昭", "张无忌")
role_para_df <- data.frame(
  role = factor(rep(main_roles, colSums(role_para)[1:5])),
  para = which(role_para[, (1:5)], arr.ind = T)[, 1]
)

# 提取出三个主角的出场顺序
role_para_df1 <- role_para_df[is.element(role_para_df$role, c("赵敏", "周芷若", "张无忌")), ]

# 作出三个主角的出场顺序密度图
(p <- ggplot(role_para_df1, aes(para, fill = role)) +
  geom_density(aes(y = ..density..), alpha = I(0.3), color = "white", size = 0) +
  labs(x = "\n出场顺序", y = "密 度\n") +
  scale_fill_manual("角 色", values = terrain.colors(7)[3:1]) +
  theme_grey(base_family = "STKaiti") +
  theme(
    plot.margin = unit(c(0, 1, 1, 0), "cm"),
    axis.text = element_text(size = 24),
    axis.title.x = element_text(vjust = -2),
    axis.title = element_text(size = 24, face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.line = element_line(color = "grey40"),
    legend.key.size = unit(0.8, "cm"),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 24)
  ))

# 计算亲密度逻辑值
co_para <- crossprod(role_para)

# 将无意义的矩阵对角线元素化为0
diag(co_para) <- 0

# 构建前5个人物亲密度矩阵
intimacy_main <- co_para[1:5, 1:5]

# 调整数据组织形式以便画图
intimacy_main <- melt(intimacy_main)

# 绘制亲密度矩阵图
(p <- ggplot(intimacy_main, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "#32CD32") +
  geom_text(data = intimacy_main, aes(Var2, Var1, label = value), size = 13, color = "black") +
  scale_fill_gradient(low = "white", high = "#32CD32") +
  xlab("") +
  ylab("") +
  scale_x_discrete(expand = c(0, 0), limits = c("张无忌", "赵敏", "周芷若", "殷离", "小昭")) +
  scale_y_discrete(expand = c(0, 0), limits = c("张无忌", "赵敏", "周芷若", "殷离", "小昭")[5:1]) +
  theme_grey(base_family = "STKaiti") +
  theme(
    axis.text = element_text(size = 24, angle = 90),
    axis.title = element_text(size = 24, face = "bold"),
    axis.title.x = element_text(vjust = -2),
    legend.position = "none"
  ))
styler:::style_active_file()
