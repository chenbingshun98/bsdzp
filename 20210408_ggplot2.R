library(tidyverse)
library(cowplot)
#笛卡尔坐标系
p <- ggplot(mtcars, aes(disp, wt))+
  geom_smooth()+
  geom_point()
p

#范围之外的数据会被设置为NA
p1 <- p + scale_x_continuous(limits = c(325, 500))
p1

p2 <- p + coord_cartesian(xlim = c(325, 500))
p2

plot_grid(p1, p2, labels = LETTERS[1:2])

#可以设置expand = FALSE,
#不添加扩展因子，允许数据与坐标轴重叠
p1 <- p + coord_cartesian(xlim = c(325, 500), expand = FALSE)
p1

p2 <- p + coord_cartesian(expand = FALSE)

plot_grid(p1, p2, labels = LETTERS[1:2])

#热图
d <- ggplot(diamonds,
            aes(carat, price))+
  stat_bin2d(bins = 25, 
             color = 'white')
d

#缩放的方式不同
p1 <- d + scale_x_continuous(limits = c(0, 1))
p2 <- d + coord_cartesian(xlim = c(0, 1))

plot_grid(p1, p2, labels = LETTERS[1:2])

#coord_fixed
#确保x轴和y轴具有相同的标度，
#即保证在任何情况下坐标轴的纵横比不变
#默认1:1（y：x），即ratio = 1
p <- ggplot(mtcars,
            aes(mpg, wt))+
  geom_point()
p

p1 <- p + coord_fixed(ratio = 1)
p2 <- p + coord_fixed(ratio = 5)
p3 <- p + coord_fixed(ratio = 1/5)
p4 <- p + coord_fixed(xlim = c(15, 30))

plot_grid(p1, p2, p3, p4, labels = LETTERS[1:4], nrow = 4)

#coord_flip
p1 <- ggplot(diamonds,
             aes(cut, price))+
  geom_boxplot()
p2 <- ggplot(diamonds,
             aes(cut, price))+
  geom_boxplot()+
  coord_flip()

h <- ggplot(diamonds,
            aes(carat))+
  geom_histogram()

h1 <- h + coord_flip()

h2 <- h + coord_flip()+ scale_x_reverse()

plot_grid(h, h1, h2,
          labels = LETTERS[1:3],
          nrow = 3)

#面积翻转
df <- tibble(x = 1:5,
                 y = (1:5)^2)
df

p1 <- ggplot(df,
             aes(x, y))+
  geom_area()

p2 <- p1 + coord_flip()

plot_grid(p1, p2, labels = LETTERS[1:2], nrow = 2)

#coord_trans
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]

#A的坐标轴范围是根据转换后的值确定的
p1 <- ggplot(dsamp,
             aes(log10(carat), log10(price)))+
  geom_point()

#B保留了原始数据的坐标范围
p2 <- ggplot(dsamp,
             aes(carat, price))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()

#C的坐标轴之间的距离经过对数变换之后像是被拉扯过一样
#但是数据还是和原来的一样
p3 <- ggplot(dsamp,
             aes(carat, price))+
  geom_point()+
  coord_trans(x = "log10",
              y = "log10")

plot_grid(p1, p2, p3, labels = LETTERS[1:3], nrow = 3)

d <- filter(diamonds, carat > 0.5)

p1 <- d %>% 
  ggplot(aes(carat, price))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10()+
  scale_y_log10()

p2 <- d %>% 
  ggplot(aes(carat, price))+
  geom_point()+
  geom_smooth(method = "lm")+
  coord_trans(x = "log10",
              y = "log10")

plot_grid(p1, p2, labels = LETTERS[1:2], nrow = 2)

p1 <- dsamp %>% 
  ggplot(aes(carat, price))+
  geom_point()+
  geom_smooth(method = "lm")

p2 <- dsamp %>% 
  ggplot(aes(carat, price))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_x_log10()+
  scale_y_log10()+
  coord_trans(x = scales::exp_trans(10),
              y = scales::exp_trans(10))

plot_grid(p1, p2, labels = LETTERS[1:2], nrow = 2)

#极坐标系
pie <- ggplot(mtcars,
              aes(x = factor(1),
                  fill = factor(cyl)))+
  geom_bar(width = 1)

p1 <- pie + coord_polar(theta = "y")

cxc <- ggplot(mtcars,
              aes(x = factor(cyl)))+
  geom_bar(width = 1,
           color = "black")

p2 <- cxc + coord_polar()

plot_grid(pie, p1, cxc, p2,
          labels = LETTERS[1:4],
          nrow = 2)

#不知道什么图
p3 <- cxc + coord_polar(theta = "y")

#牛眼图
p4 <- pie + coord_polar()

plot_grid(p3, p4,
          labels = LETTERS[1:2],
          nrow = 1)

#百分比图
df <- tibble(
  variable = c("does not resemble", "resembles"),
  value = c(20, 80)
)
df

df %>% 
  ggplot(aes(x = "", y = value, fill = variable))+
  geom_col(width = 1)+
  coord_polar("y",
              start = pi/3)+
  labs(title = "Pac man")

library(vegan)

data("varespec")
df <- varespec

#计算距离
bray_dist <- vegdist(df,
                     method = "bray")
#PCoA分析
library(ape)
df.pcoa <- pcoa(bray_dist,
                correction = "cailliez")
df.pcoa

df.plot <- data.frame(df.pcoa$vectors)
df.plot2 <- tibble(df.pcoa$vectors) 
df.plot2 <- data.frame(df.pcoa$vectors) %>% as_tibble()

head(df.plot)

x_label <- round(df.pcoa$values$Rel_corr_eig[1]*100,2)
y_label <- round(df.pcoa$values$Rel_corr_eig[2]*100,2)

x_label
y_label

df.plot2 %>% 
  ggplot(aes(x = Axis.1,
         y = Axis.2))+
  geom_point()+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,
             lty = "dashed")+
  geom_hline(yintercept = 0,
             lty = "dashed")+
  labs(x = paste0("PCoA1", x_label, "%"),
       y = paste0("PCoA2", y_label, "%"))

df.plot2$group <- if_else(df.plot2$Axis.1 < 0, "AAA", "BBB")

df.plot2 %>% 
  ggplot(aes(x = Axis.1,
             y = Axis.2,
             color = group,
             shape = group))+
  geom_point(size = 5)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,
             lty = "dashed")+
  geom_hline(yintercept = 0,
             lty = "dashed")+
  labs(x = paste0("PCoA1", x_label, "%"),
       y = paste0("PCoA2", y_label, "%"))+
  stat_ellipse(data = df.plot2,
               geom = "polygon",
               aes(fill = group),
               alpha = 0.3)+
  scale_fill_manual(values = c("#e31a1c", "#1f78b4"))
