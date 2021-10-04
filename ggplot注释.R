library(tidyverse)

p <- ggplot(mtcars,
            aes(x = wt, y = mpg))+
  geom_point()

p + annotate("text", x = 4, y = 25, label = "text")

p <- ggplot(mtcars, aes(x = wt, y = mpg))+
  geom_point()

#如果想要显示中文，需要设置字体，否则会乱码。
p <- ggplot(mtcars, aes(x = wt, y = mpg))+
  geom_point()

library(fontcm)
library(extrafont)
library(extrafontdb)
library(showtext)
library(showtextdb)
fonts()
font_install(source_han_serif())
font_files() %>% view()
sysfonts::font_families()
showtextdb::font_install("STKAITI.TTF")

showtextdb::source_han_sans()
showtextdb::font_installed()
showtextdb::load_showtext_fonts()
font_add("STKaiti", "STKAITI.TTF")
p + annotate("text",
             x = 4, 
             y = 25,
             label = "注释",
             family = "STXinwei")

p + annotate("text",
             x = 2:5, 
             y = 25,
             label = "注释",
             family = "STXinwei")

p + annotate("text",
             x = 4, 
             y = 25,
             label = "italic(R)^2 == .75",
             parse = TRUE)

p + annotate("text",
             x = 4, 
             y = 25,
             label = "paste(italic(R)^2, \" = .75\")",
             parse = TRUE)

p <- ggplot(mtcars,
            aes(wt, mpg,
                label = rownames(mtcars)))

p + geom_text()

#避免重叠
p + geom_text(check_overlap = TRUE)

p +geom_label()

#添加表达式
p + geom_text(aes(label = paste(wt, "^(", cyl, ")", sep = "")),
              parse = TRUE,
              family = "Times New Roman")

#添加线条
p + annotate("segment",
             x = 2.5,
             xend = 4,
             y = 15,
             yend = 25,
             color = "#66c2a5")

#添加误差线
p1 <-  p + annotate("pointrange",
                    x = 3.5,
                    y = 20,
                    ymin = 12,
                    ymax = 28,
                    color = '#e41a1c',
                    size = 1)

p2 <- p + annotate("errorbar",
                   x = 3.5,
                   y = 20,
                   ymin = 12,
                   ymax = 28,
                   color = '#377eb8',
                   size = 1)

p3 <- p + annotate("linerange",
                   x = 3.5,
                   y = 20,
                   ymin = 12,
                   ymax = 28,
                   color = '#4daf4a',
                   size = 1)

p4 <- p + annotate("crossbar",
                   x = 3.5,
                   y = 20,
                   ymin = 12,
                   ymax = 28,
                   color = '#984ea3',
                   size = 1)

cowplot::plot_grid(p1, p2, p3, p4,
                   labels = c("pointrange", "errorbar", "linerange", "crossbar"))                  

do.call("plot_grid", 
        list(paste0("p", 1:4) %>% as_mapper(), 
          labels = c("pointrange", "errorbar", "linerange", "crossbar")))
#添加形状
p + annotate("rect",
             xmin = 3,
             xmax = 4.2,
             ymin = 12,
             ymax = 21,
             fill = "#66c2a5",
             color = 'black',
             alpha = .2)

ggplot(faithful, aes(waiting, eruptions)) +
  geom_point() +
  stat_ellipse()

#分组
ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3)) +
  geom_point() +
  stat_ellipse()

#设置不同的分布类型，默认是 "t" 假设数据为多变量 t 分布
ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3)) +
  geom_point() +
  stat_ellipse(type = "norm", linetype = 2) +
  stat_ellipse(type = "t") +
  scale_color_manual(values = c("#984ea3", "#377eb8"))

#如果想要绘制圆形，需要与 coord_fixed 搭配使用，圆半径为 level
ggplot(faithful,
       aes(waiting, eruptions,
           color = eruptions > 3))+
  geom_point()+
  stat_ellipse(type = 'norm',
               linetype = 2)+
  stat_ellipse(type = 'euclid',
               level = 3)+
  coord_fixed()

#stat_ellipse 默认使用的是对象是 path，也可以使用多边形填充
ggplot(faithful,
       aes(waiting, eruptions,
           fill = eruptions > 3))+
  stat_ellipse(geom = "polygon")+
  geom_point(shape = 21)

#那如果我想绘制分组数据的边界，要怎么办呢？

# 我们可以先对数据进行分组，
#然后使用 chull 函数计算出每组数据的凸点，
#再将每个分组的凸点合并为数据框
# 
# 注意：我们应该先对数据进行拆分，
#因为 chull 函数返回的是点的索引，
#对数据 group_by 之后，
#索引是基于每个分组而不是整个数据而言的，
#切记。
df <- group_by(faithful, eruptions > 3) %>%
  group_split() %>%
  lapply(., function (x) x[chull(x$waiting, x$eruptions),]) %>%
  do.call(rbind, .)

ggplot(faithful, aes(waiting, eruptions, fill = eruptions > 3)) +
  geom_polygon(data = df, aes(waiting, eruptions), alpha = 0.5, colour = "grey40") +
  geom_point(shape = 21, alpha = 0.6) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62")) +
  theme_bw()
