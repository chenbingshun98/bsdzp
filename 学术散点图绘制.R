install.packages("ggpubr")
library("ggpubr")
ggplot(d07,aes(x=d07$sex,y=d07$expenditure))+
  geom_point(shape = 15)+
  geom_smooth(method = "lm",#绘制拟合线
              se = F,#不显示置信区间
              color = "red",
              size = 1)+
  geom_abline(slope = 1.15,#绘制上误差线
              intercept = .05,
              linetype = "dashed",
              size = 1)+
  geom_abline(slope = .85,#绘制下误差线
              intercept = -.05,
              linetype = "dashed",
              size = 1)+
  #使用ggpubr包添加R2等元素
  stat_regline_equation(label.x = .1,
                        label.y = 1.8,
                        size = 6,
                        family = "SimSun",
                        fontface = "bold")+
  stat_cor(aes(label = paste(..rr.label..,..p.label..,sep = "~`,`~")),
           label.x = .1,label.y = 1.6,
           size = 6,
           family = "SimSun",
           fontface = "bold")+
  geom_text(x=.1,y=1.4,
            label="N = 4348",
            size=6,
            family='SimSun',
            hjust = 0)+
  #修改坐标轴刻度
  scale_x_continuous(limits = c(0,2),
                     breaks = seq(0,2,0.2),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(0,2),
                     breaks = seq(0,2,0.2),
                     expand = c(0,0)) +
  labs(x ='性别',
       y="模型估计",
       title = "数据散点图",
       subtitle = "性别与补习费用支出 的关系",
       caption = '由陈冰顺可视化')+
  #添加图序号（a）
  geom_text(x=1.85,y=1.85,
            label='(a)',
            size=9,
            family='SimSun',
            fontface='bold')+
  #添加误差个数
  geom_text(x=1.4,y=.4,
            label='Within EE = 52%',
            size=5,
            family='SimSun',
            hjust = 0)+
  geom_text(x=1.4,y=.3,
            label='Above EE = 39%',
            size=5,
            family='SimSun',
            hjust = 0)+
  geom_text(x=1.4,y=.2,label='Below EE = 9%',
            size=5,
            family='SimSun',
            hjust = 0)+
  # theme_base() +
  theme(text = element_text(family = "SimSun",face='bold'),
        axis.text = element_text(family = 'SimSun',
                                 size = 12,
                                 face = 'bold'),
        #修改刻度线内
        axis.ticks.length=unit(-0.22, "cm"), 
        #加宽图边框
        panel.border = element_rect(size=1),
        plot.background = element_rect(color = "white"),
        axis.line = element_line(size = .8),
        axis.ticks = element_line(size = .8),
        #去除图例标题
        #legend.title = element_blank(),
        #设置刻度label的边距
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))