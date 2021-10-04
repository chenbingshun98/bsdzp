dat <- read_csv("https://github.com/IQSS/dss-workshops-archived/blob/master/R/Rgraphics/dataSets/EconomistData.csv")
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R脚本/EconomistData.csv")
dat <- read.csv("EconomistData.csv")

pc1 <- ggplot(dat, aes(x = CPI, y = HDI, color = Region))
pc1 + geom_point()

# 要完成整个图表，我们需要：
# 添加趋势线将点的形状改为空心圆圈
# 更改图例Region的顺序和标签
# 选择展示部分点的标签
# 雕琢刻度线和标签
# 将彩色图例放置于顶部
# 增加标题、坐标轴标题，并去掉图例的标题
# 使用没有垂直网格线的主题
# 添加模型的 R^2（难）
# 添加注释（难）最后使其完美（使用图像编辑器）

#添加趋势线
pc2 <- pc1 + 
  geom_smooth(mapping = aes(linetype = "r2"),
              method = "lm",
              formula = y ~ x + log(x),
              se = F,
              color = "red")
pc2+geom_point()

## A look at all 25 symbols
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
s <- ggplot(df2, aes(x = x, y = y))
s + geom_point(aes(shape = z), size = 4) + scale_shape_identity()
## While all symbols have a foreground colour, symbols 19-25 also take a
## background colour (fill)
s + geom_point(aes(shape = z), size = 4, colour = "Red") +
  scale_shape_identity()
s + geom_point(aes(shape = z), size = 4, colour = "Red", fill = "Black") +
  scale_shape_identity()

pc2 +
  geom_point(shape = 1, size = 4)

(pc3 <- pc2 + geom_point(shape = 1, size = 2.5, stroke = 1.25))

#标签
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

#使用geom_text来标记
(pc4 <- pc3 +
    geom_text(aes(label = Country),
              color = "gray20",
              data = subset(dat, Country %in% pointsToLabel)))

#重叠了，所以用ggrepel手动调整

library("ggrepel")
(pc4 <- pc3 +
    geom_text_repel(aes(label = Country),
                    color = "gray20",
                    data = subset(dat,
                                  Country %in% pointsToLabel),
                    force = 10))

#将我们的图表与原始图表比较，
#我们注意到颜色图例中region的标签和顺序不同。
#为了纠正这个问题，
#我们需要改变region变量的标签和顺序。
#我们可以用这个factor功能来做到这一点。
dat$Region <- factor(dat$Region,
                     levels = c("EU W. Europe",
                                "Americas",
                                "Asia Pacific",
                                "East EU Cemt Asia",
                                "MENA",
                                "SSA"),
                     labels = c("OECD",
                                "Americas",
                                "Asia &/nOceania",
                                "Central &/nEastern Europe",
                                "Middle East &/nnorth Africa",
                                "Sub-Saharan/nAfrica"))

#现在，当我们使用这些数据构建绘图时，
#顺序应该像初始那样出现。
pc4$data <- dat
pc4

#添加标题和坐标轴
library(grid)
(pc5 <- pc4 +
    scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                       limits = c(.9, 10.5),#好像ggplot里的xmin和xmax
                       breaks = 1:10) +
    scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",
                       limits = c(0.2, 1.0),
                       breaks = seq(0.2, 1.0, by = 0.1)) +
    scale_color_manual(name = "",
                       values = c("#24576D",
                                  "#099DD7",
                                  "#28AADC",
                                  "#248E84",
                                  "#F2583F",
                                  "#96503F")) +
    ggtitle("Corruption and Human development"))

library(grid) # for the 'unit' function
(pc6 <- pc5 +
    theme_minimal() + # start with a minimal theme and add what we need
    theme(text = element_text(color = "gray20"),
          legend.position = c("top"), # position the legend in the upper left 
          legend.direction = "horizontal",
          legend.justification = 0.1, # anchor point for legend.position.
          legend.text = element_text(size = 11, color = "gray10"),
          axis.text = element_text(face = "italic"),
          axis.title.x = element_text(vjust = -1), # move title away from axis
          axis.title.y = element_text(vjust = 2), # move away for axis
          axis.ticks.y = element_blank(), # element_blank() is how we remove elements
          axis.line = element_line(color = "gray40", size = 0.5),
          axis.line.y = element_blank(),
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank()
    ))
#添加模型的 [R^2] 和源注释

mR2 <- summary(lm(HDI ~ CPI + log(CPI), data = dat))$r.squared
mR2 <- paste0(format(mR2, digits = 2), "%")

png(file = "images/econScatter10.png", width = 800, height = 600)

p <- ggplot(dat,
            mapping = aes(x = CPI, y = HDI)) +
  geom_point(mapping = aes(color = Region),
             shape = 1,
             size = 4,
             stroke = 1.5) +
  geom_smooth(mapping = aes(linetype = "r2"),
              method = "lm",
              formula = y ~ x + log(x), se = FALSE,
              color = "red") +
  geom_text_repel(mapping = aes(label = Country, alpha = labels),
                  data = transform(dat,
                                   labels = Country %in% c("Russia",
                                                           "Venezuela",
                                                           "Iraq",
                                                           "Mayanmar",
                                                           "Sudan",
                                                           "Afghanistan",
                                                           "Congo",
                                                           "Greece",
                                                           "Argentinia",
                                                           "Italy",
                                                           "Brazil",
                                                           "India",
                                                           "China",
                                                           "South Africa",
                                                           "Spain",
                                                           "Cape Verde",
                                                           "Bhutan",
                                                           "Rwanda",
                                                           "France",
                                                           "Botswana",
                                                           "France",
                                                           "US",
                                                           "Germany",
                                                           "Britain",
                                                           "Barbados",
                                                           "Japan",
                                                           "Norway",
                                                           "New Zealand",
                                                           "Sigapore"))) +
  scale_x_continuous(name = "Corruption Perception Index, 2011 (10=least corrupt)",
                     limits = c(1.0, 10.0),
                     breaks = 1:10) +
  scale_y_continuous(name = "Human Development Index, 2011 (1=best)",
                     limits = c(0.2, 1.0),
                     breaks = seq(0.2, 1.0, by = 0.1)) +
  scale_color_manual(name = "",
                     values = c("#24576D",
                                "#099DD7",
                                "#28AADC",
                                "#248E84",
                                "#F2583F",
                                "#96503F"),
                     guide = guide_legend(nrow = 1)) +
  scale_alpha_discrete(range = c(0, 1),
                       guide = FALSE) +
  scale_linetype(name = "",
                 breaks = "r2",
                 labels = list(bquote(R^2==.(mR2))),
                 guide = guide_legend(override.aes = list(linetype = 1, size = 2, color = "red"))) +
  ggtitle("Corruption and human development") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        axis.line.x = element_line(color = "gray"),
        axis.text = element_text(face = "italic"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"))
p
dev.off()
