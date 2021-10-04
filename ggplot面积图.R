library(tidyverse)
library(cowplot)
# 以x轴作为绘制方向
#geom_ribbon()
#制定y轴区域（ymin， ymax）

#geom_area
#绘制线条与x轴之间的范围（ymin = 0，ymax = y）
df <- tibble(x = sample(30:50, 20),
             y = sample(1:20, 20))

p1 <- ggplot(df, aes(x, y))+
  geom_line()+
  geom_area(fill = 'blue',
            alpha = 0.5)

p2 <- ggplot(df, aes(x, y))+
  geom_line()+
  geom_ribbon(aes(ymin = y - 1,
                  ymax = y + 1),
              fill = 'lightgreen',
              alpha = 0.5)

p3 <- ggplot(df, aes(x, y))+
  geom_line(orientation = 'y')+
  geom_area(fill = 'blue',
            alpha = 0.5,
            orientation = 'y')

p4 <- ggplot(df, aes(x, y))+
  geom_line(orientation = 'y')+
  geom_ribbon(aes(xmin = x - 1,
                  xmax = x + 1),
              orientation = 'y',
              fill = 'lightgreen',
              alpha = 0.5)

plot_grid(p1, p2, p3, p4)

p <- subset(economics_long,
            variable %in% c("pce", "unemploy")) %>% 
  ggplot(aes(x = date))

p1 <- p + geom_area(aes(y = value01,
                        fill = variable),
                    alpha = 0.4,
                    position = 'stack')

p2 <- p + geom_area(aes(y = value01,
                        fill = variable),
                    alpha = 0.4,
                    position = 'fill')

plot_grid(p1 ,p2)

#绘制两条曲线之间的区域面积
p <- subset(economics_long,
            variable %in% c("pce", "unemploy")) %>% 
  select(c(date,variable, value01)) %>% 
  pivot_wider(
    names_from = variable,
    values_from = value01) %>% 
  ggplot(aes(x = date))+
  geom_ribbon(aes(
    ymin = if_else(pce > unemploy, unemploy, pce),
    ymax = if_else(pce > unemploy, pce, unemploy)
    ),
    fill = "#decbe4",
    color = 'black'
              )
p

#使用不同的颜色区分
p + geom_ribbon(
  aes(
    ymin = pmin(unemploy, pce),
    ymax = pmax(pce, unemploy),
    fill = pce > unemploy,
    color = pce > unemploy
      ),
  show.legend = FALSE,
  na.rm = TRUE
  )+
  scale_fill_manual(
    values = c('#8dd3c7', '#fdb462')
  )

#去除黏连
##方法：分开绘制
##在绘制某一种区域时，将相反区域的值都设置为NA
df <- filter(economics_long,
             variable %in% c('pce', 'unemploy')) %>% 
  select(c(date, variable, value01)) %>% 
  pivot_wider(
    names_from = 'variable',
    values_from = 'value01') %>% 
  mutate(
    low_min = pmin(pce, unemploy),
    low_max = pmax(pce, unemploy),
    high_min = low_min,
    high_max = low_max
  )

df$low_min[df$pce > df$unemploy] <- NA
df$low_max[df$pce > df$unemploy] <- NA

df$high_min[df$pce <= df$unemploy] <- NA
df$high_max[df$pce <= df$unemploy] <- NA

ggplot(df, aes( x = date))+
  geom_ribbon(aes(ymin = low_min, ymax = low_max),
              fill = '#8dd3c7',
              alpha = 0.7)+
  geom_ribbon(aes(ymin = high_min, ymax = high_max),
              fill = '#fdb462',
              alpha = 0.7)+
  geom_line(aes(y = pce),
            color = '#fb8072',
            size = .75)+
  geom_line(aes(y = unemploy),
            color = '#80b1d3',
            size = .75)
