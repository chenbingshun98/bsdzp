#定义公式
sigmoid <- function(x) 1/(1+exp(-x))

#画sigmid图像
curve(sigmoid,-10,10)

#画sin(x)函数图像
curve(sin,-10,10)

# 画y=x^2的图像
curve(x^2,-10,10)

# geom_path() 按照观测值在数据中出现的顺序连接观测值(如果画函数图,推荐此映射,原因后面知晓)。
# 定义函数
sigmoid <- function(x) 1/(1+exp(-x))

# 创建数据点
x<-seq(-5, 5, by=0.01)
y<-sigmoid(x)
df<-data.frame(x, y)

# 用ggplot2来画图
g <- ggplot(df, aes(x,y))
g <- g + geom_path(col='red') ## 用geom_line 替代也是可以的,但不推荐
g <- g + geom_hline(yintercept = 0.5) + geom_vline(xintercept = 0) #坐标轴
g <- g +  labs(title="sigmoid", x="x", y="y")
g <- g +theme(plot.title = element_text(hjust = 0.5))  #标题居中
g

# 2. 函数图像具有参数方程
# 例如: 圆, 椭圆, 抛物线, 双曲线 方程.
# 
# 2.1 画x^2+y^2 = 4x 
# 2
# +y 
# 2
# =4的函数图像
library(ggplot2)
r = 2
theta=seq(0, 2*pi, by=0.001)
x = r*cos(theta)
y = r*sin(theta) 
df <- data.frame(x, y, theta,frame = 1:length(theta))
g <- ggplot(df, aes(x,y))
g <- g + geom_path(col='red')
g

###################
####让上面的图动起来
library(gganimate)
library(transformr)
temp = g + transition_reveal(along = frame)

animate(temp,
        nframes=100,#总帧数(默认)
        duration=10 #总时长，单位为秒，默认为10秒
)

#椭圆参数方程
library(ggplot2)
a = 2
b = 3
theta=seq(0, 2*pi, by=0.001)
x = a*cos(theta)
y = b*sin(theta) 
df <- data.frame(x, y, theta,frame = 1:length(theta))
g <- ggplot(df, aes(x,y))
g <- g + geom_path(col='red')
## 让图动起来
temp = g + transition_reveal(along = frame)

animate(temp)
temp

#抛物线
p = 4
t = seq(3,-3,-0.2)
x = 2*p*t^2
y = 2*p*t 
df <- data.frame(x, y, t,frame = 1:length(t))
g <- ggplot(df, aes(x,y))
g <- g + geom_path(col='red')
## 让图动起来
temp = g + transition_reveal(along = frame)

animate(temp)

library(covdata) # remotes::install_github("kjhealy/covdata")

covdata::covnat_daily %>%
  dplyr::filter(iso3 == "USA") %>%
  dplyr::filter(cu_cases > 0) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_path() +
  labs(
    title = "美国新冠肺炎累积确诊病例",
    subtitle = "数据来源https://kjhealy.github.io/covdata/"
  )

uscov <- covdata::covnat_daily %>%
  dplyr::filter(iso3 == "USA") %>%
  dplyr::filter(cu_cases > 0) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_path() +
  labs(
    title = "美国新冠肺炎累积确诊病例 {frame_along}",
    subtitle = "数据来源https://kjhealy.github.io/covdata/"
  ) +
  transition_reveal(along = date)
uscov
anim_save("first_saved_animation.gif", animation = uscov)