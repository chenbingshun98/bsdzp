#方法的区分
#根据X变量的个数，方差分析又分为单因素方差分析和多因素方差分析，当X的个数（不是组别数量）为1个时，我们称之为单因素方差；X的个数为2个时，则为双因素方差。

library(tidyverse)
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
wages <- read.csv("wages.csv")
wages %>% 
  head() %>% 
  knitr::kable()

# 单因素方差分析
t.test(earn ~ sex, data = wages)

lm(earn ~ sex,
   data = wages) %>% 
  summary()

wages %>% 
  aov(formula = earn ~ sex) %>% 
  summary()

# 双因素方差分析
# 我们采用ggpubr宏包下的ToothGrowth来说明，
#这个数据集包含60个样本，
#记录着每10只豚鼠在不同的喂食方法和不同的药物剂量下，
#牙齿的生长情况.
# 
# len : 牙齿长度
# supp : 两种喂食方法 (橙汁和维生素C)
# dose : 抗坏血酸剂量 (0.5, 1, and 2 mg)
library("ggpubr")
my_data <- ToothGrowth %>% 
  mutate_at(vars(supp, dose),
            ~as.factor(.))
my_data %>% head()

my_data %>% 
  ggplot(aes(x = supp,
             y = len,
             fill = supp))+
  geom_boxplot(position = position_dodge())+
  facet_wrap(vars(dose))+
  labs(title = "VC剂量和摄入方式对豚鼠牙齿的影响")

#问题：豚鼠牙齿的长度是否与药物的食用方法和剂量有关？

# 线性回归时，
#我们是通过独立变量来预测响应变量，
#但现在我们关注的重点会从预测转向不同组别差异之间的分析，
#这即为方差分析（ANOVA）。
# 
# 这里是两个解释变量，
#所以问题需要双因素方差分析 (ANOVA)
my_data %>% 
  aov(formula = len ~ supp + dose) %>% 
  broom::tidy()

# 检验表明不同类型之间存在显著差异，
#但是并没有告诉我们具体谁与谁之间的不同。
#需要多重比较帮助我们解决这个问题。
#使用TurkeyHSD函数
my_data %>% 
  aov(formula = len ~ supp + dose) %>% 
  TukeyHSD(which = "dose") %>% 
  broom::tidy()

my_data %>% 
  aov(formula = len ~ supp + dose) %>% 
  TukeyHSD(which = "supp") %>% 
  broom::tidy()

#思考：交互效应是否显著？
my_data %>% 
  aov(formula = len ~ supp * dose) %>% 
  broom::tidy()

#tidyverse中的应用
mtcars %>%
  group_by(cyl) %>%
  summarise(
    broom::tidy(aov(mpg ~ gear, data = cur_data())),
    .groups = "keep"
  ) %>% 
  select(term, statistic, p.value) %>% 
  filter(term != "Residuals") %>% 
  arrange(p.value)
