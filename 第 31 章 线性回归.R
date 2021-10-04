library(tidyverse)
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
wages <- read.csv("wages.csv")

wages %>% 
  head()

#缺失值检查
wages %>% colnames()

# 如何检查数据是否有缺失值？
wages %>%
  summarise(
    earn_na = sum(is.na(earn)),
    height_na = sum(is.na(height)),
    sex_na = sum(is.na(sex)),
    race_na = sum(is.na(race)),
    ed_na = sum(is.na(ed)),
    age_na = sum(is.na(age))
  )

wages %>% 
  summarise(
    across(everything(), ~sum(is.na(.)))
  )

wages %>%
  summarise_all(
    ~ sum(is.na(.))
  )

wages %>% 
  map_df(~sum(is.na(.)))

wages %>% 
  group_by(sex) %>% 
  summarise(
    n = n(),
    mean_height = mean(height),
    mean_earn = mean(earn)
  )

# 也有可以用可视化的方法，呈现男女收入的分布情况
wages %>% 
  ggplot(aes(x = earn,
             color = sex))+
  geom_density()

# 大家可以自行探索其他变量的情况。现在提出几个问题，希望大家带着这些问题去探索：
# 
# 长的越高的人挣钱越多？
# 
# 是否男性就比女性挣的多？
# 
# 影响收入最大的变量是哪个？
# 
# 怎么判定我们建立的模型是不是很好？

wages %>%
  ggplot(aes(x = height, y = earn)) +
  geom_point()

mod1 <- lm(
  formula = earn ~height,
  data = wages
)

names(mod1)

print(mod1)

summary(mod1)

# predict(mod1) # predictions at original x values
wages %>% modelr::add_predictions(mod1)

# resid(mod1)
wages %>%
  modelr::add_predictions(mod1) %>%
  modelr::add_residuals(mod1)

wages %>%
  ggplot(aes(x = height, y = earn)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE)

#多元线性回归
mod2 <- lm(earn ~ height + ed, data = wages)

lm(earn ~ sex, data = wages)
lm(earn ~ ed, data = wages)
lm(earn ~ age, data = wages)

lm(earn ~ height + sex, data = wages)
lm(earn ~ height + ed, data = wages)
lm(earn ~ height + age, data = wages)
lm(earn ~ height + race, data = wages)


lm(earn ~ height + sex + ed, data = wages)
lm(earn ~ height + sex + age, data = wages)
lm(earn ~ height + sex + race, data = wages)
lm(earn ~ height + ed + age, data = wages)
lm(earn ~ height + ed + race, data = wages)
lm(earn ~ height + age + race, data = wages)

lm(earn ~ height + sex + ed + age, data = wages)
lm(earn ~ height + sex + ed + race, data = wages)
lm(earn ~ height + sex + age + race, data = wages)
lm(earn ~ height + ed + age + race, data = wages)
lm(earn ~ sex + ed + age + race, data = wages)

lm(earn ~ height + sex + ed + age + race, data = wages)

#变量重要性
#方法一，变量都做标准化处理后，再放到模型中计算，然后对比系数的绝对值
fit <- wages %>%
  mutate_at(vars(earn, height, ed, age), scale) %>%
  lm(earn ~ 1 + height + ed + age, data = .)

summary(fit)

# 方法二，通过比较模型参数的t-statistic的绝对值，可以考察参数的重要程度
v <- caret::varImp(fit)
v$name <- row.names(v)
v
v2 <- caret::varImp(fit, scale = F)
v2
ggplot(v, aes(name, Overall))+
  geom_point(size = 5)+
  geom_segment(aes(x = name, xend = name,
                   y = 0, yend = Overall))+
  coord_flip()
#截距项
# 包含截距，以下两者是等价的
lm(earn ~ 1 + height, data = wages)
lm(earn ~ height, data = wages)

# 去掉截距，以下两者是等价的
lm(earn ~ height - 1, data = wages)
lm(earn ~ 0 + height, data = wages)

#只有截距项
lm(earn ~ 1, data = wages)

#只有截距项，实质上就是计算y变量的均值
wages %>%
  summarise(
    mean_wages = mean(earn)
  )

#分类变量
wages %>% distinct(race)
?distinct

wages %>%
  ggplot(aes(x = race, y = earn, fill = race)) +
  geom_boxplot(position = position_dodge()) +
  scale_y_continuous(limits = c(0, 20000))

mod3 <- lm(earn ~ race, data = wages)
mod3
mod4 <- lm(earn ~ sex, data = wages)

broom::tidy(mod3)
broom::tidy(mod4)
#我们看到输出结果，只有race_hispanic、 race_other和race_white三个系数和Intercept截距，race_black去哪里了呢？

# 事实上，race变量里有4组，回归时，选择black为基线，hispanic的系数，可以理解为由black切换到hispanic，引起earn收入的变化（效应）



# 31.8.4 因子变量
# hispanic组的估计最低，适合做基线，
#因此可以将race转换为因子变量，
#这样方便调整因子先后顺序
wages_fct <- wages %>% 
  mutate(race = factor(race,
                       levels = c("hispanic", "white", "black", "other")
  )) %>% 
  select(earn, race)

head(wages_fct)

mod4 <- lm(earn ~ race, data = wages_fct)
broom::tidy(mod4)

# 以hispanic组作为基线，各组系数也调整了，但加上截距后，实际值是没有变的。
# 大家可以用sex变量试试看
lm(earn ~ sex, data = wages)

# 31.8.5 一个分类变量和一个连续变量
mod5 <- lm(earn ~ height + sex, data = wages)
coef(mod5)
p1 <- wages %>% 
  ggplot(aes(x = height,
             y = earn,
             color = sex))+
  geom_point(alpha = 0.1)+
  geom_line(aes(y = predict(mod5)))+
  scale_y_continuous(limits = c(0, 100000))
p1

#偷懒的写法
lm(earn ~ height + sex + race + ed + age, data = wages)
lm(earn ~ ., data = wages)

lm(earn ~ height + sex + race + ed, data = wages)
lm(earn ~ . - age, data = wages)

#交互项
lm(earn ~ height + sex + height:sex, data = wages)
lm(earn ~ height * sex, data = wages)
lm(earn ~ (height + sex)^2, data = wages)

lm(earn ~ height:sex, data = wages)
lm(earn ~ height:sex:race, data = wages)

mod6 <- lm(earn ~ height + sex + height:sex, data = wages)
coef(mod6)

p2 <- wages %>%
  ggplot(aes(x = height, y = earn, color = sex)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(y = predict(mod6))) +
  scale_y_continuous(limits = c(0, 100000))
p2

library(patchwork)

combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

#虚拟变量
wages %>% mutate(sexmale = if_else(sex == "female", 0, 1))

wages %>% 
  group_by(sex) %>% 
  group_modify(
    ~broom::tidy(lm(earn ~ height,
                    data = .))
  )

wages %>%
  ggplot(aes(x = height, y = earn, color = sex)) +
  geom_smooth(method = lm, se = F)

wages %>%
  ggplot(aes(x = height, y = earn, color = sex)) +
  geom_line(aes(y = predict(mod6)))

mod7 <- lm(earn ~ height + height:sex, data = wages)
coef(mod7)

#回归和相关关系
r <- cor(wages$height, wages$earn)
print(r^2)

#然后看看，身高和收入的线性模型
lm(formula = earn ~ height,
   data = wages) %>% 
  broom::glance() %>% 
  pull(r.squared)
