library(tidyverse)
library(knitr)
library(kableExtra)

setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
# edata <- read_csv("Experim.csv",
#                   locale = locale(encoding = "UTF-8"))

edata <- read.csv("Experim.csv") %>% 
  mutate(group = if_else(group == "maths skills", 1, 2)) %>% 
  mutate(
    across(c(sex, id, group), as.factor)
  ) %>% 
  as_tibble()

edata

glimpse(edata)

#T检验
# Run t-test
model_1_t <- t.test(edata$depress1, mu = 0)
model_1_t#输出结果显示，p-value很小接近0，拒绝零假设，也就说均值不大可能为0。事实上，通过密度图可以看到depress1的分布均值在42附近，与0相距很远。

edata %>% 
  ggplot(aes(x = depress1))+
  geom_density()

t.test(depress1 ~ 1,
       data = edata)

# Run equivalent linear model
model_1_lm <- lm(depress1 ~ 1, data = edata)
summary(model_1_lm)

library(broom)

# tidy() gets model outputs we usually use to report our results
model_1_t_tidy <- tidy(model_1_t) %>% mutate(model = "t.test(y)")
model_1_lm_tidy <- tidy(model_1_lm) %>% mutate(model = "lm(y ~ 1)")

results <- bind_rows(model_1_t_tidy, model_1_lm_tidy) %>%
  select(model, estimate, statistic, p.value)
results#事实上，在R语言t.test()内部会直接调用lm()函数，其函数语句和我们的这里代码也是一样的。

#绝大部分时候，我们想考察实验中的干预是否有效，
#换句话说，基线得分 depress1 和 干预后得分 depress3 是否存在显著差异？
#这就需要进行配对样本t检验。
# run paired t-test testing depression from g1 against g2
model_2_t <- t.test(edata$depress1, edata$depress3, paired = TRUE)
model_2_t_tidy <- tidy(model_2_t) %>% mutate(model = "t.test(x, y, paired = TRUE")
model_2_t

# 写成线性模型为lm(depress1 - depress3 ~ 1)，
#即先让两个变量相减，然后去做回归
# run linear model
model_2_lm <- lm(depress1 - depress3 ~ 1, data = edata)
model_2_lm_tidy <- tidy(model_2_lm) %>% mutate(model = "lm(y-x ~ 1)")
summary(model_2_lm)

# we combine the two model outputs, rowwise
results <- bind_rows(model_2_t_tidy, model_2_lm_tidy) %>%
  select(model, estimate, statistic, p.value)
results

# run paired t-test testing depression from g1 against g2
model_2_t2 <- t.test(edata$depress1- edata$depress3)
model_2_t2_tidy <- tidy(model_2_t2) %>% mutate(model = "t.test(x - y")
model_2_t2

# calculate the difference between baseline and tp3 depression
edata <- edata %>%
  mutate(
    dep_slope = depress1 - depress3
  )

model_2_lm2 <- lm(dep_slope ~ 1, data = edata)
model_2_lm2_tidy <- tidy(model_2_lm2) %>% mutate(model = "lm(delta ~ 1)")

# we combine the three model outputs, rowwise
results <- bind_rows(model_2_t_tidy, model_2_t2_tidy) %>% 
  bind_rows(model_2_lm_tidy) %>%
  bind_rows(model_2_lm2_tidy) %>%
  select(model, estimate, statistic, p.value)
results

#关联
# Run correlation test
model_3_cor <- cor.test(edata$depress3, edata$depress1, method = "pearson")
model_3_cor_tidy <- tidy(model_3_cor) %>% mutate(model = "cor.test(x, y)")
model_3_cor

# Run equivalent linear model
model_3_lm <- lm(depress3 ~ depress1, data = edata)
model_3_lm_tidy <- tidy(model_3_lm) %>% mutate(model = "lm(y ~ x)")
summary(model_3_lm)

# we combine the two model outputs, rowwise
results <- bind_rows(model_3_cor_tidy, model_3_lm_tidy) %>%
  select(model, term, estimate, statistic, p.value)
results#那是因为，线性模型会不仅输出系数，而且还输出了模型的截距，因此两个模型的系数会不一样，但在 t-statistic 和 p.value 是一样的。

#单因素方差分析
# Run one-way anova
model_4_anova <- aov(depress1 ~ group, data = edata)
model_4_anova_tidy <- tidy(model_4_anova) %>% mutate(model = "aov(y ~ factor(x))")
summary(model_4_anova)

# Run equivalent linear model
model_4_lm <- lm(depress1 ~ group, data = edata)
model_4_lm_tidy <- tidy(model_4_lm) %>% mutate(model = "lm(y ~ factor(x))")
summary(model_4_lm)

# we combine the two model outputs, rowwise
results <- bind_rows(model_4_anova_tidy, model_4_lm_tidy) %>%
  select(model, term, estimate, statistic, p.value)
results

# take the square root of the anova stat
sqrt(model_4_anova_tidy$statistic[1])

# same as stat from lm
model_4_lm_tidy$statistic[2]

# or, square the lm stat
model_4_lm_tidy$statistic[2]^2

# same as anova stat
model_4_anova_tidy$statistic[1]

#单因素协变量分析
model_5_ancova <- aov(dep_slope ~ group + confid1,
                      data = edata)
model_5_ancova_tidy <- tidy(model_5_ancova) %>% 
  mutate(model = "aov(y ~ x + z)")
summary(model_5_ancova)

# Run equivalent linear model
model_5_lm <- lm(dep_slope ~ group + confid1, data = edata)
model_5_lm_tidy <- tidy(model_5_lm) %>% mutate(model = "lm(y ~ x + z)")
summary(model_5_lm)

# we combine the two model outputs, rowwise
results <- bind_rows(model_5_ancova_tidy, model_5_lm_tidy) %>%
  select(model, term, estimate, statistic, p.value)
results

# or, square the lm stat
model_5_lm_tidy$statistic[-1]^2

# same as anova stat
model_5_ancova_tidy$statistic

#双因素方差分析
model_6_anova <- aov(dep_slope ~ group * sex,
                     data = edata)
model_6_anova_tidy <- tidy(model_6_anova) %>% 
  mutate(model = "aov(y ~ x * z)")

summary(model_6_anova)

# Run equivalent linear model
model_6_lm <- lm(dep_slope ~ group * sex, data = edata)
model_6_lm_tidy <- tidy(model_6_lm) %>% mutate(model = "lm(y ~ x * z)")
summary(model_6_lm)

# we combine the two model outputs, rowwise
results <- bind_rows(model_6_anova_tidy, model_6_lm_tidy) %>%
  select(model, term, estimate, statistic, p.value)
results

#最后，为了更好地演示aov与lm之间的等价性，
#给group弄成一个有多个水平（>2）的因子, 
#具体过程如下
edata_mock <- edata %>%
  # Add 2 to numeric version of groups
  mutate(group = as.numeric(group) + 2) %>%
  # bind this by row to the origincal eData (with group as numeric)
  bind_rows(edata %>%
              mutate(group = as.numeric(group))) %>%
  # make group a factor again so the correct test is applied
  mutate(group = as.factor(group))
