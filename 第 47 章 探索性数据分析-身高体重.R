library(tidyverse)

setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")

d <- data.table::fread("weight-height.csv")
d

d %>% 
  summarise(
    across(
      everything(), ~sum(is.na(.))
    )
  ) 

#可视化
d %>% 
  ggplot(aes(x = Height, fill = Gender))+
  geom_density(alpha = 0.5)

d %>% 
  ggplot(aes(x = Height, fill = Gender))+
  geom_density(alpha = 0.5)+
  facet_wrap(vars(Gender))

#（子集之间对比，子集与全局对比）

d %>% 
  ggplot(aes(x = Height))+
  geom_density(
    data = d %>% select(-Gender)
  )+
  geom_density()+
  facet_wrap(vars(Gender))

#y轴的调整
d %>%
  ggplot(aes(x = Height, y = after_stat(count))) +
  geom_density(
    data = d %>% select(-Gender)
  ) +
  geom_density() +
  facet_wrap(vars(Gender))

density_colors <- c(
  "Male" = "#247BA0",
  "Female" = "#F25F5C",
  "all people" = "grey85"
)

d %>% 
  ggplot(aes(x = Height,
             y = after_stat(count)))+
  geom_density(
    data = d %>% select(-Gender),
    aes(fill = "all people",
        color = "all people")
  )+
  geom_density(
    aes(color = Gender,
        fill = Gender)
  )+
  facet_wrap(vars(Gender))+
  scale_fill_manual(name = NULL,
                    values = density_colors)+
  scale_color_manual(name = NULL,
                     values = density_colors)+
  theme_minimal()+
  theme(legend.position = "bottom")

#
d %>% 
 ggplot(aes(x = Height,
            y = after_stat(count)))+
  geom_density(
    data = d %>% select(-Gender),
    aes(fill = "all people",
        color = "all people")
  )+
  geom_density(aes(color = Gender,
                   fill = Gender))+
  facet_wrap(vars(Gender))+
  scale_fill_manual(
    name = NULL,
    values = density_colors
  )+
  scale_color_manual(
    name = NULL,
    values = density_colors
  )+
  theme_minimal()+
  theme(legend.position = "bottom")

#或者，用不同的主题风格
density_colors <- c(
  "Male" = "#56B4E9",
  "Female" = "#EF8A17",
  "all participants" = "grey85"
)

d %>%
  ggplot(aes(x = Height, y = after_stat(count))) +
  geom_density(
    data = function(x) dplyr::select(x, -Gender),
    aes(fill = "all participants", color = "all participants")
  ) +
  geom_density(aes(fill = Gender, color = Gender)) +
  facet_wrap(vars(Gender)) +
  scale_color_manual(name = NULL, values = density_colors) +
  scale_fill_manual(name = NULL, values = density_colors) +
  cowplot::theme_minimal_hgrid(16) +
  theme(legend.position = "bottom", legend.justification = "center")

#画出不同性别的体重分布
d %>% 
  ggplot(aes(x = Weight,
             fill = Gender))+
  geom_density(alpha = 0.5)

#建模
d %>%
  ggplot(aes(x = Height, y = Weight, color = Gender)) +
  geom_point()

fit <- lm(Weight ~ 1 + Height,data = d)
summary(fit)

broom::tidy(fit)

#47.4.3 建立不同性别下的身高与体重的线性模型
d %>% 
  group_by(Gender) %>% 
  group_modify(
    ~broom::tidy(
      lm(Weight ~ 1 + Height,
         data = .)
    )
  )

d %>%
  ggplot(aes(x = Height, y = Weight, group = Gender)) +
  geom_point(aes(color = Gender)) +
  geom_smooth(method = lm)
