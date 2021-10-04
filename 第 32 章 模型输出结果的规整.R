library(tidyverse)
library(gapminder)

#可视化探索
gapminder %>% 
  ggplot(aes(x = log(gdpPercap), y = lifeExp))+
  geom_point(alpha = 0.2)

library(colorspace)

# model_colors <- c("darkorange", "purple", "cyan4")
model_colors <- colorspace::qualitative_hcl(4,
                                            palette = "dark 2")

ggplot(data = gapminder,
       mapping = aes(x = log(gdpPercap),
                     y = lifeExp))+
  geom_point(alpha = 0.2)+
  geom_smooth(
    method = "lm",
    aes(color = "OLS",
        fill = "OLS")#要加冒号
  )+
  geom_smooth(
    method = "lm",
    formula = y ~splines::bs(x, df = 3),
    aes(color = "Cubic Spline",
        fill = "Cubic Spline")
  )+
  geom_smooth(
    method = "loess",
    aes(color = "LOESS",
        fill = "LOESS")
  )+
  scale_color_manual(name = "Models",
                     values = model_colors)+
  scale_fill_manual(name = "Models",
                    values = model_colors)+
  theme(legend.position = "top")

#简单模型
out <- lm(
  formula = lifeExp ~ gdpPercap + pop + continent,
  data = gapminder
)
out

str(out)

summary(out)#模型的输出结果是一个复杂的list

library(broom)
tidy(out)

out %>% 
  tidy() %>% 
  ggplot(mapping = aes(
    x = term,
    y = estimate
  ))+
  geom_point()+
  coord_flip()

#可以很方便的获取系数的置信区间
out %>% 
  tidy(conf.int = T)

out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(!term %in% c("Intercept")) %>% 
  ggplot(aes(
    x = reorder(term, estimate),
    y = estimate,
    ymin = conf.low,
    ymax = conf.high
  ))+
  geom_pointrange()+
  coord_flip()+
  labs(
    x = "",
    y = "OLS Estimate"
  )

#augment
#augment()会返回一个数据框，这个数据框是在原始数据框的基础上，增加了模型的拟合值（.fitted）, 拟合值的标准误（.se.fit）, 残差（.resid）等列。
augment(out)

out %>%
  augment() %>%
  ggplot(mapping = aes(x = lifeExp, y = .fitted)) +
  geom_point()

#glance() 函数也会返回数据框，但这个数据框只有一行，内容实际上是summary()输出结果的最底下一行。
glance(out)

penguins <- palmerpenguins::penguins %>% 
  drop_na()

penguins %>% 
  group_nest(species) %>% 
  mutate(model = map(data,
                     ~lm(bill_depth_mm ~ bill_length_mm,
                         data = .))) %>% 
  mutate(glance = map(model,
                      ~glance(.))) %>% 
  unnest(glance)


fit_ols <- function(df){
  lm(body_mass_g ~ bill_depth_mm + bill_length_mm,
     data = df)
}

out_tidy <- penguins %>% 
  group_nest(species) %>% 
  mutate(model = map(data,
                     fit_ols)) %>% 
  mutate(tidy = map(model,
                    ~tidy(.))) %>% 
  unnest(tidy) %>% 
  filter(!term %in% "(Intercept)")

out_tidy

out_tidy %>% 
  ggplot(aes(
    x = species,
    y = estimate,
    ymin = estimate - 2*std.error,
    ymax = estimate + 2*std.error,
    color = term
  ))+
  geom_pointrange(position = position_dodge(width = 0.25))+
  theme(legend.position = "top")+
  labs(
    x = NULL,
    y = "Estimate",
    color = "系数"
  )

#练习
df <- tibble(
  x = runif(30, 2, 10),
  y = -2*x + rnorm(30, 0, 5)
)
df

#用broom::augment()和ggplot2做出类似的残差图
fit <- lm(
  formula = y ~ x,
  data = df
)
summary(fit)

augment(fit)
fit %>% 
  augment() %>% 
  ggplot(mapping = aes(x = x,
                       y = y))+
  geom_smooth(method = "lm",
              se = F)+  
  geom_pointrange(position = position_dodge(width = 0.25),
                                        shape = abs(.resid),
                                        ymin = ,
                                        ymax = ,
                                        color = "yellow")

fit %>% 
  augment() %>% 
  ggplot(mapping = aes(x = x,
                       y = y))+
  geom_smooth(method = "lm",
              se = F,
              color = "lightgrey")+
  geom_point(aes(y = y, 
                 color = "yellow",
                 size = abs(.resid),
                 alpha = abs(.resid)))+
  geom_segment(aes(xend = x, yend = .fitted), alpha = .2)+
  scale_color_manual(values = "#F5DA51")+
  theme_classic()+
  scale_x_continuous()+
  scale_y_continuous(breaks = c(-20, -10, 0))+
  theme(legend.position = "none")
