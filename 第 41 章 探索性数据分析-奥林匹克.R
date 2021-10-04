library(tidyverse)
library(readxl)
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
d <- read_excel("olympics.xlsx")
d

d %>%
  ggplot() +
  geom_point(aes(x = Olympic_year, y = Men_score), color = "blue") +
  geom_point(aes(x = Olympic_year, y = Women_score), color = "red")

d1 <- d %>%
  pivot_longer(
    cols = -Olympic_year,
    names_to = "sex",
    values_to = "winning_time"
  )

d1

d1 %>%
  ggplot(aes(x = Olympic_year, y = winning_time, color = sex)) +
  geom_point() +
  # geom_smooth(method = "lm") +
  scale_color_manual(
    values = c("Men_score" = "blue", "Women_score" = "red")
  ) +
  scale_x_continuous(
    breaks = seq(1900, 2004, by = 4),
    labels = seq(1900, 2004, by = 4)
  ) +
  theme(axis.text.x = element_text(
    size = 10, angle = 45, colour = "black",
    vjust = 1, hjust = 1
  ))

fit_1 <- lm(Men_score ~ 1 + Olympic_year, data = d)

summary(fit_1)

fit_2 <- lm(Women_score ~ 1 + Olympic_year, data = d)

summary(fit_2)



#41.4 预测
df <- data.frame(Olympic_year = 2020)
df
?predict
predict(fit_1, newdata = df)


# 为了图片中的一致，
# 我们使用1900年到2252年(seq(1900, 2252, by = 4))建立预测项，
# 并整理到数据框里
grid <- tibble(
  Olympic_year = as.numeric(seq(1900, 2252, by = 4))
)
grid

tb <- grid %>%
  mutate(
    Predict_Men = predict(fit_1, newdata = grid),
    Predict_Women = predict(fit_2, newdata = grid)
  )
tb

# 有时候我喜欢用modelr::add_predictions()函数实现相同的功能
library(modelr)
grid %>%
  add_predictions(fit_1, var = "Predict_Men") %>%
  add_predictions(fit_2, var = "Predict_Women")

# 再次可视化
tb1 <- tb %>%
  pivot_longer(
    cols = -Olympic_year,
    names_to = "sex",
    values_to = "winning_time"
  )
tb1

tb1 %>%
  ggplot(aes(
    x = Olympic_year,
    y = winning_time,
    color = sex
  )) +
  geom_line(size = 2) +
  geom_point(data = d1) +
  scale_color_manual(
    name = "标记",#标签的名字
    values = c(
      "Men_score" = "blue",
      "Women_score" = "red",
      "Predict_Men" = "#588B8B",
      "Predict_Women" = "#C8553D"
    ),
    labels = c(
      "Men_score" = "男性历史成绩",
      "Women_score" = "女性历史成绩",
      "Predict_Men" = "男性预测成绩",
      "Predict_Women" = "女性预测成绩"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1900, 2252, by = 16),
    labels = as.character(seq(1900, 2252, by = 16))
  ) +
  theme(axis.text.x = element_text(
    size = 10, angle = 45, colour = "black",
   vjust = 1, hjust = 1
  ))

library(modelr)
d1 <- d %>%
  pivot_longer(
    cols = -Olympic_year,
    names_to = "sex",
    values_to = "winning_time"
  )

fit_model <- function(df) lm(winning_time ~ Olympic_year, data = df)

d2 <- d1 %>%
  group_nest(sex) %>%
  mutate(
    mod = map(data, fit_model)
  )
d2

# d2 %>% mutate(p = list(grid, grid))
# d3 <- d2 %>% mutate(p = list(grid, grid))
# d3
# d3 %>%
#   mutate(
#     predictions = map2(p, mod, add_predictions),
#   )

# or
tb4 <- d2 %>%
  mutate(
    predictions = map(mod, ~ add_predictions(grid, .))
  ) %>%
  select(sex, predictions) %>%
  unnest(predictions)

tb4 %>%
  ggplot(aes(
    x = Olympic_year,
    y = pred,
    group = sex,
    color = sex
  )) +
  geom_point() +
  geom_line(size = 2) +
  geom_point(
    data = d1,
    aes(
      x = Olympic_year,
      y = winning_time,
      group = sex,
      color = sex
    )
  ) +
  scale_x_continuous(
    breaks = seq(1900, 2252, by = 16),
    labels = as.character(seq(1900, 2252, by = 16))
  ) +
  theme(axis.text.x = element_text(
    size = 10, angle = 45, colour = "black",
    vjust = 1, hjust = 1
  ))
