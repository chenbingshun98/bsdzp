library(tidyverse)
create_data <- function() {
  df <- tibble(
    ids = 1:100,
    department = rep(c("sociology", "biology", "english", "informatics", "statistics"), 20),
    bases = rep(c(40000, 50000, 60000, 70000, 80000), 20) * runif(100, .9, 1.1),
    experience = floor(runif(100, 0, 10)),
    raises = rep(c(2000, 500, 500, 1700, 500), 20) * runif(100, .9, 1.1)
  )


  df <- df %>% mutate(
    salary = bases + experience * raises
  )
  df
}

library(tidyverse)
library(lme4)
library(modelr)
library(broom)
library(broom.mixed)

df <- create_data()
df

# Model without respect to grouping
m1 <- lm(salary ~ experience, data = df)
m1

broom::tidy(m1)

df %>% modelr::add_predictions(m1)

# Model without respect to grouping
# 固定效应
df %>%
  modelr::add_predictions(m1) %>%
  ggplot(aes(x = experience, y = salary)) +
  geom_point() +
  geom_line(aes(x = experience, y = pred)) +
  labs(x = "Experience", y = "Predicted Salary") +
  ggtitle("linear model Salary Prediction") +
  scale_colour_discrete("Department")

# 37.4 变化的截距
# Model with varying intercept
m2 <- lme4::lmer(salary ~ experience + (1 | department), data = df)
m2

df %>%
  modelr::add_predictions(m2) %>%
  ggplot(aes(
    x = experience, y = salary, group = department,
    colour = department
  )) +
  geom_point() +
  geom_line(aes(x = experience, y = pred)) +
  labs(x = "Experience", y = "Predicted Salary") +
  ggtitle("Varying Intercept Salary Prediction") +
  scale_colour_discrete("Department")

# 37.5 变化的斜率
# Model with varying slope
m3 <- lmer(salary ~ experience + (0 + experience | department), data = df)

#都变
m3.test <- lmer(salary ~ experience + (experience | department), data = df)
m3

broom.mixed::tidy(m3, effects = "fixed")
broom.mixed::tidy(m3.test, effects = "fixed")

broom.mixed::tidy(m3, effects = "ran_vals")
vignette()
df %>%
  modelr::add_predictions(m3) %>%
  ggplot(aes(
    x = experience, y = salary, group = department,
    colour = department
  )) +
  geom_point() +
  geom_line(aes(x = experience, y = pred)) +
  labs(x = "Experience", y = "Predicted Salary") +
  ggtitle("Varying slope Salary Prediction") +
  scale_colour_discrete("Department")

# 37.6 变化的斜率 + 变化的截距
# Model with varying slope and intercept
# 1
m4 <- lmer(salary ~ experience + (1 + experience | department), data = df)
m4

broom.mixed::tidy(m4, effects = "fixed")

broom.mixed::tidy(m4, effects = "ran_vals")

df %>%
  modelr::add_predictions(m4) %>%
  ggplot(aes(
    x = experience, y = salary, group = department,
    colour = department
  )) +
  geom_point() +
  geom_line(aes(x = experience, y = pred)) +
  labs(x = "Experience", y = "Predicted Salary") +
  ggtitle("Varying Intercept and Slopes Salary Prediction") +
  scale_colour_discrete("Department")

# 信息池
# 问题：
# 薪酬制定规则四中，
# 不同的院系起薪不同，
# 年度增长率也不同，#
# 我们得出了5组不同的截距和斜率，
# 那么是不是可以等价为，
# 先按照院系分5组，
# 然后各算各的截距和斜率? 比如
df %>%
  group_by(department) %>%
  group_modify(
    ~ broom::tidy(lm(salary ~ 1 + experience,
      data = .
    ))
  )

# 37.7.2 信息共享
# 完全共享
broom::tidy(m1)
complete_pooling <- m1 %>%
  broom::tidy() %>%
  dplyr::select(term, estimate) %>%
  tidyr::pivot_wider(
    names_from = term,
    values_from = estimate
  ) %>%
  dplyr::rename(
    Intercept = `(Intercept)`,
    slope = experience
  ) %>%
  dplyr::mutate(pooled = "complete_pool") %>%
  dplyr::select(pooled, Intercept, slope)

# 部分共享
fix_effect <- broom.mixed::tidy(m4, effects = "fixed")
fix_effect
fix_effect$estimate[1]
fix_effect$estimate[2]

var_effect <- broom.mixed::tidy(m4, effects = "ran_vals")
var_effect

# random effects plus fixed effect parameters
partial_pooling <- var_effect %>%
  dplyr::select(level, term, estimate) %>%
  tidyr::pivot_wider(
    names_from = term,
    values_from = estimate
  ) %>%
  dplyr::rename(Intercept = `(Intercept)`, estimate = experience) %>%
  dplyr::mutate(
    Intercept = Intercept + fix_effect$estimate[1],
    estimate = estimate + fix_effect$estimate[2]
  ) %>%
  dplyr::mutate(pool = "partial_pool") %>%
  dplyr::select(pool, level, Intercept, estimate)

partial_pooling

partial_pooling <-
  coef(m4)$department %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(level = rowname, Intercept = `(Intercept)`, slope = experience) %>%
  dplyr::mutate(pooled = "partial_pool") %>%
  dplyr::select(pooled, level, Intercept, slope)

partial_pooling

#不共享
no_pool <- df %>%
  dplyr::group_by(department) %>%
  dplyr::group_modify(
    ~ broom::tidy(lm(salary ~ 1 + experience, data = .))
  )
no_pool

un_pooling <- no_pool %>%
  dplyr::select(department, term, estimate) %>%
  tidyr::pivot_wider(
    names_from = term,
    values_from = estimate
  ) %>%
  dplyr::rename(Intercept = `(Intercept)`, slope = experience) %>%
  dplyr::mutate(pooled = "no_pool") %>%
  dplyr::select(pooled, level = department, Intercept, slope)

un_pooling

#可视化
library(ggrepel)

un_pooling %>%
  dplyr::bind_rows(partial_pooling) %>%
  ggplot(aes(x = Intercept, y = slope)) +
  purrr::map(
    c(seq(from = 0.1, to = 0.9, by = 0.1)),
    .f = function(level) {
      stat_ellipse(
        geom = "polygon", type = "norm",
        size = 0, alpha = 1 / 10, fill = "gray10",
        level = level
      )
    }
  ) +
  geom_point(aes(group = pooled, color = pooled)) +
  geom_line(aes(group = level), size = 1 / 4) +
  # geom_point(data = complete_pooling, size = 4, color = "red") +
  geom_text_repel(
    data = un_pooling %>% filter(pooled == "no_pool"),
    aes(label = level)
  ) +
  scale_color_manual(
    name = "信息池",
    values = c(
      "no_pool" = "black",
      "partial_pool" = "red" # ,
      # "complete_pool" = "#A65141"
    ),
    labels = c(
      "no_pool" = "不共享",
      "partial_pool" = "部分共享" # ,
      # "complete_pool" = "完全共享"
    )
  ) #+

un_pooling %>%
  dplyr::bind_rows(partial_pooling) %>%
  filter(pooled == "no_pool")

# 37.8 更多
lmer(salary ~ 1 + (0 + experience | department),
     data = df)

lmer(salary ~ 1 + experience + (0 + experience | department),
     data = df)

lmer(salary ~ 1 + (1 + experience | department),
     data = df)

# vs
lmer(salary ~ 1 + (1 | department) + (0 + experience | department), data = df)
