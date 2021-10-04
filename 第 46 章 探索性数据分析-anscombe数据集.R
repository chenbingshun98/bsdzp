d <- datasets::anscombe
head(d)

library(tidyverse)

dt <- tibble(
  id = c("a", "b"),
  x_1 = 1:2,
  x_2 = 3:4,
  y_1 = 5:6,
  y_2 = 8:9
)
dt

dt %>% 
  pivot_longer(
    -id,
    names_to = "name",
    values_to = "values"
  )

#有时候，我们不想要下划线后面的编号，只想保留前面的第一个字母
dt %>% 
  pivot_longer(
    cols = -id,
    names_to = "name",
    names_pattern = "(.)_.",
    values_to = "values"
  )

#有时候人的需求是多样的，比如不想要前面的第一个字母，只要下划线后面的编号
dt %>% 
  pivot_longer(
    cols = -id,
    names_to = "name",
    names_pattern = "._(.)",
    values_to = "values"
  )

#都想要
dt %>% 
  pivot_longer(
    cols = -id,
    names_to = c("name", "group"),
    names_pattern = "(.)_(.)",
    values_to = "values"
  )

#有时候，我们希望"x", "y"保留在列名，那么匹配出来的第一个字母，就不能给"name"，而是传给特殊的符号".value"，它会收集匹配出来的字符，然后放在列名中

dt %>% 
  pivot_longer(
    cols = -id,
    names_to = c(".value", "group"),
    names_pattern = "(.)_(.)",
    values_to = "values"
  ) %>% 
  knitr::include_graphics("pivot_longer_values.jpg")

?knitr::include_graphics

#46.2.2 回到案例
tidy_d <- d %>% 
  pivot_longer(
    cols = everything(),
    names_to = c(".value",
                 "set"),
    names_pattern = "(.)(.)"
  )

tidy_d

#summary
tidy_d_summary <- tidy_d %>% 
  group_by(set) %>% 
  summarise(
    across(
      .cols = everything(),
      .fns = lst(mean, sd, var),
      .names = "{col}_{fn}"
    )
  )
tidy_d_summary

#modeling
tidy_d %>% 
  group_nest(set) %>% 
  mutate(
    fit = map(data, ~lm(y ~ x, data = .x)),
    tidy = map(fit, broom::tidy),
    glance = map(fit, broom::glance)
  ) %>% 
  unnest(tidy)

tidy_d %>%
  group_by(set) %>%
  group_modify(
    ~ broom::tidy(lm(y ~ x, data = .))
  )

tidy_d %>% 
  group_by(set) %>% 
  summarise(
    broom::tidy(
      lm(y ~ x, data = cur_data())
    )
  )

tidy_d %>%
  ggplot(aes(x = x, y = y, colour = set)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "none") +
  facet_wrap(~set)
