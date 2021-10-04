library(tidyverse)
library(lubridate)

setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
nobel_winners <- data.table::fread("nobel_winners.csv")
nobel_winners <- read_csv("https://github.com/perlatex/R_for_Data_Science/blob/master/demo_data/nobel_winners.csv") %>% read_table()
nobel_winners %>%
  map_df(~ sum(is.na(.)))

# 性别缺失怎么造成的？
nobel_winners %>%
  count(laureate_type)

# 42.5 我们想探索哪些问题？
# 你想关心哪些问题，可能是
#
# 每个学科颁过多少次奖？
# 这些大神都是哪个年代的人？
# 性别比例
# 平均年龄和获奖数量
# 最年轻的诺奖获得者是谁？
# 中国诺奖获得者有哪些？
# 得奖的时候多大年龄？
# 获奖者所在国家的经济情况？
# 有大神多次获得诺贝尔奖，而且在不同科学领域获奖？
# 出生地分布？工作地分布？迁移模式？
# GDP经济与诺奖模型

# 42.6 每个学科颁过多少次奖
nobel_winners %>%
  count(category)

nobel_winners %>%
  count(category) %>%
  ggplot(aes(
    x = category,
    y = n,
    fill = category
  )) +
  geom_col() +
  geom_text(aes(label = n),
    vjust = -0.25
  ) +
  labs(
    title = "不同学科诺贝尔奖次数对比",
    x = "学科",
    y = "数量"
  ) +
  theme(legend.position = "none")

nobel_winners %>%
  ggplot() +
  geom_bar(aes(
    x = category,
    fill = category
  )) +
  geom_text(aes(label = count),
    vjust = -0.25
  ) +
  labs(
    title = "不同学科诺贝尔奖次数对比",
    x = "学科",
    y = "数量"
  ) +
  theme(legend.position = "none")

#
nobel_winners %>%
  count(category) %>%
  ggplot(aes(
    x = fct_reorder(category, n),
    y = n,
    fill = category
  )) +
  geom_col() +
  geom_text(aes(label = n),
    vjust = -0.25
  ) +
  labs(title = "不同学科诺贝奖获奖次数对比", x = "学科", y = "数量") +
  theme(legend.position = "none")

library(ggthemr)
# devtools::install_github('cttobin/ggthemr')
ggthemr("dust")

nobel_winners %>%
  count(category) %>%
  ggplot(aes(
    x = fct_reorder(category, n),
    y = n,
    fill = category
  )) +
  geom_col() +
  labs(title = "不同学科诺贝奖获奖次数对比", x = "学科", y = "数量") +
  theme(legend.position = "none")

nobel_winners %>%
  count(category) %>%
  ggplot(aes(x = fct_reorder(category, n), y = n)) +
  geom_col(fill = c("#003f5c", "#444e86", "#955196", "#dd5182", "#ff6e54", "#ffa600")) +
  labs(title = "不同学科诺贝奖获奖次数对比", x = "学科", y = "数量") +
  theme(legend.position = "none")

library(gganimate) # install.packages("gganimate", dependencies = T)

nobel_winners %>%
  count(category) %>%
  mutate(category = fct_reorder(category, n)) %>%
  ggplot(aes(x = category, y = n)) +
  geom_text(aes(label = n), vjust = -0.25) +
  geom_col(fill = c("#003f5c", "#444e86", "#955196", "#dd5182", "#ff6e54", "#ffa600")) +
  labs(title = "不同学科诺贝奖获奖次数对比", x = "学科", y = "数量") +
  theme(legend.position = "none") +
  transition_states(category) +
  shadow_mark(past = TRUE)

# 中国
nobel_winners %>%
  dplyr::filter(birth_country == "China") %>%
  dplyr::select(full_name, prize_year, category)

# 我们发现获奖者有多个地址，就会有重复的情况，比如 Charles Kuen Kao在2009年Physics有两次，为什么重复计数了呢？

# 下面我们去重吧， 去重可以用distinct()函数

dt <- tibble::tribble(
  ~x, ~y, ~z,
  1, 1, "a",
  1, 1, "b",
  1, 2, "c",
  1, 2, "d"
)

dt

dt %>%
  distinct_at(vars(x),
    .keep_all = T
  )
dt %>%
  distinct_at(vars(x, y),
              .keep_all = T)

nobel_winners <- nobel_winners %>% 
  mutate_if(is.character, tolower) %>% 
  distinct_at(vars(full_name, prize_year, category),
              .keep_all = T) %>% 
  mutate(
    decade = 10 * (prize_year %/% 10),
    prize_age = prize_year - year(birth_date)
  )

#再看
nobel_winners %>%
  dplyr::filter(birth_country == "china") %>%
  dplyr::select(full_name, prize_year, category)

#42.8 哪些大神多次获得诺贝尔奖
nobel_winners %>% count(full_name, sort = T)

nobel_winners %>% 
  group_by(full_name) %>% 
  mutate(
    number_price = n(),
    number_cateory = n_distinct(category)
  ) %>% 
  arrange(desc(number_price), full_name) %>% 
  filter(number_price == 2)

#42.9 大神在得奖的时候是多大年龄？
nobel_winners %>% 
  count(prize_age) %>% 
  ggplot(aes(x = prize_age,
             y = n))+
  geom_col()

nobel_winners %>%
  group_by(category) %>%
  summarise(mean_prize_age = mean(prize_age, na.rm = T))

nobel_winners %>%
  mutate(category = fct_reorder(category, prize_age, median, na.rm = TRUE)) %>%
  ggplot(aes(category, prize_age)) +
  geom_point() +
  geom_boxplot() +
  coord_flip()

nobel_winners %>% 
  filter(!is.na(prize_age)) %>% 
  group_by(decade, category) %>% 
  summarise(
    average_age = mean(prize_age),
    median_age = median(prize_age)
  ) %>% 
  ggplot(aes(decade, average_age, color = category))+
  geom_line()
#
library(ggridges)

nobel_winners %>%
  ggplot(aes(
    x = prize_age,
    y = category,
    fill = category
  )) +
  geom_density_ridges()

nobel_winners %>% 
  ggplot(aes(x = prize_age,
             fill = category,
             color = category))+
  geom_density()+
  facet_wrap(vars(category))+
  theme(legend.position = 'none')

nobel_winners %>% 
  group_split(category) %>% 
  map(
    ~ggplot(data = .x,
            aes(x = prize_age))+
      geom_density()+
      ggtitle(.x$category)
  )

#也可以用强大的group_by() + group_map()组合，我们会在第 22 章讲到
nobel_winners %>% 
  group_by(category) %>% 
  group_map(
    ~ggplot(data = .x,
            aes(x = prize_age))+
      geom_density()+
      ggtitle(.y)
  )

#42.10 性别比例
nobel_winners %>% 
  filter(laureate_type == "individual") %>% 
  count(category, gender) %>% 
  group_by(category) %>% 
  mutate(prop = n / sum(n))

#各年代性别比例
nobel_winners %>% 
  filter(laureate_type == "individual") %>% 
  # mutate(decade = glue::glue("{round(prize_year - 1, -1)}s")) %>%
  count(decade, category, gender) %>% 
  group_by(decade, category) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(decade, category,
             fill = prop))+
  geom_tile(size = 0.7)+
  # geom_text(aes(label = scales::percent(prop, accuracy = .01))) +
  geom_text(aes(label = scales::number(prop, accuracy = .01)))+
  facet_grid(vars(gender))+
  scale_fill_gradient(low = "#FDF4E9", high = "#834C0D")

library(ggbeeswarm) # install.packages("ggbeeswarm")

nobel_winners %>%
  ggplot(aes(
    x = category,
    y = prize_age,
    colour = gender,
    alpha = gender
  )) +
  ggbeeswarm::geom_beeswarm() +
  coord_flip() +
  scale_color_manual(values = c("#BB1288", "#5867A6")) +
  scale_alpha_manual(values = c(1, .4)) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(
    title = "诺奖获得者性别不平衡",
    subtitle = "1901年-2016年数据",
    colour = "Gender",
    alpha = "Gender",
    x = "学科",
    y = "获奖年龄"
  )
reprex::reprex()

#
nobel_winners %>%
  count(decade,
        category,
        gender = coalesce(gender, laureate_type)
  ) %>%
  group_by(decade, category) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(decade, n, fill = gender)) +
  geom_col() +
  facet_wrap(~category) +
  labs(
    x = "Decade",
    y = "# of nobel prize winners",
    fill = "Gender",
    title = "Nobel Prize gender distribution over time"
  )

#这些大神都是哪个年代出生的人？
nobel_winners %>%
  select(category, birth_date) %>%
  mutate(year = floor(year(birth_date) / 10) * 10) %>%
  count(category, year) %>%
  dplyr::filter(!is.na(year)) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1810, 1990, 20)) +
  geom_text(aes(label = n), vjust = -0.25) +
  facet_wrap(vars(category))

# 最年轻的诺奖获得者？
nobel_winners %>% 
  filter(prize_age == min(prize_age, na.rm = T))

nobel_winners %>% 
  filter(
    rank(prize_year - year(birth_date)) == 1
  )

nobel_winners %>% 
  arrange(
    prize_year - year(birth_date)
  )

nobel_winners %>% 
  top_n(1, year(birth_date) - prize_year)

# 平均年龄和获奖数量
df1 <- nobel_winners %>% 
  group_by(category) %>% 
  summarise(
    mean_prise_age = mean(prize_age, na.rm = T),
    total_num = n()
  )
df1

df1 %>% 
  ggplot(aes(mean_prise_age, total_num))+
  geom_point(aes(color = category))+
  geom_smooth(method = lm, se = FALSE)

#出生地与工作地分布
nobel_winners_clean <- nobel_winners %>% 
  mutate_at(
    vars(birth_country, death_country),
    ~if_else(str_detect(., "\\("),
             str_extract(., "(?<=\\().*?(?=\\))"), .)
  ) %>% 
  mutate_at(
    vars(birth_country, death_country),
    ~case_when(
      . == "scotland" ~ "united kingdom",
      . == "northern ireland" ~ "united kingdom",
      str_detect(., "czech") ~ "czechia",
      str_detect(., "germany") ~ "germany",
      TRUE ~ .
    )
  ) %>% 
  select(full_name, prize_year, category, birth_date, birth_country, gender, organization_name, organization_country, death_country)

nobel_winners_clean %>% count(death_country, sort = TRUE)

nobel_winners_clean %>% 
  mutate(
    color = case_when(
      death_country == "united states of america" ~ "#FF2B4F",
      death_country == "germany" ~ "#fcab27",
      death_country == "united kingdom" ~ "#3686d3",
      death_country == "france" ~ "#88398a",
      death_country == "switzerland" ~ "#20d4bc",
      TRUE ~ "gray60"
    )
  ) %>% 
  ggplot(aes(
    x = 0,
    y = fct_rev(factor(birth_country)),
    xend = death_country,
    yend = 1,
    color = color,
    alpha = (color != 'gray60')
  ))+
  geom_curve(
    curvature = -0.5,
    arrow = arrow(length = unit(0.01, "npc"))
  )+
  scale_x_discrete() +
  scale_y_discrete() +
  scale_color_identity() +
  scale_alpha_manual(values = c(0.1, 0.2), guide = F) +
  scale_size_manual(values = c(0.1, 0.4), guide = F) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#F0EFF1", colour = "#F0EFF1"),
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )
