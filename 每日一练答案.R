library(tidyverse)

d <- tibble::tribble(
  ~name, ~chinese, ~math, ~physics, ~english, ~music, ~sport,
  "Alice", 88L, 63L, 98L, 89L, 85L, 72L,
  "Bob", 85L, 75L, 85L, 82L, 73L, 83L,
  "Carlo", 95L, 98L, 75L, 75L, 68L, 84L
)
d

d %>%
  pivot_longer(
    cols = -name,
    names_to = "discipline",
    values_to = "score"
  ) %>%
  pivot_wider(
    names_from = name,
    values_from = score
  )

## day02

# 排序，要求按照score从大往小排，但希望all是最下面一行。
d <-
  tibble::tribble(
    ~name, ~score,
    "a1", 2,
    "a2", 5,
    "a3", 3,
    "a4", 7,
    "a5", 6,
    "all", 23
  )

d %>%
  arrange(desc(score)) %>%
  arrange(name %in% c("all"))

d %>%
  arrange(score) %>%
  arrange(name %in% c("all"))

## day03

# 统计每位同学，成绩高于各科均值的个数，
d <- tibble::tribble(
  ~name, ~chinese, ~engish, ~physics, ~sport, ~music,
  "Aice", 85, 56, 56, 54, 78,
  "Bob", 75, 78, 77, 56, 69,
  "Cake", 69, 41, 88, 89, 59,
  "Dave", 90, 66, 74, 82, 60,
  "Eve", 68, 85, 75, 69, 21,
  "Fod", 77, 74, 62, 74, 88,
  "Gimme", 56, 88, 75, 69, 34
)
d

d %>%
  mutate(
    across(-name, list(RC = ~ . > mean(.)))
  ) %>%
  rowwise() %>%
  mutate(
    num_above_mean = sum(c_across(ends_with("RC")))
  ) %>%
  ungroup() %>%
  select(-ends_with("RC"))

## day04
data <- tribble(
  ~id, ~corr, ~period,
  1, 0, "a",
  1, 0, "b",
  2, 0, "a",
  2, 1, "b",
  3, 1, "a",
  3, 0, "b",
  4, 1, "a",
  4, 1, "b"
)
data

# 先按id分组，
# 
# - 如果corr中都是0  就"none"
# - 如果corr中都是1  就"both"
# - 如果corr中只有一个1，就输出1对应period
my_function <- function(corr,period){
  sum <- sum(corr)
  
  if(sum==0){
    res <- "none"
  }
  
  if(sum==2){
    res <- "both"
  }
  
  if(sum==1){
    res <- period[corr=1]
  }
  
  return(res)
}

data %>% 
  group_by(id) %>% 
  summarize(resp_period=my_function(corr,period))

### day05

# 图中柱子上的字体没有显示完整，请改进。

d <- tibble::tribble(
  ~name, ~value,
  "Alice",    2.12,
  "Bob",   68.45,
  "Carlie",   15.84,
  "Dave",    7.38,
  "Eve",    0.56
)
d %>% 
  ggplot(aes(x = value, y = fct_reorder(name, value)) ) +
  geom_col(width = 0.6, fill = "gray60") +
  geom_text(aes(label = value, hjust =1))  +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL)

d %>% 
  ggplot(aes(x = value, y = fct_reorder(name, value)) ) +
  geom_col(width = 0.6, fill = "gray60") +
  geom_text(aes(label = value, hjust = ifelse(value > 50, 1, -.1)) ) +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL)

### day06
d <- tibble::tribble(
  ~area,           ~group, ~value,
  "Texas A&M", "white Students",   0.03,
  "Texas A&M", "Black Students",   0.07,
  "Umass Amherst", "white Students",   0.07,
  "Umass Amherst", "Black Students",   0.23,
  "UW-Milwaukee", "white Students",   0.13,
  "UW-Milwaukee", "Black Students",   0.31
)
d

## a stupid way
text_subtitle <- glue::glue("<span style = 'font-size:13pt; '>Percentage of student body labeled as high risk to not graduate within their <br> selected major</span><br>",
                            "<span style = 'color:#F42F5D; '>", 
                            str_dup("-", 100), 
                            "</span>"
)
library(ggtext)
d %>% 
  mutate(
    across(group, as_factor),
  ) %>% 
  ggplot(aes(x = group, y = value, color = group, fill = group)) +
  geom_col(width = 0.4) +
  geom_text(aes(label = scales::label_percent(scale = 100, accuracy = 1)(value)), 
            vjust = -1, 
            size = rel(6),
            fontface = "bold"
  ) +
  facet_wrap(vars(area), ncol = 3, scales = "free_y") +
  scale_x_discrete(
    labels = function(x) str_replace(x, " ", "\n"),
    expand = expansion(mult = .8)
  ) +
  scale_y_continuous(
    limits = c(0, 0.46),
    breaks = c(0, 0.2, 0.4),
    labels = scales::label_percent(scale = 100, accuracy = 1),
    expand = expansion(mult = 0)
  ) +
  scale_fill_manual(
    values = c("white Students" = "#252A4A", "Black Students" = "#F42F5D"),
    aesthetics = c("colour", "fill")
  ) +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = rel(2)),
    plot.subtitle = element_markdown(size = 11 ),
    plot.caption = element_text(size = 12, color = "grey50", hjust = 0),
    axis.text.y = element_text(size = rel(1.5)),
    axis.text.x = element_text(size = rel(1.5),
                               face = "bold", 
                               color = c("#252A4A", "#F42F5D")#,
                               #margin = margin(t = -5, unit = "pt")
    ),
    axis.ticks = element_blank(),
    panel.background = element_rect(color = "white", fill = NA),
    panel.grid.major.y = element_line(colour = "gray", 
                                      size = 0.8, 
                                      linetype = "dotted"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = rel(1)),
    panel.spacing = unit(2, "lines")
  ) +
  labs(
    title = "Black students are regularly labeled a higher risk for failure\nthan White students",
    subtitle = text_subtitle,
    caption = "Sources: Texas A&M, University of Massachusetts Amherst, and University of Wisconsin–\nMilwaukee",
    x = NULL, y = NULL) 

### day07

# 告诉你一个你可能不知道的事情，`summarise()`一定要输出数据框吗？
iris %>%
  nest_by(Species) %>%
  rowwise() %>%
  summarise(
    write_csv(data, glue::glue("{Species}.csv"))
  )

## day08

# 运行以下两个代码，结果和你期望的一样？为什么？

mtcars %>%
  group_by(cyl) %>%
  summarise(
    broom::tidy(lm(mpg ~ wt, data = .))
  )
mtcars %>%
  group_by(cyl) %>%
  summarise(
    broom::tidy(lm(mpg ~ wt))
  )

## day09
# 缺失值替换，数值型的缺失值用0替换，字符串型的用""

df <- tibble(
  x = c(NA, 1, 2),
  y = c("a", NA, NA),
)

df %>% 
  mutate(
    across(where(is.numeric),coalesce,0),
    across(where(is.character),coalesce,"")
  )

#day10
#六年级的年级主任让学生提交自己所在的班级号，看到结果后，他很苦恼，你能帮忙他规整下？
d <- tibble::tribble(
  ~id,
  "2",
  "03",
  "小学2015级2班",
  "小学2015级3班",
  "0601",
  "0602",
  "201502",
  "201604",
  "6.10",
  "6.11",
  "6.5",
  "6.8",
  "06"
)
d

parse_class_id <- function(x){
  res <- NA_character_
  
  if(str_length(x)<3){
    res <- x
  }
  
  if(str_detect(x,"班$")){
    res <- str_extract(x,"\\d+(?=班)")
  }
  
  if(str_detect(x,"\\.")){
    res <- str_extract(x,"(?<=\\.)\\d+")
  }
  
  if(str_detect(x,"\\d{6}$")){
    res <- str_extract(x,"(?<=\\.)\\d+")
  }
  
  if(stringr::str_detect(x,"\\d{4}$")){
    res <- str_extract(x,"\\d{2}$")
  }
  
  res <- stringr::str_pad(res, width = 2, side = "left", pad = "0")
  return(res)
}

d %>% mutate(x=map_chr(id,parse_class_id))
d %>% 
  mutate(x=map_chr(id,is.character))
