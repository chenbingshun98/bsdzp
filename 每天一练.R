library(tidyverse)
d <- tibble::tribble(
  ~name, ~chinese, ~math, ~physics, ~english, ~music, ~sport,
  "Alice",      88L,   63L,      98L,      89L,    85L,    72L,
  "Bob",      85L,   75L,      85L,      82L,    73L,    83L,
  "Carlo",      95L,   98L,      75L,      75L,    68L,    84L
)
d
colnames(d)
#现在希望变成：
## # A tibble: 6 x 4
##   discipline Alice   Bob Carlo
##   <chr>      <int> <int> <int>
## 1 chinese       88    85    95
## 2 math          63    75    98
## 3 physics       98    85    75
## 4 english       89    82    75
## 5 music         85    73    68
## 6 sport         72    83    84
tribble(~number, ~letter,   ~greek,
        1,     "a",  "alpha",
        2,     "b",   "beta",
        3,     "c",  "gamma")
tibble(number = c(1, 2, 3), 
       letter = c("a", "b", "c"),
       greek = c("alpha", "beta", "gamma"))
library(broom)
d %>% t() %>% as_tibble()

e <- d %>% as.matrix() %>% t() %>% as.data.frame()
colnames(e)
?colnames
colnames(e) <- NULL
fix(e)

#第二天
#排序，要求按照score从大往小排，但希望all是最下面一行。
d <- 
  tibble::tribble(
    ~name, ~score,
    "a1",     2,
    "a2",     5,
    "a3",     3,
    "a4",     7,
    "a5",     6,
    "all",    23
  )
#想变成
## # A tibble: 6 x 2
##   name  score
##   <chr> <dbl>
## 1 a4        7
## 2 a5        6
## 3 a2        5
## 4 a3        3
## 5 a1        2
## 6 all      23
arrange(d, desc(score))
arrange(d,score)


class(d$name)
d %>% fct_reorder(name,score,levels = c("a4","a5","a2","a3","a1","all"))

d$name <- factor(d$name)
fct_reorder(d$name,
            levels = )
fct <- factor(d[,1],levels = c("a4","a5","a2","a3","a1","all"))
fct
levels(d$name) <- c("a4","a5","a2","a3","a1","all")
levels(d)
view(d)
d$name <- factor(d$name, levels = c("a4","a5","a2","a3","a1","all"))
d$name
d %>% arrange(name)

#day 3
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
  rowwise() %>% 
  mutate(
   num_above_mean = across(is.numeric, ~sum(. > mean(.)))
  ) %>% view()

d %>% 
  summarise(
    across(where(is.numeric),~mean(.))
  )

#计算每个科目的平均值
course_mean <- d %>% 
  summarise(
    across(where(is.numeric), ~mean(.))
  )
course_mean_list <- as.list(course_mean)
class(course_mean_list)
d %>% 
  rowwise() %>% 
  mutate(
    num_above_mean = c_across(is.numeric, ~sum(. > course_mean))
  )

d %>% 
  mutate(
    num_above_mean = across(is.numeric, ~sum(. > mean(.)))
  ) %>% view()

##
d %>% dplyr::mutate(
  across(all_of(names(course_mean)), ~ if_else(.x > course_mean[[cur_column()]], 1, 0))
) %>% rowwise() %>% 
  mutate(
    num_above_mean = sum(c_across(is.numeric))
    )


#day4
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
# 如果corr中都是0 就“none”
# 如果corr中都是1 就“both”
# 如果corr中只有一个1，就输出1对应period
data %>% 
  group_by(id) %>% 
  mutate(
    res_period = case_when(
      if_all(contains("corr") == 0) ~ "none",
      if_all(contains("corr") == 1) ~ "both",
      if_any(contains("corr") == 1) ~ "period"
    )
  )

#day5
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
  geom_text(aes(label = value,
                angle = -90,
 #               hjust = "inward",
                vjust = "inward"))  +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL)


#day6
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
colorspace::swatchplot(c("#F42F5D","#252A4A"))

b_w_tag <- c("Black Students", "White Students")

d %>% 
  ggplot(aes(x = group,
             y = value,
             fill = group))+
  geom_col(aes(x = fct_relevel(group,
                               levels = c("white Students", "Black Students")),
               y = value))+
  facet_wrap(vars(area),
             ncol = 3,
             scales = "free")+
  scale_fill_manual(
    values = c("#F42F5D","#252A4A")
  )+
  scale_x_discrete(labels = c("White\nStudent", "Black\nStudent"))+
  scale_y_continuous(
    limits = c(0,0.4),
    breaks = c(0,0.2, 0.4),
    labels = scales::percent_format()
      )+
  geom_text(aes(label = value %>% 
                  scales::percent(accuracy = 1),
            vjust = -0.25))+
  theme_minimal()+
  labs(title = "Black students are regularly labeled a higher risk for failure than White students",
       subtitle = 'Percentage of student body labeled as "high risk" to not graduate within theirselected major',
       caption = "Sources: Texas A&M, University of Massachusetts Amherst, and University of Wisconsin-Milwaukee",
       x = NULL,
       y = NULL)+
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    legend.position = "none",
    axis.text.x = element_text(
      size = 12,
      color = c("#252A4A","#F42F5D"),
      face = "bold"
    ),
    panel.spacing = unit(2, "inch"),
    strip.text = element_text(
      size = 15,
      color = "#252A4A",
      face = "bold"
    ),
    plot.title = element_text(
      size = 20
    ),
    plot.subtitle = element_text(
      size = 12
    ),
    plot.caption = element_text(hjust = 0,
                                face = "italic"),
    plot.caption.position = "panel"
  )

?font_install
showtextdb::font_install(showtextdb::source_han_serif())

library(ggtext)
library(grid)
library(gridExtra)
d %>%
  mutate(
    across(group, as_factor),
  ) %>%
  ggplot(aes(x = group, y = value, color = group, fill = group)) +
  geom_col(width = 0.4) +
  geom_text(aes(label = scales::label_percent(tscale = 100, accuracy = 1)(value)),
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
    plot.subtitle = element_markdown(),
    plot.caption = element_text(size = 12, color = "grey50", hjust = 0),
    axis.text.y = element_text(size = rel(1.5)),
    axis.text.x = element_text(
      size = rel(1.5),
      face = "bold",
      color = c("#252A4A", "#F42F5D")
    ),
    axis.ticks = element_blank(),
    panel.background = element_rect(color = "white", fill = NA),
    panel.grid.major.y = element_line(
      colour = "gray",
      size = 0.8,
      linetype = "dotted"
    ),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = rel(1)),
    panel.spacing = unit(2, "lines")
  ) +
  labs(
    title = "Black students are regularly labeled a higher risk for failure\nthan White students",
    subtitle = "<span style = 'font-size:13pt; '>Percentage of student body labeled as high risk to not graduate within their <br> selected major</span><br>
                <hr>",
    caption = "Sources: Texas A&M, University of Massachusetts Amherst, and University of Wisconsin–\nMilwaukee",
    x = NULL, y = NULL
  )
<span style = 'color:#F42F5D; '>--</span>
###
txt <- glue::glue("<span style='color:red'>", str_dup("-", 1000), "</span>")

mtcars %>% 
  ggplot(aes(x = mpg, y = cyl)) +
  geom_point() +
  labs(
    title = "I <span style='color:red'> love </span> ggtext.",
    subtitle = txt
  ) +
  theme(
    plot.title = element_markdown(),
    plot.subtitle = element_markdown()
  )
