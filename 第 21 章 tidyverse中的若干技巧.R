library(tidyverse)
library(patchwork)  # install.packages("patchwork")

df <- tibble(
  name = c("Alice", "Alice", "Bob", "Bob", "Carol", "Carol"),
  type = c("english", "math", "english", "math", "english", "math"),
  score = c(60.2, 90.5, 92.2, 98.8, 82.5, 74.6)
)

df

df %>% count(name)

df %>% 
  group_by(name) %>% 
  summarise(n = n())

df %>% count(name,
             sort = T,
             wt = score,
             name = "total_score")

df %>% group_by(name) %>% 
  summarise(
    n = n(),
    total_score = sum(score, na.rm = T)
  ) %>% 
  arrange(desc(total_score))

#在count()中创建新变量
df %>% count(range = 10*(score %/% 10))

#add_count()
df %>% 
  group_by(name) %>% 
  mutate(n = n()) %>% 
  ungroup()

df %>% add_count(name)

#nth(),first(),last()
v <- c("a", "c", "d", "k")
v[1]

v[length(v)]

c("a", "c", "d", "k") %>% nth(3)

c("a", "c", "d", "k") %>% first()

c("a", "c", "d", "k") %>% last()

df %>% 
  filter(score == first(score))

df %>% 
  group_by(name) %>% 
  filter(score == last(score))

#列变量重新排序
df %>% 
  select(score, everything())

#if_else
df %>% mutate(
  assess = if_else(score > 85, "very_good", "good")
)

#case_when
#多种条件的if_else
df %>% 
  mutate(
    assess = case_when(
      score < 70 ~ "general",
      score >= 70 & score < 80 ~ "good",
      score >= 80 & score < 90 ~ "very_good",
      score >= 90 ~ "best",
      TRUE ~ "other"
    )
  )

#找出前几名
df %>% 
  top_n(2, score)

str_trim(" excess    whitespace in a string be gone!")

# Use str_squish() to remove any leading, trailing, or excess whitespace
str_squish(" excess    whitespace in a string be gone!")

#取反操作
3:10 %in% c(1:5)

#有时候需要一个不属于的操作符
`%nin%` <- Negate(`%in%`)
3:10 %nin% c(1:5)

# 使用purrr::negate()自定义反向操作符
`%nin%` <- purrr::negate(`%in%`)
3:10 %nin% c(1:5)

#drop_na()
dt <- tribble(
  ~x, ~y,
  1, NA,
  2, NA,
  NA, -3,
  NA, -4,
  5, -5
)

dt

dt %>% drop_na()

#replace_na()
dt <- tribble(
  ~x, ~y,
  1, NA,
  2, NA,
  NA, -3,
  NA, -4,
  5, -5
)

dt %>% mutate(x = replace_na(x, 0))

dt %>% mutate(
  x = replace_na(x, mean(x, na.rm = T))
)

dt <- tribble(
  ~x, ~y,
  1, NA,
  2, NA,
  NA, -3,
  NA, -4,
  5, -5
)

dt %>% mutate(
  z = coalesce(x, 0)
  # z = coalesce(x, y)
)

dt <- tribble(
  ~name, ~age,
  "a", 1,
  "b", 2,
  "c", NA,
  "d", 2
)


dt %>%
  mutate(
    age_adj = ifelse(is.na(age), mean(age, na.rm = TRUE), age)
  )

#summarise()生成List-column
library(gapminder)
gapminder %>% 
  group_by(continent) %>% 
  summarise(
    avg_gdpPercap = mean(gdpPercap)
  )

gapminder %>% 
  group_by(continent) %>% 
  summarise(test = list(t.test(gdpPercap))) %>% 
  mutate(tided = map(test, broom::tidy)) %>% 
  unnest(tided) %>% 
  ggplot(aes(estimate, continent))+
  geom_point()+
  geom_errorbarh(aes(
    xmin = conf.low,
    xmax = conf.high
  ))

gapminder %>% 
  group_by(continent) %>% 
  summarise(test = list(lm(lifeExp~gdpPercap))) %>% 
  mutate(tidied = map(test,
                     broom::tidy,
                     conf.int = T)) %>% 
  unnest(tidied) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(estimate, continent))+
  geom_point()+
  geom_errorbarh(aes(
    xmin = conf.low,
    xmax = conf.high,
    height = .3
    ))

#下面两种方法一样完成上面的工作
gapminder %>% 
  group_nest(continent) %>% 
  mutate(test = map(data,
                    ~t.test(.x$gdpPercap))) %>% 
  mutate(tidied = map(test, broom::tidy)) %>% 
  unnest(tidied)

gapminder %>% 
  group_by(continent) %>% 
  group_modify(
    ~broom::tidy(t.test(.x$gdpPercap))
  )

#count()+fct_reorder()+geom_col()+coord_flip()
gapminder %>% 
  distinct(continent, country) %>% 
  count(continent) %>% 
  ggplot(aes(x = continent, y = n))+
  geom_col()

gapminder %>% 
  distinct(continent, country) %>% 
  count(continent) %>% 
  ggplot(aes(x = fct_reorder(continent, n), 
             y = n))+
  geom_col()+
  coord_flip()

gapminder %>% 
  distinct(continent, country) %>% 
  count(continent) %>% 
  mutate(coll = if_else(continent == "Asia",
                        "red","gray")) %>% 
  ggplot(aes(x = fct_reorder(continent, n),
             y = n))+
  geom_text(aes(label = n), hjust = -0.25)+
  geom_col(width = 0.8, aes(fill = coll))+
  coord_flip()+
  theme_classic()+
  scale_fill_manual(values = c("#b3b3b3a0","#D55E00"))+
  theme(legend.position = "none",
        axis.text = element_text(size = 11))+
  labs(title = "我的标题")

gapminder %>% 
  distinct(continent, country) %>% 
  count(continent) %>% 
  ggplot(aes(x = fct_reorder(continent, n),
             y = n))+
  geom_text(aes(label = n),
            hjust = -0.25)+
  geom_col(width = 0.8,
           aes(fill = continent == "Asia"))+
  coord_flip()+
  theme_classic()+
  scale_fill_manual(values = c("#b3b3b3a0", "#D55E00"))+
  annotate("text",
           x = 3.8,
           y = 48,
           label = "this is important\ncase",
           color = "#D55E00",
           size = 5)+
  annotate(
    geom = "curve",
    x = 4.1,
    y = 48,
    xend = 4.1,
    yend = 35,
    curvature = .3,
    arrow = arrow(length = unit(2, "mm"))
    )+
      theme(
        legend.position = "none",
        axis.text = element_text(size = 11)
      )+
  labs(title = "我的标题", x = "")

#fct_lump
tb <- tibble::tribble(
  ~disease,  ~n,
  "鼻塞", 112,
  "流涕", 130,
  "发热",  89,
  "腹泻",   5,
  "呕吐",  12,
  "咳嗽", 102,
  "咽痛",  98,
  "乏力",  15,
  "腹痛",   2,
  "妄想",   3,
  "幻听",   6,
  "失眠",   1,
  "贫血",   8,
  "多动",   2,
  "胸痛",   4,
  "胸闷",   5
)

p1 <- tb %>% 
  uncount(n) %>% 
  
  ggplot(aes(x = disease, fill = disease)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "none")



p2 <- tb %>% 
  uncount(n) %>% 
  mutate(
    disease = forcats::fct_lump(disease, 5),
    disease = forcats::fct_reorder(disease, .x = disease, .fun = length)
  ) %>% 
  ggplot(aes(x = disease, fill = disease)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "none")

p1 + p2

#fct_reorder2
dat_wide <- tibble(
  x = 1:3,
  top = c(4.5, 4, 5.5),
  middle = c(4, 4.75, 5),
  bottom = c(3.5, 3.75, 4.5)
)


dat_wide %>%
  pivot_longer(
    cols = c(top, middle, bottom),
    names_to = "region",
    values_to = "awfulness")

dat <- dat_wide %>%
  pivot_longer(
    cols = c(top, middle, bottom),
    names_to = "region",
    values_to = "awfulness") %>%
  mutate(
    region_ABCD = factor(region),
    region_sane = fct_reorder2(region, x, awfulness)
  )

p_ABCD <- ggplot(dat, aes(x, awfulness, colour = region_ABCD)) +
  geom_line() + theme(legend.justification = c(1, 0.85))

p_sane <- ggplot(dat, aes(x, awfulness, colour = region_sane)) +
  geom_line() + theme(legend.justification = c(1, 0.85))

p_ABCD + p_sane +
  plot_annotation(
    title = 'Make the legend order = data order, with forcats::fct_reorder2()')

#unite
dfa <- tribble(
  ~school, ~class,
  "chuansi", "01",
  "chuansi", "02",
  "shude", "07",
  "shude", "08",
  "huapulu", "101",
  "huapulu", "103"
)

dfa

df_united <- dfa %>% 
  tidyr::unite(school, class, col = "school_plus_class", sep = "_", remove = FALSE)

df_united

#另一种方法
dfa %>% mutate(newcol = str_c(school, "_", class))

#separate()
df_united %>%
  tidyr::separate(school_plus_class, into = c("sch", "cls"), sep = "_", remove = F)

df_united %>% 
  mutate(sch = str_split(school_plus_class,
                         "_" %>% map_chr(1))) %>% 
  mutate(cls = str_split(school_plus_class,
                         "_") %>% map_chr(2))

# 如果每行不是都恰好分隔成两部分呢？
#就需要tidyr::extract(), 
#使用方法和tidyr::separate()类似
dfb <- tribble(
  ~school_class,
  "chuansi_01",
  "chuansi_02_03",
  "shude_07_0",
  "shude_08_0",
  "huapulu_101_u",
  "huapulu_103__p"
)
dfb

dfb %>% separate(school_class,
                 into = c("sch", "cls"),
                 sep = "_",
                 extra = "drop",
                 remove = F)

#extract()
dfc <- tibble(x = c("1-12week", "1-10wk", "5-12w", "01-05weeks"))
dfc

dfc %>% tidyr::extract(
  x,
  c("start", "end", "letter"), "(\\d+)-(\\d+)([a-z]+)",
  remove = FALSE
)

#crossing()
crossing(x = c("F", "M"),
         y = c("a", "b"),
         z = c(1:2))

# cross(x = c("F", "M"),
#          y = c("a", "b"),
#          z = c(1:2))不行

crossing(trials = 1:10, m =1:5) %>% 
  group_by(trials) %>% 
  mutate(
    guess = sample.int(5, n()),
    result = m == guess
  ) %>% 
  summarise(score = sum(result) / n())

#再来一个例子
sim <- tribble(
  ~f, ~params,
  "rbinom", list(size = 1, prob = 0.5, n = 10)
)
sim %>%
  mutate(sim = invoke_map(f, params))

rep_sim <- sim %>% 
  crossing(rep = 1:1e5) %>% 
  mutate(sim = invoke_map(f, params)) %>% 
  unnest(sim) %>% 
  group_by(rep) %>% 
  summarise(mean_sim = mean(sim))
head(rep_sim)

rep_sim %>% 
  ggplot(aes(x = mean_sim))+
  geom_histogram(binwidth = 0.05,
                 fill = "skyblue")+
  theme_classic()

#中央极限定理
sim <- tribble(
  ~n_tosses, ~f, ~params,
  10, "rbinom", list(size = 1, prob = 0.5, n = 15),
  30, "rbinom", list(size = 1, prob = 0.5, n = 30),
  100, "rbinom", list(size = 1, prob = 0.5, n = 100),
  1000, "rbinom", list(size = 1, prob = 0.5, n = 1000),
  10000, "rbinom", list(size = 1, prob = 0.5, n = 1e4)
)
sim_rep <- sim %>%
  crossing(replication = 1:50) %>%
  mutate(sims = invoke_map(f, params)) %>%
  unnest(sims) %>%
  group_by(replication, n_tosses) %>%
  summarise(avg = mean(sims))

sim_rep %>%
  ggplot(aes(x = factor(n_tosses), y = avg)) +
  ggbeeswarm::geom_quasirandom(color = "lightgrey") +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(
    yintercept = 0.5,
    color = "skyblue", lty = 1, size = 1, alpha = 3 / 4
  ) +
  ggthemes::theme_pander() +
  labs(
    title = "50 Replicates Of Mean 'Heads' As Number Of Tosses Increase",
    y = "mean",
    x = "Number Of Tosses"
  )
