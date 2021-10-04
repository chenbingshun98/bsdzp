library(tidyverse)
d <- ggplot2movies::movies
d

movies_genre_sample <- d %>% 
  select(title, year, rating, Action, Romance) %>% 
  filter(!(Action == 1 & Romance == 1)) %>% 
  mutate(genre = case_when(
    Action == 1 ~ "Action",
    Romance == 1 ~ "Romance",
    TRUE ~ "Neither"
  )) %>% 
  filter(genre != "Neither") %>% 
  select(-Action, -Romance) %>% 
  group_by(genre) %>% 
  slice_sample(n = 34) %>% 
  ungroup()

movies_genre_sample

movies_genre_sample %>% 
  ggplot(aes(x = genre, y = rating))+
  geom_boxplot()+
  geom_jitter()

#看下两种题材电影评分的分布
movies_genre_sample %>% 
  ggplot(aes(x = rating))+
  geom_histogram(binwidth = 1,
                 color = "white")+
  facet_grid(vars(genre))

#统计两种题材电影评分的均值
summary_rating <- movies_genre_sample %>% 
  group_by(genre) %>% 
  summarise(
    mean = mean(rating),
    std_dev = sd(rating),
    n = n()
  )
summary_rating

#统计两种题材电影评分的均值
t_test_eq <- t.test(rating ~ genre,
                    data = movies_genre_sample,
                    var.equal = TRUE
) %>%
  broom::tidy()
t_test_eq

t_test_uneq <- t.test(rating ~ genre,
                      data = movies_genre_sample,
                      var.equal = FALSE
) %>%
  broom::tidy()
t_test_uneq

# infer:基于模拟的检验
library(infer)

obs_diff <- movies_genre_sample %>%
  specify(formula = rating ~ genre) %>%
  calculate(
    stat = "diff in means",
    order = c("Romance", "Action")
  )
obs_diff

#模拟
null_dist <- movies_genre_sample %>%
  specify(formula = rating ~ genre) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000, type = "permute") %>% 
  calculate(
    stat = "diff in means",
    order = c("Romance", "Action")
  )
head(null_dist)

#可视化
null_dist %>% 
  visualize()

null_dist %>% 
  visualize() +
  shade_p_value(
    obs_stat = obs_diff,
    direction = "both"
  )

#计算p值
pvalue <- null_dist %>% 
  get_pvalue(
    obs_stat = obs_diff,
    direction = "two_sided"
  )
pvalue

# 案例2: 航天事业的预算有党派门户之见？
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
gss <- read_rds("gss.rds")

gss %>%
  select(NASA, party) %>%
  count(NASA, party) %>%
  head(8)

gss %>% 
  ggplot(aes(x = party, fill = NASA))+
  geom_bar()

chisq.test(gss$party, gss$NASA)

gss %>%
  chisq_test(NASA ~ party) %>%
  dplyr::select(p_value) %>%
  dplyr::pull()

#infer:Simulation-based tests
obs_stat <- gss %>% 
  specify(NASA ~ party) %>% 
  calculate(stat = "Chisq")

obs_stat

null_dist <- gss %>% 
  specify(NASA ~ party) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 5000,
           type = "permute") %>% 
  calculate(stat = "Chisq")

null_dist

null_dist %>%
  visualize() +
  shade_p_value(obs_stat = obs_stat, method = "both", direction = "right")

null_dist %>%
  get_pvalue(obs_stat = obs_stat, direction = "greater")

# 案例3:原住民中的女学生多？
td <- MASS::quine %>%
  as_tibble() %>%
  mutate(
    across(c(Sex, Eth), as_factor)
  )
td

#从民族背景有两组（A， N）来看，性别为 F 的占比 是否有区别？
td %>% count(Eth, Sex)

# 传统方法
prop.test(table(td$Eth, td$Sex), correct = FALSE)

#基于模拟的方法
obs_diff <- td %>%
  specify(Sex ~ Eth, success = "F") %>% # #被解释变量 sex中F的占比
  calculate(
    stat = "diff in props",
    order = c("A", "N") # 解释变量中两组A，N
  )

obs_diff

null_distribution <- td %>%
  specify(Sex ~ Eth, success = "F") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("A", "N"))

null_distribution %>%
  visualize()

pvalue <- null_distribution %>%
  get_pvalue(obs_stat = obs_diff, direction = "both")

pvalue

null_distribution %>%
  get_ci(level = 0.95, type = "percentile")
