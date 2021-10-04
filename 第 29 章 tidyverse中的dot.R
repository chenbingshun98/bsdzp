library(tidyverse)
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
read.csv("./wages.csv") %>%
  mutate(letter = str_extract(race, "(?<=h)(.)")) %>%
  select(., -letter) %>%
  mutate_at(vars(race), ~ as.factor(.)) %>%
  mutate_at(vars(sex), ~ if_else(. == "male", 1, 0)) %>%
  filter_if(~ is.numeric(.), all_vars(. != 0)) %>%
  split(.$sex) %>%
  map(~ lm(earn ~ ., data = .)) %>%
  map_dfr(~ broom::tidy(.), .id = "sex")

#占位符
#y %>% f(x, .) is equivalent to f(x, y)
#z %>% f(x, y, arg = .) is equivalent to f(x, y, arg = z)

mtcars %>% 
  select(cyl, disp, hp) %>% 
  head(2)

#lambda函数
mtcars %>% 
  select_at(vars(contains("ar")),
            ~toupper(.)) %>% 
  head(3)

#有时候程序员会将~toupper(.)简写成 toupper
mtcars %>% 
  select_at(vars(contains("ar")),
            toupper) %>% 
  head(3)

# 正则表达式
words <- "the fattest cat."

words %>% str_replace_all("t.", "-")

words %>% str_replace_all("t\\.", "-")

#Unary funciton (只带一个参数的函数)
mean_rm <- . %>% 
  mean(na.rm = T)

c(1, 2, 3, NA) %>% mean_rm()

# is equivalent to
c(1, 2, 3, NA) %>% mean(., na.rm = T)

#more placeholder
iris %>% subset(1:nrow(.) %% 30 == 0)

1:10 %>% {
  c(min(.), max(.))
}

#当mutate遇到map
iris %>% 
  head(3) %>% 
  mutate(., r_sum = pmap_dbl(select_if(., is.numeric), sum))
#这里mutate()行，有两个., 实际这两个.都是等待iris %>% head(3)传来的data.frame


df <- tibble(
  mean = c(1, 2),
  sd = c(2, 4)
)
df

df %>% 
  mutate(., rand = map(mean, ~rnorm(5, .))) %>% 
  unnest_wider(rand)

#第一个.， 是df
#第二个.， 是df中的mean


df %>% 
  mutate(rand = map2(mean, sd, ~rnorm(5, .x, .y))) %>% 
  unnest_wider(rand)
#mean传给.x
#sd传给.y

df <- tribble(
  ~a, ~b,
  1, 10,
  2, 11
)

df %>% 
  mutate(., sum = pmap_dbl(., ~sum(...)))

#dot dot dot 
commas <- function(...){
  stringr::str_c(..., collapse = ", ")
}

commas(letters[1:10])
