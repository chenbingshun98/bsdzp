library(dplyr)

df <- data.frame(
  name = c("Alice", "Alice", "Bob", "Bob", "Carol", "Carol"),
  type = c("english", "math", "english", "math", "english", "math")
)

df

score2020 <- c(80.2, 90.5, 92.2, 90.8, 82.5, 84.6)
score2020

df$newscore <- score2020
df

df <- mutate(df, newscore = score2020)
df

df["name"]

df %>% select(name)

df %>% select(name, newscore)

df %>% select(-type)
df %>% select(!type)

df %>% filter(newscore >= 90)
df %>% filter(type == "english",
              newscore >= 90)

#summarise()主要用于统计，
#往往与其他函数配合使用，
#比如计算所有同学考试成绩的均值
df %>% summarise(mean_score = mean(newscore))
df %>% summarise( sd_score = sd(newscore))

df %>% summarise(
  mean_score = mean(newscore),
  median_score = median(newscore),
  n = n(),
  sum = sum(newscore)
)

#事实上，summarise()往往配合group_by()一起使用，
#即，先分组再统计。
#比如，我们想统计每个学生的平均成绩，
#那么就需要先按学生name分组，
#然后求平均
df %>% 
  group_by(name) %>% 
  summarise( 
    mean_score = mean(newscore),
    sd_score = sd(newscore)
  )

df %>% arrange(newscore)
df %>% arrange(-newscore)
df %>% arrange(desc(newscore))

df %>% 
  arrange(type, desc(newscore))

df1 <- df %>% 
  group_by(name) %>% 
  summarise( mean_score = mean(newscore) )

df1

df2 <- tibble(
  name = c("Alice", "Bob"),
  age =  c(12, 13)
)

df2

left_join(df1, df2, by = "name")
df1 %>% left_join(df2, by = "name")

df1 %>% right_join(df2, by = "name")
install.packages(tinytex)
tinytex::install_tinytex()
