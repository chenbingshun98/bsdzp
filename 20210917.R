library(tidyverse)
library(lubridate)
library(tidymodels)
library(prophet)
library(rvest)

html <- read_html("https://github.com/andrew-couch/Tidy-Tuesday/blob/master/Season%202/Data/example_retail_sales.csv")

df <- html %>% 
  html_element(xpath = '//*[@id="repo-content-pjax-container"]/div/div[3]/div[2]/div[2]/table') %>% 
  html_table()
df <- 
  df %>% 
  select(-X1) %>% 
  rename(ds = X2, y = X3) %>% 
  tail(-1)

df$y <- df$y %>% as.numeric()  

cleaned_df <- df %>% 
  mutate(ds = as.Date(ds, "%m/%d/%Y"))


cleaned_df %>% 
  ggplot(aes(x = ds, y = y)) + 
  geom_line() + 
  geom_smooth(method = "lm")


wrong_split <- initial_split(cleaned_df)
bind_rows(
  training(wrong_split) %>% mutate(type = "train"),
  testing(wrong_split) %>% mutate(type = "test")
) %>% 
  ggplot(aes(x = ds, y= y, color = type, group = NA)) + 
  geom_line()


correct_split <- initial_time_split(cleaned_df %>% arrange(ds))
bind_rows(
  training(correct_split) %>% mutate(type = "train"),
  testing(correct_split) %>% mutate(type = "test")
) %>% 
  ggplot(aes(x = ds, y= y, color = type, group = NA)) + 
  geom_line()


m <- prophet(df = cleaned_df %>% arrange(ds))
future <- make_future_dataframe(m, periods = 6, freq = "month", include_history = TRUE)
forecast <- predict(m, future)
prophet_plot_components(m, forecast)
plot(m, forecast) + 
  add_changepoints_to_plot(m)

rolling_origin(cleaned_df %>% arrange(ds), initial = 52, assess = 6) 
sliding_period(cleaned_df %>% arrange(ds), ds, period = "year", lookback = Inf, assess_stop = 1) %>% 
  mutate(train_data = map(splits, analysis),
         test_data = map(splits, assessment)) %>% 
  select(-splits) %>% 
  pivot_longer(-id) %>% 
  filter(id %in% c("Slice01", "Slice02", "Slice03")) %>% 
  unnest(value) %>% 
  ggplot(aes(x = ds, y = y, color = name, group = NA)) + 
  geom_line() + 
  facet_wrap(~id, scales = "fixed")
