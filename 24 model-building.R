library(tidyverse)
library(nycflights13)
library(lubridate)
library(modelr)
# 24.3 What affects the number of daily flights?
daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(
    n = n()
  )
daily  

ggplot(daily, aes(date, n))+
  geom_line()

# 24.3.1 Day of week
daily <- daily %>% 
  mutate(wday = wday(date, label = T))
ggplot(daily, aes(wday, n))+
  geom_boxplot()

mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n))+
  geom_boxplot()+
  geom_point(data = grid,
             color = 'red',
             size = 4)

#可视化残差
daily <- daily %>% 
  add_residuals(mod)

daily %>% 
  ggplot(aes(date, resid))+
  geom_ref_line(h = 0)+
  geom_line()

#Drawing a plot with one line for each day of the week makes the cause easier to see:
ggplot(daily, aes(date, resid, color = wday))+
  geom_ref_line(h = 0)+
  geom_line()

daily %>% 
  filter(resid < -100)

daily %>% 
  ggplot(aes(date, resid))+
  geom_ref_line(h = 0)+
  geom_line(color = 'grey50')+
  geom_smooth(se = F,
              span = 0.2)
# 24.3.2 Seasonal Saturday effect
daily %>% 
  filter(wday == '周六') %>% 
  ggplot(aes(date, n))+
  geom_point()+
  geom_line()+
  scale_x_date(NULL,
               date_breaks = '1 month',
               date_labels = '%b')

#Lets create a “term” variable that roughly captures the three school terms, and check our work with a plot:
term <- function(date){
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825,20140101),
      labels = c('spring', 'summer', 'fall')
      )
}

daily <- daily %>% 
  mutate(term = term(date))

daily %>% 
  filter(wday == '周六') %>% 
  ggplot(aes(date, n, 
             color = term))+
  geom_point(alpha = 1/3)+
  geom_line()+
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1,
                   with_term = mod2) %>% 
  ggplot(aes(date, resid, color = model))+
  geom_line(alpha = 0.75)

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, 'n')

ggplot(daily, aes(wday, n))+
  geom_boxplot()+
  geom_point(data = grid,
             color = 'red')+
  facet_wrap(~term)

#It’s now much easier to see the long-term trend, and the positive and negative outliers.
mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>% 
  add_residuals(mod3, "resid") %>% 
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()

#24.3.3 Computed variables
compute_vars <- function(data){
  data %>% 
    mutate(
      term = term(date),
      wday = wday(date, label = T)
    )
}

wday2 <- function(x) wday(x, label = TRUE)
mod3 <- lm(n ~ wday2(date) * term(date), data = daily)

# 24.3.4 Time of year: an alternative approach
library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>% 
  data_grid(wday, date = seq_range(date, n = 13)) %>% 
  add_predictions(mod) %>% 
  ggplot(aes(date, pred, colour = wday)) + 
  geom_line() +
  geom_point()
