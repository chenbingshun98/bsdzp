windows(7, 3)
prb = replicate(100, { #括号内程序重复100次
  x = sample(c(0, 5, 10), 1, prob = c(0.7, 0.2, 0.1))
  y = sample(c(28, 30, 32, 34), 1, prob = c(0.3, 0.4, 0.2, 0.1))
  plot(0:40, rep(1, 41), type = "n", xlab = "time", ylab = "",
       axes = FALSE)
  axis(1, 0:40)
  r = rnorm(1, 30, 2)
  points(x, 1, pch = 15)
  i = 0
  while (i <= r) {
    i = i + 1
    segments(x, 1, x + i, 1)
    if (x + i >= y)
      points(y, 1, pch = 19)
    Sys.sleep(0.1)
  }
  points(y, 1, pch = 19)
  title(ifelse(x + r <= y, "poor... missed the train!", "Bingo!
catched the train!"))
  Sys.sleep(4)
  x + r > y
})
mean(prb)

library(tidyverse)
tidy_data <- tibble(
  x = replicate(
  100,sample(c(0, 5, 10), 1, prob = c(0.7, 0.2, 0.1))
  ),
  y = replicate(
    100,sample(c(28, 30, 32, 34), 1, prob = c(0.3, 0.4, 0.2, 0.1))
    )
  )
tidy_data
tidy_data <- tidy_data %>% mutate(r=rnorm(100, 30, 2)) %>% 
  mutate(bool=if_else(x+r>y,1,0))

tidy_data %>% pull(bool) %>% mean()
tidy_data %>% 
  summarise(
    across(bool,.names="{.fn}_{.col}",list(平均=mean))
  ) 
tidy_data %>% 
  summarise(
    across(bool,.names="{.fn}_{.col}",~mean(.))
  ) 
?summarise
