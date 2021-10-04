library(tidyverse)

setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
df <- read_rds("fish.rds")
df

df %>%
  ggplot(aes(x = pollution_level, y = number_of_fish)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    title = "Number of fish counted under different pollution level",
    x = "Pollution level",
    y = "Number of fish counted"
  )

m0 <- lm(number_of_fish ~ pollution_level, data = df)
summary(m0)

df %>% 
  ggplot(aes(x = number_of_fish))+
  geom_histogram()+
  labs(
    title = "number of fishes(Poisson distribution)"
  )

generate_pois <- function(lambda_value){
  tibble(
    lambda = as.character(lambda_value),
    x = seq(1, 10),
    d = dpois(x = x, lambda = lambda_value)
  )
}

tb <- seq(0.1, 1.8, by = 0.2) %>% 
  map_dfr(generate_pois)
tb

tb %>% 
  ggplot(aes(x = x, y = d,
             color = lambda))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  theme_bw()

#38.2.3 正态分布换成泊松分布就行了？
# 代码实现
m <- glm(number_of_fish ~ pollution_level,
         family = poisson(link = "log"),
         data = df
)
m
summary(m)

confint(m)

broom::tidy(m)

#系数
coef(m)

exp(coef(m)[2])

exp(coef(m))

#marginal
margins::margins(m, type = "link")

margins::marginal_effects(m, type = "response", se = TRUE) %>%
  as.data.frame() %>%
  dplyr::mutate(pollution_level = df$pollution_level) %>%
  ggplot(aes(x = pollution_level, y = dydx_pollution_level)) +
  geom_point()

#拟合
fitted(m) %>% 
  head()

intercept <- coef(m)[1]

beta <- coef(m)[2]

df %>% 
  dplyr::mutate(theory_pred = fitted(m)) %>% 
  dplyr::mutate(
    myguess_pred = exp(intercept + beta * pollution_level)
  )

df %>%
  dplyr::mutate(theory_pred = fitted(m)) %>%
  ggplot(aes(x = pollution_level, y = theory_pred)) +
  geom_point()

pred <- predict(m, type = "response", se = TRUE) %>% as.data.frame()
pred %>% 
  head()

df_pred <- df %>%
  dplyr::mutate(
    fit = pred$fit,
    se_fit = pred$se.fit
  )
df_pred

real_df <-
  tibble(
    x = seq(0, 1, length.out = 100),
    y = 4 * exp(-3.2 * x)
  )

real_df <-
  tibble(
    x = seq(0, 1, length.out = 100),
    y = 4 * exp(-3.2 * x)
  )

df_pred %>%
  ggplot(aes(x = pollution_level, y = number_of_fish)) +
  geom_point() +
  geom_pointrange(aes(
    y = fit,
    ymax = fit + se_fit,
    ymin = fit - se_fit
  ), color = "red") +
  # geom_point(aes(y = fit + se_fit), color = "red") +
  # geom_point(aes(y = fit - se_fit), color = "red") +
  geom_line(data = real_df, aes(x = x, y = y), color = "black") +
  labs(
    title = "Number of fish counted under different pollution level",
    x = "Pollution level",
    y = "Number of fish counted"
  )

#

margins::margins(m)

ggeffects::ggpredict(m)

ggeffects::ggpredict(m, terms = c("pollution_level"))

performance::model_performance(m)

performance::check_model(m)

performance::check_overdispersion(m)

performance::check_zeroinflation(m)

#思考
#这两者为什么是相等的？
d <- tibble(
  x = 1:100,
  y = 4 + 2 * x + rnorm(100)
)

lm(y ~ x, data = d)

glm(y~x,
    family = gaussian(link = "identity"),
    data = d)

#log link 与 log transforming
set.seed(10)
df %>% glm(number_of_fish ~ pollution_level,
    family = gaussian(link = "log"),
    data = .)

# lm(log(number_of_fish) ~ pollution_level, data = df)
glm(log(number_of_fish) ~ pollution_level,
    family = gaussian(link = "identity"),
    data = df
)

#更多分布
x <- c(1, 2, 3, 4, 5)
y <- c(1, 2, 4, 2, 6)

regNId <- glm(y ~ x, family = gaussian(link = "identity"))
regNlog <- glm(y ~ x, family = gaussian(link = "log"))
regPId <- glm(y ~ x, family = poisson(link = "identity"))
regPlog <- glm(y ~ x, family = poisson(link = "log"))
regGId <- glm(y ~ x, family = Gamma(link = "identity"))
regGlog <- glm(y ~ x, family = Gamma(link = "log"))
regIGId <- glm(y ~ x, family = inverse.gaussian(link = "identity"))
regIGlog <- glm(y ~ x, family = inverse.gaussian(link = "log"))

dx <- tibble(
  x = c(1, 2, 3, 4, 5),
  y = c(1, 2, 4, 2, 6)
)

dx %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

regNId <- glm(y ~ x,
              family = gaussian(link = "identity"),
              data = dx)
regNId

dx %>% 
  mutate(pred = predict(regNId,
                        type = "response")) %>% 
  ggplot(aes(x,y))+
  geom_point()+
  geom_line(aes(y = pred, group = 1))

regPlog <- glm(y ~ x,
               family = poisson(link = "log"),
               data = dx)
regPlog

dx %>% 
  mutate(pred = predict(regPlog, 
                        type = "response")) %>% 
  ggplot(aes(x,y))+
  geom_point()+
  geom_line(aes(y = pred, group = 1))

regGId <- glm(y ~ x,
              family = Gamma(link = "identity"),
              data = dx)
regGId

dx %>% 
  mutate(pred = predict(regGId,
                        type = "response")) %>% 
  ggplot(aes(x,y))+
  geom_point()+
  geom_line(aes(y = pred, group = 1))

#更复杂的模型
glm(number_of_fish ~ 1 + (1 | pollution_level),
    family = poisson(link = "log"),
    data = df
)
knitr::include_graphics(path = "images/One_Picture.png")
