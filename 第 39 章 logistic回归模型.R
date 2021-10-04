library(tidyverse)
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
gredata <- data.table::fread("gredata.csv")
gredata

model_logit <- glm(admit ~ gre,
                   data = gredata,
                   family = binomial(link = "logit"))
summary(model_logit)

coef(model_logit)[2]

#39.2 模型的输出
#模型给出系数（0.003582）事实上就是log-odds度量方式的结果
logit_log_odds <- model_logit %>% broom::augment(.,
                               data = gredata,
                               type.predict = c('link')) %>% 
  rename(log_odds = .fitted)

library(latex2exp)
logit_log_odds %>% 
  ggplot(aes(x = gre, y = log_odds)) +
  geom_path(color = "#771C6D", size = 2) +
  labs(title = TeX("Log odds $(\\beta)$"), #"Log odds (β)"
       subtitle = "This is linear!",
       x = NULL,
       y = TeX("$log  \\frac{p}{1 - p}$")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(angle = 90)
  )

logit_odds <- broom::augment_columns(
  model_logit,
  data = gredata,
  type.predict = c("link")
) %>%
  rename(log_odds = .fitted) %>%
  mutate(odds_ratio = exp(log_odds))

logit_odds %>% 
  ggplot(aes(x = gre, y = odds_ratio)) +
  geom_line(color = "#FB9E07", size = 2) +
  labs(title = TeX("Odds ($e^{\\beta }$)"),  #"Odds (exp(β))"
       subtitle = "This is curvy, but it's a mathy transformation of a linear value",
       x = NULL,
       y = TeX("$\\frac{p}{1 - p}$")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(angle = 90)
  )

#39.2.3 The probability scale.
logit_probs <- broom::augment_columns(
  model_logit,
  data = gredata,
  type.predict = c("response")
) %>% 
  rename(pred_prob = .fitted)

logit_probs %>% 
  ggplot(aes(x = gre, y = pred_prob)) +
  #geom_point(aes(x = gre, y = admit)) +
  geom_line(color = "#CF4446", size = 2) +
  labs(title = "Predicted probabilities", 
       sutitle = "Plug values of X into ",
       x = "X (value of explanatory variable)",
       y = TeX("\\hat{P(Y)} ")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


latex2exp::latex2exp_examples()

#39.3 预测
gredata %>% 
  mutate(
    pred = predict(model_logit),
    fiitted = fitted(model_logit)
  )

gredata %>% 
  mutate(
    pred = predict(model_logit),
    fitted = fitted(model_logit),
    pred2 = exp(pred) / (1 + exp(pred))
  )

gredata %>% 
  mutate(
    pred = predict(model_logit, type = "response"),
    fitted = fitted(model_logit)
  )


#39.3.2 预测
newdata <- tibble(
  gre = c(550, 660, 700, 780)
)

newdata

#前面讲到predict()中type参数有若干选项type = c("link", "response", "terms"),

#type = "link"，预测的是log_odds，实际上就是coef(model_logit)[1] + gre * coef(model_logit)[2]

newdata %>% 
  mutate(
    pred_log_odds = predict(model_logit,
                            newdata = newdata,
                            type = "link"),
    pred_log_odds2 = coef(model_logit)[1]+gre*coef(model_logit)[2]
  )

#type = "response""，预测的是probabilities， 实际上就是exp(pred_log_odds) / (1 + exp(pred_log_odds) )
newdata %>% 
  mutate(
    pred_log_odds = predict(model_logit,
                            newdata = newdata,
                            type = "link")
  ) %>% 
  mutate(
    pred_prob = predict(model_logit,
                        newdata = newdata,
                        type = "response"),
    pred_prob2 = exp(pred_log_odds) / (1 + exp(pred_log_odds))
    )

#type = "terms"，返回一个矩阵，具体计算为：模型的设计矩阵中心化以后，与模型返回的系数相乘而得到的新矩阵
predict(model_logit, gredata, type = 'terms') %>% 
  as_tibble() %>% 
  head()

x <- model.matrix(admit ~ gre, data = gredata)

x %>% 
  as.data.frame() %>% 
  mutate(
    across(everything(), ~.x - mean(.x))
  ) %>% 
  transmute(
    term = coef(model_logit)[2]*gre
  ) %>% 
  head()
