library(modeldata)

data(ames)

dim(ames)

library(tidymodels)

tidymodels_prefer()

ames %>% 
  ggplot(aes(x = Sale_Price))+
  geom_histogram(bins = 50) 

ames %>% 
  ggplot(aes(x = Sale_Price))+
  geom_histogram(bins = 50) +
  scale_x_log10()

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))


set.seed(123)

ames_split <- initial_split(ames, prop = 0.80)

ames_split

ames_train <- training(ames_split)
ames_test <- testing(ames_split)
dim(ames_train)

linear_reg() %>% set_engine('lm') %>% translate()



linear_reg(penalty = 1) %>% set_engine("glmnet") %>% translate()

lm_model <- linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- lm_model %>% fit(Sale_Price~Longitude+Latitude,data=ames_train)

lm_xy_fit <- lm_model %>% 
  fit_xy(
    x=ames_train %>% select(Longitude,Latitude),
    y=ames_train %>% pull(Sale_Price)
  )
lm_form_fit %>% tidy()

lm_xy_fit %>% tidy()

# install.packages(httr)
library(httr)
cookie = ""
headers = c('Accept' = 'application/json',
            'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.117 Safari/537.36',
            'Referer' = 'http://study.163.com/courses',
            'edu-script-token' = '1c1f84a1b85a48aba8a4d440552f5f69',
            'Connection' = 'keep-alive',
            'Cookie' = cookie)
# 构造参数信息
payload = list('pageIndex' = 1, 'pageSize' = 50, 'relativeOffset' = 0,'frontCatgoryId' = '-1')

# 二次请求的实际url
url = "http://study.163.com/p/search/studycourse.json"
# POST方法执行单词请求
result = POST(url, add_headers(.headers = headers), body = payload, encode = "json")
result
##  Response [http://study.163.com/p/search/studycourse.json]
##    Date: 2018-08-01 06:52
##    Status: 200
##    Content-Type: application/json;charset=UTF-8
##    Size: 84.6 kB