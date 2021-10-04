p_load(stargazer)
p_load(magrittr)
x1 <- rnorm(1000,0,1)
x2 <- rnorm(1000,0,1)
x3 <- rnorm(1000,0,1)
x4 <- rnorm(1000,0,1)
x5 <- rnorm(1000,0,1)
x6 <- rnorm(1000,0,1)

data <- cbind(x1, x2, x3, x4, x5, x6) %>%
  as.data.frame()
data

mod_list_test <- lapply(data[, 1:4], function(x) lm(x ~ data$x5 + data$x6))
mod_list_test
dep_vars_test <- c("A","B","C","D")
stargazer(mod_list_test, header = F,
          dep.var.labels = dep_vars_test,
          type ="text")
?stargazer
#建议
mod_list_test <- lapply(data[, 1:4], function(x) {
  df <- data.frame(y = x, x5=data$x5, x6=data$x6)
  lm(y ~ x5 + x6, data=df)
})
#或
mod_list_test <- lapply(1:4, function(k) {
  frm <- as.formula(paste(names(data)[k],"~ x5+x6"))
  lm(frm, data=data)
})#z这是列表
unlist(mod_list_test)  %>% kable()#不行
mod_list_test %>% tibble()

library(knitr)
library(broom)

kable(mod_list_test)

stargazer(mod_list_test, header = F, type ="text")#可接受列表

model1 <- lm(data = d1,formula = Sepal.Width~Petal.Width)
class(summary(model1))
summary(model1) %>% tidy() %>% kable()
summary(model1) %>% tidy.summary.lm()
?tidy#不能在列表中使用
?tidy.summary.lm
?list_tidiers
