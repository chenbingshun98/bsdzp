rm(list = ls())  # 清空工作空间
summary(iris)
(sepal = iris[iris$Species == 'setosa', 1])
t.test(sepal, mu = 4.5)
t.test(sepal, mu = 4.5, alternative = "less")

data("iris")
x = iris[iris$Species == 'setosa', 3]
y = iris[iris$Species == 'versicolor', 3]
t.test(x, y)

data(sleep)
head(sleep)
x = sleep[sleep$group == 1, 1]
y = sleep[sleep$group == 2, 1]
t.test(x, y)

x = sleep[sleep$group == 1, 1]
y = sleep[sleep$group == 2, 1]
t.test(x, y, paired = TRUE)
t.test(x - y)
