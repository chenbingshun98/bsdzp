library(tidyverse)
install.packages("forcats")
library(forcats)

income <- c("low", "high", "medium", "medium", "low", "high",  "high")
factor(income)#因子层级会自动按照字符串的字母顺序排序

factor(income, levels = c("low", "high", "medium") )

#不属于因子层级中的值, 
#比如这里因子层只有c("low", "high")，
#那么income中的“medium”会被当作缺省值NA
factor(income, levels = c("low", "high") )



x <- factor(income)
x

#也可以指定顺序
x %>% fct_relevel(levels = c("high", "medium", "low"))

#或者让“medium” 移动到最前面
x %>% fct_relevel(levels = c("medium"))

# 或者让“medium” 移动到最后面
#添加参数 after设定为Inf
x %>% fct_relevel("medium", after = Inf)

#可以按照字符串第一次出现的次序
x %>% fct_inorder()

#按照其他变量的中位数的升序排序
x %>% fct_reorder(c(1:7), .fun = median)  

d <- tibble(
  x = c("a","a", "b", "b", "c", "c"),
  y = c(2, 2, 1, 5,  0, 3)
  
)
d

library(ggplot2)

#我们看到，横坐标上是a-b-c的顺序
d %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

#fct_reorder()可以让x的顺序按照x中
#每个分类变量对应y值的中位数升序排序

# 因此，x的因子层级的顺序调整为c-a-b
d %>% 
  ggplot(aes(x = fct_reorder(x, y, .fun = median), y = y)) +
  geom_point()

#但这样会造成x坐标标签一大串，
#因此建议可以写mutate()函数里
d %>% 
  mutate(x = fct_reorder(x, y, .fun = median, .desc = TRUE)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

d %>% 
  mutate(x = fct_reorder(x, y, .fun = min, .desc = TRUE)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

# 可视化中应用
ggplot(penguins, aes(y = species)) +
  geom_bar()

ggplot(penguins, aes(y = fct_rev(species))) +
  geom_bar()

#逆序
penguins %>% 
  count(species) %>% 
  pull(species)

penguins %>% 
  count(species) %>% 
  mutate(species = fct_relevel(species,
                               "Chinstrap","Gentoo","Adelie")) %>% 
  pull(species)


# Move "Chinstrap" in front, rest alphabetic
ggplot(penguins, aes(y = fct_relevel(species, "Chinstrap"))) +
  geom_bar()

# Use order "Chinstrap", "Gentoo", "Adelie"
ggplot(penguins, aes(y = fct_relevel(species, "Chinstrap", "Gentoo", "Adelie"))) +
  geom_bar()

penguins %>%
  mutate(species = fct_relevel(species, "Chinstrap", "Gentoo", "Adelie")) %>%
  ggplot(aes(y = species)) +
  geom_bar()


#把species 中的Adelie放在最后
ggplot(penguins, aes(y = fct_relevel(species, "Adelie", after = Inf))) +
  geom_bar()

#Use the order defined by the number of penguins 
#of different species .
#the order is descending, from most frequent to 
#least frequent

penguins %>% 
  mutate(species = fct_inseq(species)) %>% 
  ggplot(aes(y = species))+
  geom_bar()

penguins %>% 
  mutate(species = 
           fct_rev(fct_inseq(species))) %>% 
  ggplot(aes(y = species))+
  geom_bar()

#reorder based on numeric values
#fct_reorder()可以让x的顺序按照x中每个分类变量
#对应y值的中位数升序排序，具体为
#a对应的y值c(2, 2) 中位数是median(c(2, 2)) = 2
# b对应的y值c(1, 5) 中位数是median(c(1, 5)) = 3
# c对应的y值c(0, 3) 中位数是median(c(0, 3)) = 1.5
penguins %>% 
  count(species) %>% 
  mutate(species = fct_reorder(species,n)) %>% 
  ggplot(aes(n, species))+
  geom_col()
