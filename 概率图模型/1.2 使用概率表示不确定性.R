prior <- c(working = 0.99, broken = 0.01)
likelihood <- rbind(
  working = c(good = 0.99, bad = 0.01),
  broken = c(good = 0.6, bad = 0.4)
)

# data变量包含观察到的，用于测试机器和计算后验概率的灯泡序列
data <- c("bad", "bad", "bad", "bad")

# ■ 创建一个矩阵，存储后验分布的连续计算结果。

# ■ 然后对于每一个数据，给定当前先验概率计算后验概率：
# 和之前的一样，你可以看到贝叶斯公式的R代码。

# ■ 最后，新的先验概率是当前的后验概率，
# 而且同样的过程可以迭代。
# 最终，函数返回了一个矩阵，
# 包含初始先验概率和所有后续后验概率。
bayes <- function(prior, likelihood, data) {
  posterior <- Matrix::Matrix(0, nrow = length(data), ncol = length(prior))

  dimnames(posterior) <- list(data, names(prior))
  
  initial_prior <- prior
  for (i in 1:length(data))
  {
    posterior[i, ] <-
      prior * likelihood[, data[i]] /
        sum(prior * likelihood[, data[i]])

    prior <- posterior[i, ]
  }


  return(rbind(initial_prior, posterior))
}
print(bayes(prior, likelihood, data)) 

matplot(bayes(prior, likelihood, data), t = "b", lty = c(1, 2), pch = 20, col = c(3, 2))
# 让我们多运行几次，理解一下工作原理。
# 我们使用函数matplot绘出两个分布的演化情况。
# 一个是机器正常（绿色线）的后验概率，
# 一个是机器故障（红色线）的后验概率，如图1-1所示。
prior <- c(working = 0.5, broken = 0.5)
prior <- c(working = 0.6, broken = 0.4)

# 如果一直变换数据，我们可以看到不同的行为。例如，假设机器正常工作的概率是99%。我们观察10个灯泡，其中第一个灯泡是坏的。我们有R代码：

prior <- c(working = 0.99, broken = 0.01)
data <- c(
  "bad", "good", "good", "good",
  "good", "good", "good",
  "good", "good"
)

matplot(bayes(prior, likelihood, data), t = "b", lty = c(1, 2), pch = 20, col = c(3, 2))



###
library(tidyverse)
result <- bayes(prior, likelihood, data) %>% as.matrix() %>% #不能直接转成tibble
  as_tibble() %>% 
  pivot_longer(
    cols = working:broken
  ) %>% 
  group_by(name) %>% 
  mutate(n = row_number()) 

result %>% filter(name == "working")
result%>% 
  ggplot(aes(y = value, color = name, group = name))+
  geom_line(aes(x = n))+
  geom_point(aes(x = n, group = n),
             data = result %>% filter(name == "working"))+
  geom_point(aes(x = n, group = n),
             data = result %>% filter(name == "broken"))+
  theme_classic()+
  labs(
    x = NULL,
    y = "prob"
  )+
  theme(legend.title = element_blank(),
        legend.position = "bottom")+
  gganimate::transition_reveal(n)+
  gganimate::view_follow(fixed_y = TRUE)

####
bayes <- function(prior, likelihood, data) {
  posterior <- Matrix::Matrix(0, nrow = length(data), ncol = length(prior))
  
  dimnames(posterior) <- list(data, names(prior))
  
  bayes_cal <- function(i, prior, likelihood, data){
    
    return(
      prior * likelihood[, data[i]]/
      sum(prior * likelihood[, data[i]])
      )
    
  }

  
  initial_prior <- prior
  for (i in 1:length(data))
  {
    posterior[i, ] <- bayes_cal(i, prior, likelihood, data)
    
    prior <- posterior[i, ]
  }
  
  
  return(rbind(initial_prior, posterior))
}

####
initial_prior <- prior
for (i in 1:length(data))
{
  posterior[i, ] <-
    prior * likelihood[, data[i]] /
    sum(prior * likelihood[, data[i]])
  
  prior <- posterior[i, ]
}
