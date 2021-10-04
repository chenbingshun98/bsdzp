#list.files
?list.files



#如果要爬取网上的内用的话，那么readlines函数太基础了
ss2 <- readLines("http://www.lzu.edu.cn", encoding = "UTF-8")
?readLines
##会出现warning，因为在windows系统中默认了最后一行不是空行

head(ss2)

ss1 <- c(1,12,123,"abcdedf")
length(ss1)

nchar(ss1)

?example
example(grep, prompt.prefix = "##")

example("substr",prompt.prefix = "##")

#截取
?strtrim
strtrim(ss1,2)

paste(ss1,"###")
paste(ss1,"###",sep = "_")
paste0(ss1,"###")

library(rvest)
url <- "https://en.wikipedia.org/wiki/Economy_of_Malaysia"
my <- read_html(url)
my

my %>%
  html_nodes("table") %>%
  html_table(fill = T) %>% #If TRUE, automatically fill rows with fewer than the maximum number of columns with NAs.
  .[[1]] %>% class()

tt <- my %>% 
  html_nodes("table") %>% 
  html_table(fill = T) %>% 
  .[[2]]
view(tt)
