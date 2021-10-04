library(tidyverse)
data.frame(
  x = c("a", "R for data science", NA)
) %>%
  mutate(y = str_length(x))

#字符串组合
str_c("x", "y")
str_c("x", "y", sep = ",")
str_c(c("x", "y", "z"), 
      sep = ",")
# 是不是和你想象的不一样，那就?str_c，或者试试这个
str_c(c("x", "y", "z"),
      c("x", "y", "z"),
      sep = ",")

#用在数据框里
data.frame(
  x = c("I", "love", "you"),
  y = c("you", "like", "me")
) %>% 
  mutate(z = str_c(x, y, sep = "|"))

str_c(c("x", "y", "z"), c("a", "b", "c"), sep = "|")

str_c(c("x", "y", "z"), c("a", "b", "c"), collapse = "|")

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

x <- c("Apple", "Banana", "Pear")
#开始位置和结束位置如果是负整数，
# 就表示位置是从后往前数，
#比如下面这段代码，
# 截取倒数第3个至倒数第1个位置上的字符串
str_sub(x, -3, -1)

str_sub(x, 1, 1) <- "Q"
x

#9.4.1 基础匹配
x <- c("apple", "banana", "pear")
str_view(string = x, pattern = "an")

x <- c("apple", "banana", "pear")
str_view(x, ".a.")

c("s.d") %>%
  str_view(".")

# 如果向表达.本身呢？
c("s.d") %>% 
  str_view("\\.")

# 9.4.2 锚点
x <- c("apple", "banana", "pear")
str_view(x, "^a")

x <- c("apple", "banana", "pear")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "^apple$")

# 9.4.3 字符类与字符选项
# 前面提到，.匹配任意字符，事实上还有很多这种特殊含义的字符：
# 
# \d: matches any digit.
# \s: matches any whitespace (e.g. space, tab, newline).
# [abc]: matches a, b, or c.
# [^abc]: matches anything except a, b, or c.
str_view(c("grey", "gray"), "gr[ea]y")

str_view(x, "X+")

# 控制匹配次数:
#   
#   {n}: exactly n
# {n,}: n or more
# {,m}: at most m
# {n,m}: between n and m

x <- "Roman numerals: MDCCCLXXXVIII"
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")

# 默认的情况，
#*, + 匹配都是贪婪的，
#也就是它会尽可能的匹配更多
# 如果想让它不贪婪，
#而是变得懒惰起来，
#可以在*, + 加个?
x <- "Roman numerals: MDCCCLXXXVIII"

str_view(x, "CLX+")

str_view(x, "CLX+?")
str_view(x, "CLX?")

#9.4.5 分组与回溯引用
ft <- fruit %>% head(10)
ft

#我们想看看这些单词里，
#有哪些字母是重复两次的，
#比如aa, pp.
#如果用上面学的方法
str_view(ft, ".{2}", match = T)

str_view(ft, "(.)\\1", match = T)
str_view(ft, "(.)\\1")#不一样


# . 是匹配任何字符
# (.) 将匹配项括起来，
#它就用了一个名字，
#叫\\1； 
#如果有两个括号，
#就叫\\1和\\2
# \\1 表示回溯引用，
#表示引用\\1对于的(.)
# 所以(.)\\1的意思就是，
#匹配到了字符，
#后面还希望有个同样的字符
# 
# 如果是匹配abab, wcwc
str_view(ft, "(..)\\1", match = T)
str_view(ft, "(..)\\1", match = T)

# 如果是匹配abba, wccw呢？
str_view(ft, "(.)(.)\\2\\1", match = TRUE)
 
#9.5.1 确定一个字符向量是否匹配一种模式
# 实际问题中，想判断是否匹配？可以用到str_detect()函数
x <- c("apple", "banana", "pear")
str_detect(x, "e")

# 数据框中也是一样
## # A tibble: 3 x 1
##   x     
##   <chr> 
## 1 apple 
## 2 banana
## 3 pear

d <- tibble(x = c("apple", "banana", "pear"))
d

d %>% mutate(has_e = str_detect(x, "e"))

#用于筛选
d %>% 
  dplyr::filter(str_detect(x, "e"))

#stringr::words包含了牛津字典里常用单词
words %>% head()

# How many common words start with t?
sum(str_detect(words, "^t"))

# proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# 放在数据框里看看, 看看以x结尾的单词是哪些？
tibble(
  word = words
) %>% 
  dplyr::filter(str_detect(word, "x$"))

d %>% 
  mutate(
    has_e = str_detect(x, "e")
  )

#str_detect() 有一个功能类似的函数str_count()，
#区别在于，
#后者不是简单地返回是或否，
#而是返回字符串中匹配的数量
x <- c("apple", "banana", "pear")
str_count(x, "a")

tibble(
  word = words
) %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

#9.5.2 确定匹配的位置
# 大家放心，
#正则表达式不会重叠匹配。
#比如用"aba"去匹配"abababa"，
#肉眼感觉是三次，
#但正则表达式告诉我们是两次，
#因为不会重叠匹配

str_count("abababa", "aba")

str_view_all("abababa", "aba")

#9.5.3 提取匹配的内容
colours <- c(
  "red", "orange", "yellow",
  "green", "blue", "purple"
)
colours

colour_match <- str_c(colours,collapse = "|")
colour_match
# colour_match 这里是一个字符串，
#放在pattern参数位置上也是正则表达式了,

#这里注意以下两者的区别
str_view("abcd", "ab|cd")
str_view("abc", "a[bc]d")

more <- "It is hard to erase blue or red ink"
str_extract(more, pattern = colour_match)
str_extract_all(more, pattern = colour_match)

more <- sentences[str_count(sentences, colour_match) > 1]
more

#取出sentences中，
#含有有两种和两种颜色以上的句子。
#不过，不喜欢这种写法，
#看着费劲，
#还是用tidyverse的方法
tibble(sentence = sentences) %>% 
  filter(str_count(sentences, colour_match) > 1)

# str_extract()提取匹配, 谁先匹配就提取谁
tibble(x = more) %>% 
  mutate(color = str_extract(x, colour_match))

# str_extract_all()提取全部匹配项
tibble(x = more) %>% 
  mutate(color = str_extract_all(x, colour_match))

tibble(x = more) %>% 
  mutate(color =str_extract_all(x, colour_match)) %>% 
  unnest(color)

# 9.5.4 替换匹配内容
# 只替换匹配的第一项
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")

str_replace_all(x, "[aeiou]", "-")

# 9.5.5 拆分字符串
# 这个和str_c()是相反的操作
lines <- "I love my country"
lines

str_split(lines,"")
str_split(lines," ")

fields <- c("Name : Hadley", "Country : NZ", "Age : 35")
fields %>% 
  str_split(" : ", 
            n = 2,
            simplify = T)

# 9.6 进阶部分
# 带有条件的匹配
# 
# 9.6.1 look ahead
# 想匹配Windows，
#同时希望Windows右侧是
#"95","98", "NT", "2000"中的一个

win <- c("Windows2000", "Windows", "Windows3.1")
str_view(win, "Windows(?=95|98|NT|2000)")

# Windows后面的 () 是匹配条件，事实上，有四种情形：
# 
# (?=pattern) 要求此位置的后面必须匹配表达式pattern
# (?!pattern) 要求此位置的后面不能匹配表达式pattern
# (?<=pattern) 要求此位置的前面必须匹配表达式pattern
# (?<!pattern) 要求此位置的前面不能匹配表达式pattern
win <- c("2000Windows", "Windows", "3.1Windows")
str_view(win, "(?<=95|98|NT|2000)Windows")

win <- c("2000Windows", "Windows", "3.1Windows")
str_view(win, "(?<!95|98|NT|2000)Windows")

dt <- tibble(
  x = 1:4,
  y = c("wk 5553", "week-91","700","w#9")
)
dt

#现在把第二列的数字提取出来当新一列的元素
dt %>% 
  mutate(
    z = str_extract(y, "[0-9]")
  )

dt %>% 
  mutate(
    z = str_extract(y, "[0-9]")
  )
dt %>% 
  extract(y,c("z"),regex = "([0-9]+)", remove = F)

dt %>% 
  mutate(
    z = str_extract_all(y, "[0-9]")
  ) %>% tibble::view()

# 提取第二列中的大写字母
df <- data.frame(
  x = seq_along(1:7),
  y = c("2016123456", "20150513", "AB2016123456", "J2017000987", "B2017000987C", "aksdf", "2014")
)

df <- data.frame(
  x = seq(1:7),
  y = c("2016123456", "20150513", "AB2016123456", "J2017000987", "B2017000987C", "aksdf", "2014")
)
df

df %>% 
  mutate(
    item = str_extract_all(y, "[A-Z]")
  ) %>% 
  tidyr::unnest(item)
  
# 要求：中英文分开
tb <- tibble(x = c("I我", "love爱", "you你"))
tb

tb %>% 
  tidyr::extract(
    # x, c("en", "cn"), "([:alpha:]+)([^:alpha:]+)",
    x, c("en", "cn"),
    "([a-zA-Z]+)([^a-zA-Z]+)",
    remove = F
  )

tb %>% 
  str_extract(#这个是不对的，因为它没有创建新列的功能
    x,c("en","cn"),
    "([a-zA-Z]+)([^a-zA-Z]+)"
  )
?str_extract

tb %>% 
  tidyr::separate(
    # x, c("en", "cn"), "([:alpha:]+)([^:alpha:]+)",
    x, c("en", "cn"),
    "([a-zA-Z]+)([^a-zA-Z]+)",
    remove = F
  )

d <- data.frame(x = c(NA, "a?b", "a.d", "b:c"))
d
d %>% separate(x, c("A","B"), sep = "([.?:])")

# 要求：提取起始数字
df <- tibble(x = c("1-12周", "1-10周", "5-12周"))
df

df %>% extract(
  x,
  # c("start", "end", "cn"), "([:digit:]+)-([:digit:]+)([^:alpha:]+)",
  c("start", "end", "cn"), "(\\d+)-(\\d+)(\\D+)",
  remove = FALSE
)

# 要求：提取大写字母后的数字
df <- tibble(
  x = c("12W34", "AB2C46", "B217C", "akTs6df", "21WD4")
)

df %>% 
  mutate(
    item = str_extract_all(x, "(?<=[A-Z])[0-9]")
  ) %>% tidyr::unnest(item)


df %>% 
  mutate(
    item = str_extract(x, "(?<=[A-Z])[0-9]")
  ) %>% tidyr::unnest(item)
#akTs6df在这里消失了

# 思考题，
# 
# 如何提取大写字母后的连续数字，比如B217C后面的217
# 如何提取提取数字前的大写字母？
# 为什么第一个正则表达式返回结果为""

x <- "Roman numerals: MDCCCLXXXVIII"
str_match_all(x, "C?") # "?"的意思是匹配0次或者1次

str_match_all(x, "CC?")

# 提取数字并求和
df <- tibble(
  x = c("1234", "B246", "217C", "2357f", "21WD4")
)
df

df %>% 
  mutate(num = str_match_all(x, "\\d")) %>% 
  unnest(num) %>% 
  mutate_at(vars(num), as.numeric) %>% 
  group_by(x) %>% 
  summarise(sum = sum(num))

#错误，忘了依x分组
df %>% 
  mutate(num = str_match_all(x, "\\d")) %>% 
  unnest(num) %>% 
  mutate_at(vars(num), as.numeric) %>% 
  summarise(sum = sum(num))

text <- "Quantum entanglement is a physical phenomenon that occurs when pairs or groups of particles are generated, interact, or share spatial proximity in ways such that the quantum state of each particle cannot be described independently of the state of the others, even when the particles are separated by a large distance."


pairs <-
  tibble::tribble(
    ~item, ~code,
    "Quantum entanglement", "A01",
    "physical phenomenon", "A02",
    "quantum state", "A03",
    "quantum mechanics", "A04"
  ) %>%
  tibble::deframe()

class(pairs)

text %>% str_replace_all(pairs)

#回到上课前的提问：如何提取Sichuan Univ后面的学院？
d %>% 
  dplyr::mutate(
    coll = str_extract(address,
                       "(?<=Sichuan Univ,).*")
  ) %>% 
  tidyr::unnest(coll,
                keep_empty = T)

d %>% mutate(
  coll = str_remove_all(address, ".*,")
)

d %>% tidyr::separate(
  address,
  into = c("univ", "coll"), sep = ",", remove = FALSE
)

d %>%
  tidyr::extract(
    address, c("univ", "coll"), "(Sichuan Univ), (.+)",
    remove = FALSE
  )

remotes::install_github("daranzolin/inferregex")
library(inferregex)
s <- "abcd-9999-ab9"
infer_regex(s)$regex
