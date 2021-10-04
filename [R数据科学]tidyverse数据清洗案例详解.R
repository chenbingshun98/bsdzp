library(tidyverse) # 加载包
who # 数据展示

# 变量名中给出的结构
# （例如new_sp_m014，new_ep_m014，new_ep_f014）可能是值，
# 而不是变量。
# 因此，我们需要将从new_sp_m014到newrel_f65的所有列汇总在一起。
# 我们用通用名称"key"来表示他们。
# 我们知道单元格代表案件数，
# 因此我们将变量数存储在cases中,并用na.rm去除含有缺失值的行。

who1 <- who %>%
  pivot_longer( # 这里使用pivot_longer()将数据变长，具体见后面函数详情。
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = T
  )

who1
who1 %>% count(key)

# 我们需要对列名称的格式进行较小的修正：
# 将new_rel替换为newrel
# （很难在这里找到它，但是如果您不修正它，我们将在后续步骤中出错）。
# 这里使用了stringr包中的str_replace()，
# 将newrel替换new_rel。
who2 <- who1 %>%
  mutate(names_from = stringr::str_replace(key, "newrel", "new_rel"))

who2
?all_equal
# 接下来就是将key中的字符进行分割,我们使用separate()对字符进行两次分割。
# 1.将在每个下划线处拆分代码。
who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")
who3

# 利用select()删除没用的列:new,iso2，iso3。
who3 %>% count(new)
who4 <- who3 %>% select(-new, -iso2, -iso3)
who4

janitor::tabyl(who5, sex, age)
# table(who5, sex, age) 错误写法
table(who5, sex, age)
table(who5$sex, who5$age)
with(who5, table(sex, age))

# 2.将分离sexage到sex和age通过的第一个字符后拆分：
who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep = 1)
who5

# 数据清洗完毕,就可以做一些初步的可视化,探索性分析.
# 这里简单绘制了 前几个国家不同年份,不同性别的结核病病例总数。
who5 %>%
  group_by(country, year, sex) %>%
  filter(year < 2003) %>%
  count() %>%
  head(100) %>%
  ggplot(aes(
    x = as.factor(year), y = n,
    fill = country
  )) +
  geom_col() +
  facet_wrap(~sex, nrow = 1) +
  scale_fill_brewer(palette = "Paired")

# 复杂的管道函数
# 事实上你可以直接只用管道函数构建一个复杂的函数，
# 这样做去除了中间变量，而且可读性很强，强烈推荐。
who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>%
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)
