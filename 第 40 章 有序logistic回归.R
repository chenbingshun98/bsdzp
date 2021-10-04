library(tidyverse)

setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")

# 二元logistic回归
#多分类logistic回归
#有序logistic回归:
#Y为定类且有序，幸福感(不幸福、比较幸福和非常幸福)

#南京大学池彪的《教育人力资本的代际传递研究》硕士论文，

tb <- readr::read_rds("cfps.rds")
head(tb)

tb %>% count(edu)
tb %>% count(edu_f)
tb %>% count(edu_m)

#为了方便处理，减少分类，我们将大专以及大专以上的都归为一类
df <- tb %>% 
  dplyr::mutate(
    across(
      starts_with("edu"),
      ~case_when(
        . %in% c(5:8) ~ 5,
        TRUE ~ .
      )
    )
  )
df

tb %>%
  dplyr::mutate(
    across(
      starts_with("edu"),
      ~ if_else(. %in% c(5, 6, 7, 8), 5, .)
    )
  )

df %>% count(edu)
df %>% count(edu_f)
df %>% count(edu_m)

# 问题的提出：
# 
# 学历上父母是否门当户对？
# 父母的受教育程度对子女的受教育水平是正向影响？
# 父亲和母亲谁的影响大？
# 对男孩影响大？还是对女孩影响大？
# 以上情况城乡有无差异？

df %>%
  dplyr::summarise(
    eq_n = sum(edu_m == edu_f),
    n = n()
  ) %>%
  dplyr::mutate(prop = eq_n / n)

df %>%
  dplyr::count(edu_m, edu_f) %>%
  dplyr::group_by(edu_m) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()

df %>%
  dplyr::count(edu_m, edu_f) %>%
  dplyr::group_by(edu_m) %>%
  dplyr::mutate(percent = n / sum(n)) %>%
  dplyr::select(-n) %>%
  tidyr::pivot_wider(
    names_from = edu_f,
    values_from = percent
  )

library(ggridges)
df %>%
  dplyr::mutate(
    across(edu_m, as.factor)
  ) %>%
  ggplot(aes(x = edu, y = edu_m)) +
  geom_density_ridges() +
  scale_x_continuous(limits = c(0, 6), breaks = c(1:5)) +
  labs(
    title = "家庭中母亲的教育程度对子女的影响",
    subtitle = "数字越大，教育程度越高",
    x = "子女的教育程度",
    y = "母亲的教育程度"
  )

#父亲和母亲谁的影响大

library(MASS)

df1 <- df %>% 
  dplyr::mutate(
      across(c(edu, sex, urban), as.factor),
      across(edu, ~fct_inseq(., ordered = TRUE))
  )


mod_mass <- polr(edu ~ edu_f + edu_m + sex + num_siblings + urban,
                 data = df1,
                 method = c('logistic'))
summary(mod_mass)
mod_mass %>% broom::tidy()

library(equatiomatic)
extract_eq(mod_mass, use_coefs = TRUE)

#系数的解释
coef(mod_mass) %>% exp()

library(margins)
# me_mass <- marginal_effects(mod_mass, variables = "sex")
me_mass <- marginal_effects(mod_mass, variables = "edu_m")
me_mass %>% 
  head()
