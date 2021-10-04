# 比distinct()更知我心
df <- tribble(
  ~id, ~date, ~store_id, ~sales,
  1, "2020-03-01", 1, 100,
  2, "2020-03-01", 2, 100,
  3, "2020-03-01", 3, 150,
  4, "2020-03-02", 1, 110,
  5, "2020-03-02", 3, 101
)

df %>%
  janitor::get_dupes(store_id) %>% beepr::beep(sound = "coin")
df

df %>%
  janitor::get_dupes(date)

#谁帮我敲模型的公式
install.packages("equatiomatic")
library(equatiomatic)
## https://github.com/datalorax/equatiomatic

mod1 <- lm(mpg ~ cyl + disp, mtcars)
extract_eq(mod1)
extract_eq(mod1, use_coefs = TRUE)

install.packages("performance")
library(performance)
model <- lm(mpg ~ wt * cyl + gear, data = mtcars)
performance::check_model(model)

install.packages("gtsummary")
library(gtsummary)
## https://github.com/ddsjoberg/gtsummary

# 统计表格不用愁
gtsummary::trial %>%
  dplyr::select(trt, age, grade, response) %>%
  gtsummary::tbl_summary(
    by = trt,
    missing = "no"
  ) %>%
  gtsummary::add_p() %>%
  gtsummary::add_overall() %>%
  gtsummary::add_n() %>%
  gtsummary::bold_labels()

t1 <-
  glm(response ~ trt + age + grade, trial, family = binomial) %>%
  gtsummary::tbl_regression(exponentiate = TRUE)

t2 <-
  survival::coxph(survival::Surv(ttdeath, death) ~ trt + grade + age, trial) %>%
  gtsummary::tbl_regression(exponentiate = TRUE)



gtsummary::tbl_merge(
  tbls = list(t1, t2),
  tab_spanner = c("**Tumor Response**", "**Time to Death**")
)

library(ggplot2)
install.packages("statsExpressions")
library(statsExpressions)
# https://github.com/IndrajeetPatil/statsExpressions


ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Spearman's rank correlation coefficient",
    subtitle = expr_corr_test(mtcars, mpg, wt, type = "nonparametric")
  )
#程序结束后记得提醒我
beepr::beep(sound = "mario")
library(beepr)
library(inferregex)
## remotes::install_github("daranzolin/inferregex")
s <- "abcd-9999-ab9"
infer_regex(s)$regex
