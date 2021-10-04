library(tidyverse)
scooby_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv")

scooby_raw %>%
  filter(monster_amount > 0) %>%
  count(monster_real)

scooby_raw %>%
  filter(monster_amount > 0) %>%
  count(
    year_aired = 10 * ((lubridate::year(date_aired) + 1) %/% 10),
    monster_real
  ) %>%
  mutate(year_aired = factor(year_aired)) %>%
  ggplot(aes(year_aired, n, fill = monster_real)) +
  geom_col(position = position_dodge(preserve = "single"), alpha = 0.8) +
  labs(x = "Date aired", y = "Monsters per decade", fill = "Real monster?")

scooby_raw %>%
  filter(monster_amount > 0) %>%
  mutate(imdb = parse_number(imdb)) %>%
  ggplot(aes(imdb, after_stat(density), fill = monster_real)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(x = "IMDB rating", y = "Density", fill = "Real monster?")

set.seed(123)
library(tidymodels)
scooby_split <- scooby_raw %>%
  mutate(
    imdb = parse_number(imdb),
    year_aired = lubridate::year(date_aired)
  ) %>%
  filter(monster_amount > 0, !is.na(imdb)) %>%
  mutate(
    monster_real = case_when(
      monster_real == "FALSE" ~ "fake",
      TRUE ~ "real"
    ),
    monster_real = factor(monster_real)
  ) %>%
  select(year_aired, imdb, monster_real, title) %>%
  initial_split(strata = monster_real)
scooby_train <- training(scooby_split)
scooby_test <- testing(scooby_split)

set.seed(234)
scooby_folds <- bootstraps(scooby_train, strata = monster_real)
scooby_folds


tree_spec <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("rpart")

tree_spec

tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 4)
tree_grid

doParallel::registerDoParallel()

set.seed(345)
tree_rs <-
  tune_grid(
    tree_spec,
    monster_real ~ year_aired + imdb,
    resamples = scooby_folds,
    grid = tree_grid,
    metrics = metric_set(accuracy, roc_auc, sensitivity, specificity)
  )

tree_rs

show_best(tree_rs)

autoplot(tree_rs) + theme_light(base_family = "IBMPlexSans")


simpler_tree <- select_by_one_std_err(tree_rs,
                                      -cost_complexity,
                                      metric = "roc_auc"
)

final_tree <- finalize_model(tree_spec, simpler_tree)

final_fit <- fit(final_tree, monster_real ~ year_aired + imdb, scooby_train)

final_rs <- last_fit(final_tree, monster_real ~ year_aired + imdb, scooby_split)

collect_metrics(final_rs)


library(parttree)

scooby_train %>%
  ggplot(aes(imdb, year_aired)) +
  geom_parttree(data = final_fit, aes(fill = monster_real), alpha = 0.2) +
  geom_jitter(alpha = 0.7, width = 0.05, height = 0.2, aes(color = monster_real))
