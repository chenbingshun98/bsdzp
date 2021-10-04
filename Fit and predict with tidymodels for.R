library(tidyverse)

bird_baths <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv")

bird_baths %>%
  count(urban_rural)

top_birds <-
  bird_baths %>%
  filter(is.na(urban_rural)) %>%
  arrange(-bird_count) %>%
  slice_max(bird_count, n = 15) %>%
  pull(bird_type)

top_birds

bird_parsed <-
  bird_baths %>%
  filter(
    !is.na(urban_rural),
    bird_type %in% top_birds
  ) %>%
  group_by(urban_rural, bird_type) %>%
  summarise(bird_count = mean(bird_count), .groups = "drop")


p1 <-
  bird_parsed %>%
  ggplot(aes(bird_count, bird_type)) +
  geom_segment(
    data = bird_parsed %>%
      pivot_wider(
        names_from = urban_rural,
        values_from = bird_count
      ),
    aes(x = Rural, xend = Urban, y = bird_type, yend = bird_type),
    alpha = 0.7, color = "gray70", size = 1.5
  ) +
  geom_point(aes(color = urban_rural), size = 3) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Probability of seeing bird", y = NULL, color = NULL)

p1


bird_df <-
  bird_baths %>%
  filter(
    !is.na(urban_rural),
    bird_type %in% top_birds
  ) %>%
  mutate(bird_count = if_else(bird_count > 0, "bird", "no bird")) %>%
  mutate_if(is.character, as.factor)


library(tidymodels)

set.seed(123)
bird_split <- initial_split(bird_df, strata = bird_count)
bird_train <- training(bird_split)
bird_test <- testing(bird_split)

set.seed(234)
bird_folds <- vfold_cv(bird_train, strata = bird_count)
bird_folds

glm_spec <- logistic_reg()

rec_basic <-
  recipe(bird_count ~ urban_rural + bird_type, data = bird_train) %>%
  step_dummy(all_nominal_predictors())

wf_basic <- workflow(rec_basic, glm_spec)

doParallel::registerDoParallel()
ctrl_preds <- control_resamples(save_pred = TRUE)
rs_basic <- fit_resamples(wf_basic, bird_folds, control = ctrl_preds)

rs_basic %>% collect_metrics()

augment(rs_basic) %>%
  roc_curve(bird_count, .pred_bird) %>%
  autoplot()

rec_interact <-
  rec_basic %>%
  step_interact(~ starts_with("urban_rural"):starts_with("bird_type"))

wf_interact <- workflow(rec_interact, glm_spec)
rs_interact <- fit_resamples(wf_interact, bird_folds, control = ctrl_preds)

rs_interact %>% collect_metrics()

augment(rs_interact) %>% 
  roc_curve(bird_count, .pred_bird) %>% 
  autoplot()

#Evaluate model on new data
bird_fit <- fit(wf_interact, bird_train)
predict(bird_fit, bird_test, type = "prob")


new_bird_data <-
  tibble(bird_type = top_birds) %>%
  crossing(urban_rural = c("Urban", "Rural"))

new_bird_data

bird_preds <-
  augment(bird_fit, new_bird_data) %>%
  bind_cols(
    predict(bird_fit, new_bird_data, type = "conf_int")
  )

bird_preds

p2 <-
  bird_preds %>%
  ggplot(aes(.pred_bird, bird_type, color = urban_rural)) +
  geom_errorbar(aes(
    xmin = .pred_lower_bird,
    xmax = .pred_upper_bird
  ),
  width = .2, size = 1.2, alpha = 0.5
  ) +
  geom_point(size = 2.5) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Predicted probability of seeing bird", y = NULL, color = NULL)

p2

library(patchwork)

p1 + p2
