library(tidyverse)
library(tidymodels)
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")

library(palmerpenguins)

penguins <- penguins %>% janitor::clean_names() %>% drop_na()

penguins %>%
  head()

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm,
             color = species,
             shape = species))+
  geom_point()

#机器学习
split <- penguins %>% 
  mutate(species = forcats::as_factor(species)) %>% 
  mutate(species = forcats::fct_lump(species, 1)) %>% 
  rsample::initial_split()

split %>% class()

training_data <- training(split)
training_data
testing_data <- testing(split)
testing_data

#model01
model_logitic <- parsnip::logistic_reg() %>% 
  parsnip::set_engine("glm") %>% 
  parsnip::set_mode("classification") %>% 
  parsnip::fit(species ~ bill_length_mm + bill_depth_mm,
              data = training_data)

dplyr::bind_cols(
  predict(model_logitic,
          new_data = testing_data,
          type = "class"),
  predict(model_logitic,
          new_data = testing_data,
          type = "prob"),
  testing_data
)

predict(model_logitic,
        new_data = testing_data) %>% 
  bind_cols(testing_data) %>% 
  count(.pred_class, species)

#model2
model_neighbor <- parsnip::nearest_neighbor(neighbors = 10) %>% 
  parsnip::set_engine("kknn") %>% 
  parsnip::set_mode("classification") %>% 
  parsnip::fit(species ~ bill_length_mm,
               data = training_data)

predict(model_neighbor,
        new_data = testing_data) %>% 
  bind_cols(testing_data) %>% 
  count(.pred_class, species)

#model3
model_multinom <- parsnip::multinom_reg() %>% 
  parsnip::set_engine("nnet") %>% 
  parsnip::set_mode("classification") %>% 
  parsnip::fit(species ~ bill_length_mm,
               data = training_data)

predict(model_multinom,
        new_data = testing_data) %>% 
  bind_cols(testing_data) %>% 
  count(.pred_class, species)

# model_multinom %>% conf_mat_resampled()


#model4
model_decision <- parsnip::decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification") %>% 
  fit(species ~ bill_length_mm, data = training_data)

predict(model_decision, new_data = testing_data) %>% 
  bind_cols(testing_data) %>% 
  count(.pred_class, species)

#workflow

split <- penguins %>% 
  drop_na() %>% 
  rsample::initial_split(prop = 3/4)

training_data <- rsample::training(split)
testing_data <- rsample::testing(split)

penguins_lm <- parsnip::linear_reg() %>% 
  parsnip::set_engine("stan")

penguins_recipe <- recipes::recipe(bill_length_mm~bill_depth_mm+sex,
                                   data = training_data) %>% 
  recipes::step_normalize(all_numeric(), -all_outcomes()) %>% 
  recipes::step_dummy(all_nominal())

broom::tidy(penguins_recipe)

#
penguins_recipe %>% 
  recipes::prep(data = training_data) %>%  #or prep(retain = TRUE)
  recipes::juice()


penguins_recipe %>%   
  recipes::prep(data = training_data) %>% 
  recipes::bake(new_data = testing_data)   # recipe used in new_data



train_data <- 
  penguins_recipe %>%   
  recipes::prep(data = training_data) %>% 
  recipes::bake(new_data = NULL) 


test_data <- 
  penguins_recipe %>%   
  recipes::prep(data = training_data) %>% 
  recipes::bake(new_data = testing_data)   

#workflow
wflow <- 
  workflows::workflow() %>% 
  workflows::add_recipe(penguins_recipe) %>% 
  workflows::add_model(penguins_lm) 


wflow_fit <- 
  wflow %>% 
  parsnip::fit(data = training_data)

wflow_fit %>% 
  workflows::pull_workflow_fit() %>% 
  broom.mixed::tidy()

wflow_fit %>% 
  workflows::pull_workflow_prepped_recipe() 

wflow_fit %>% 
  workflows::pull_workflow_fit() %>% 
  stats::predict(new_data = test_data) # note: test_data not testing_data
