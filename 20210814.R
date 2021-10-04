library(tidymodels)

tidymodels_prefer()


data(ames)
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

set.seed(123)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

simple_ames <- recipe(Sale_Price~Neighborhood+Gr_Liv_Area+Year_Built+Bldg_Type,
                      data = ames_train) %>% 
  step_log(Gr_Liv_Area,base=10) %>% 
  step_dummy(all_nominal_predictors())

simple_ames

lm_wflow %>% 
  add_recipe(simple_ames)##> Error: A recipe cannot be added when variables already exist.

# We can only have one preprocessing method at a time
lm_wflow <- 
  lm_wflow %>% 
  remove_variables() %>% 
  add_recipe(simple_ames)

lm_wflow

lm_fit <- fit(lm_wflow,ames_train)

predict(lm_fit,ames_test %>% slice(1:3))

lm_fit %>% 
  pull_workflow_prepped_recipe()

lm_fit %>% 
  pull_workflow_fit() %>% 
  tidy() %>% 
  slice(1:5)


ames_train %>% ggplot(aes(y=Neighborhood))+
  geom_bar()+
  labs(y=NULL)

simple_ames <- recipe(Sale_Price~Neighborhood+)