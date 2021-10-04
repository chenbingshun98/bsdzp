rf_model <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_wflow <- 
  workflow() %>% 
  add_formula(
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
      Latitude + Longitude) %>% 
  add_model(rf_model) 

rf_fit <- rf_wflow %>% fit(data = ames_train)

estimate_perf <- function(model, dat) {
  # Capture the names of the objects used
  cl <- match.call()
  obj_name <- as.character(cl$model)
  data_name <- as.character(cl$dat)
  data_name <- gsub("ames_", "", data_name)
  
  # Estimate these metrics:
  reg_metrics <- metric_set(rmse, rsq)
  
  model %>% 
    predict(dat) %>% 
    bind_cols(dat %>% select(Sale_Price)) %>% 
    reg_metrics(Sale_Price, .pred) %>% 
    select(-.estimator) %>% 
    mutate(object = obj_name, data = data_name)
}

estimate_perf(rf_fit, ames_train)
#> # A tibble: 2 × 4
#>   .metric .estimate object data 
#>   <chr>       <dbl> <chr>  <chr>
#> 1 rmse       0.0364 rf_fit train
#> 2 rsq        0.961  rf_fit train
estimate_perf(lm_fit, ames_train)
#> # A tibble: 2 × 4
#>   .metric .estimate object data 
#>   <chr>       <dbl> <chr>  <chr>
#> 1 rmse       0.0751 lm_fit train
#> 2 rsq        0.819  lm_fit train