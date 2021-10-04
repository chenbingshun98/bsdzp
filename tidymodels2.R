library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results

urchins <-
  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))
#> 
#> ── Column specification ──────────────────────────────────────────────
#> cols(
#>   TREAT = col_character(),
#>   IV = col_double(),
#>   SUTW = col_double()
#> )


ggplot(urchins,
       aes(x = initial_volume, 
           y = width, 
           group = food_regime, 
           col = food_regime)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = .7)
#> `geom_smooth()` using formula 'y ~ x'

lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

lm_fit <- 
  lm_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)
lm_fit
#> parsnip model object
#> 
#> Fit time:  3ms 
#> 
#> Call:
#> stats::lm(formula = width ~ initial_volume * food_regime, data = data)
#> 
#> Coefficients:
#>                    (Intercept)                  initial_volume  
#>                      0.0331216                       0.0015546  
#>                 food_regimeLow                 food_regimeHigh  
#>                      0.0197824                       0.0214111  
#>  initial_volume:food_regimeLow  initial_volume:food_regimeHigh  
#>                     -0.0012594                       0.0005254

tidy(lm_fit)
#> # A tibble: 6 x 5
#>   term                            estimate std.error statistic  p.value
#>   <chr>                              <dbl>     <dbl>     <dbl>    <dbl>
#> 1 (Intercept)                     0.0331    0.00962      3.44  0.00100 
#> 2 initial_volume                  0.00155   0.000398     3.91  0.000222
#> 3 food_regimeLow                  0.0198    0.0130       1.52  0.133   
#> 4 food_regimeHigh                 0.0214    0.0145       1.47  0.145   
#> 5 initial_volume:food_regimeLow  -0.00126   0.000510    -2.47  0.0162  
#> 6 initial_volume:food_regimeHigh  0.000525  0.000702     0.748 0.457
lm_fit %>% 
  tbl_regression()

tidy(lm_fit) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))


# set the prior distribution
prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)

# make the parsnip model
bayes_mod <-   
  linear_reg() %>% 
  set_engine("stan", 
             prior_intercept = prior_dist, 
             prior = prior_dist) 

# train the model
bayes_fit <- 
  bayes_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)

print(bayes_fit, digits = 5)
#> parsnip model object
#> 
#> Fit time:  8.5s 
#> stan_glm
#>  family:       gaussian [identity]
#>  formula:      width ~ initial_volume * food_regime
#>  observations: 72
#>  predictors:   6
#> ------
#>                                Median   MAD_SD  
#> (Intercept)                     0.03281  0.00992
#> initial_volume                  0.00157  0.00041
#> food_regimeLow                  0.01990  0.01286
#> food_regimeHigh                 0.02136  0.01519
#> initial_volume:food_regimeLow  -0.00126  0.00052
#> initial_volume:food_regimeHigh  0.00052  0.00073
#> 
#> Auxiliary parameter(s):
#>       Median  MAD_SD 
#> sigma 0.02144 0.00192
#> 
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg
