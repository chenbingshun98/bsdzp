starwars %>% summarise(avg = mean(height, na.rm = TRUE))
#> # A tibble: 1 x 1
#>     avg
#>   <dbl>
#> 1  174.

value <- mean(height, na.rm = TRUE)
#> Error in mean(height, na.rm = TRUE): object 'height' not found
starwars %>% summarise(avg = value)
#> Error: Problem with `summarise()` input `avg`.
#> ✖ object 'value' not found
#> ℹ Input `avg` is `value`.

x <- 1

rlang::qq_show(
  starwars %>% summarise(out = x)
)
#> starwars %>% summarise(out = x)

rlang::qq_show(
  starwars %>% summarise(out = !!x)
)
#> starwars %>% summarise(out = 1)

col <- "height"

rlang::qq_show(
  starwars %>% summarise(out = sum(!!col, na.rm = TRUE))
)
#> starwars %>% summarise(out = sum("height", na.rm = TRUE))

starwars %>% summarise(out = sum("height", na.rm = TRUE))
#> Error: Problem with `summarise()` input `out`.
#> ✖ invalid 'type' (character) of argument
#> ℹ Input `out` is `sum("height", na.rm = TRUE)`.

sym(col)
#> height