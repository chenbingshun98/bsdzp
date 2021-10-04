library(tidyverse)
triple <- function(x) x * 3
map(1:3, triple)

simple_map <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

# producing atomic vectors
map_chr(mtcars, typeof)

map_lgl(mtcars, is.double)

n_unique <- function(x) length(unique(x))
map_int(mtcars, n_unique)

map_dbl(mtcars, mean)


## .f must return a single value
pair <- function(x) c(x, x)

map_dbl(1:2, pair)
# .f returns the wrong type of resul
map_dbl(1:2, as.character)

# map() can accept any type of output.

map(1:2, pair)

map(1:2, as.character)

# avoid sapply() because it tries to simplify the result,

# vapply() is safer because it allows you to provide a template,
# FUN.VALUE, that describes the output shape.

map_dbl(x, mean, na.rm = TRUE)
vapply(x, mean, na.rm = TRUE, FUN.VALUE = double(1))

# anonymous functions and shortcuts
map_dbl(mtcars, function(x) length(unique(x)))
# >  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb
# >   25    3   27   22   22   29   30    2    2    3    6
# 可简化为
map_dbl(mtcars, ~ length(unique(.x)))

# You can see what’s happening behind the scenes by calling as_mapper():
as_mapper(~ length(unique(.x)))

# 简化的形式有助于生产随机的数据
x <- map(1:3, ~ runif(2))
str(x)

x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

# Select by name
map_dbl(x, "x")
# > [1] 1 4 8

# Or by position
map_dbl(x, 1)
# > [1] -1 -2 -3

# Or by both
map_dbl(x, list("y", 1))
# > [1] 2 5 9

# You'll get an error if a component doesn't exist:
map_chr(x, "z")
# > Error: Result 3 must be a single string, not NULL of length 0

# Unless you supply a .default value
map_chr(x, "z", .default = NA)
# > [1] "a" "b" NA

# passing arguments with...
x <- list(1:5, c(1:10, NA))
map_dbl(x, ~ mean(.x, na.rm = TRUE))

# 简化为
map_dbl(x, mean, na.rm = TRUE)

plus <- function(x, y) x + y

x <- c(0, 0, 0, 0)
map_dbl(x, plus, runif(1))
# > [1] 0.0625 0.0625 0.0625 0.0625
map_dbl(x, ~ plus(.x, runif(1)))
# > [1] 0.903 0.132 0.629 0.945

# argument names
boostrap_summary <- function(x, f) {
  f(sample(x, replace = TRUE))
}

simple_map(mtcars, boostrap_summary, f = mean)
# ...
map(mtcars, boostrap_summary, f = mean)

# varying another argument
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
x

map_dbl(trims, ~ mean(x, trim = .x))
map_dbl(trims, ~ mean(x))

map_dbl(trims, function(trim) mean(x, trim = trim))

# flexible argument matching
map_dbl(trims, mean, x = x)

map(1:3, ~ runif(2))
map(1:3, runif(2))
map(1:3, runif, n = 2)

as_mapper(~ runif(2))
# > <lambda>
# > function (..., .x = ..1, .y = ..2, . = ..1)
# > runif(2)
# > attr(,"class")
# > [1] "rlang_lambda_function" "function"
as_mapper(runif(2))
# > function (x, ...)
# > pluck(x, 0.0807501375675201, 0.834333037259057, .default = NULL)
# > <environment: 0x7fd304ed4550>

map_dbl(mtcars, sd)

penguins <- palmerpenguins::penguins

penguins_numeric <- map_lgl(penguins, is.numeric)
map_dbl(penguins[penguins_numeric], sd, na.rm = TRUE)

penguins_factor <- map_lgl(penguins, is.factor)
map_int(penguins[penguins_factor], ~ length(levels(.x)))

#
trials <- map(1:100, ~ t.test(rpois(10, 10), rpois(10, 7)))

df_trials <- tibble(p_value = map_dbl(trials, "p.value"))

df_trials %>%
  ggplot(aes(x = p_value, fill = p_value < 0.05)) +
  geom_dotplot(binwidth = 0.01) +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "top"
  )

# why the nested map is failed?
x <- list(
  list(1, c(3, 9)),
  list(c(3, 6), 7, c(4, 7, 6))
)

triple <- function(x) x * 3
map(x, map, .f = triple)

# Don't name the argument
map(x, map, triple)

# Use magrittr-style anonymous function
map(x, . %>% map(triple))

# Use purrr-style anonymous function
map(x, ~ map(.x, triple))

# fit with map
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
models <- map(formulas, lm, data = mtcars)
models

# 提取R方
bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}

bootstraps <- map(1:10, ~ bootstrap(mtcars))

bootstraps %>%
  map(~ lm(mpg ~ disp, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")
# >  [1] 0.588 0.822 0.745 0.746 0.784 0.749 0.613 0.792 0.653 0.726

# purr-style
by_cyl <- split(mtcars, mtcars$cyl)

by_cyl %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map(coef) %>%
  map_dbl(2)

by_cyl %>%
  lapply(function(data) lm(mpg ~ wt, data = data)) %>%
  lapply(coef) %>%
  vapply(function(x) x[[2]], double(1))

models <- lapply(by_cyl, function(data) lm(mpg ~ wt, data = data))
vapply(models, function(x) coef(x)[[2]], double(1))
# >     4     6     8
# > -5.65 -2.78 -2.19

slopes <- double(length(by_cyl))
for (i in seq_along(by_cyl)) {
  model <- lm(mpg ~ wt, data = by_cyl[[i]])
  slopes[[i]] <- coef(model)[[2]]
}
slopes
# > [1] -5.65 -2.78 -2.19

df <- data.frame(
  x = 1:3,
  y = 6:4
)
df
map(df, ~ .x * 2)
modify(df, ~ .x * 2) # modify() doesn’t modify in place, it returns a modified copy
df <- modify(df, ~ .x * 2)


simple_modify <- function(x,f,...){
  for (i in seq_along(x)) {
    x[[i]] <- f(x[[i]],...)
  }
  x
}

#2个输入值
xs <- map(1:8, ~ runif(10))
xs[[1]][[1]] <- NA
ws <- map(1:8, ~ rpois(10, 5) + 1)

map_dbl(xs,mean)
map(xs,mean)

#But passing ws as an additional argument doesn’t work 
#because arguments after .f are not transformed:
map_dbl(xs,weighted.mean,w=ws)

map2_dbl(xs,ws,weighted.mean)

map2_dbl(xs, ws, weighted.mean, na.rm = TRUE)
#> [1] 0.504 0.451 0.603 0.452 0.563 0.510 0.342 0.464

simple_map2 <- function(x,y,f,...){
  out <- vector("list",length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]],y[[i]],...)
  }
  out
}

#无输出
welcome <- function(x) {
  cat("Welcome ", x, "!\n", sep = "")
}
names <- c("Hadley", "Jenny")

# As well as generate the welcomes, it also shows 
# the return value of cat()
map(names, welcome)
#> Welcome Hadley!
#> Welcome Jenny!
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL

#解决方案
walk(names,welcome)

#walk2
temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

dir(temp)
#> [1] "cyl-4.csv" "cyl-6.csv" "cyl-8.csv"

#9.4.4 Iterating over values and indices
imap_chr(iris, ~ paste0("The first value of ", .y, " is ", .x[[1]]))
#>                             Sepal.Length 
#> "The first value of Sepal.Length is 5.1" 
#>                              Sepal.Width 
#>  "The first value of Sepal.Width is 3.5" 
#>                             Petal.Length 
#> "The first value of Petal.Length is 1.4" 
#>                              Petal.Width 
#>  "The first value of Petal.Width is 0.2" 
#>                                  Species 
#>   "The first value of Species is setosa"

#If the vector is unnamed, the second argument will be the index:
x <- map(1:6,~sample(1000,10))
imap_chr(x,~paste0("The hightest value of ",.y," is ",max(.x)))

pmap_dbl(list(xs, ws), weighted.mean)
#> [1]    NA 0.451 0.603 0.452 0.563 0.510 0.342 0.464
pmap_dbl(list(xs,ws),weighted.mean,na.rm=TRUE)

trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

pmap_dbl(list(trim = trims), mean, x = x)
#> [1] -6.6740  0.0210  0.0235  0.0151

params <- tibble::tribble(
  ~ n, ~ min, ~ max,
  1L,     0,     1,
  2L,    10,   100,
  3L,   100,  1000
)

pmap(params, runif)
#> [[1]]
#> [1] 0.332
#> 
#> [[2]]
#> [1] 53.5 47.6
#> 
#> [[3]]
#> [1] 231 715 515
modify(mtcars,1)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
names(cyls) <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
iwalk(cyls, ~ write.csv(.x, .y))

cd <- getwd()
mtcars %>% 
  split(mtcars$cyl) %>% 
  set_names(~file.path(temp,paste0("cyl-",.x,".csv"))) %>% 
  iwalk(~write_csv(.x,.y))

mtcars %>% 
  group_split(cyl)

#reduce
l <- map(1:4, ~ sample(1:10, 15, replace = T))
str(l)
#> List of 4
#>  $ : int [1:15] 7 1 8 8 3 8 2 4 7 10 ...
#>  $ : int [1:15] 3 1 10 2 5 2 9 8 5 4 ...
#>  $ : int [1:15] 6 10 9 5 6 7 8 6 10 8 ...
#>  $ : int [1:15] 9 8 6 4 4 5 2 9 9 6 ...

out <- l[[1]]
out <- intersect(out,l[[2]])
out <- intersect(out,l[[3]])
out <- intersect(out,l[[4]])
out

reduce(l,intersect)
reduce(l,union)

simple_reduce <- function(x,f){
  out <- x[[1]]
  for (i in seq(2,length(x))) {
    out <- f(out,x[[i]])
  }
  out
}

#accumulate
accumulate(l,intersect)
x <- c(4,3,10)
reduce(x,`+`)
accumulate(x,`+`)

do.call("complex", list(imaginary = 1:3))
do.call(paste, list(as.name("A"), as.name("B")), quote = TRUE)
