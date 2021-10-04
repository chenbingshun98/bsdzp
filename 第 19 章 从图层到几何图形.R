# 19.1 图层的五大元素
# 数据data
# 美学映射mapping
# 几何图形geom
# 统计变换stat
# 位置调整position
# 我们现在按照layer() -> stat_*() -> geom_*()这个思路来，理解各种图形。

# 如果想要这些新变量映射到图形属性，就需要使用 after_stat()或者stage()函数，具体见下面的案例。

library(tidyverse)
library(palmerpenguins)
penguins <- penguins %>% drop_na()

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    stat = "identity",
    geom = "point",
    params = list(na.rm = FALSE),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  stat_identity(
    geom = "point"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  geom_point()

#stat_count()
penguins %>% 
  ggplot(aes(x = species)) +
  layer(
    stat = "count",
    geom = "bar",
    mapping = aes(y = after_stat(count)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species)) +
  layer(
    stat = "count",
    geom = "point",
    mapping = aes(y = after_stat(count)),
    position = "identity"
  )

# aes(y = after_stat(count)) 
# 可以看作是aes(y = stage(start = NULL, after_stat = count))的简写

penguins %>% 
  ggplot(aes(x = species)) +
  layer(
    stat = "count",
    geom = "bar",
    mapping = aes(y = stage(start = NULL, after_stat = count)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species, y = after_stat(count))) +
  stat_count(
    geom = "bar"       
  )

penguins %>% 
  ggplot(aes(x = species, y = after_stat(count)))+
  geom_bar(
    stat = "count"
  )

penguins %>% 
  ggplot(aes(x = species, y = after_stat(count)))+
  stat_count(
    geom = "point"
  )

penguins %>% 
  ggplot(aes(x = species, y = after_stat(count)))+
  geom_point(
    stat = "count"
  )

#19.5 stat_bin()
penguins %>% 
  ggplot(aes(x = bill_length_mm))+
  layer(
    stat = "bin",
    geom = "bar",
    mapping = aes(y = after_stat(count)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm)) +
  layer(
    stat = "bin",
    geom = "point",
    mapping = aes(x = stage(start = bill_length_mm, after_stat = x), 
                  y = after_stat(count)
    ),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = after_stat(count)))+
  stat_bin(
    geom = "point"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = after_stat(count))) +
  geom_bar(
    stat = "bin"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm))+
  layer(
    stat = "bin",
    geom = "bar",
    mapping = aes(y = after_stat(count)),
    position = 'identity'
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm)) +
  layer(
    stat = "bin",
    geom = "bar",
    mapping = aes(y = after_stat(ncount)), 
    position = 'identity'
  ) 

penguins %>% 
  ggplot(aes(x = bill_length_mm)) +
  stat_bin(
    mapping = aes(y = after_stat(count)),
    geom = "bar",
    position = 'identity'
  ) 

penguins %>% 
  ggplot(aes(x = bill_length_mm)) +
  geom_histogram(
    mapping = aes(y = after_stat(count)),
    stat = "bin",
    position = 'identity'
  ) 

#复杂点的geom_histogram()
penguins %>% 
  ggplot(aes(x = bill_length_mm, fill = sex))+
  layer(
    mapping = aes(y = after_stat(density)),
    geom = "bar",
    stat = "bin",
    position = "dodge"
  )+
  facet_wrap(vars(species))

penguins %>% 
  ggplot(aes(x = bill_length_mm, fill = sex))+
  stat_bin(
    mapping = aes(y = after_stat(density)),
    geom = "bar",
    position = "dodge"
  )+
  facet_wrap(vars(species))

penguins %>% 
  ggplot(aes(x = bill_length_mm, fill = sex))+
  geom_histogram(
    aes(y = after_stat(density)),
    position = "dodge"
  )+
  facet_wrap(vars(species))

#stat_density()
kernel = c("gaussian", "epanechnikov", "rectangular",
           "triangular", "biweight",   "cosine", 
           "optcosine")

penguins %>% 
  ggplot(aes(x = bill_length_mm))+
  layer(
    stat = "density",
    geom = "area",
    params = list(kernel = "gaussian"),
    position = "identity"
  )

penguins %>%
  ggplot(aes(x = bill_length_mm)) +
  layer(
    stat = "density",
    geom = "line",
    params = list(kernel = "gaussian"),
    position = "identity"
  )

penguins %>%
  ggplot(aes(x = bill_length_mm)) +
  layer(
    stat = "density",
    geom = "point",
    params = list(kernel = "gaussian"),
    position = "identity"
  )

penguins %>%
  ggplot(aes(x = bill_length_mm)) +
  stat_density(
    geom = "point",
    kernel = "gaussian"
  )

# stat_boxplot()
penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  layer(
    stat = "boxplot",
    geom = "boxplot",
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  stat_boxplot(
    geom = "boxplot"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  geom_boxplot()

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  layer(
    stat = "boxplot",
    geom = "boxplot",
    mapping = aes(color = after_stat(middle)),#中位数
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  layer(
    stat = "boxplot",
    geom = "point",
    mapping = aes(y = after_stat(width)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  layer(
    stat = "boxplot",
    geom = "point",
    mapping = aes(y = stage(bill_length_mm,
                            after_stat = notchupper)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  layer(
    stat = "boxplot",
    geom = "point",
    mapping = aes(y = stage(bill_length_mm, after_stat = ymax)),
    position = "identity"
  ) 

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  stat_boxplot()

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  layer(
    stat = "boxplot",
    geom = "point",
    mapping = aes(y = stage(bill_length_mm, after_stat = middle)),
    params = list(color = "red", size = 5),
    position = "identity"
  ) 

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  geom_boxplot(
    aes(color = species,
        fill = after_scale(alpha(color, 0.4)))
  )

#19.8 stat_ydensity()
penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_point() +
  layer(
    geom     = "violin",
    stat     = "ydensity",
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_point() +
  layer(
    geom     = "point",
    stat     = "ydensity",
    position = "identity"
  )

#stat_bindot()
penguins %>% 
  ggplot(aes(x = bill_length_mm))+
  layer(
    stat = "bindot",
    geom = "dotplot",
    mapping = aes(y = stage(start = NULL, 
                            after_stat = count)),
    params = list(binwidth = 1,
                  dotsize = 0.5),
    position = position_nudge(-0.025)
  )

  
penguins %>% 
  ggplot(aes(x = bill_length_mm))+
  layer(
    stat = "bindot",
    geom = "point",
    mapping = aes(y = stage(start = NULL,
                            after_stat = count)),
    params = list(binwidth = 1),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm))+
  geom_dotplot(
    binwidth = 1,
    dotsize = 0.5
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  geom_dotplot(
    binaxis = "y",
    stackdir = "down",
    dotsize = 0.4,
    position = position_nudge(-0.025)
  )

#stat_sum()
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    stat = "sum",
    geom = "point",
    mapping = aes(size = after_stat(n)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  stat_sum(
    geom = "point"
  )

#stat_smooth
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    geom = "smooth",
    stat = "smooth",
    params = list(se = TRUE),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  stat_smooth(
    geom = "smooth",
    se = TRUE
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  geom_smooth(se = TRUE)

# 统计转换后，可以根据 Computed variables 画出更多的几何图形
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  layer(
    geom = "point",
    stat = "smooth",
    mapping = aes(size = after_stat(ymax), color = after_stat(ymin)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  layer(
    geom = "point",
    stat = "smooth",
    mapping = aes(color = after_stat(ymin)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    geom = "point",
    stat = "smooth",
    mapping = aes(color = stage(NULL, after_stat = ymin)),
    position = "identity"
  )

penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  layer(
    geom = "line",
    stat = "smooth", 
    mapping = aes(color = after_stat(ymin)),    
    position = "identity"
  )

penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  layer(
    geom = "pointrange",
    stat = "smooth", 
    mapping = aes(color = after_stat(se)),    
    position = "identity"
  )

penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  layer(
    stat = "smooth",
    mapping = aes(color = after_stat(y)),    
    geom = "point",
    params = list(method  = "lm", formula = y ~ splines::ns(x, 2)),    
    position = "identity"
  )

#stat_bin_2d()
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  layer(
    geom = "tile",
    stat = "bin_2d",
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  layer(
    geom = "point",
    stat = "bin_2d",
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  stat_bin_2d(
    geom = "point"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(
    stat = "bin_2d"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    geom = "point",
    stat = "bin_2d",
    mapping = aes(size = after_stat(count)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    geom = "tile",
    stat = "bin_2d",
    mapping = aes(fill = after_stat(count)),
    position = "identity"
  )

#stat_bin_hex()
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  layer(
    geom = "hex",
    stat = "binhex",
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  stat_bin_hex(
    geom = "hex"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_hex(
    stat = "binhex"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    geom = "text",
    stat = "binhex",
    mapping = aes(label = stage(NULL, after_stat = count)),
    position = "identity"
  )

# stat_density_2d()
#不计算等高线 (contour = FALSE)
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    stat = "density_2d",
    geom = "path",
    params = list(contour = TRUE),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  stat_density_2d(
    contour = TRUE
  )

penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_density_2d()

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  geom_path(
    stat = "density_2d",
    contour = TRUE
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    stat = "density_2d",
    geom = "point",
    params = list(contour = TRUE),
    position = "identity"
  )

penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  layer(
    stat = "density_2d",
    geom = "polygon",
    mapping = aes(fill = after_stat(level)),
    params = list(contour = TRUE),
    position = "identity"
  )

#看看无等高线的情形
penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  layer(
    stat = "density_2d",
    geom = "raster",
    mapping = aes(fill = after_stat(density)),
    params = list(contour = FALSE),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    stat = "density_2d",
    geom = "tile",
    mapping = aes(fill = after_stat(count)),
    params = list(contour = FALSE),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  stat_density2d(
    geom = "tile",
    mapping = aes(fill = after_stat(density)),
    contour = FALSE
  )

penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_tile(
    stat = "density_2d",
    mapping = aes(fill = after_stat(density)),
    contour = FALSE
  )

#可以根据 Computed variables 画出更多的几何图形
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    stat = "density_2d",
    geom = "point",
    mapping = aes(size = after_stat(count)),
    params = list(n = 20, contour = FALSE),
    position = "identity"
  )

#stat_ellipse()
#假定数据服从多元分布，计算椭圆图形需要的参数
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  geom_point()+
  layer(
    stat = "ellipse",
    geom = "path",
    params = list(type = "norm", linetype = 2),
    position = "identity"
  )

penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  stat_ellipse(
    geom = "path",
    type = "norm", 
    linetype = 2
  )

penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_path(
    stat = "ellipse",
    type = "norm", 
    linetype = 2
  )

#可以根据 Computed variables 画出更多的几何图形
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  geom_point()+
  layer(
    stat = "ellipse",
    geom = "path",
    mapping = aes(color = after_stat(y)),
    params = list(type = "norm"),
    position = "identity"
  )

#stat_summary
penguins %>%
  ggplot(aes(x = species, y = bill_length_mm)) +
  layer(
    stat = "summary", 
    params = list(fun.data = "mean_cl_normal"),
    geom = "errorbar", 
    position = "identity"
  )
?mean_cl_normal

penguins %>% 
  ggplot(aes(x = species, y = bill_depth_mm))+
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar"
  )

penguins %>%
  ggplot(aes(x = sex, y = bill_length_mm)) +
  layer(
    stat     = "summary",
    geom     = "point",
    mapping  = aes(size = after_stat(ymin)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  geom_point()+
  layer(
    geom = "point",
    stat = "summary",
    params = list(fun = "mean",
                  color = "red",
                  size = 5),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  layer(
    geom = "point", 
    stat = "summary",
    params = list(fun = median), 
    mapping = aes(y = stage(start = bill_length_mm, after_stat = y)),
    position = "identity"
  )

penguins %>%
  ggplot(aes(x = sex, y = bill_length_mm)) +
  geom_point() +
  layer(
    geom = "pointrange",   
    stat = "summary", 
    params = list(fun.data = ~mean_se(., mult = 5), color = "red", size = 2),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_point() +
  stat_summary(
    geom  = "point",
    fun   = "mean",
    color = "red", 
    size  = 5
  )

penguins %>%
  ggplot(aes( x = body_mass_g, y = species)) +
  geom_jitter() +
  stat_summary(
    fun = mean, 
    geom = "point", 
    size = 5, 
    color = "red",
    alpha = 1
  )

penguins %>%
  ggplot(aes(x = sex, y = bill_length_mm)) +
  geom_point() +
  stat_summary(  
    fun.data = ~mean_se(., mult = 5),
    color = "red",
    geom = "pointrange",    
    size = 2
  )

penguins %>%
  ggplot(aes(x = sex, y = bill_length_mm)) +
  geom_point() +
  geom_pointrange(
    stat = "summary", 
    fun.data = ~mean_se(., mult = 5),
    color = "red",
    size = 2
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm,
             group = sex))+
  geom_point()+
  stat_summary(
    fun.data = ~mean_se(., mult = 2),
    color = "red",
    geom = "pointrange"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm,
             group = sex))+
  stat_summary(
    fun = mean,
    fun.min = function(x)mean(x) - sd(x),
    fun.max = function(x)mean(x) + sd(x),
    geom = "pointrange"
  )+
  stat_summary(
    fun = mean,
    geom = "line"
  )+
  facet_wrap(~sex)

my_count <- function(x){
  tibble(
    y = length(x),
  )
}


penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  stat_summary(
    geom = "bar",
    fun.data = my_count
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_bar(
    stat = "summary",
    fun.data = my_count,
  )
penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  layer(
    geom = "bar",
    stat = "summary",
    params = list(fun.data = my_count),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_point() +
  stat_summary(
    geom = "point",
    fun = "mean",
    color = "red", 
    size = 5
  ) +
  stat_summary(
    aes(label = after_stat(y)),
    geom = "text",
    fun.data = "mean_se",
    color = "red", 
    size = 5
  )

n_fun <- function(x) {
  data.frame(y = 62,
             label = length(x),
             color = ifelse(length(x) > 100, "red", "blue")
  )
}


penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm)) +
  geom_boxplot() +
  geom_jitter() +
  stat_summary(
    fun.data = n_fun,
    geom = "text"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  geom_point()+
  stat_summary(
    geom = "pointrange",
    fun.data = "mean_cl_boot",
    color = "red"
  )

penguins %>% 
  ggplot(aes(x = species, y = bill_length_mm))+
  geom_point()+
  stat_summary(
    geom = "pointrange",
    fun.data = ~mean_se(., mult = 5),
    color = "red",
    size = 1
  )+
  stat_summary(
    fun = "mean",
    geom = "text",
    mapping = aes(y = stage(bill_length_mm, after_stat = 30),
                  label = round(after_stat(y), 2)),
    color = "blue",
    size = 5
  )+
  stat_summary(
    fun = "length",
    geom = "text",
    mapping = aes(y = stage(bill_length_mm,
                            after_stat = 62),
                  label = after_stat(y)),
    color = "black",
    size = 5
  )

calc_median_and_fill <- function(x, threshold = 40){
  tibble::tibble(
    y = median(x),
    fill = if_else(y < threshold, "red", "gray50")
  )
}

penguins %>%
  ggplot(aes(x = species, y = bill_length_mm)) +
  stat_summary(
    fun.data = calc_median_and_fill, 
    geom = "bar" 
  )

calc_median_and_color <- function(x, threshold = 40) {
  tibble(
    y = median(x),
    color = if_else(y < threshold, "red", "gray50")
  )
}

penguins %>%
  ggplot(aes(x = species, y = bill_length_mm)) +
  stat_summary(
    fun.data = calc_median_and_color, 
    geom = "point",
    size = 5
  )

#
penguins %>% 
  ggplot(aes(species, bill_depth_mm))+
  stat_summary(
    fun.data = function(x){
      scaled_size <- length(x) / nrow(penguins)
      
      mean_se(x) %>% 
        mutate(size = scaled_size)
    }
  )

penguins %>% 
  ggplot(aes(species, bill_depth_mm))+
  geom_point(position = position_jitter(width = .2), alpha = .3)+
  stat_summary(
    fun = mean,
    na.rm = TRUE,
    geom = "point",
    color = "dodgerblue",
    size = 4,
    shape = "diamond"
  )+
  stat_summary(
    fun.data = mean_cl_normal,
    na.rm = TRUE,
    geom = "errorbar",
    width = .2,
    color = "dodgerblue"
  )+
  stat_summary(
    fun = mean,
    na.rm = TRUE,
    aes(group = 1),
    geom = "line",
    color = "dodgerblue",
    size = .75
  )

penguins %>% 
  ggplot(aes(species, 
             bill_depth_mm,
             group = sex,
             color = sex))+
  geom_point(
    position = position_jitterdodge(
      jitter.width = .2,
      dodge.width = .7
    ),
    alpha = .1
  )+
  stat_summary(
    fun = mean,
    na.rm = TRUE,
    geom = "point",
    shape = "diamond",
    size = 4,
    color = "black",
    position = position_dodge(width = .7)
    )+
  stat_summary(
    fun.data = mean_cl_normal,
    na.rm = TRUE,
    geom = "errorbar",
    width = .2,
    color = "black",
    position = position_dodge(width = .7)
  )+
  scale_color_brewer(palette = "Set1")

penguins %>%
  ggplot(aes(species, bill_depth_mm, group = sex, color = sex)) +
  geom_point(
    position = position_jitterdodge(
      jitter.width = .2,
      dodge.width = .7
    ),
    alpha = .1
  ) +
  stat_summary(
    fun = mean,
    na.rm = TRUE,
    geom = "point",
    shape = "diamond",
    size = 4,
    color = "black",
    position = position_dodge(width = .7)
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
    na.rm = TRUE,
    geom = "errorbar",
    width = .2,
    color = "black",
    position = position_dodge(width = .7)
  ) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~sex)

#stat_summary_bin
penguins %>% 
  ggplot(aes(x = bill_depth_mm,
             y = bill_length_mm))+
  layer(
    stat = "summary_bin",
    geom = "bar",
    params = list(fun = mean,
                  color = "red",
                  orientation = "x"),
    position = "identity"
  )

penguins %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  stat_summary_bin(   
    fun = mean,
    color = "red",
    geom = "bar",
    orientation = 'x'     # bin在 x 上，summary mean on y
  )

penguins %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_bar(
    stat = "summary_bin",
    fun = mean,
    color = "red"
  )

penguins %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  stat_summary_bin( 
    fun = mean,
    color = "red",
    geom = "bar",
    orientation = 'y'  # bin在 y 上，summary mean on x
  )

#stat_functtion()
tibble(x = runif(n = 100, min = -5, max = 5)) %>% 
  ggplot()+
  layer(
    stat = "function",
    geom = "point",
    params = list(fun = dnorm,
                  args = list(mean = 0, sd = 0.5)),
    position = "identity"
  )+
  xlim(-2, 2)

tibble(x = runif(n = 100, min = -5, max = 5)) %>% 
  ggplot()+
  layer(
    stat = "function",
    geom = "point",
    params = list(fun = ~0.5*exp(-abs(.x))),
    position = "identity"
  )+
  xlim(-2, 2)

#stat_spoke
penguins %>% 
  mutate(angle = flipper_length_mm / (2 * pi)) %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm))+
  layer(
    stat = "identity",
    geom = "spoke",
    mapping = aes(angle = angle),
    params = list(radius = 0.5),
    position = "identity"
  )

penguins %>% 
  mutate(angle = flipper_length_mm / (2 * pi)) %>% 
  ggplot(aes(x = bill_length_mm,
             y = bill_depth_mm))+
  geom_spoke(
    mapping = aes(angle = angle),
    radius = 0.5
  )

#stat_quantile
quantreg::rq(
  bill_depth_mm~bill_length_mm,
  data = penguins,
  tau = c(0.25, 0.5, 0.75)
)

penguins %>% 
  ggplot(aes(x = bill_length_mm,
             y = bill_depth_mm))+
  layer(
    stat = "quantile",
    geom = "quantile",
    params = list(quantiles = c(0.25, 0.5, 0.75)),
    position = "identity"
  )

penguins %>% 
  ggplot(aes(x = bill_length_mm,
             y = bill_depth_mm))+
  layer(
    stat = "quantile",
    geom = "point",
    mapping = aes(color = after_stat(quantile)),
    params = list(quantiles = c(0.25, 0.5, 0.75)),
    position = "identity"
  )

# stat_summary_2d()
