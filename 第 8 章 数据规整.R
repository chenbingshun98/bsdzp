library(tidyverse)
plant_height <- data.frame(
  Day = 1:5,
  A = c(0.7, 1.0, 1.5, 1.8, 2.2),
  B = c(0.5, 0.7, 0.9, 1.3, 1.8),
  C = c(0.3, 0.6, 1.0, 1.2, 2.2),
  D = c(0.4, 0.7, 1.2, 1.5, 3.2)
)
plant_height
# plant_height %>% 
#   filter( ___ >= 0.8)
# filter(plant_height,___>=0.8)

# plant_height %>% 
#   ggplot(aes(x = Day, y = ___, color = ___)) +
#   geom_line()

melted <- gather(plant_height, variable, value, 2:3)
melted
## Column names instead of indices
melted <- gather(plant_height, variable, value, A, B)
melted
## Excluding instead of including
melted <- gather(plant_height, variable, value, -1)
melted
## Excluding using column name
melted <- gather(plant_height, variable, value, -Day)
melted

#宽表格变成长表格
long <- plant_height %>%
  pivot_longer(
    cols = A:D,
    names_to = "plant",
    values_to = "height"
  )
long


plant_height %>% 
  pivot_longer(
    cols = -Day,         # A:D 或者 c(A, B, C, D) 或者 c("A", "B", "C", "D")
    names_to = "plant",
    values_to = "height"
  )

long %>% 
  ggplot(aes(x = Day, y = height, color = plant)) +
  geom_line()

#长表格变回宽表格呢？需要用到pivot_wider()
wide <- long %>% 
  pivot_wider(
    names_from = "plant",
    values_from = "height"
  )
wide

# 列名转换成多个变量
plant_record <- data.frame(
  day = c(1L, 2L, 3L, 4L, 5L),
  A_height = c(1.1, 1.2, 1.3, 1.4, 1.5),
  A_width = c(2.1, 2.2, 2.3, 2.4, 2.5),
  A_depth = c(3.1, 3.2, 3.3, 3.4, 3.5),
  B_height = c(4.1, 4.2, 4.3, 4.4, 4.5),
  B_width = c(5.1, 5.2, 5.3, 5.4, 5.5),
  B_depth = c(6.1, 6.2, 6.3, 6.4, 6.5),
  C_height = c(7.1, 7.2, 7.3, 7.4, 7.5),
  C_width = c(8.1, 8.2, 8.3, 8.4, 8.5),
  C_depth = c(9.1, 9.2, 9.3, 9.4, 9.5)
)
plant_record %>% 
  knitr::kable()

plant_record %>% 
  tidyr::pivot_longer(
    cols = !day,
    names_to = c("species", "parameter"),
    names_pattern = "(.*)_(.*)",#正则表达式
    values_to = "value"
  )

plant_record %>% 
  pivot_longer(
    cols = !day,
    names_to = c("species", "parameter"),
    names_pattern = "(.*)_(.*)",
    values_to = "val"
  )
#面板数据？
#我们希望原始数据框的列名中，
#一部分进入变量，一部分保持原来的列名，比如，

#复杂的情况
# 我们希望原始数据框的列名中，一部分进入变量，一部分保持原来的列名，比如，
plant_record_longer <- plant_record %>% 
  pivot_longer(
    cols = !day,
    names_to = c("species", ".value"),
    names_pattern = "(.*)_(.*)"
  )
plant_record_longer

plant_record_longer

us_rent_income %>% 
  pivot_wider(
    names_from = variable,
    names_glue = "{variable}_{.value}",
    values_from = c(estimate, moe)
    
  )
us_rent_income

plant_record_longer %>% 
  pivot_wider(
    names_from = species,
    values_from  = c(height,width,depth),
    names_glue = "{species}_{.value}"
  )

#看明白.value它代表的意思了吗？

# 注意 .value 而不是value，说明这里不是单个列名，
#而是匹配得到的多个值做列名