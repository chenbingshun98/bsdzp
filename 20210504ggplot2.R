library(tidyverse)
departures <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv")

departures %>%
  filter(fyear <= 2016 & fyear >= 1990) %>%
  mutate(
    reason = case_when(
      departure_code == 1 ~ "Involuntary", # height > 200 | mass > 200
      departure_code == 2 ~ "Involuntary",
      departure_code == 3 ~ "Involuntary",
      departure_code == 4 ~ "Involuntary",
      departure_code == 5 ~ "Voluntary",
      departure_code == 6 ~ "Voluntary",
      departure_code == 7 ~ "Other Reason",
      departure_code == 8 ~ "Missing",
      departure_code == 9 ~ "Other Reason",
      TRUE ~ "Missing"
    ),
    reason = factor(reason, levels = c(
      "Voluntary", "Involuntary",
      "Other Reason", "Missing"
    ))
  ) %>%
  group_by(fyear, reason) %>%
  count() %>%
  ggplot(aes(
    x = fyear, y = n,
    fill = as.factor(reason)
  )) +
  geom_col(
    position = "fill",
    alpha = 0.8
  ) +
  scale_fill_manual(values = c(
    "darkslategray4",
    "peachpuff3",
    "gray40",
    "black"
  )) +
  scale_x_continuous(breaks = scales::breaks_width(5)) +
  scale_y_continuous(
    labels = scales::label_percent(),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Turnover at the Top: voluntary DEO departures have decreased since 1992",
    subtitle = "CEO departures from S&P 1500 firms, 1992 - 2016",
    # x = NULL,
    # y = NULL,
    fill = NULL
  )

mat <- as.data.frame(round(cor(mtcars), 2))
mat$var1 <- row.names(mat)
data <- gather(mat, key = "var2", value = "corr", -var1)
data <- mat %>% pivot_longer(
  cols = !var1,
  names_to = "var2",
  values_to = "corr"
)
data

library(RColorBrewer)
my_color <- brewer.pal(5, "Spectral")

ggplot(data, aes(var2, var1, fill = corr)) +
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = my_color) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

data %>%
  ggplot(aes(var2, var1, fill = corr)) +
  geom_point(aes(size = abs(corr)),
    shape = 21,
    color = "black"
  ) +
  scale_fill_gradientn(colors = my_color) +
  scale_size_area(
    max_size = 15,
    guide = FALSE
  ) +
  geom_text(aes(label = corr),
    size = 3,
    color = "black",
    alpha = 0.8
  )

# 右上角为NA
for (i in 1:10) {
  for (j in (i + 1):11) {
    mat[i, j] <- NA
  }
}
mat

mat$var1 <- rownames(mat)
data <- mat %>%
  pivot_longer(
    cols = !var1,
    names_to = "var2",
    values_to = "corr"
  ) %>%
  mutate(
    var1 = factor(var1, levels = rownames(mat)),
    var2 = factor(var2, levels = rownames(mat)),
  )

data %>% ggplot(aes(var1, var2)) +
  geom_point(aes(
    fill = corr,
    size = corr
  ),
  shape = 21
  ) +
  geom_text(aes(label = corr),
    size = 3,
    color = "white"
  ) +
  scale_fill_gradientn(colors = my_color) +
  scale_size_area(
    max_size = 15,
    guide = FALSE
  ) +
  theme(legend.position = "none")

mat1 <- cor(mtcars) %>% round(., 2) %>% as.data.frame()
tibble::has_rownames(mat1)
for (i in 1:10) {
  for (j in (i + 1):11) {
    mat1[i, j] <- NA
  }
}
mat1

mat2 <- cor(mtcars) %>% round(., 2) %>% as.data.frame()

for (i in 1:11) {
  for(j in 1:i){
    mat2[i, j] <- NA
  }
}
mat1$var1 <- row.names(mat1)
mat2$var1 <- row.names(mat2)
mat1 <- mat1 %>% tibble::rownames_to_column(., var = "var1")
mat2 <- mat2 %>% tibble::rownames_to_column(., var = "var1")
tibble::has_rownames(mat1)
tibble::has_rownames(mat)
tibble::row
data1 <- mat1 %>% pivot_longer(
  cols = !var1,
  names_to = "var2",
  values_to = "corr"
) %>% 
  mutate(
    var1 = factor(var1, levels = rownames(mat1)),
    var2 = factor(var2, levels = rownames(mat1))
  )

data1 <- gather(mat1,
                key = "var2",
                value = "corr",
                -var1)%>% 
  mutate(
    var1 = factor(var1, levels = rownames(mat1)),
    var2 = factor(var2, levels = rownames(mat1))
  )

mat2$var1 <- rownames(mat2)

data2 <- mat2 %>% pivot_longer(
  cols = !var1,
  names_to = "var2",
  values_to = "corr"
) %>% 
  mutate(
    var1 = factor(var1, levels = rownames(mat2)),
    var2 = factor(var2, levels = rownames(mat2))
  )

data2 <- gather(mat2,
                key = "var2",
                value = "corr",
                -var1)%>% 
  mutate(
    var1 = factor(var1, levels = rownames(mat2)),
    var2 = factor(var2, levels = rownames(mat2))
  )
my_color <- brewer.pal(5, "Spectral")

ggplot(data1, aes(var1, var2))+
  geom_point(aes(fill = corr, 
                 size = corr),
             shape = 21)+
  geom_text(data = data2,
            aes(label = corr,
                color = corr),
            size = 5)+
  scale_fill_gradientn(colors = my_color)+
  scale_color_gradientn(colors = my_color)+
  scale_size_area(max_size = 15,
                  guide = FALSE)+
  theme(legend.position = "none")

###
var_name <- data1 %>% 
  filter(var1 == var2)

mat1$var1 <- rownames(mat1)
data1 <- gather(mat1, key = "var2", value = "corr", -var1) %>%
  mutate(var1 = factor(var1, levels = rownames(mat1)),
         var2 = factor(var2, levels = rownames(mat1)))

mat2$var1 <- rownames(mat2)
data2 <- gather(mat2, key = "var2", value = "corr", -var1) %>%
  mutate(var1 = factor(var1, levels = rownames(mat2)),
         var2 = factor(var2, levels = rownames(mat2)))

my_color <- brewer.pal(5, "Spectral")

ggplot(data1, aes(var1, var2)) +
  geom_tile(data = data2, aes(fill = corr)) +
  geom_text(data = data2, aes(label = corr), colour = "black", size = 5, na.rm = TRUE) +
  geom_point(aes(fill = corr, size = corr), shape = 21, na.rm = TRUE) +
  geom_text(data = var_name, aes(label = var1), size = 5) +
  scale_fill_gradientn(colours = my_color, na.value = "white") +
  scale_colour_gradientn(colours = my_color) +
  scale_size_area(max_size = 15, guide = FALSE) +
  scale_x_discrete(position = 't') +
  theme(
    panel.background = element_blank(),
    legend.position = "none",
    axis.title = element_blank()
  )

