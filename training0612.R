str_extract_all("He(Tom) loves her (Mary),and she (Kate) loves him(Hope).",
  pattern = "\\(.+?\\)"
) %>% unlist()

library(readxl)
library(tidyverse)
setwd("C:/try_to_success")
coding <- read_excel("coding.xlsx", sheet = "coding")
score <- read_excel("coding.xlsx", sheet = "score")
score$學期 <- factor(score$學期)
glimpse(score)

score %>%
  ggplot(aes(學期, 平均成績, group = 1)) +
  geom_line(linejoin = "mitre") +
  geom_point()

coding <- coding %>%
  arrange(人數) %>%
  mutate(acc = cumsum(人數)) %>%
  rename(人数 = 人數, 修课单位 = 修課單位)

coding

coding %>%
  ggplot(aes(fct_reorder(修课单位, acc), acc, fill = 人数)) +
  geom_col(position = "stack")

coding %>%
  ggplot(aes(fct_reorder(修课单位, acc), acc)) +
  geom_col(aes(factor(length(修课单位), acc)),
    position = "stack"
  )

coding %>%
  ggplot(aes(factor(1), 人数, fill = 修课单位)) +
  geom_col(width = 1)

coding %>%
  ggplot(aes(fct_reorder(修课单位, 人数), 人数, group = 1)) +
  geom_line() +
  geom_col() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 70, size = 10, vjust = 0.4)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

coding %>%
  ggplot(aes(x = factor(1), fill = fct_reorder(修课单位, 人数))) +
  geom_bar(width = 1) +
  coord_polar("y")

data$group <- factor(data$group)
fct_reorder(data$group, data$value)
ggplot(data, aes(x = "", y = value, fill = group)) +
  geom_bar(aes(y = fct_reorder(group, value, .desc = TRUE)), stat = "identity", width = 1) +
  coord_polar("y", start = 0)

# prepare data
stacked_df <- gapminder %>%
  filter(year == 2007) %>%
  mutate(lifeExpGrouped = cut(lifeExp,
    breaks = c(0, 50, 65, 80, 90),
    labels = c("Under 50", "50-65", "65-80", "80+")
  )) %>%
  group_by(continent, lifeExpGrouped) %>%
  summarise(continentPop = sum(as.numeric(pop)))

# set order of stacks by changing factor levels
stacked_df$lifeExpGrouped <- factor(stacked_df$lifeExpGrouped, levels = rev(levels(stacked_df$lifeExpGrouped)))

# create plot
stacked_bars <- ggplot(
  data = stacked_df,
  aes(
    x = continent,
    y = continentPop,
    fill = lifeExpGrouped
  )
) +
  geom_bar(
    stat = "identity",
    position = "fill"
  ) +
  # bbc_style() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(direction = -1) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  labs(
    title = "How life expectancy varies",
    subtitle = "% of population by life expectancy band, 2007"
  ) +
  theme(
    legend.position = "top",
    legend.justification = "left"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

install.packages("echarts4r")
library(echarts4r)
df <- data.frame(val = c(0.9, 0.8, 0.6))
df <- df %>%
  mutate(color = c("#FFD700", "yellow", "#F2FC83"))
df %>%
  e_charts() %>%
  e_liquid(val) %>%
  e_theme("dark")
df %>%
  e_charts() %>%
  e_liquid(val, color) %>%
  e_theme("purple-passion")

date <- c(
  "2017-01-01",
  "2017-01-02",
  "2017-01-03",
  "2017-01-04",
  "2017-03-05",
  "2017-01-06",
  "2017-01-07"
)
stock <- data.frame(
  date = date,
  opening = c(200.60, 200.22, 198.43, 199.05, 203.54, 203.40, 208.34),
  closing = c(200.72, 198.85, 199.05, 203.73, 204.08, 208.11, 211.88),
  low = c(197.82, 198.07, 197.90, 198.10, 202.00, 201.50, 207.60),
  high = c(203.32, 200.67, 200.00, 203.95, 204.90, 208.44, 213.17)
)
stock %>%
  e_charts(date) %>%
  e_candle(opening, closing, low, high) %>%
  e_y_axis(min = 190, max = 220)

library(dplyr)
df <- tibble(
  name = "earth",
  # 1st level
  children = list(
    tibble(
      name = c("land", "ocean"),
      # 2nd level
      children = list(
        tibble(name = c("forest", "river")),
        # 3rd level
        tibble(
          name = c("fish", "kelp"),
          children = list(
            tibble(
              name = c("shark", "tuna"),
              # 4th level
              NULL # kelp
            )
          )
        )
      )
    )
  )
)
df %>%
  e_charts() %>%
  e_tree(initialTreeDepth = 3, label = list(offset = c(0, -11)))
