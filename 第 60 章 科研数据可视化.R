library(tidyverse)

#统计分布图
lincoln_df <- ggridges::lincoln_weather %>%
  mutate(
    month_short = fct_recode(
      Month,
      Jan = "January",
      Feb = "February",
      Mar = "March",
      Apr = "April",
      May = "May",
      Jun = "June",
      Jul = "July",
      Aug = "August",
      Sep = "September",
      Oct = "October",
      Nov = "November",
      Dec = "December"
    )
  ) %>%
  mutate(month_short = fct_rev(month_short)) %>%
  select(Month, month_short, `Mean Temperature [F]`)

lincoln_df %>%
  head(5)

#60.1.1 points-errorbars
lincoln_errbar <- lincoln_df %>%
  ggplot(aes(x = month_short, y = `Mean Temperature [F]`)) +
  stat_summary(
    fun.y = mean, fun.ymax = function(x) {
      mean(x) + 2 * sd(x)
    },
    fun.ymin = function(x) {
      mean(x) - 2 * sd(x)
    }, geom = "pointrange",
    fatten = 5
  ) +
  xlab("month") +
  ylab("mean temperature (°F)") +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(3, 7, 3, 1.5)
  )

lincoln_errbar

#相线图
lincoln_box <- lincoln_df %>%
  ggplot(aes(x = month_short, y = `Mean Temperature [F]`)) +
  geom_boxplot(fill = "grey90") +
  xlab("month") +
  ylab("mean temperature (°F)") +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(3, 7, 3, 1.5)
  )

lincoln_box

#小提琴图
lincoln_violin <- lincoln_df %>%
  ggplot(aes(x = month_short, y = `Mean Temperature [F]`)) +
  geom_violin(fill = "grey90") +
  xlab("month") +
  ylab("mean temperature (°F)") +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(3, 7, 3, 1.5)
  )

lincoln_violin

#sina图
lincoln_points <- lincoln_df %>%
  ggplot(aes(x = month_short, y = `Mean Temperature [F]`)) +
  geom_point(size = 0.75) +
  xlab("month") +
  ylab("mean temperature (°F)") +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(3, 7, 3, 1.5)
  )

lincoln_points

lincoln_jitter <- lincoln_df %>%
  ggplot(aes(x = month_short, y = `Mean Temperature [F]`)) +
  geom_point(position = position_jitter(width = .15, height = 0, seed = 320), size = 0.75) +
  xlab("month") +
  ylab("mean temperature (°F)") +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(
      color = "black",
      size = 12
    ),
    plot.margin = margin(3, 7, 3, 1.5)
  )

lincoln_jitter

lincoln_sina <- lincoln_df %>%
  ggplot(aes(x = month_short, y = `Mean Temperature [F]`)) +
  geom_violin(color = "transparent", fill = "gray90") +
  # dviz.supp::stat_sina(size = 0.85) +
  geom_jitter(width = 0.25, size = 0.85) +
  xlab("month") +
  ylab("mean temperature (°F)") +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(
      color = "black",
      size = 12
    ),
    plot.margin = margin(3, 7, 3, 1.5)
  )

lincoln_sina
