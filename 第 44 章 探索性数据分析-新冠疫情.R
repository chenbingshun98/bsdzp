library("tidyverse")
library(lubridate)
library(maps)
library(viridis)
library(ggrepel)
library(paletteer)
library(shadowtext)
library(showtext)
showtext_auto()

setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
d <- read.csv("time_series_covid19_confirmed_global.csv")
d <- data.table::fread("time_series_covid19_confirmed_global.csv")
d <- as_tibble(d)
d
glimpse(d)

d %>% select(-c(1:4)) 
d %>% select(5:ncol(.))
d %>% select(matches("/20")) %>% view()
d %>% select(ends_with("/20"))


table4a
longer <- table4a %>% 
  pivot_longer(
    cols = `1999`:`2000`,
    names_to = "year",
    values_to = "cases"
  )
longer


longer %>%
  pivot_wider(
    names_from = year,
    values_from = cases
  )

#日期格式
c("2020-3-25", "20200325", "20-03-25", "2020 03 25") %>% lubridate::ymd()

c("3/25/20", "03-25-20", "3-25/2020") %>% lubridate::mdy()

lubridate::dmy(010210)
lubridate::dym(010210)
lubridate::mdy(010210)
lubridate::myd(010210)
lubridate::ymd(010210)
lubridate::ydm(010210)

difftime(ymd("2020-03-24"),
         ymd("2020-03-23"),
         units = "days"
)
ymd("2020-03-24") - ymd("2020-03-23")

(ymd("2020-03-24") - ymd("2020-03-23")) %>% as.numeric()

tb <- tibble(
  days_since_100 = 0:18,
  cases = 100 * 1.33^days_since_100
)
tb

p1 <- tb %>%
  ggplot(aes(days_since_100, cases)) +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1)
p1
p2 <- tb %>%
  ggplot(aes(days_since_100, log10(cases))) +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1)


p3 <- tb %>%
  ggplot(aes(days_since_100, cases)) +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  scale_y_log10()

library(patchwork)
p1 + p2 + p3

d <- d %>% rename_at(.vars = vars(starts_with("X")),
                .funs = funs(sub("^X", "", .)))
#数据清洗规整
d1 <- d %>% 
  pivot_longer(
    cols = 5:ncol(.),
    names_to = "date",
    values_to = "cases"
  ) %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  janitor::clean_names() %>% 
  group_by(country_region, date) %>% 
  summarise(cases = sum(cases)) %>% 
  ungroup()

d1 %>%
  group_by(date) %>%
  summarise(confirmed = sum(cases))

#
d1 %>% 
  group_by(date) %>% 
  summarise(confirmed = sum(cases)) %>% 
  ggplot(aes(x = date, y = confirmed))+
  geom_point()+
  scale_x_date(
    date_labels = "%m-%d",
    date_breaks = "1 week"
  )+
  scale_y_continuous(
    breaks = c(0,50000, 100000, 200000,
               300000, 500000, 900000),
    labels = scales::comma
  )

# d1 %>% distinct(country_region) %>% pull(country_region)
d1 %>% distinct(country_region)

d1 %>% 
  filter(country_region == "China")

d1 %>% 
  filter(country_region == "China") %>% 
  ggplot(aes(x = date, y = cases))+
  geom_point()+
  # scale_x_date(date_breaks = "1 week",
  #              date_labels = "%m-%d")+
  # scale_y_log10(labels = scales::comma)

d1 %>% 
  group_by(country_region) %>% 
  filter(max(cases) >= 20000) %>% 
  ungroup() %>% 
  ggplot(aes(x = date,
             y = cases,
             color = country_region))+
  geom_point()+
  scale_x_date(date_breaks = "1 week",
               date_labels = "%m-%d")+
  scale_y_log10()+
  facet_wrap(vars(country_region), ncol = 2)+
  theme(axis.title.x = element_text(angle = 45,
                                    hjust = 1))+
  theme(legend.position = "none")

#可视化探索
d2 <- d1 %>%
  group_by(country_region) %>%
  filter(max(cases) >= 100) %>%
  mutate(
    days_since_100 = date - min(date[cases >= 100])
  ) %>%
  mutate(days_since_100 = as.numeric(days_since_100)) %>%
  filter(days_since_100 >= 0) %>%
  ungroup()
d2

d2_most <- d2 %>% 
  group_by(country_region) %>% 
  top_n(1, days_since_100) %>% 
  filter(cases >= 10000) %>% 
  ungroup() %>% 
  arrange(desc(cases))

d2_most

d2 %>%
  bind_rows(
    tibble(country = "33% daily rise", days_since_100 = 0:30) %>%
      mutate(cases = 100 * 1.33^days_since_100)
  ) %>%
  ggplot(aes(days_since_100, cases, color = country_region)) +
  geom_hline(yintercept = 100) +
  geom_vline(xintercept = 0) +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  scale_y_log10(
    expand = expansion(mult = c(0, .1)),
    breaks = c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000),
    labels = scales::comma
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, .1)),
    breaks = c(0, 5, 10, 15, 20, 25, 30, 40, 50, 60)
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FFF1E6"),
    legend.position = "none",
    panel.spacing = margin(3, 15, 3, 15, "mm")
  ) +
  labs(
    x = "Number of days since 100th case",
    y = "",
    title = "Country by country: how coronavirus case trajectories compare",
    subtitle = "Cumulative number of cases, by Number of days since 100th case",
    caption = "data source from @www.ft.com"
  ) +
  gghighlight::gghighlight(country_region %in% highlight,
                           label_key = country_region, use_direct_label = TRUE,
                           label_params = list(segment.color = NA, nudge_x = 1),
                           use_group_by = FALSE
  )

d2 %>%
  bind_rows(
    tibble(country = "33% daily rise", days_since_100 = 0:30) %>%
      mutate(cases = 100 * 1.33^days_since_100)
  ) %>%
  
  ggplot(aes(days_since_100, cases, color = country_region)) +
  geom_hline(yintercept = 100) +
  geom_vline(xintercept = 0) +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  #   scale_colour_manual(
  #    values = c(
  #     "US" = "#EB5E8D",
  #     "Italy" = "black", 
  #     "Spain" = "#c2b7af",
  #     "China" = "red",
  #     "Germany" = "#c2b7af",
  #     "France" = "#c2b7af",
  #     "Iran" = "#9dbf57",
  #     "United Kingdom" = "#ce3140",
  #     "Korea, South" = "#208fce",
#     "Japan" = "#208fce",
#     "Singapore" = "#1E8FCC",
#      "33% daily rise" = "#D9CCC3",
#     "Switzerland" = "#c2b7af",
#     "Turkey" = "#208fce",
#     "Belgium" = "#c2b7af",
#     "Netherlands" = "#c2b7af",
#     "Austria" = "#c2b7af",
#     "Hong Kong" = "#1E8FCC",
#     # gray
#     "India" = "#c2b7af",
#     "Switzerland" = "#c2b7af",
#     "Belgium" = "#c2b7af",
#     "Norway" = "#c2b7af",
#      "Sweden" = "#c2b7af",
#     "Austria" = "#c2b7af",
#     "Australia" = "#c2b7af",
#     "Denmark" = "#c2b7af",
#     "Canada" = "#c2b7af",
#     "Brazil" = "#c2b7af",
#     "Portugal" = "#c2b7af"
#   )
# ) +

geom_shadowtext(
  data = d2_most, aes(label = paste0("  ", country_region)),
  bg.color = "white"
) +
  scale_y_log10(
    expand = expansion(mult = c(0, .1)),
    breaks = c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000),
    labels = scales::comma
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, .1)),
    breaks = c(0, 5, 10, 15, 20, 25, 30)
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FFF1E6"),
    legend.position = "none",
    panel.spacing = margin(3, 15, 3, 15, "mm")
  ) +
  labs(
    x = "Number of days since 100th case",
    y = "",
    title = "Country by country: how coronavirus case trajectories compare",
    subtitle = "Cumulative number of cases, by Number of days since 100th case",
    caption = "data source from @www.ft.com"
  )

#简便
d2a <- d1 %>%
  group_by(country_region) %>%
  filter(cases >= 100) %>%
  mutate(days_since_100 = 0:(n() - 1)) %>%
  # same as
  # mutate(edate = as.numeric(date - min(date)))
  ungroup()
d2a

# 疫情持续时间最久的国家
d3 <- d2a %>%
  group_by(country_region) %>%
  filter(days_since_100 == max(days_since_100)) %>%
  # same as
  # top_n(1, days_since_100) %>%
  ungroup() %>%
  arrange(desc(days_since_100))
d3
highlight <- d3 %>%
  top_n(10, days_since_100) %>%
  pull(country_region)
highlight

d2a %>%
  bind_rows(
    tibble(country = "33% daily rise", days_since_100 = 0:30) %>%
      mutate(cases = 100 * 1.33^days_since_100)
  ) %>%
  ggplot(aes(days_since_100, cases, color = country_region)) +
  geom_hline(yintercept = 100) +
  geom_vline(xintercept = 0) +
  geom_line(size = 0.8) +
  geom_point(pch = 21, size = 1) +
  scale_y_log10(
    expand = expansion(mult = c(0, .1)),
    breaks = c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000),
    labels = scales::comma
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, .1)),
    breaks = c(0, 5, 10, 15, 20, 25, 30, 40, 50, 60)
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FFF1E6"),
    legend.position = "none",
    panel.spacing = margin(3, 15, 3, 15, "mm")
  ) +
  labs(
    x = "Number of days since 100th case",
    y = "",
    title = "Country by country: how coronavirus case trajectories compare",
    subtitle = "Cumulative number of cases, by Number of days since 100th case",
    caption = "data source from @www.ft.com"
  ) +
  gghighlight::gghighlight(country_region %in% highlight,
                           label_key = country_region, use_direct_label = TRUE,
                           label_params = list(segment.color = NA, nudge_x = 1),
                           use_group_by = FALSE
  )
