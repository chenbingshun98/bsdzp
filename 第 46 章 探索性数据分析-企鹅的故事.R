library(tidyverse)
setwd("C:/Users/terry/Documents/我爱学习/统计软件/R/R_for_Data_Science-master/R_for_Data_Science-master/demo_data")
penguins <- read.csv("penguins.csv") %>%
  janitor::clean_names()

penguins %>%
  head()

#数据清洗
#检查缺失值
#across :Apply a function (or functions) across multiple columns

#everything():selects all variable. 
#It is also useful in combination with other tidyselect operators.
penguins %>% summarise(
  across(everything(), ~ sum(is.na(.)))
)

#有缺失值的地方找出来看看
penguins %>% filter_all(
  any_vars(is.na(.))
)

penguins <- penguins %>% drop_na()
penguins

#计算每一种类型的企鹅有多少只
penguins %>%
  count(species, sort = T)

#计算每一座岛屿有多少只企鹅
penguins %>%
  count(island, sort = T)

#每种类型企鹅各种体征属性的均值和分布
penguins %>%
  group_by(species) %>%#以种类分组
  summarize(across(where(is.numeric), mean, na.rm = TRUE))#计算数值型数据的均值

#每种类型企鹅的嘴峰长度的分布
library(ggplot2)
penguins %>%
  ggplot(aes(x = bill_length_mm)) +
  geom_density() +
  facet_wrap(vars(species), scales = "free")

#每种类型企鹅的嘴峰长度的分布（分性别）
penguins %>%
  ggplot(aes(x = bill_length_mm)) +
  geom_density(aes(fill = sex),alpha = 0.5) +
  facet_wrap(vars(species), scales = "free")#	Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?

penguins %>%
  ggplot(aes(x = bill_length_mm, fill = sex)) +
  geom_histogram(
    position = "identity",
    alpha = 0.7,
    bins = 25
  ) +
  scale_fill_manual(values = c("#66b3ff", "#8c8c8c")) +
  ylab("number of penguins") +
  xlab("length (mm)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(color = "white", size = 10),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 12, hjust = 1)
  ) +
  facet_wrap(vars(species), scales = "free")

install.packages("ggridges")
library(ggridges)
penguins %>%
  ggplot(aes(x = bill_length_mm, y = species, fill = species)) +
  ggridges::geom_density_ridges()

penguins %>%
  ggplot(aes(x = bill_length_mm, y = species, fill = sex)) +
  geom_density_ridges(alpha = 0.5)

penguins %>%
  ggplot(aes(x = bill_depth_mm, fill = species)) +
  ggridges::geom_density_ridges(aes(y = species))

penguins %>%
  ggplot(aes(x = bill_depth_mm, fill = sex)) +
  ggridges::geom_density_ridges(aes(y = species),alpha = 0.6)


penguins %>%
  ggplot(aes(x = body_mass_g, y = species, fill = sex)) +
  ggridges::geom_density_ridges(alpha = 0.5)  

penguins %>%
  dplyr::select(species, bill_length_mm:body_mass_g) %>%
  pivot_longer(-species, names_to = "measurement", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = species, fill = species), size = 1.2, alpha = 0.2) +
  facet_wrap(vars(measurement), ncol = 2, scales = "free")
?pivot_longer
?vars
?aes

penguins %>%
  dplyr::select(species, bill_length_mm:body_mass_g) %>%
  pivot_longer(-species, names_to = "measurement", values_to = "value") %>%
  ggplot(aes(x = species, y = value)) +
  geom_boxplot(aes(color = species, fill = species), size = 1.2, alpha = 0.2) +
  facet_wrap(vars(measurement), ncol = 2, scales = "free")

penguins %>%
  dplyr::select(species, bill_length_mm:body_mass_g) %>%
  pivot_longer(-species, names_to = "measurement", values_to = "value") %>%
  ggplot(aes(x = value, y = species, fill = species)) +
  ggridges::geom_density_ridges() +
  facet_wrap(vars(measurement), scales = "free")

penguins %>%
  dplyr::select(species,sex, bill_length_mm:body_mass_g) %>%
  pivot_longer(-c(species, sex), names_to = "measurement", values_to = "value") %>%
  ggplot(aes(x = value, y = species, fill = sex)) +
  ggridges::geom_density_ridges(alpha = 0.4) +
  facet_wrap(vars(measurement), scales = "free")

penguins %>%
  ggplot(aes(
    x = bill_length_mm, y = bill_depth_mm,
    shape = species, color = species
  )) +
  geom_point()

penguins %>%
  ggplot(aes(
    x = bill_length_mm, y = bill_depth_mm,
    shape = species, color = species
  )) +
  geom_point(aes(size = body_mass_g))

penguins %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = lm) +
  geom_smooth(method = lm, aes(color = species))

penguins %>%
  group_by(species, island, sex) %>%
  ggplot(aes(
    x = body_mass_g, y = reorder(species, -body_mass_g),
    color = species
  )) +
  geom_jitter(position = position_jitter(seed = 2020, width = 0.2),
              alpha = 0.4,
              size = 2) +
  stat_summary(fun = mean, geom = "point", size = 5, alpha = 1)

install.packages("ggtext")
library(ggtext)
penguins %>%
  ggplot(aes(flipper_length_mm, body_mass_g, group = species)) +
  geom_point(aes(colour = species, shape = species), alpha = 0.7) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(
    title = "Penguin Size, Palmer Station LTER",
    subtitle = "Flipper length and body mass for <span style = 'color:darkorange;'>Adelie</span>, <span style = 'color:purple;'>Chinstrap</span> and <span style = 'color:cyan4;'>Gentoo</span> Penguins",
    x = "flipper length (mm)",
    y = "body mass (g)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    # text = element_text(family = "Futura"),
    # (I only have 'Light' )
    plot.title = element_text(size = 16),
    plot.subtitle = element_markdown(), # element_markdown from `ggtext` to parse the css in the subtitle
    plot.title.position = "plot",
    plot.caption = element_text(size = 8, colour = "grey50"),
    plot.caption.position = "plot"
  )

penguins %>%
  group_by(species) %>%
  summarise(
    count = n(),
    mean_body_mass = mean(body_mass_g),
    sd_body_mass = sd(body_mass_g)
  )

penguins %>%
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  geom_jitter()

stats::aov(formula = body_mass_g ~ species, data = penguins) %>%
  summary()

oneway.test(body_mass_g ~ species, data = penguins)

stats::aov(formula = body_mass_g ~ species, data = penguins) %>%
  TukeyHSD(which = "species") %>%
  broom::tidy()
#表格第一行instrap-Adelie 的 p-value = 0.916，
#没通过显著性检验；
#而Gentoo-Adelie 和 Gentoo-Chinstrap 
#他们的p-value都接近0，通过显著性检验，
#这和图中的结果是一致的。

install.packages("ggstatsplot")
library(ggstatsplot)
penguins %>%
  ggstatsplot::ggbetweenstats(
    x = species, # > 2 groups
    y = body_mass_g,
    type = "parametric",
    pairwise.comparisons = TRUE, 
    pairwise.display = "all",
    messages = FALSE,
    var.equal = FALSE
  )
kruskal.test(body_mass_g ~ species, data = penguins)

penguins %>%
  ggstatsplot::ggbetweenstats(
    x = species,
    y = body_mass_g,
    type = "nonparametric",
    mean.ci = TRUE,
    pairwise.comparisons = TRUE, # <<
    pairwise.display = "all",    # ns = only non-significant
    p.adjust.method = "fdr",     # <<
    messages = FALSE
  )

penguins %>%
  mutate(ratio = bill_length_mm / bill_depth_mm) %>%
  group_by(species) %>%
  summarise(mean = mean(ratio))

penguins %>%
  mutate(ratio = bill_length_mm / bill_depth_mm) %>%
  ggplot(aes(x = ratio, fill = species)) +
  ggridges::geom_density_ridges(aes(y = species))

scale_fun <- function(x) {  # 标准化的子函数
  (x - mean(x)) / sd(x)
}

d <- penguins %>%
  select(sex, species, bill_length_mm:body_mass_g) %>%
  mutate(
    across(where(is.numeric), scale_fun)
  ) %>%
  mutate(male = if_else(sex == "male", 1, 0))
d

penguins %>%
  select(sex, species, bill_length_mm:body_mass_g) %>%
  group_by(species) %>%
  mutate(
    across(where(is.numeric), scale_fun)
  ) %>%
  ungroup()

logit_mod1 <- glm(
  male ~ 1 + species + bill_length_mm + bill_depth_mm +
    flipper_length_mm + body_mass_g,
  data = d,
  family = binomial(link = "logit")
)

summary(logit_mod1)

library(margins)

logit_mod1_m <- logit_mod1 %>% 
  margins() %>% 
  summary() %>% 
  as_tibble()

logit_mod1_m

logit_mod1_m %>%
  ggplot(aes(
    x = reorder(factor, AME),
    y = AME, ymin = lower, ymax = upper
  )) +
  geom_hline(yintercept = 0, color = "gray80") +
  geom_pointrange() +
  coord_flip() +
  labs(x = NULL, y = "Average Marginal Effect")

install.packages("ggeffects")
library(ggeffects)
ggpredict(logit_mod1, terms = "bill_length_mm") 

library(ggeffects)
ggpredict(logit_mod1, "bill_depth_mm [all]") %>%
  plot()

library(brms)
brms_mod2 <- brm(
  male ~ 1 + bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g + (1 | species),
  data = d,
  family = binomial(link = "logit")
)

penguins %>%
  ggplot(aes(x = flipper_length_mm, y = bill_length_mm, color = species)) +
  geom_point()

install.packages("brms")

brms_mod3 <- brm(bill_length_mm ~ flipper_length_mm + (1|species),
                 data = penguins
)
penguins %>%
  group_by(species) %>%
  modelr::data_grid(flipper_length_mm) %>%
  tidybayes::add_fitted_draws(brms_mod3, n = 100) %>%
  ggplot() +
  geom_point(
    data = penguins,
    aes(flipper_length_mm, bill_length_mm, color = species, shape = species)
  ) +
  geom_line(aes(flipper_length_mm, .value, group = interaction(.draw, species), color = species), alpha = 0.1)
