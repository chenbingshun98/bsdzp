glimpse(mpg)

df <- mpg %>% 
  as_tibble() %>% 
  filter(class != "2seater",
         manufacturer %in% c("toyota", "volkswagen"))
df

df %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  facet_grid(vars(manufacturer), vars(class)) +
  ggtitle("这是我的标题") +
  labs(x = "x_displ", y = "y_hwy")

df %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  facet_grid(vars(manufacturer), vars(class)) +
  ggtitle("这是我的标题") +
  labs(x = "x_displ", y = "y_hwy") +
  theme(
    plot.background = element_rect(fill = "orange", color = "black", size = 10),
    plot.title = element_text(hjust = 1, color = "red", face = "italic"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  )

#坐标轴元素
df %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  facet_grid(vars(manufacturer), vars(class)) +
  ggtitle("这是我的标题") +
  labs(x = "x_displ", y = "y_hwy") +
  theme(
    axis.line = element_line(color = "orange", size = 2),
    axis.title = element_text(color = "red", face = "italic"),
    axis.ticks = element_line(color = "purple", size = 3),
    axis.text = element_text(color = "blue"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#面板元素
df %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  facet_grid(vars(manufacturer), vars(class)) +
  ggtitle("这是我的标题") +
  labs(x = "x_displ", y = "y_hwy") +
  theme(
    panel.background = element_rect(fill = "orange", color = "red"),
    panel.grid = element_line(color = "grey80", size = 0.5)
  )

df %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  facet_grid(vars(manufacturer), vars(class)) +
  ggtitle("这是我的标题") +
  labs(x = "x_displ", y = "y_hwy") +
  theme(
    panel.background = element_rect(fill = "orange"),
    panel.grid = element_line(color = "grey80", size = 0.5),
    panel.border = element_rect(color = "red", fill = NA)
  )

#图例元素
df %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  facet_grid(vars(manufacturer), vars(class)) +
  ggtitle("这是我的标题") +
  labs(x = "x_displ", y = "y_hwy") +
  theme(
    legend.background = element_rect(fill = "orange"),
    legend.title = element_text(color = "blue", size = 10),
    legend.key = element_rect(fill = "grey80"),
    legend.text = element_text(color = "red"),
    legend.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
    legend.position = "bottom"
  )

df %>%
  ggplot(aes(x = displ, y = hwy, color = factor(cyl))) +
  geom_point() +
  facet_grid(vars(manufacturer), vars(class)) +
  ggtitle("这是我的标题") +
  labs(x = "x_displ", y = "y_hwy") +
  theme(
    strip.background = element_rect(fill = "orange"),
    strip.text = element_text(color = "red"),
    panel.spacing = unit(0.3, "inch") # ,
    # strip.switch.pad.grid =
  )

diamonds %>%
  ggplot(aes(carat, price)) +
  geom_hex() +
  labs(title = "Diamond") +
  theme(
    axis.title.x = element_text(
      size = 30,
      color = "red",
      face = "bold",
      angle = 10
    ),
    legend.title = element_text(
      size = 25,
      color = "#ff6361",
      margin = margin(b = 5)
    ),
    plot.title = element_text(
      size = 35,
      face = "bold",
      color = "blue"
    )
  )

library(palmerpenguins)
penguins %>%
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point() +
  theme(
    axis.line.y = element_line(
      color = "black",
      size = 1.2,
      arrow = grid::arrow()
    ),
    axis.line.x = element_line(
      linetype = "dashed",
      color = "brown",
      size = 1.2
    ),
    axis.ticks = element_line(color = "red", size = 1.1),
    axis.ticks.length = unit(3, "mm"),
    panel.grid.major = element_line(
      color = "blue",
      size = 1.2
    ),
    panel.grid.minor = element_line(
      color = "#58508d",
      size = 1.2,
      linetype = "dotted"
    )
  )

penguins %>%
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point(aes(color = species)) +
  theme(
    legend.background = element_rect(
      fill = "#fff6c2",
      color = "black",
      linetype = "dashed"
    ),
    legend.key = element_rect(fill = "grey", color = "brown"),
    panel.background = element_rect(
      fill = "#005F59",
      color = "red", size = 3
    ),
    panel.border = element_rect(
      color = "black",
      fill = "transparent",
      linetype = "dashed", size = 3
    ),
    plot.background = element_rect(
      fill = "#a1dce9",
      color = "black",
      size = 1.3
    ),
    legend.position = "bottom"
  )

library(tidyverse)
set.seed(12)

d1 <- data.frame(x = rnorm(50, 10, 2), type = "Island #1")
d2 <- data.frame(x = rnorm(50, 18, 1.2), type = "Island #2")

dd <- bind_rows(d1, d2) %>%
  set_names(c("Height", "Location"))

head(dd)

dd %>%
  ggplot(aes(x = Height, fill = Location)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_fill_manual(values = c("green3", "turquoise3"))+
  theme_bw()+
  theme(
    axis.line = element_line(),
    legend.position = c(0.15,0.8),
    panel.border = element_blank(),
    panel.grid = element_blank()
  )+
  scale_y_continuous(expand = c(0,0),
                     breaks = c(10))
